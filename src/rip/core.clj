(ns rip.core
  "Provides a resources abstraction."
  (:use [compojure.core :only (let-request make-route routes)]
        hiccup.util
        clout.core)
  (:require [clojure.string :as st]))

(def ^:dynamic *current-action* nil)
(def ^:dynamic *routes* {})

;;Routing

(defn scope*
  [name path]
  {:name       name
   :path       path
   :middleware []
   :type :scope})

(defmacro scope
  [name path & body]
  `(-> (scope* ~name ~path)
       ~@body))

(defmacro defscope
  [scope-name path & body]
  `(def ~scope-name
     (-> (scope ~(keyword (name scope-name)) ~path)
         ~@body)))

(defn route*
  [name path method handler]
  {:handler handler
   :name    name
   :method  method
   :path    path
   :type    :route})

(defmacro defroute
  [route-name path method & body]
  `(def ~route-name
     (route* ~(keyword (name route-name)) ~path ~method (h ~@body))))

(defmacro resources
  [res-name & body]
  `(-> (scope ~res-name ~(str "/" (name res-name)))
       ~@body))

(defmacro defresources
  [res & body]
  `(def ~res (resources ~(keyword (name res)) ~@body)))

(defn include*
  [scope path actions]
  (reduce
   (fn [scope [name action]]
     (assoc-in
      scope
      [:routes name]
      (assoc action
        :path
        (str path (:path action)))))
   scope
   actions))

(defmacro include
  [res path & body]
  `(include* ~res ~path (:routes (-> {:path ~path} ~@body))))

(defmacro member
  [res & body]
  `(include ~res "/:id" ~@body))

(defn nest
  [scope & routes]
  (reduce
   (fn [scope scope*]
     (assoc-in scope [:routes (:name scope*)]
               scope*))
   scope
   routes))

(defmacro nest-with
  [scope path & routes]
  `(include
    ~scope
    ~path
    (nest ~@routes)))

(defmacro nest-resources
  [scope item-name & routes]
  `(nest-with
    ~scope
    ~(str "/" item-name "-id")
    ~@routes))

;; Actions

(defmacro h
  "h is for handler, a macro for wrapping a function with bindings used by compojure's destructuring request forms"
  [bindings & body]
  `(fn [request#]
     (let-request [~bindings request#] ~@body)))

(defmacro defh
  "h is for handler, a macro for wrapping a function with bindings used by compojure's destructuring request forms"
  [name bindings & body]
  `(def ~name
     (fn [request#]
       (let-request [~bindings request#] ~@body))))

(defn action
  [scope opts method handler]
  (let [{:keys [name path] :as opts} (cond
                                      (map? opts) opts
                                      (keyword? opts) {:name opts})]
    (assoc-in
     scope
     [:routes name]
     (merge
      (route* name path method handler)
      opts))))

(defmacro GET
  [scope opts & body]
  `(action ~scope ~opts :get (h ~@body)))

(defmacro POST
  [scope opts & body]
  `(action ~scope ~opts :post (h ~@body)))

(defmacro PUT
  [scope opts & body]
  `(action ~scope ~opts :put (h ~@body)))

(defmacro DELETE
  [scope opts & body]
  `(action ~scope ~opts :delete (h ~@body)))

(defmacro HEAD
  [scope opts & body]
  `(action ~scope ~opts :head (h ~@body)))

(defmacro PATCH
  [scope opts & body]
  `(action ~scope ~opts :patch (h ~@body)))

(defmacro OPTIONS
  [scope opts & body]
  `(action ~scope ~opts :options (h ~@body)))

(defmacro ANY
  [scope opts & body]
  `(action ~scope ~opts :any (h ~@body)))

(defmacro index
  [scope & body]
  `(GET ~scope :index ~@body))

(defmacro make
  [scope & body]
  `(POST ~scope :make ~@body))

(defmacro show
  [scope & body]
  `(GET ~scope {:name :show :path "/:id"} ~@body))

(defmacro change
  [scope & body]
  `(PUT ~scope {:name :change :path "/:id"} ~@body))

(defmacro destroy
  [scope & body]
  `(DELETE ~scope {:name :destroy :path "/:id"} ~@body))

;; Other

(defn wrap
  [scope wrapper & [{:keys [name actions]}]]
  (update-in scope [:middleware] conj [name actions wrapper]))

(defn before-wrap
  [scope before wrapper & [{:keys [name actions]}]]
  (assoc scope
    :middleware
    (reduce
     (fn [middlewares [before-name :as middleware]]
       (if (= before-name before)
         (conj middlewares [name actions wrapper] middleware)
         (conj middlewares middleware)))
     []
     (:middleware scope))))

(defn after-wrap
  [scope after wrapper & [{:keys [name actions]}]]
  (assoc scope
    :middleware
    (reduce
     (fn [middlewares [after-name :as middleware]]
       (if (= after-name after)
         (conj middlewares middleware [name actions wrapper])
         (conj middlewares middleware)))
     []
     (:middleware scope))))

(defn- select-actions
  [{:keys [except only]} actions]
  (if except
    (let [except (set except)]
      (filter (fn [a] (not (contains? except a))) actions))
    (if only
      (let [except (set except)]
        (filter (fn [a] (contains? except a)) actions))
      (throw (Exception. "option map must contain :only or :exception")))))

(defn add-current-action-middleware
  [scope]
  (reduce
   (fn [scope [action]]
     (update-in scope
                [:routes action :handler]
                (fn [h]
                  (fn [r]
                    (binding [*current-action* action]
                      (h r))))))
   scope
   (:routes scope)))

(defn add-middleware
  [scope]
  (reduce
   (fn [scope [_ actions middleware]]
     (reduce
      (fn [scope action]
        (update-in scope [:routes action :handler] middleware))
      scope
      (if (empty? actions)
        (keys (:routes scope))
        (cond
         (map? actions)
         (select-actions actions (keys (:routes scope)))
         (fn? actions)
         (map first (filter (comp actions second) (:routes scope)))
         (sequential? actions)
         actions))))
   scope
   (reverse (:middleware scope))))

(defmulti route-for :type)

(defmethod route-for :route
  [{:keys [method path handler]}]
  (make-route method path handler))

(defmethod route-for :scope
  [scope]
  (apply
   routes
   (map
    (fn [route]
      (route-for
       (assoc route
         :path
         (str (:path scope) (:path route)))))
    (-> scope
        add-middleware
        add-current-action-middleware
        :routes
        vals))))

(defn wrap-routes
  [handler routes]
  (let [routes* (reduce
                 (fn [routes route]
                   (assoc routes (:name route) route))
                 {}
                 routes)]
    (fn [request]
      (binding [*routes* routes*]
        (handler request)))))

(defn routes-for
  [& routes*]
  (wrap-routes (apply routes (map route-for routes*)) routes*))

(defn- throwf [msg & args]
  (throw (Exception. (apply format msg args))))

(defn- ^{:skip-wiki true} route-arguments
  "returns the list of route arguments in a route"
  [route]
  (let [args (re-seq #"/(:([^\/\.]+)|\*)" route)]
    (map #(keyword (or (nth % 2) (second %))) args)))

(defn- path-url [url route-args]
  (let [url (if (vector? url) ;;handle complex routes
              (first url)
              url)
        route-arg-names (route-arguments url)]
    (when-not (every? (set (keys route-args)) route-arg-names)
      (throwf "Missing route-args %s" (vec (filter #(not (contains? route-args %)) route-arg-names))))
    (reduce (fn [path [k v]]
              (if (= k :*)
                (st/replace path "*" (str v))
                (st/replace path (str k) (str v))))
            url
            route-args)))

(defn- query-url [uri params]
  (str (url uri params)))

(defn compile-path
  [path params]
  (let [args (route-arguments path)
        path-params (take (count args) params)
        query-params (first (drop (count args) params))]
    (-> path
        (path-url (apply hash-map (interleave args params)))
        (query-url query-params))))

(declare link-for)

(defmulti link-for* (fn [value & args] (:type value)))

(defmethod link-for* :route
  [route & args]
  {:method (.toUpperCase (name (:method route)))
   :href   (compile-path (:path route) args)})

(defmethod link-for* :scope
  [scope [route & routes] & args]
  (if-let [route (get-in scope [:routes route])]
    (let [route (assoc route :path (str (:path scope) (:path route)))]
      (if routes
        (apply link-for* route (concat [routes] args))
        (apply link-for* route args)))
    (throw (Exception. "Path not found"))))

(defn link-for
  [route & args]
  (if-let [route (route *routes*)]
    (apply link-for* (cons route args))
    (throw (Exception. "Path not found"))))

(defn path-for
  [route & args]
  (:href (apply link-for (cons route args))))
