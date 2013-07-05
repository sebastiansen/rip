(ns rip.core
  "Provides a set of abstraction for named routes definitions"
  (:use [compojure.core :only (let-request make-route routes)]
        hiccup.util
        clout.core)
  (:require [clojure.string :as st]))

(def ^:dynamic *current-action* nil)
(def ^:dynamic *routes* {})
(declare ^{:dynamic true} *request*)

;;Routing

(defmacro h
  "A macro for creating a function with bindings used by compojure's destructuring."
  [bindings & body]
  `(fn [request#]
     (let-request [~bindings request#] ~@body)))

(defmacro defhandler
  "Define a handler using the h macro."
  [name bindings & body]
  `(def ~name
     (fn [request#]
       (let-request [~bindings request#] ~@body))))

(defn route*
  [name path method handler]
  {:handler handler
   :name    name
   :method  method
   :path    path
   :type    :route})

(defmacro defroute
  "Define a named route using compojure's destructuring for the arguments.
Usage:
  (defroute root :get \"/\" [] ...)"
  [route-name method path args & body]
  `(def ~route-name
     (route* ~(keyword (name route-name)) ~path ~method (h ~args ~@body))))

(defn scope*
  [name path]
  {:name       name
   :path       path
   :routes     {}
   :middleware []
   :type       :scope})

(defmacro scope
  "Create a named set of routes with a prefixed path.
Applies the -> macro to the body."
  [name path & body]
  `(-> (scope* ~name ~path)
       ~@body))

(defmacro defscope
  "Define a scope binded to a symbol of the same name.
Applies the -> macro to the body."
  [scope-name path & body]
  `(def ~scope-name
     (-> (scope ~(keyword (name scope-name)) ~path)
         ~@body)))

(defmacro resources
  "Create a scope with path \"/{scope-name}\"."
  [res-name & body]
  `(-> (scope ~res-name ~(str "/" (name res-name)))
       ~@body))

(defmacro defresources
  "Define a resources."
  [res & body]
  `(def ~res (resources ~(keyword (name res)) ~@body)))

(defn action
  "Create a nested route for a scope.
Options can be a keyword for the name of the action or a map with :name and :path."
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
  "Create a nested route for a scope with GET method.
Wraps a handler using the h macro.
Options can be a keyword for the name of the action or a map with :name and :path"
  [scope opts args & body]
  `(action ~scope ~opts :get (h ~args ~@body)))

(defmacro POST
  "Create a nested route for a scope with POST method.
Wraps a handler using the h macro.
Options can be a keyword for the name of the action or a map with :name and :path"
  [scope opts args & body]
  `(action ~scope ~opts :post (h ~args ~@body)))

(defmacro PUT
  "Create a nested route for a scope with PUT method.
Wraps a handler using the h macro.
Options can be a keyword for the name of the action or a map with :name and :path"
  [scope opts args & body]
  `(action ~scope ~opts :put (h ~args ~@body)))

(defmacro DELETE
  "Create a nested route for a scope with DELETE method.
Wraps a handler using the h macro.
Options can be a keyword for the name of the action or a map with :name and :path"
  [scope opts args & body]
  `(action ~scope ~opts :delete (h ~args ~@body)))

(defmacro HEAD
  "Create a nested route for a scope with HEAD method.
Wraps a handler using the h macro.
Options can be a keyword for the name of the action or a map with :name and :path"
  [scope opts args & body]
  `(action ~scope ~opts :head (h ~args ~@body)))

(defmacro PATCH
  "Create a nested route for a scope with PATCH method.
Wraps a handler using the h macro.
Options can be a keyword for the name of the action or a map with :name and :path"
  [scope opts args & body]
  `(action ~scope ~opts :patch (h ~args ~@body)))

(defmacro OPTIONS
  "Create a nested route for a scope with OPTIONS method.
Wraps a handler using the h macro.
Options can be a keyword for the name of the action or a map with :name and :path"
  [scope opts args & body]
  `(action ~scope ~opts :options (h ~args ~@body)))

(defmacro ANY
  "Create a nested route for a scope with ANY method.
Wraps a handler using the h macro.
Options can be a keyword for the name of the action or a map with :name and :path"
  [scope opts args & body]
  `(action ~scope ~opts :any (h ~args ~@body)))

(defmacro index
  "Simplifies (GET :index ...)"
  [scope args & body]
  `(GET ~scope :index ~args ~@body))

(defmacro make
  "Simplifies (POST :make ...)"
  [scope args & body]
  `(POST ~scope :make ~args ~@body))

(defmacro show
  "Simplifies (GET {:name :show :path \"/:id\"} ...)"
  [scope args & body]
  `(GET ~scope {:name :show :path "/:id"} ~args ~@body))

(defmacro change
  "Simplifies (PUT {:name :change :path \"/:id\"} ...)"
  [scope args & body]
  `(PUT ~scope {:name :change :path "/:id"} ~args ~@body))

(defmacro destroy
  "Simplifies (DELETE {:name :destroy :path \"/:id\"} ...)"
  [scope args & body]
  `(DELETE ~scope {:name :destroy :path "/:id"} ~args ~@body))

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
  "Adds a prefixed path to routes inside a scope.
Applies the -> macro to the body.
Usage:
  (scope :users
    \"/users\"
    (include
      \"/:id\"
      (GET :show ...)))"
  [res path & body]
  `(include* ~res ~path (:routes (-> {:path ~path} ~@body))))

(defmacro member
  "Same as using (include \"/:id\" ...)"
  [res & body]
  `(include ~res "/:id" ~@body))

(defn nest
  "Nests scopes.
Usage:
  (scope :api
    \"/api\"
    (nest
      (scope :users
        \"/users\"
        (GET :index [] ...))
      (resources :posts
        (GET :index [] ...))))"
  [scope & routes]
  (reduce
   (fn [scope scope*]
     (assoc-in scope [:routes (:name scope*)]
               scope*))
   scope
   routes))

(defmacro nest-with
  "Nests scopes with a prefixed path.
Usage:
  (scope :users
    \"/users\"
    (nest-with \"/:id\"
      (scope :documents
        \"/documents\"
        (GET :index [] ...))))"
  [scope path & routes]
  `(include
    ~scope
    ~path
    (nest ~@routes)))

(defmacro nest-resources
  "Same as usig (nest-with \"/:id\" ...) "
  [scope item-name & routes]
  `(nest-with
    ~scope
    ~(str "/" item-name "-id")
    ~@routes))

;; Wrap

(defn wrap
  "Adds middleware to a scope.
Every use of wrap will be stacked and later applied when the scope is compiled to a handler.
Options:
  name:    Useful for before-wrap and after-wrap
  actions: A list of actions names or a filter function.
Usage:
  (resources :users
    (wrap wrap-json)
    (wrap (fn [handler] (fn [request] ...))
          {:name :exists :actions [:show :change :destroy]}))"
  [scope wrapper & [{:keys [name actions]}]]
  (update-in scope [:middleware] conj [name actions wrapper]))

(defn before-wrap
  "Similar to wrap, but it adds middleware to a scope before a specific wrap.
Usage:
  (resources :users
   (wrap wrap-json {:name :json})
   (before-wrap :json wrap-auth))"
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
  "Similar to wrap, but it adds middleware to a scope after a specific wrap.
Usage:
  (resources :users
   (wrap wrap-json {:name :json})
   (wrap (fn [handler] (fn [request] ...)))
   (after-wrap :json wrap-auth))"
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

(defn- add-current-action-middleware
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

(defn- add-middleware
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

(defmulti route-for
  "Compiles a scope or a defroute definition to a compojure's handler"
  :type)

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

(defn- wrap-routes
  [handler routes]
  (let [routes* (reduce
                 (fn [routes route]
                   (assoc routes (:name route) route))
                 {}
                 routes)]
    (fn [request]
      (binding [*routes* routes*]
        (handler request)))))

(defn wrap-request
  "Wrap the request map in the *request* symbol"
  [h]
  (fn [r]
    (binding [*request* r]
      (h r))))

(defn routes-for
  "Compiles scopes and defroute definitions into a single compojure's handler.
Adds a middleware for path-for and link-for usage, and binds the request to *request* for url-for usage."
  [& routes*]
  (-> (apply routes (map route-for routes*))
      (wrap-routes routes*)
      wrap-request))

(defn- throwf [msg & args]
  (throw (Exception. (apply format msg args))))

(defn- route-arguments
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

(defn- compile-path
  [path params]
  (let [args (route-arguments path)
        path-params (take (count args) params)
        query-params (first (drop (count args) params))]
    (-> path
        (path-url (apply hash-map (interleave args params)))
        (query-url query-params))))

(defmulti ^:private link-for* (fn [value & args] (:type value)))

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
  "Generate a map with href and method for a route.
If route is a scope, a path of actions must be passed.
Arguments depend if path contians parameter.
Usage:
  (link-for :home)
  ;; => {:href \"/\" :method \"GET\"}

  (link-for :users [:show] 123)
  ;; => {:href \"/users/123\" :method \"GET\"}

  (link-for :users [:index] {:page 15})
  ;; => {:href \"/users?page=15\" :method \"GET\"}"
  [route & args]
  (if-let [route (route *routes*)]
    (apply link-for* (cons route args))
    (throw (Exception. "Path not found"))))

(defn path-for
  "Generate a string with the path for a route.
If route is a scope, a path of actions must be passed.
Arguments depend if path contians parameter.
Usage:
  (path-for :home)
  ;; => \"/\"

  (path-for :users [:show] 123)
  ;; => \"/users/123\"

  (path-for :users [:index] {:page 15})
  ;; => \"/users?page=15\""
  [route & args]
  (:href (apply link-for (cons route args))))

(defn url-for
  "Generate a string with the absolute url for a route.
If route is a scope, a path of actions must be passed.
Arguments depend if path contians parameter.
Usage:
  (url-for :home)
  ;; => \"http://{hostname}/\"

  (url-for :users [:show] 123)
  ;; => \"http://{hostname}/users/123\"

  (url-for :users [:index] {:page 15})
  ;; => \"http://{hostname}/users?page=15\""
  [route & args]
  (str
   (name (:scheme *request*))
   "://"
   (get-in *request* [:headers "host"])
   (apply path-for route args)))
