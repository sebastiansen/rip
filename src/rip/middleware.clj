(ns rip.middleware
  (:require [cheshire.core :as json]
            [clojure.data.xml :as xml])
  (:use rip.util
        rip.core
        rip.validation
        [ring.util.response :only (response)]
        ring.middleware.json)
  (:require [com.twinql.clojure.conneg :as conneg]))

(def ^{:dynamic true :doc "Default xml serialization tags"}
  *xml-tags* {:list :list :item :item})

(def ^{:dynamic true :doc "Default error responses"} *responses*
  {:not-found               {:status 404 :body "Not Found."}
   :forbidden               {:status 403 :body "Forbidden."}
   :unsupported-media-tpye  {:status 415 :body "Unsupported Media Type"}
   :request-entity-too-long {:status 413 :body "Request entity too large."}
   :not-acceptable          {:status 406 :body "Not Acceptable"}
   :precondition-failed     {:status 412 :body "Precondition Failed."}
   :unauthorized            {:status 401 :body "Unauthorized."}
   :not-modified            {:status 304 :body "Not Modified."}
   :bad-request             {:status 400 :body "Bad Request"}})

(defn- xml->hash-map
  "Transforms clojure.data.xml.Element to clojure maps.
   To keep an analog form to json transformations, a 'list' tag must be specified
   (default to :list in *xml-tags* global varible)."
  [{:keys [tag content]}]
  (if (= tag (*xml-tags* :list))
    (reduce (fn [m e]
              (let [[tag content] (first (xml->hash-map e))]
                (conj m content)))
            []
            content)
    {tag (if (= (type (first content)) clojure.data.xml.Element)
           (let [first-content (xml->hash-map (first content))]
             (if (vector? first-content)
               first-content
               (reduce (fn [m e]
                         (let [[tag content] (first (xml->hash-map e))]
                           (assoc m tag content)))
                       {}
                       content)))
           (first content))}))

(defn- map->xml
  [tag cont]
  (apply
   xml/element
   (concat [tag nil]
           (if (coll? cont)
             (if (map? cont)
               (map #(apply map->xml %) cont)
               [(apply xml/element
                       (concat [(*xml-tags* :list) nil]
                               (map (fn [val]
                                      (map->xml (*xml-tags* :item) val))
                                    cont)))])
             [(str cont)]))))

(defn gen-xml [value tag]
  (xml/emit-str (map->xml tag value)))

(defn parse-xml [s]
  (val (first (xml->hash-map (xml/parse-str s)))))

(defn- get-cause [e]
  (if-let [cause (.getCause e)]
    (get-cause cause)
    e))

(defn wrap-server-error
  "Wrap a handler such that exceptions are handled with the given error-function."
  [handler error-function]
  (fn [request]
    (try
      (handler request)
      (catch Exception ex
        (error-function (get-cause ex))))))

(defn wrap-request-entity-length
  "Validates the length of the request body."
  [handler body-max-length]
  (fn [request]
    (if (> (count (slurp (:body request))) body-max-length)
      (handler request)
      (*responses* :request-entity-too-long))))

(defn wrap-body-parser
  "Wrap the body to a clojure map, only for json and xml inputs.
   The result map is stored as :input in the :context map of the request."
  [handler xml-tags]
  (fn [request]
    (let [bstr   (slurp (:body request))
          entity (case (second (conneg/best-allowed-content-type
                                (get-in request
                                        [:headers "content-type"])
                                #{"application/*"}))
                   "json" (json/parse-string bstr true)
                   "xml"  (binding [*xml-tags* (merge xml-tags *xml-tags*)]
                            (parse-xml bstr))
                   :else bstr)]
      (handler (update-in request [:params] merge entity)))))

(defn wrap-accept-header
  "Checks the Accept header and validates based on the given supported content types.
   If the the content type is supported then the best type from the content negotiation is
   stored as :accept-content-type in the :context map of the request."
  [handler content-types & [default-type]]
  (fn [{headers :headers :as request}]
    (if-let [accept (headers "accept")]
      (if-let [[app format] (not-empty
                             (conneg/best-allowed-content-type
                              accept
                              content-types))]
        (handler
         (assoc-in request
                   [:context :accept-content-type]
                   (str app "/" format)))
        (*responses* :not-acceptable))
      (handler request))))

(defn with-if-match
  "Compares the etag from the result of calling the given function."
  [handler get-etag]
  (fn [request]
    (let [etag (get-in request [:headers "if-match"])]
      (if (and etag (or (= etag "*") (= etag (get-etag request))))
        (handler request)
        (*responses* :precondition-failed)))))

(defn wrap-supported-content-types
  "Validates the content type from the request."
  [handler & content-types]
  (fn [request]
    (if-let [type (request :content-type)]
      (if (and (some (fn [c-t] (.startsWith type c-t)) content-types)
               (contains? #{:post :put} (:request-method request)))
        (handler request)
        (*responses* :unsupported-media-tpye))
      (if (contains? #{:post :put} (:request-method request))
        (*responses* :unsupported-media-tpye)
        (handler request)))))

(defn with-find-resource
  "Given a handler to get the resource "
  [handler resource-name]
  (fn [h]
    (fn [r]
      (if-let [resource (handler r)]
        (h (assoc-in r [:params resource-name] resource))))))

(defn with-parse-params
  "Parses the request params giving a map of name -> type.
   Available types in parsers value on rip.validation namespace."
  [params]
  (fn [handler]
    (fn [request]
      (loop [[[param type] & rest] (into [] params)
             new-params {}]
        (if param
          (let [new-value  (try
                             ((parsers type) (get-in request [:params param]))
                             (catch Exception e :error))
                new-params (assoc new-params param new-value)]
            (if (not= new-value :error)
              (if rest
                (recur rest new-params)
                (handler (update-in request [:params] merge new-params)))))
          (handler request))))))

(loop [[[k v] & rest] (into [] {})] k)

(defn with-created
  "Adds the location header to the response, given a function
   that takes the response as argument."
  [url-fn]
  (fn [handler]
    (fn [request]
      (let [response (handler request)]
        (-> response
            (assoc :status 201)
            (assoc-in [:headers "Location"]
                      (url-fn response)))))))

(defn with-response
  "Takes the response from the following handler and wraps it to the body."
  [& [f]]
  (fn [handler]
    (fn [request]
      (response ((or f identity) (handler request))))))

(defn with-json
  "Wraps json for both request and response content-type."
  [& [opts]]
  (fn [handler]
    (-> handler
        (wrap-json-response opts)
        (wrap-json-body (merge {:keywords? true} opts))
        ;; wrap-json-params
        (wrap-supported-content-types "application/json"))))

(defn with-validate
  "Validates the body using a validator for the giving parameter.
   Optional response function takes the errors as argument."
  [param validator & [response]]
  (fn [handler]
    (fn [request]
      (if-valid
       (validate validator (get-in request [:body param]))
       [value errors]
       (handler (assoc-in request [:params param] value))
       (if response
         (response errors)
         {:body   errors
          :status 422})))))
