(ns rip.actions
  "Some useful predefined actions based on webmachine."
  (:use rip.middleware
        ring.middleware.multipart-params
        ring.middleware.multipart-params.byte-array
        ring.middleware.multipart-params.temp-file
        rip.core))

(let [methods-map {:get    "GET"
                   :post   "POST"
                   :put    "PUT"
                   :delete "DELETE"}]
  (defn action-link
    [method]
    {:url-handler (fn [url]
                    {:href   url
                     :method (method methods-map)})}))

(wrap-accept-header accepted-types)
(wrap-supported-content-type supported-types)

(defn wrap-entity-response-headers
  [handler {:keys [generate-etag-response last-modified-response]}]
  (-> handler
      (wrap-last-modified-header last-modified-response)
      (wrap-etag-header generate-etag-response)))

(defn wrap-entity-request-headers
  [handler {:keys [generate-etag last-modified]}]
  (-> handler
      wrap-if-modified-since
      wrap-if-none-match
      wrap-if-unmodified-since
      wrap-if-match
      (wrap-generate-etag generate-etag)
      (wrap-last-modified last-modified)))

(defn wrap-entity
  [handler find-resource {:keys [access-fn allow-fn]}]
  (-> handler
      (wrap-access (or access-fn (constantly true)))
      (wrap-entity find-resource)))

(defn wrap-auth
  [handler {:keys [allow-fn auth-handler]}]
  (-> handler
      (wrap-forbidden? allow-fn)
      (wrap-authorized? auth-handler)))

(defn show
  "Creates a show action to be passed to resources.
   The get-entity function should return a map representing the resource
   with optional keys etag and last-modified.
   Options:
     authorized?:            handler - authorization handler
     allowed?:               handler - checks permissions on this action
     access?:                hanlder - checks permissions on the resource
     content_types_provided: set -     matching types for the accept header
     generate-etag:          handler -
     last-modified:          handler -
     generate-etag-response: handler -
     last-modified-response: handler - "

  [wrap-show get-entity & [opts]]
  (memb :show
        :get
        (ok-entity-response
         (-> show-handler
             (wrap-entity-response-headers opts)
             (wrap-entity-request-headers opts)
             (wrap-entity opts)
             (wrap-auth opts)))
        (action-link :get)))

(defn index
  "Creates a index action to be passed to resources."
  [index-handler & [opts]]
  (coll :index
        :get
        (-> ok-entity-response
            index-handler
            (wrap-auth opts))))

(defn add
  "Creates an add action to be passed to resources."
  [add-handler & [opts]]
  (coll :add
        :post
        (-> created-response
            (wrap-location-header opts)
            add-handler
            (wrap-access opts)
            (wrap-validator opts)
            (wrap-auth opts))
        (action-link :put)))

(defn update
  "Creates an update action to be passed to resources."
  [update-handler & [opts]]
  (memb :update
        :put
        (-> entity-response
            wrap-etag-header
            wrap-last-modified
            update-handler
            (wrap-access opts)
            (wrap-validator opts)
            (wrap-entity-headers opts)
            (wrap-entity opts)
            (wrap-auth opts))
        (action-link :put)))

(defn destroy
  "Creates a destroy action to be passed to resources."
  [handler & [allow-fn]]
  (action memb :destroy :delete handler entity-response allow-fn))

(defn upload
  "Creates a generic action for multipart uploading.

   Visit http://mmcgrana.github.com/ring/ring.middleware.multipart-params.html
   for more info.

   Parameter store-type can be either :temp or :byte-array.

   Options map:
     allow-fn:    function to check permissions to the resource
     expires-in:  value passed to temp-file-store if store-type is :temp"
  [handler location-handler store-type & [{:keys [allow-fn expires-in]}]]
  (coll :upload
        :post
        (wrap handler
              (wrap-multipart-params
               {:store (case store-type
                         :temp (if expires-in
                                 (temp-file-store expires-in)
                                 (temp-file-store))
                         :byte-array (byte-array-store))}))
        entity-response
        allow-fn))
