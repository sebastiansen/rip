(ns rip.test.core
  (:use rip.core
        clojure.test
        ring.middleware.params
        ring.middleware.keyword-params
        ring.mock.request))

(defroute home "/" :get [] "root")

(defresources users
  (index [] (path-for :users [:index] {:page 1}))
  (show [id] id)
  (member
   (PATCH {:name :activate :path "/activate"}
          [id]
          (str id " activated")))
  (nest-resources
   :user
   (resources
    :documents
    (index [user-id] (str user-id " documents"))
    (show [user-id id] (path-for :users [:documents :show] 1 2)))))

(defscope api
  "/api"
  (GET :home [] "api")
  (POST {:name :login :path "/login"} [] "login")
  (include
   "/version/:version"
   (GET :version [version] version)
   (make [version] (path-for :api [:make] version)))
  (nest users))

(defn wrap-body
  [handler]
  (h req (str (handler req))))

(defn wrap-exists
  [handler]
  (h [id :as req]
     (if (> (Integer/parseInt id) 2)
       {:status 404 :body "Not Found"}
       (handler req))))

(defn wrap-body-params
  [handler]
  (h req
     (handler (update-in req [:params :post] read-string))))

(defresources posts
  (wrap wrap-body {:name :response})
  (index [] [{:post {:title "title"}}])
  (show [id] {:post {:title "title"}})
  (change [id post] post)
  (GET {:name :test :path "/test"} []
       (name *current-action*))
  (before-wrap :response wrap-exists
               {:name :exists :actions [:show :change]})
  (after-wrap
   :exists
   #(-> % wrap-body-params wrap-keyword-params wrap-params)
   {:name :body-to-params
    :actions [:change]}))

(def app
  (routes-for
   home
   api
   users
   posts))

(deftest scope-and-route
  (are [req resp] (-> req app :body (= resp))
       (request :get "/")
       "root"
       (request :get "/api")
       "api"
       (request :post "/api/login")
       "login"))

(deftest resources-and-actions
  (are [req resp] (-> req app :body (= resp))
       (request :get "/users")
       "/users?page=1"
       (request :get "/users/1")
       "1"))

(deftest include-and-member
  (are [req resp] (-> req app :body (= resp))
       (request :patch "/users/1/activate")
       "1 activated"
       (request :get "/api/version/1")
       "1"
       (request :post "/api/version/2.0")
       "/api/version/2.0"))

(deftest nest-and-nest-resources
  (are [req resp] (-> req app :body (= resp))
       (request :get "/users/1/documents")
       "1 documents"
       (request :get "/users/1/documents/2")
       "/users/1/documents/2"
       (request :get "/api/users/1")
       "1"))

(deftest wrappers
  (are [req resp] (-> req app :body (= resp))
       (request :get "/posts/1")
       "{:post {:title \"title\"}}"
       (request :get "/posts/3")
       "Not Found"
       (request :put "/posts/1" {:post {:title "hola"}})
       (str {:title "hola"})))

(deftest current-action-test
  (are [req resp] (-> req app :body (= resp))
       (request :get "/posts/test")
       "test"))
