(ns rip.test.middleware
  (:use rip.core
        rip.validation
        rip.middleware
        ring.mock.request
        clojure.test)
  (:require [ring.util.response :as response]))

(defvalidator user-schema
  (field :name)
  (field :email required))

(defn add-links
  [{id :id :as user}]
  (assoc user :links {:show   (link-for :users [:show] id)
                      :update (link-for :users [:change] id)
                      :delete (link-for :users [:destroy] id)}))

(defhandler find-user [id] (if (= id 1) {:id id}))

(defn user-location [{{id :id} :body}] (path-for :users [:show] id))

(defresources users

  (wrap (with-json {:keywords? true}) {:except [:destroy]})

  (wrap (with-validate :user user-schema) {:only [:make :change]})

  (wrap (with-created user-location) {:only [:make]})

  (wrap (with-parse-params {:id :int}) {:only [:show :change :destroy]})

  (wrap (with-find-resource find-user :user*) {:only [:show :change :destroy]})

  (wrap (with-response add-links) {:only [:make :change :show]})

  (wrap (with-response) {:only [:index :destroy]})

  (index _ (map add-links [{:id 1}]))

  (show [user*] user*)

  (make [user] (assoc user :id 1))

  (change [id user] (assoc user :id id))

  (destroy [id] "ok"))

(def handler (routes-for users))

(deftest current-action-test
  (are [req resp] (-> req handler :body (= resp))

       (request :get "/users")
       "[{\"links\":{\"show\":{\"method\":\"GET\",\"href\":\"/users/1\"},\"update\":{\"method\":\"PUT\",\"href\":\"/users/1\"},\"delete\":{\"method\":\"DELETE\",\"href\":\"/users/1\"}},\"id\":1}]"

       (request :get "/users/1")
       "{\"links\":{\"show\":{\"method\":\"GET\",\"href\":\"/users/1\"},\"update\":{\"method\":\"PUT\",\"href\":\"/users/1\"},\"delete\":{\"method\":\"DELETE\",\"href\":\"/users/1\"}},\"id\":1}"

       (-> (request :put "/users/1")
           (content-type "application/json")
           (body "{\"user\": {\"email\": \"boom\"}}"))
       "{\"links\":{\"show\":{\"method\":\"GET\",\"href\":\"/users/1\"},\"update\":{\"method\":\"PUT\",\"href\":\"/users/1\"},\"delete\":{\"method\":\"DELETE\",\"href\":\"/users/1\"}},\"id\":1,\"email\":\"boom\"}"

       (-> (request :post "/users")
           (content-type "application/json")
           (body "{\"user\": {\"email\": \"boom\"}}"))
       "{\"links\":{\"show\":{\"method\":\"GET\",\"href\":\"/users/1\"},\"update\":{\"method\":\"PUT\",\"href\":\"/users/1\"},\"delete\":{\"method\":\"DELETE\",\"href\":\"/users/1\"}},\"id\":1,\"email\":\"boom\"}"

       (request :delete "/users/1")
       "ok"))

(-> (request :get "/users/1") handler)
