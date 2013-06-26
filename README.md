RIP
===
[![Build Status](https://travis-ci.org/sebastiansen/rip.png?branch=master)](https://travis-ci.org/sebastiansen/rip)

REST in Peace is a library for RESTful APIs built on top of compojure with some korma utilities.

## Installation
### Not currently on Clojars, clone the project instead and use lein install
Add the following dependency to your `project.clj` file:

```clj
[rip "0.0.10"]
```

## Routes
```clojure
;; Single named route
(defroute home "/" :get [] "welcome")

;; Named nested routes
(defscope api
  "/api"

  ;; Routes can be defined using GET*, POST*, etc.
  ;; A name must be provided for wrappers and reverse routing
  ;; GET /api
  (GET* :home [] "api home")

  ;; Use a map instead of a keyword to specify a path
  ;; GET /api/about
  (GET* {:name :about :path "/about"} [] "a REST API for the win")

  ;; Include other actions with a default path
  (include
   "/paths"

   ;; Obtain paths for actions
   (GET* :paths []
         (str (path-for :api [:home])
              " and "
              (path-for :api [:about]))
         ;;=> /api and /api/about
         ))

  ;; Nest other scopes or resources
  (nest

   ;; Resources are scopes with path "/{resource-name}", check also 'defresource'
   (resources
    :users
    ;; Rip includes some default actions like:
    ;;   index   => GET    /
    ;;   make    => POST   /
    ;;   show    => GET    /:id
    ;;   change  => PUT    /:id
    ;;   destroy => DELETE /:id
    ;; Examples:
    (index [] "users")
    (change [id] (str "user " id " changed"))

    ;; Obtain links
    (show [id]
          (str {:links
                {:change (link-for :api [:users :change] id)}}))
    ;; => {:links {:change {:method "PUT" :href "/api/users/{user id}"}}}

    ;; Include other actions with default /:id path
    ;; Same as using (include "/:id")
    (member
     ;; PATCH /api/users/:id/activate
     (PATCH* {:name :activate :path "/activate"} [id] (str "user " id " activated")))

    ;; Nest resources passing the member key
    (nest-resources
     :user
     (resources
      :documents

      ;; GET /api/users/:user-id/documents
      (index [user-id] (str "documents for user " user-id)))))))

;; Generate handler
(defroutes app
  (routes-for
  ;; Generates a compojure route, and also will bind *routes*
  ;; for path-for and link-for functions
    home
    api))
```
## Validations
```clojure
;; Define a validator
(defvalidator user
  ;; Add a field
  (field :name)
  
  ;; Specify the type of a field with the type-of function
  ;; In case the parser fails, a type error will be added to the validation.
  ;; Default types :int :double :float :boolean :long :bigint :bigdecimal :uuid
  (field :age (type-of :int))
  
  ;; Custom parsers can also be passed to the type-of
  (field :birthday (type-of #(java.sql.Date/valueOf %)))
  
  ;; Add constraints like 'required' or custom validations using the validates function
  ;; validates requires a predicate function and an optional message
  (field :email 
         required 
         (validates (fn [email] (boolean (re-matches #".+\@.+\..+" email)))
         "invalid format"))
  
  ;; Message can also be a map in case you want to specify an error code for your API
  (field :password required (validates (min-length 6) {:code 123 :message "too short"}))  
  
  (field :password-confirmation required)
  
  ;; Include global validations
  (validates
   (fn [user] (= (:password user) (:password-confirmation user)))
   "Password confirmation doesn't match confirmation")
  
  ;; Add more constraints to fields
  (constraint :name required)
  
  ;; Set required fields
  (required-fields [:password :password-confirmation]))
   
;; Validate using the if-valid  macro to simplify evaluation
(if-valid (validate user {:email "sebastian@rip.com"})
  ;; bindings 
  [value errors]
  ;; if else expressions
  "ok"
  "error")
```
