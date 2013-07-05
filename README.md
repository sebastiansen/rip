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

## Named Routes
The main idea behind RIP's routing system is a thin layer over compojure that provides named routes to facilitate their construction, allowing more control over a set of routes, 
and include some reverse routing functionality.
```clojure
;; Single named route
(defroute home :get "/" [] "welcome")

;; Named nested routes
(defscope api
  "/api"

  ;; Actions can be defined using GET, POST, etc.
  ;; A name must be provided for wrappers and reverse routing
  ;; GET /api
  (GET :home [] ...)

  ;; Use a map instead of a keyword to specify a path
  ;; GET /api/about
  (GET {:name :about :path "/about"} [] ...)

  ;; Include other actions with a default path
  (include
   "/v1"

   ;; GET /api/v1
   (GET :v1 [] ...))

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
    (index [] ...)
    (change [id] ...)

    ;; Include other actions with default /:id path
    ;; Same as using (include "/:id")
    (member
     ;; PATCH /api/users/:id/activate
     (PATCH {:name :activate :path "/activate"} [id] ...))

    ;; Nest resources passing the member key
    (nest-resources
     :user
     (resources
      :documents

      ;; GET /api/users/:user-id/documents
      (index [user-id] ...))))))

;; Reverse Routing
(defroute paths "/paths" :get
  []
  (path-for :home)
  ;; => "/"

  (path-for :api [:v1])
  ;; => "/api/v1"
  
  (path-for :api [:users :show] 1)
  ;; => "/api/users/1"

  (path-for :api [:users :documents :index] 1 {:page 10})
  ;; => "/api/users/1/documents?page=10"

  (link-for :api [:users :show] 1)
  ;; => {:href "/api/users/1" :method "GET"}
  
  (url-for :api [:about])
  ;; => "http://{hostname}/api/about"
)

;; Generate a single handler and add a middleware for path-for, link-for and url-for usage.
(routes-for home api paths)

;; path-for, link-for and url-for work only in a handler of a route compiled using routes-for.
```
## Wrappers
Use the wrap function to add middleware to the actions inside a scope. Every use of wrap will be stacked and later applied when the scope is compiled to a handler. 
If necessary, a before-wrap and after-wrap function are provided for middleware applying order.
```clojure
(defresources posts
  ;; adds a named middleware to all the actions defined in the scope
  (wrap -wrapper- {:name :response})
  
  (show [id] ...)
  (change [id post] ...)
  
  ;; adds middleware before the :response wrapper
  (before-wrap :response -wrapper-
               {:name :exists :actions [:show :change]})
               
  ;; adds middleware after the :exists wrapper
  (after-wrap :exists -wrapper-
              {:name :body-to-params :actions [:change]}))
```
## Validators
The concept of validator is an abstraction for validating clojure's maps. They are intended to represent most of the validation process of an application. 
They are very extendable, composable and can be used for APIs or web forms validations.
```clojure
;; Define a validator
(defvalidator user
  ;; Add a field
  (field :name)
  
  ;; Specify the parser to be applied to a field with the parse-to function.
  ;; In case the parser fails, a type error will be added to the validation.
  ;; Default types :int :double :float :boolean :long :bigint :bigdecimal :uuid
  (field :age (parse-to :int))
  
  ;; Custom parsers can also be passed to the parse-to function
  (field :birthday (parse-to #(java.sql.Date/valueOf %)))
  
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
  (required-fields [:password :password-confirmation])
  
  ;;Nest validators
  (assoc-one :address
    (validator 
      (field :street))
    {:required true})
    
  (assoc-many :documents
    (validator
      (field :name))))

;; Validate using the validate function
(def validation (validate user {:email "sebastian@rip.com"}))
;; => {:valid? false 
;;     :value  {:email "sebastian@rip.com"} 
;;     :errors [{:field :password :message "Can't be blank"}]}

;; Minimize the validation using if-valid for binding and if-else expressions
(if-valid validation
  ;; bind a [value errors] vector using any destructuring you want.
  [{name :name} _]
  ;; if else expressions
  "ok"
  "error")
```
