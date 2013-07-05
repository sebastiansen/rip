(ns rip.test.validation
  (:use rip.validation
        clojure.test)
  (:require [taoensso.tower :as tower]))

(defvalidator user
  (field :name required)
  (field :age
         (parse-to :int)
         required
         (validates (min-val 10) {:message "must be 10 at least"}))
  (field :token (parse-to :uuid))
  (field :email (validates
                 (fn [email]
                   (boolean
                    (re-matches
                     #"^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$"
                     email)))
                 (fn [email] (format "%s is not a valid email" email))))
  (field :password)
  (field :password-confirmation)
  (field :description)
  (field :status)
  (validates
   (fn [{:keys [password password-confirmation]}]
     (= password password-confirmation))
   {:field   :password-confirmation
    :message "Password confirmation doesn't match confirmation"})
  (constraint :description (validates (max-size 20)))
  (required-fields [:status])
  (assoc-one
   :address
   (validator
    (field :street required)
    (assoc-one :city (validator (field :name required)
                                (field :code)
                                (validates
                                 (fn [v]
                                   (> (:code v) 10))
                                 "code error")))))
  (assoc-many :documents (validator (field :name required))))

(defvalidator post
  (field :id)
  (field :name (required {:if #(nil? (:id %))})))

(deftest test-if-for-required
  (are [a b] (= a b)
       (validate post {:name "" :id 3})
       {:valid? true, :value {:id 3}, :errors '()}
       (validate post {:name ""})
       {:valid? false, :value {}, :errors '({:field :name, :message "Can't be blank"})}))

(deftest test-schema-fields
  (are [a b] (= a b)
       (:valid? (validate user {:name "123" :age "123" :token "1-1-1-1-1"}))
       false

       (first (:errors (validate user {:name "123" :age "123" :token "1-1-1-1-" :status "hi"})))
       {:field :token :message "Invalid type"}

       (first (:errors (validate user {:age "123" :status "hi"})))
       {:field :name :message "Can't be blank"}

       (first (:errors (validate user {:age "123"})))
       {:field :status :message "Can't be blank"}

       (first (:errors (validate user {:age 3 :status "hi"})))
       {:field :age :message "must be 10 at least"}

       (first (:errors (validate user {:password "123" :password-confirmation "1234"})))
       {:field   :password-confirmation
        :message "Password confirmation doesn't match confirmation"}

       (first (:errors (validate user {:age 10 :name "123" :email "asdf" :status "hi"})))
       {:field   :email
        :message "asdf is not a valid email"}))

(deftest test-if-valid
  (are [a b] (= a b)
       (if-valid (validate user {:name "123" :age 123 :status "hi"})
                  [value]
                  value)
       {:name "123"  :status "hi" :age 123}

       (if-valid (validate user {:age 123 :token "1"})
                 [value errors]
                 value
                 errors)
       '({:field :status :message "Can't be blank"}
         {:field :token :message "Invalid type"}
         {:field :name :message "Can't be blank"})))

(deftest test-nested
  (are [a b] (= a b)
       (:errors (validate user {:address {:street ""}
                                :status "123"
                                :age "123"
                                :name "123"}))
       '({:field :address.street :message "Can't be blank"})
       (:errors (validate user {:address {:street "asd"
                                          :city {:hola "dsfds"
                                                 :code 1}}
                                :status "123"
                                :age "123"
                                :name "123"}))
       '({:field :address.city :message "code error"}
         {:field :address.city.name :message "Can't be blank"})
       (:errors (validate user {:documents [{:name "sdfs"} {:name ""}]
                                :status "123"
                                :age "123"
                                :name "123"}))
       '({:field :documents.1.name :message "Can't be blank"})))

;; (deftest test-other-validations
;;   (are [a b] (= a b)
;;        ()))

;; (defn query
;;   [[_ f] & [ent]]
;;   (let [{:keys [sql-str params]}
;;         (query-only
;;          (select (or ent "entity")
;;                  (where f)))]
;;     [sql-str (vec params)]))

;; (defn test-field-value
;;   [field value type & [alias]]
;;   (query
;;    (validate-field-value
;;     {} false field value (get-valid? type) "entity" alias)))

;; (defn test-field-op
;;   [field value type op & [alias]]
;;   (query
;;    (validate-field-op
;;     {} false field value (get-valid? type) "entity" op alias)))

;; (defn test-field-map
;;   [field map type & [alias]]
;;   (query
;;    (validate-field-map field map (get-valid? type) "entity" alias)))

;; (defn test-field
;;   [field value type & [alias]]
;;   (query
;;    (validate-field
;;     {} false field value (get-valid? type) "entity" alias)))

;; (deftest field-value
;;   (are [a b] (= a b)
;;        (test-field-value :valid false Boolean)
;;        ["SELECT \"entity\".* FROM \"entity\" WHERE ? = FALSE" ["entity.valid"]]

;;        (test-field-value :name "hello" String)
;;        ["SELECT \"entity\".* FROM \"entity\" WHERE ? = ?" ["entity.name" "hello"]]

;;        (test-field-value :number "3456456" Integer :num)
;;        ["SELECT \"entity\".* FROM \"entity\" WHERE ? = ?" ["entity.num" 3456456]]))

;; (deftest field-op
;;   (are [a b] (= a b)
;;        (test-field-op :number 1 Integer :$lt)
;;        ["SELECT \"entity\".* FROM \"entity\" WHERE (? < ?)" ["entity.number" 1]]))

;; (deftest field-map
;;   (are [a b] (= a b)
;;        (test-field-map :valid {:$or [true {:$ne false}]} Boolean :field)
;;        ["SELECT \"entity\".* FROM \"entity\" WHERE (? = TRUE OR (? IS NOT FALSE))" ["entity.field" "entity.field"]]

;;        (test-field-map :valid {:$or ["1" 2 {:$lt 4} {:$and [4 8]}]
;;                                :$lt 3
;;                                :$and [4 2]}
;;                        Integer
;;                        :field)
;;        ["SELECT \"entity\".* FROM \"entity\" WHERE (((? < ?) AND (? = ? AND ? = ?)) AND (((? = ? OR ? = ?) OR (? < ?)) OR (? = ? AND ? = ?)))" ["entity.field" 3 "entity.field" 4 "entity.field" 2 "entity.field" 1 "entity.field" 2 "entity.field" 4 "entity.field" 4 "entity.field" 8]]))

;; (deftest field-test
;;   (are [a b] (= a b)
;;        (test-field
;;         :a
;;         {:$lt  "3"
;;          :$and [4 2]
;;          :$or  [1 2 {:$lt "4"}
;;                 {:$and [4 8]}]}
;;         Integer)
;;        ["SELECT \"entity\".* FROM \"entity\" WHERE (FALSE AND (((? < ?) AND (? = ? AND ? = ?)) AND (((? = ? OR ? = ?) OR (? < ?)) OR (? = ? AND ? = ?))))" ["entity.a" "3" "entity.a" 4 "entity.a" 2 "entity.a" 1 "entity.a" 2 "entity.a" "4" "entity.a" 4 "entity.a" 8]]))

;; (testing "Nested entity predicate"
;;   (is (= (query
;;           (validate-filter "entity" {:x [Boolean] :y [String]} {:x false :y "select"} nil))
;;          ["SELECT \"entity\".* FROM \"entity\" WHERE (? = ? AND ? = FALSE)" ["entity.y" "select" "entity.x"]])))

;; (declare books users)

;; (defentity users
;;   (has-many books))

;; (defentity stores)

;; (defentity books
;;   (belongs-to users)
;;   (belongs-to stores)
;;   (belongs-to [:author users]))

;; (make-joins
;;  users
;;  (:name users)
;;  {:books [:inner {:stores :left}]})

;; {:c [:outer {:a :left}]
;;  :b :inner}

;; (let [{:keys [sql-str params]}
;;       (query-only
;;        (select (:query ((filter-validator
;;                          users
;;                          {:m [{:y [Integer :o]} :books]
;;                           :x [Integer :p]}
;;                          {:books :inner})
;;                         {:$or [{:m {:y "23"}} {:x "1111"}]}))
;;                ))]
;;   [sql-str (vec params)])

;; (query
;;  (select*
;;   (:query ((filter-validator
;;             users
;;             {:m [{:y [Integer :o]} :books]
;;              :x [Integer]}
;;             {:books :inner})
;;            {:$or [{:m {:y "23"}} {:x "1111"}]}))))

;; (test-field  :x "1111" (get-valid? Integer) "users")
