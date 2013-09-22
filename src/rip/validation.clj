(ns rip.validation
  "Functions for validators"
  (:require [clojure.string :as st]
            [taoensso.tower :as tower]))

(declare validate)

(def ^:dynamic *scope* nil)

;; Collections and strings constraints

(defn min-size [min] (fn [val] (>= (count val) min)))
(defn max-size [max] (fn [val] (<= (count val) max)))
(defn range-size [min max] (fn [val] (or (>= (count val) min) (<= (count val) max))))

;; Numbers constraints

(defn min-val [min] (fn [val] (>= val min)))
(defn max-val [max] (fn [val] (<= val max)))
(defn range-val [min max] (fn [val] (or (>= val min) (<= val max))))

(defn blank?
  "Checks if a value is nil or empty if string"
  [value]
  (or (nil? value)
      (and (string? value) (empty? value))))

(def parsers
  {:int        #(if % (Integer/parseInt (str %)))
   :double     #(if % (Double/parseDouble (str %)))
   :float      #(if % (Float/parseFloat (str %)))
   :boolean    #(Boolean/parseBoolean (str %))
   :long       #(if % (Long/parseLong (str %)))
   :bigint     #(if % (BigInteger. (str %)))
   :bigdecimal #(if % (BigDecimal. (str %)))
   :uuid       #(if % (java.util.UUID/fromString (str %)))})

(defn- make-parser
  [parser]
  (fn [value]
    (try (parser value) (catch Exception e :error))))

;; Validators

(tower/merge-config!
 {:dictionary
  {:en
   {:errors
    {:messages
     {:required     "Can't be blank"
      :invalid-type "Invalid type"
      :constraints  "Invalid"}}}}})

(defn- make-error
  [error value default]
  (cond
   (string? error) {:message error}
   (map? error) error
   (fn? error) (let [error (error value)]
                 (make-error error value default))
   :else default))

(defn validator*
  []
  {:fields      {}
   :assocs      {}
   :constraints []})

(defmacro validator
  "Create a validator and apply the -> macro to the body."
  [ & body]
  `(-> (validator*)
       ~@body))

(defmacro defvalidator
  "Define a validator"
  [name & body]
  `(def ~name (validator ~@body)))

(defn field*
  [validator name field]
  (assoc-in validator [:fields name] field))

(defn make-field
  [name]
  {:name        name
   :parser      identity
   :required?   false
   :constraints []})

(defmacro field
  "Add a field to a validator. Applies the -> macro to de body.
Usage:
  (validator
    (field :name ...))"
  [validator name & body]
  `(field* ~validator ~name
           (-> (make-field ~name)
               ~@body)))

(defn validates
  "Add a contraint to a validator or a field.
A error can be provided with a string, a map or a function."
  [validator pred & [error]]
  (let [error (fn [value]
                (make-error
                 error
                 value
                 (tower/t :errors.messages/constraints)))]
    (update-in validator [:constraints] conj [pred error])))

(defmacro constraint
  "Make changes to a validator's field applying the -> macro to the body."
  [validator field & body]
  `(update-in ~validator
              [:fields ~field]
              (fn [field#]
                (-> field#
                    ~@body))))

(defn parse-to
  "Set the parsing type of a field.
The parser can be one of the following keywords:
  :int, :double, :float, :boolean, :long, :bigint, :bigdecimal, :uuid.
Optionally a parsing function can be passed.
If parsing fails, an invalid type error will be added when validated."
  [field type]
  (assoc field
    :parser
    (make-parser
     (cond
      (keyword? type) (parsers type)
      (fn? type) type))))

(defn required
  "Make a field required."
  [field & [{:keys [if]}]]
  (assoc field
    :required? true
    :required-if if))

(defn required-fields
  "Apply the 'required' function to a set of fields."
  [validator fields]
  (reduce
   (fn [validator field]
     (constraint validator field required))
   validator
   fields))

(defn assoc-one
  "Associate a nested validator for a map. Options map will be merged with the association map.
Some options: :required?
Usage:
  (defvalidator user
    (assoc-one :address
      (validator
        (field :street))
      {:required? true}))
  ;; Valid over a map like {:address {:street \"streetname\"}}"
  [validator name validator* & [opts]]
  (assoc-in
   validator
   [:assocs name]
   (merge {:rel       :one
           :validator validator*
           :required? false}
          opts)))

(defn assoc-many
  "Associate a nested validator for a list. Options map will be merged with the association map.
Some options: required?
Usage:
  (defvalidator user
    (assoc-many :documents
      (validator
        (field :name))
      {:required? true}))
  ;; Valid over a map like {:documents [{:name \"filename\"}]}"
  [validator name validator* & [opts]]
  (assoc-in
   validator
   [:assocs name]
   (merge
    {:rel       :many
     :validator validator*
     :required? false}
    opts)))

(defn- merge-validations
  [val1 val2 & [multi?]]
  {:valid? (and (:valid? val1) (:valid? val2))
   :value  ((if multi? conj merge) (:value val1) (:value val2))
   :errors (concat (:errors val1) (:errors val2))})

(defn- default-error
  [error value type & [field]]
  (make-error
   error
   value
   (let [error {:message
                (tower/t
                 (keyword (str "errors.messages/" (name type))))}]
     (if field
       (assoc error :field field)
       error))))

(defn- validate-field
  [{:keys [required-error invalid-type-error]}
   {:keys [name parser required? required-if constraints] :as field}
   value
   record]
  (let [parsed-value (if parser (parser value) value)]
    (if (= parsed-value :error)
      {:valid? false
       :errors [(default-error invalid-type-error value :invalid-type name)]}
      (if (nil? value)
        (if required?
          (if required-if
            (if (required-if record)
              {:valid? false
               :errors [(default-error required-error value :required name)]}
              {:valid? true :value parsed-value})
            {:valid? false
             :errors [(default-error required-error value :required name)]})
          {:valid? true :value parsed-value})
        (reduce
         (fn [validation [pred error]]
           (if (pred parsed-value)
             validation
             (-> validation
                 (assoc :valid? false)
                 (update-in [:errors] conj (assoc (error parsed-value)
                                             :field name)))))
         {:valid? true :value parsed-value :errors []}
         constraints)))))

(defn- validate-fields
  [validator value]
  (reduce
   (fn [validation [field-name field]]
     (let [field-value (field-name value)
           {:keys [value errors valid?]}
           (validate-field validator field field-value value)]
       (if valid?
         (if (nil? value)
           validation
           (update-in validation [:value] assoc field-name value))
         (-> validation
             (assoc :valid? false)
             (update-in
              [:errors]
              concat
              errors)))))
   {:value {} :valid? true :errors []}
   (:fields validator)))

(defn- nest-errors
  [validation rel-name]
  (assoc validation
    :errors
    (map
     (fn [error]
       (let [field (keyword
                    (if-let [field (:field error)]
                      (str rel-name "." (name field))
                      rel-name))]
         (assoc error :field field)))
     (:errors validation))))

(defn- validate-many
  [validator value rel-name]
  (let [[_ validation] (reduce
                        (fn [[i validation] value]
                          [(inc i)
                           (merge-validations
                            validation
                            (-> (validate validator value)
                                (nest-errors
                                 (str (name rel-name) "." i)))
                            true)])
                        [0 {:valid? true :value [] :errors []}]
                        value)]
    (assoc validation :value {rel-name (:value validation)})))

(defn- validate-one
  [validator value rel-name]
  (-> (validate validator value)
      (nest-errors (name rel-name))
      (update-in [:value]
                 (fn [value] {rel-name value}))))

(defn- invalid-nested-type-error
  [rel-name]
  {:valid? false
   :value  {}
   :errors [{:field   rel-name
             :message (tower/t :errors.messages/invalid-type)}]})

(defn- validate-nested
  [validation rel-name rel validator value]
  (-> (case rel
        :one
        (if (map? value)
          (validate-one validator value rel-name)
          (invalid-nested-type-error rel-name))
        :many
        (if (sequential? value)
          (validate-many validator value rel-name)
          (invalid-nested-type-error rel-name)))
      (merge-validations validation)))

(defn- validate-nested-fields
  [{:keys [assocs required-error invalid-type-error]} value]
  (reduce
   (fn [validation [name {:keys [rel validator required?]}]]
     (let [nested (value name)]
       (if (and (coll? nested) (not-empty nested))
         (validate-nested validation name rel validator nested)
         (if required?
           (-> validation
               (assoc :valid? false)
               (update-in
                [:errors]
                conj
                (default-error required-error value :required name)))
           validation))))
   {:valid? true :value {} :errors []}
   assocs))

;; Validation

(defn validate
  "Validate a map over a validator definition.
  Returns a map with values:
    - valid?: True if validation passed, false otherwise.
    - value:  Resulting value with parser applied.
    - errors: A list of errors."
  [validator value]
  (let [validation (merge-validations
                    (validate-fields validator value)
                    (validate-nested-fields validator value))]
    (-> (reduce
         (fn [validation [pred error]]
           (if (pred value)
             validation
             (-> validation
                 (assoc :valid? false)
                 (update-in [:errors] conj (error value)))))
         validation
         (:constraints validator)))))

(defmacro if-valid
  "Simplifies evaluation of a validation result,
  with bindings [valid-value errors] and if-else expressions
  Usage:
    (if-valid (validate <validator> <value>)
      [valid-value errors]
      then
      else)"
  ([result bindings then]
     `(if-valid
       ~result
       ~bindings
       ~then
       nil))
  ([result bindings then else & oldform]
     `(let [~bindings [(:value ~result) (:errors ~result)]]
        (if (:valid? ~result)
          ~then
          ~else))))
