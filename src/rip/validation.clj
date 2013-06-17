(ns rip.validation
  "Validation functions for body input and filters for collection queries"
  (:import rip.RipException)
  (:use korma.sql.fns
        korma.core
        korma.db
        rip.db)
  (:require [clojure.string :as st]))

(def ^:dynamic *invalid-type-error* {:message "Invalid format"})

(def ^:dynamic *required-error* {:message "Required"})

(def ^:dynamic *default-messages* {})

;; Collections and strings constraints

(defn min-size [min] (fn [val] (>= (count val) min)))
(defn max-size [max] (fn [val] (<= (count val) max)))
(defn range-size [min max] (fn [val] (or (>= (count val) min) (<= (count val) max))))

;; Numbers constraints

(defn min-val [min] (fn [val] (>= val min)))
(defn max-val [max] (fn [val] (<= val max)))
(defn range-val [min max] (fn [val] (or (>= val min) (<= val max))))

(defn blank?
  [value]
  (or (nil? value)
      (and (string? value) (empty? value))))

(defn validator*
  []
  {:fields {}
   :nested []
   :constraints []})

(defmacro validator
  [ & body]
  `(-> (validator*)
       ~@body))

(defmacro defvalidator
  [name & body]
  `(def ~name (validator ~@body)))

(defn validates
  [val f & [error]]
  (let [error (if error
                (cond
                 (string? error) {:message error}
                 (map? error) error)
                {:message "Invalid"})]
    (update-in val [:constraints] conj [f error])))

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
  [validator name & body]
  `(field* ~validator ~name
           (-> (make-field ~name)
               ~@body)))

(def parsers
  {:int        #(Integer/parseInt %)
   :double     #(Double/parseDouble %)
   :float      #(Float/parseFloat %)
   :boolean    #(Boolean/parseBoolean %)
   :long       #(Long/parseLong %)
   :bigint     #(BigInteger. %)
   :bigdecimal #(BigDecimal. %)
   :uuid       #(java.util.UUID/fromString %)})

(defn- make-parser
  [parser]
  (fn [value]
    (try (parser (str value)) (catch Exception e nil))))

(defn type-of
  [field type]
  (assoc field
    :parser
    (make-parser
     (cond
      (keyword? type) (parsers type)
      (fn? type) type))))

(defn required
  [field]
  (assoc field :required? true))

(defn validate-field
  [{:keys [parser required? constraints] :as field} value]
  (if-not (blank? value)
    (if-let [value (if parser (parser value) value)]
      (reduce
       (fn [validation [pred error]]
         (if (pred value)
           validation
           (-> validation
               (assoc :valid? false)
               (update-in [:errors] conj error))))
       {:valid? true :value value :errors []}
       constraints)
      {:valid? false :errors [*invalid-type-error*]})
    (if required?
      {:valid? false :errors [*required-error*]}
      {:valid? true})))

(defn validate-fields
  [validator value]
  (reduce
   (fn [validation [name field]]
     (let [field-value (name value)
           {:keys [value errors valid?]}
           (validate-field field field-value)]
       (if valid?
         (if-not (blank? value)
           (update-in validation [:value] assoc name value)
           validation)
         (-> validation
             (assoc :valid? false)
             (update-in [:errors]
                        concat
                        (map #(assoc % :field name) errors))))))
   {:value value :valid? true :errors []}
   (:fields validator)))

(defn validate
  [validator value]
  (let [fields-validation (validate-fields validator value)]
    (-> (reduce
         (fn [validation [pred error]]
           (if (pred value)
             validation
             (-> validation
                 (assoc :valid? false)
                 (update-in [:errors] conj error))))
         fields-validation
         (:constraints validator)))))

(defmacro if-valid
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

;; (defn nest-one
;;   [name validator & [required?]]
;;   (assoc-in
;;    [:nested name]
;;    {:rel :one :validator validator :required? (boolean required?)}))

;; (defn nest-many
;;   [name validator & [required?]]
;;   (assoc-in
;;    [:nested name]
;;    {:rel :many :validator validator :required? (boolean required?)}))

;; (defn validate-nested
;;   [validator value]
;;   (reduce
;;    (fn [validation [name {:keys [rel validator required?] :as validator}]]
;;      (if-let [value (not-empty (value name))]
;;        (if (= rel :one)
;;          ()
;;          ())
;;        (if required?
;;          (-> validation
;;              (assoc :valid? false)
;;              (update-in :errors conj (assoc *required-error* :relation name)))
;;          validation)))
;;    {:valid? true :value value :errors []}
;;    (:nested validator)))

;; Filter validation

;; (declare make-filter)

;; (def invalid-filter (RipException. {:code :invalid-filter :message "Invalid filter structure"}))

;; (def preds
;;   {:$eq  (fn [field val] {field val})
;;    :$ne  (fn [field val] {field [pred-not= val]})
;;    :$gt  (fn [field val] {field [pred-> val]})
;;    :$lt  (fn [field val] {field [pred-< val]})
;;    :$ge  (fn [field val] {field [pred->= val]})
;;    :$le  (fn [field val] {field [pred-<= val]})
;;    :$lk  (fn [field val] {field [pred-like val]})
;;    ;;   :rng (fn [field [min max]] (pred-or {field [pred->= min]} {field [pred-<= max]}))
;;    })

;; (def ops
;;   {:$or  pred-or
;;    :$and pred-and})

;; (declare validate-filter)

;; (defn error-field
;;   [input output field pred]
;;   [(assoc input field {:$input pred
;;                        :$error (:query-error *default-messages*)})
;;    output])

;; (defn concat-pred
;;   [pred new-pred & [op]]
;;   (let [op (or op pred-and)]
;;     (if-not pred
;;       new-pred
;;       (if (and (coll? pred) (empty? pred))
;;         new-pred
;;         (op pred new-pred)))))

;; (defn add-preds
;;   [input output field [in out]]
;;   [(if (empty? in)
;;      input
;;      (assoc input field in))
;;    (if (and (coll? output) (empty? output))
;;      out
;;      (pred-and output out))])

;; (defn assoc-op
;;   [input output field pred op f]
;;   (add-preds
;;    input
;;    output
;;    field
;;    (reduce
;;     (fn [[input output] [in out]]
;;       [in out]
;;       [(if (empty? in) input
;;            (conj input in))
;;        (concat-pred output out op)])
;;     [{} false]
;;     (mapv f pred))))

;; (defn get-valid?
;;   [valid-type]
;;   (let [parser (cond
;;                 (fn? valid-type)
;;                 valid-type
;;                 (class? valid-type)
;;                 (parsers valid-type))]
;;     (fn [value]
;;       (try
;;         (if (= valid-type String)
;;           (if (string? value)
;;             value)
;;           (parser (str value)))
;;         (catch Exception e nil)))))

;; (defn get-alias
;;   [aliases [node & path]]
;;   (let [alias (aliases node)
;;         [alias aliases] (if (string? alias)
;;                           [alias]
;;                           alias)]
;;     (if aliases
;;       (get-alias aliases path)
;;       [alias (first path)])))

;; (declare validate-field)

;; (defn validate-field-value
;;   [input output field value valid? alias & [field-name]]
;;   (let [valid-value (valid? value)]
;;     (if-not (nil? valid-value)
;;       [input (concat-pred output
;;                           (pred-=
;;                            (str alias
;;                                 "."
;;                                 (if field-name
;;                                   (name field-name)
;;                                   (name field))) valid-value))]
;;       (error-field input output field value))))

;; (defn validate-field-op
;;   [input output field value valid? alias op & [field-name]]
;;   (let [valid-value (valid? value)]
;;     (if-not (nil? valid-value)
;;       (if-let [op (preds op)]
;;         [input (concat-pred output
;;                             (op
;;                              (str alias
;;                                   "."
;;                                   (if field-name
;;                                     (name field-name)
;;                                     (name field))) value))]
;;         (error-field input output field value))
;;       (error-field input output field value))))

;; (defn validate-field-map
;;   [field pred valid? alias & [field-name]]
;;   (reduce
;;    (fn [[input output] [op value]]
;;      (if-let [op (ops op)]
;;        (if (vector? value)
;;          (assoc-op
;;           input output field value op
;;           (fn [pred]
;;             (validate-field input {} field pred valid? alias field-name)))
;;          (error-field input output field value))
;;        (validate-field-op input output field value valid? alias op field-name)))
;;    [{} {}]
;;    pred))

;; (defn validate-field
;;   [input output field pred valid? alias & [field-name]]
;;   (if (map? pred)
;;     (add-preds input output field (validate-field-map field pred valid? alias field-name))
;;     (if-not (vector? pred)
;;       (validate-field-value input output field pred valid? alias field-name)
;;       (error-field input output field pred))))

;; (defn validate-nested
;;   [input output field pred alias valid-type aliases]
;;   (if (map? pred)
;;     (add-preds input output field (validate-filter alias valid-type pred aliases))
;;     (error-field input output field pred)))

;; (defn validate-filter
;;   [alias schema pred aliases]
;;   (reduce
;;    (fn [[input output] [field-op pred]]
;;      (if-let [op (ops field-op)]
;;        ;; $and $or
;;        (if (vector? pred)
;;          (assoc-op
;;           input output field-op pred op
;;           (fn [pred]
;;             (validate-filter alias schema pred aliases)))
;;          (error-field input output field-op pred))
;;        ;; field
;;        (if-let [[valid-type & path] (schema field-op)]
;;          (if (map? valid-type)
;;            (let [[alias aliases] (if path
;;                                    (get-alias aliases path)
;;                                    [alias aliases])]
;;              (validate-nested
;;               input output field-op pred
;;               alias valid-type aliases))
;;            (let [[alias aliases] (if-let [[field-or-rel & rest-path] path]
;;                                    (if rest-path
;;                                      (get-alias aliases path)
;;                                      [alias field-or-rel])
;;                                    [alias aliases])]
;;              (validate-field
;;               input output field-op pred
;;               (get-valid? valid-type) alias
;;               (if path (name aliases)))))
;;          [input output])))
;;    [{} {}]
;;    pred))

;; (defn make-join
;;   "Generates an aliased entity and a clause for the join"
;;   [rel alias sub-alias join-type]
;;   (let [field #(last (clojure.string/split (val (first %)) #"\""))
;;         pk    (field (:pk rel))
;;         fk    (field (:fk rel))]
;;     [[(keyword (:name (:ent rel)))
;;       (keyword sub-alias)
;;       join-type
;;       (apply pred-=
;;              (map
;;               (fn [[alias field]] (keyword (str (name alias) \. field)))
;;               [[alias pk] [sub-alias fk]]))]]))

;; (defn make-joins
;;   [ent alias joins]
;;   (reduce
;;    (fn [[joins aliases] [rel-name join-type]]
;;      (let [rel       (get-rel ent rel-name)
;;            sub-alias (str (name rel-name) "_" alias)
;;            sub-ent   (:ent rel)]
;;        join-type
;;        (let [[joins aliases join-type]
;;              (if (vector? join-type)
;;                (let [[join-type nested-joins] join-type
;;                      [j j-a]                  (make-joins sub-ent sub-alias nested-joins)]
;;                  [(concat joins j)
;;                   (assoc aliases rel-name [sub-alias j-a])
;;                   join-type])
;;                [joins aliases join-type])]
;;          (let [[join mapping-join] (make-join rel alias sub-alias join-type)]
;;            [(concat joins [join] (if mapping-join [mapping-join] []))
;;             (assoc aliases rel-name sub-alias)]))))
;;    [[] {}]
;;    joins))

;; (defn to-query
;;   [ent joins where-clause]
;;   (reduce
;;    (fn [query [name alias type clause]]
;;      (join query type [name alias] clause))
;;    (-> (select* ent)
;;        (where* where-clause))
;;    joins))

;; (defn filter-validator
;;   [ent schema joins]
;;   (fn [value]
;;     (let [[joins aliases] (make-joins ent (:name ent) joins)
;;           [errors where-clause] (validate-filter (:name ent) schema value aliases)]
;;       {:valid? (and (not (empty? where-clause)) (empty? errors))
;;        :errors errors
;;        :query  (to-query ent joins where-clause)})))
