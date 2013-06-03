(ns balzac.exth
  (:use [clojure.string :only [join]]))

;; Enumeration of known exth record types
(def exth-type-enum
  {:drm-server-id 1
   :drm-commerce-id 2
   :drm_ebookbase_book_id 3
   :author 100
   :publisher 101
   :imprint 102
   :description 103
   :isbn 104
   :subject 105
   :publishing-date 106
   :review 107
   :contributor 108
   :rights 109
   :subjct-code 110
   :type 111
   :source 112
   :asin 113
   :version-number 114
   :is-sample 115
   :start-reading 116
   :adult 117
   :retail-price 118
   :retail-price-currency 119
   :kf8-boundary-offset 121
   :count-of-resources 125
   :kf8-cover-uri 129
   :dictionary-short-name 200
   :cover-offset 201
   :thumb-offset 202
   :has-fake-cover 203
   :creator-software 204
   :creator-major-version 205
   :creator-minor-version 206
   :creator-build-number 207
   :watermark 208
   :tamper-proof-keys 209
   :font-signature 300
   :clipping-limit 401
   :publisher-limit 402
   :tts-flag 404
   :cde-type 501
   :last-update-time 502
   :updated-title 503
   :language 524
   :alignment 525})

(defn- vec-to-int
  "Converts a (little-endian) vector of bytes to int."
  [v]
  (reduce (fn [acc val] (+ val (* 256 acc))) 0 v))

(defn ubyte-to-byte
  "Converts unsigned byte to signed byte."
  [b]
  (if (< b 128) b (- b 256)))

(defn parse-int [record]
  (update-in record [:data] vec-to-int))

(defn- parse-string [record]
  (letfn [(vector-of-bytes [v]
            (apply conj (vector-of :byte) (map ubyte-to-byte v)))]
    (update-in record [:data]
      #(String. (byte-array (vector-of-bytes %)) (:encoding record)))))

(def strings [:author :publisher :imprint :decription :isbn :subject :publishing-date :review :contributor :rights
              :subjct-code :type :source :asin :version-number :adult :retail-price :retail-price-currency
              :kf8-cover-uri :dictionary-short-name :language :updated-title])

(defn- is-string? [record]
  (some #(= (second record) %) strings))

(def intses [:start-reading :kf8-boundary-offset :count-of-resources :cover-offset :thumb-offset :has-fake-cover
           :creator-major-version :creator-minor-version :creator-build-number :clipping-limit :tts-flag :sample])

(defn- is-int? [record]
  (some #(= (second record) %) intses))

(defn general-type [record]
  (cond
    (is-string? record) :string
    (is-int? record)    :int
    (= :creator-software (second record)) :creator-software
    (= :cde-type (second record)) :cde-type
    :else :default))

(defmulti parse-record general-type)

(defmethod parse-record :string [record]
  (parse-string (apply hash-map record)))

(defmethod parse-record :int [record]
  (parse-int (apply hash-map record)))

(def creator-sw-map {1 "mobigen" 2 "Mobipocket Creator" 200 "Kindlegen (Windows)" 201 "Kindlegen (Linux)" 202 "Kindlegen (Mac)"})

(defmethod parse-record :creator-software [record]
  (let [i (parse-int (apply hash-map record))]
    (update-in i [:data] creator-sw-map)))

(def cde-type-map {"PDOC" "Personal doc" "EBOK" "Ebook" "EBSP" "Ebook sample"})

(defmethod parse-record :cde-type [record]
  (let [i (parse-string (apply hash-map record))]
    (update-in i [:data] cde-type-map)))

(defmethod parse-record :default [record]
  (apply hash-map record))