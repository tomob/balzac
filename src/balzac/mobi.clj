(ns balzac.mobi
  (:use [gloss core io]
        [clojure.java.io]
        [balzac.proto])
  (:require [balzac.exth :as exth]))


;; Dummy codec to read pdb header size
(defcodec pdb-header-size
  [:num-records :int16])

;; PDB header
(defcodec pdb-header
  [:name                (string :ascii :length 32)
   :attributes          :int16
   :version             :int16
   :creation-date       :int32
   :modification-date   :int32
   :last-backup-date    :int32
   :modification-number :int32
   :app-info-id         :int32
   :sort-info-id        :int32
   :type                (string :ascii :length 4)
   :creator             (string :ascii :length 4)
   :unique-id-seed      :int32
   :next-record-list-id :int32
   :records             (repeated [:record-data-offset :int32
                                   :record-attributes :byte
                                   :unique-id [:ubyte :ubyte :ubyte]]
                                  :prefix :int16)
   :gap-to-data         :int16])

;; Palmdoc header
(defcodec palmdoc-header
  [:compression (enum :int16 {:no-compression 1 :palmdoc-compression 2 :huff-cdic 17480})
   :unused1 :int16
   :text-length :int32
   :record-count :int16
   :record-size :int16
   :encryption-type :int16
   :unused1 :int16])

;; Enumeration of known mobi types
(def mobi-type-enum
  {:mobipocket-book 2
   :palmdoc-book 3
   :audio 4
   :mobipocket-kindlegen 232
   :kf8 248
   :news 257
   :news-feed 258
   :news-magazine 259
   :pics 513
   :word 514
   :xls 515
   :ppt 516
   :text 517
   :html 518})

;; mobi header
(defcodec mobi-header
  [:identifier (string :ascii :length 4)
   :header-length :int32
   :mobi-type (enum :int32 mobi-type-enum)
   :text-encoding (enum :int32 {:cp1252 1252 :utf8 65001})
   :unique-id :int32
   :file-version :int32
   :orthographic-index :int32
   :inflection-index :int32
   :index-names :int32
   :index-keys :int32
   :extra-index-0 :int32
   :extra-index-1 :int32
   :extra-index-2 :int32
   :extra-index-3 :int32
   :extra-index-4 :int32
   :extra-index-5 :int32
   :first-non-book-index :int32
   :full-name-offset :int32
   :full-name-length :int32
   :locale :int32
   :input-language :int32
   :output-language :int32
   :min-version :int32
   :first-image-index :int32
   :huffman-record-offset :int32
   :huffman-record-count :int32
   :huffman-table-offset :int32
   :huffman-table-length :int32
   :exth-flags :int32
   :unknown1 (string :ascii :length 32)
   :unknown2 :int32
   :drm-offset :int32
   :drm-count :int32
   :drm-size :int32
   :drm-flags :int32
   :unknown3 :int64
   :first-content-record-number :int16
   :last-content-record-number :int16
   :unknown4 :int32
   :fcis-record-number :int32
   :unknown5 :int32
   :flis-record-number :int32
   :unknown6 :int32
   :unknown7 :int64
   :unknown8 :int32
   :first-compilation-data-section-count :int32
   :number-of-compilation-data-sections :int32
   :unknown9 :int32
   :extra-record-data-flags :int32
   :indx-recod-offset :int32
  ])

;; Dummy coded to read exth header length
(defcodec exth-length
  [:id :int32
   :length :int32])

;; exth header
(defcodec exth-header
  [:identifier (string :ascii :length 4)
   :header-length :int32
   :records (repeated [:type (enum :int32 exth/exth-type-enum)
                       :data (repeated :ubyte :prefix (prefix :int32 #(- % 8) #(+ % 8)))])
  ])

(defn pdb-length
  "Calculates length of PDB header."
  [pdb-]
  (+ 80 (* 8 (second pdb-))))

(defn pdb-to-map
  "Converts vector of PDB values to a map."
  [pdb]
  (let [m (apply hash-map pdb)]
    (update-in m [:records] #(map (fn [x] (apply hash-map x)) %))))

(defn parse-pdb-header
  "Parses PDB header. What did you expect?"
  [is]
  (let [buffer (byte-array 2)]
    (.mark is 80)
    (.skip is 76)
    (.read is buffer)
    (let [pdb- (decode pdb-header-size buffer false)
          buffer (byte-array (pdb-length pdb-))]
        (.reset is)
        (.read is buffer)
        (pdb-to-map (decode pdb-header buffer false)))))

(defn parse-palmdoc-header
  "Parses PalmDoc header."
  [is]
  (let [buffer (byte-array 16)]
    (.read is buffer)
    (apply hash-map (decode palmdoc-header buffer))))

(defn parse-mobi-header
  "Parses Mobi header."
  [is]
  (let [buffer (byte-array 232)]
    (.read is buffer)
    (apply hash-map (decode mobi-header buffer))))

(defn get-padding-for
  "Calculates padding for given header length."
  [length]
  (let [m (mod length 4)]
    (if (= m 0) 0 (- 4 m))))

(defn exth-to-map
  "Converts vector of EXTH values to a map and parses record data for known types."
  [exth]
  (let [m (apply hash-map exth)]
    (update-in m [:records] #(map exth/parse-record %))))

(defn parse-exth-header
  "Parses EXTH header and records."
  [is]
  (let [buffer (byte-array 8)]
    (.mark is 8)
    (.read is buffer)
    (let [exth- (decode exth-length buffer false)
          length (nth exth- 3)
          buffer (byte-array (+ length (get-padding-for length)))]
      (.reset is)
      (.read is buffer)
      (exth-to-map (decode exth-header buffer false)))))

(defn has-exth?
  "Checks whether EXTH flags indicate there is a EXTH header."
  [exth-flags]
  (bit-and 0x40 exth-flags))

(defn read-property [m property]
  (:data (first (filter #(= property (:type %)) (get-in m [:exth :records])))))

(defrecord Mobi [pdb pdoc mobi exth]
  Book
  (authors [book] (seq [(read-property book :author)]))
  (title [book] (read-property book :updated-title))
  (isbn [book] (read-property book :isbn))
  (language [book] (read-property book :language))
  (publication-date [book] (read-property book :publishing-date)))

(defn mobi
  "Parses all headers of a .mobi file."
  [is]
  (let [pdb (parse-pdb-header is)
        pdoc (parse-palmdoc-header is)
        mobih (parse-mobi-header is)
        exth (if (has-exth? (:exth-flags mobih)) (parse-exth-header is))]
    (Mobi. pdb pdoc mobih exth)))


;; Helpers for REPL

(defn is1m [] (input-stream "/Users/tomo/Dropbox/książki/Pijani Bogiem.mobi"))
(defn is2m [] (input-stream "/Users/tomo/Dropbox/książki/Czarny_Horyzont.mobi"))
