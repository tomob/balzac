(ns balzac.mobi
  (:use [gloss core io]
        [clojure.java.io]))


(defcodec pdb-header-size
  [:num-records :int16])

(defcodec pdb-header
  [:name (string :ascii :length 32)
   :attributes :int16
   :version    :int16
   :creation-date :int32
   :modification-date :int32
   :last-backup-date :int32
   :modification-number :int32
   :app-info-id :int32
   :sort-info-id :int32
   :type (string :ascii :length 4)
   :creator (string :ascii :length 4)
   :unique-id-seed :int32
   :next-record-list-id :int32
   :records (repeated [:record-data-offset :int32 :record-attributes :byte :unique-id [:ubyte :ubyte :ubyte]] :prefix :int16)
   :gap-to-data :int16
   ])

(defn pdb-length [pdb-]
  (+ 80 (* 8 (second pdb-))))

(defn pdb-to-map [pdb]
  (let [m (apply hash-map pdb)]
    (update-in m [:records] #(map (fn [x] (apply hash-map x)) %))))

(defn parse-pdb-header [is]
  (let [buffer (byte-array 2)]
    (.mark is 80)
    (.skip is 76)
    (.read is buffer)
    (let [pdb- (decode pdb-header-size buffer false)
          buffer (byte-array (pdb-length pdb-))]
        (.reset is)
        (.read is buffer)
        (pdb-to-map (decode pdb-header buffer false)))))

(defn is [] (input-stream "/Users/tomo/Dropbox/książki/Pijani Bogiem.mobi"))

(defn gimme-buffer
  ([size is]
    (let [buffer (byte-array size)]
      (.read is buffer)
      buffer))
  ([size]
    (gimme-buffer size (is))))
