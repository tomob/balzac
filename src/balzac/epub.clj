(ns balzac.epub
  (:use ([clojure.java.io]))
  (:import [nl.siegmann.epublib.epub EpubReader]))


(defn epub
  "Parses all headers of an .epub file."
  [is]
  (let [er (EpubReader.)
        book (.readEpub er is)]
    (.getMetadata book)))


;; Helpers for REPL

(defn is [] (input-stream "/Users/tomo/Desktop/epub/Metro_2033.epub"))
(defn is2 [] (input-stream "/Users/tomo/Desktop/epub/Real_World_Haskell.epub"))
