(ns balzac.epub
  (:use [balzac.proto])
  (:import [nl.siegmann.epublib.epub EpubReader]
           [nl.siegmann.epublib.domain Identifier]))

(defrecord Epub [meta]
  Book
  (authors [book] (map str (.getAuthors meta)))
  (title [book] (first (.getTitles meta)))
  (language [book] (.getLanguage meta))
  (publication-date [book] (first (.getDates meta)))
  (isbn [book]
    (let [bookid (.getValue (Identifier/getBookIdIdentifier (.getIdentifiers meta)))]
      (if (.contains bookid ":")
        (last (.split bookid ":"))
        bookid))))

(defn epub
  "Parses all headers of an .epub file."
  [is]
  (let [er (EpubReader.)
        book (.readEpub er is)]
    (Epub. (.getMetadata book))))
