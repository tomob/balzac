(ns balzac.proto)

(defprotocol Book
  "Abstracts operations over epub types"
  (authors [book] "Returns a seq of book authors.")
  (title [book] "Returns the title of the book.")
  (isbn [book] "Returns ISBN of the book.")
  (language [book] "Returns book's language.")
  (publication-date [book] "Returns publication date of the book"))