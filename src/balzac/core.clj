(ns balzac.core
  (:use [balzac.mobi :only [mobi is-mobi?]]
        [balzac.epub :only [epub]]
        [clojure.java.io])
  (:require [balzac.proto :as p])
  (:import [java.nio.charset MalformedInputException]))

(defn book
    "Parses a .mobi or .epub book."
    [is]
    (if (is-mobi? is)
      (mobi is)
      (epub is)))

(def authors p/authors)

(def title p/title)

(def isbn p/isbn)

(def language p/language)

(def publication-date p/publication-date)

;; Helpers for REPL

(defn is1e [] (input-stream "/Users/tomo/Desktop/epub/Metro_2033.epub"))
(defn is2e [] (input-stream "/Users/tomo/Desktop/epub/Real_World_Haskell.epub"))

(defn is1m [] (input-stream "/Users/tomo/Dropbox/książki/Pijani Bogiem.mobi"))
(defn is2m [] (input-stream "/Users/tomo/Dropbox/książki/Czarny_Horyzont.mobi"))