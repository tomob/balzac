(defproject balzac "0.1.0"
  :description "Read ebook metadata"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [gloss "0.2.2-rc1"]
                 [nl.siegmann.epublib/epublib-core "3.1"]]
  :repositories [["psiegman-releases" {:url "http://github.com/psiegman/mvn-repo/raw/master/releases"
                                       :checksum :ignore}]]
)
