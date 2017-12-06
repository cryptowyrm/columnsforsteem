(ns columnsforsteem.client-test
  (:require [columnsforsteem.client :as app]
            [cljs.test :refer-macros [deftest testing is are]]))

(deftest test-image-regex []
  (testing "Image regex"
    (are [expected actual] (= expected (first (re-find app/image-regex actual)))
      "http://www.example.com/path/image.png"
      "<br>http://www.example.com/path/image.png<br>"

      "http://www.example.com/path/image.png"
      "<br>http://www.example.com/path/image.png.<br>"

      "http://www.example.com/path/image.png?key=value"
      "<br>http://www.example.com/path/image.png?key=value<br>"

      nil
      "nothing")))
