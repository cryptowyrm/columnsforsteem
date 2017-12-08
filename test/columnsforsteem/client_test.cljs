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

      "http://www.example.com/path/image.png"
      "http://www.somelink.com/'>http://www.example.com/path/image.png"

      "http://www.newsbtc.com/https://s3.amazonaws.com/main-newsbtc-images/2017/05/fed-talk-bitcoin-825x510.jpg"
      "<br>http://www.newsbtc.com/https://s3.amazonaws.com/main-newsbtc-images/2017/05/fed-talk-bitcoin-825x510.jpg</br>"

      nil
      "nothing")))

(deftest test-parse-image-url []
  (are [expected actual] (= expected (app/parse-image-url actual))
    "https://ipfs.io/ipfs/QmYDnTNYMa6KWJHDiwekFrGkDgRd257nLACAHTPwejiUTh"
    {"body"
     "<img src='https://ipfs.io/ipfs/QmYDnTNYMa6KWJHDiwekFrGkDgRd257nLACAHTPwejiUTh'>"}))

(deftest test-is-post-active []
  (is (= false (app/is-post-active {"cashout_time" "2017-10-16T18:46:06"})))
  (is (= true (app/is-post-active {"cashout_time" "2117-10-16T18:46:06"}))))

(deftest test-has-whitespace
  (is (= true (app/has-whitespace "has whitespace")))
  (is (= false (app/has-whitespace "nowhitespace"))))
