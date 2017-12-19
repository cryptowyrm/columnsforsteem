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
  (are [expected actual]
    (= expected
      (app/parse-image-url {"body" actual}))

    "https://ipfs.io/ipfs/QmYDnTNYMa6KWJHDiwekFrGkDgRd257nLACAHTPwejiUTh"
    "<img src='https://ipfs.io/ipfs/QmYDnTNYMa6KWJHDiwekFrGkDgRd257nLACAHTPwejiUTh'>"

    "https://axios-img.rbl.ms/simage/https%3A%2F%2Fassets.rbl.ms%2F16627222%2F980x.jpg/2000%2C2000/8bw1H2%2F28LxIdkcO/img.jpg"
    "<center>https://axios-img.rbl.ms/simage/https%3A%2F%2Fassets.rbl.ms%2F16627222%2F980x.jpg/2000%2C2000/8bw1H2%2F28LxIdkcO/img.jpg<br></center>"))

(deftest test-is-post-active []
  (is (= false (app/is-post-active {"cashout_time" "2017-10-16T18:46:06"})))
  (is (= true (app/is-post-active {"cashout_time" "2117-10-16T18:46:06"}))))

(deftest test-has-whitespace
  (is (= true (app/has-whitespace "has whitespace")))
  (is (= false (app/has-whitespace "nowhitespace"))))

(deftest test-new-posts
  (let [new-p '({"id" 1} {"id" 2} {"id" 3} {"id" 4} {"id" 5} {"id" 6})
        old-p '({"id" 1} {"id" 2} {"id" 3})]
    (is (= '({"id" 4} {"id" 5} {"id" 6}) (app/new-posts new-p old-p)))))
