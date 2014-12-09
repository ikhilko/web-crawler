(ns ikhilko.web-crawler.core-test
  (:import (java.net UnknownHostException)
           (org.apache.http.conn ConnectTimeoutException))
  (:require [clojure.test :refer :all]
            [ikhilko.web-crawler.core :refer :all :as core]))

(def ^:private base-url "http://test.com")

(deftest test-parse-html
  (testing "Parser"
    (testing "find absolute links"
      (let [content (str "<div>"
                         "<a href=\"http://test.com/somepath\"></a>"
                         "<a href=\"https://test.com/somepath2\"></a>"
                         "<a href=\"other.com\"></a>"
                         "</div>")
            urls (#'core/parse-page-links base-url content)]
        (is (= urls ["http://test.com/somepath" "https://test.com/somepath2" "http://other.com/"]))))
    (testing "find relative links"
      (let [content (str "<div>"
                         "<a href=\"/part\"></a>"
                         "<a href=\"part?asdsad=true\"></a>"
                         "</div>")
            urls (#'core/parse-page-links base-url content)]
        (is (= urls ["http://test.com/part" "http://test.com/part?asdsad=true"]))))
    (testing "skip anchors #... "
      (let [content (str "<div>"
                         "<a href=\"#part\"></a>"
                         "<a href=\"#another/test\"></a>"
                         "</div>")
            urls (#'core/parse-page-links base-url content)]
        (is (= urls nil))))
    (testing "skip mailto:, callto:, javascript: "
      (let [content (str "<div>"
                         "<a href=\"javascript: alert(\"hello!\")\"></a>"
                         "<a href=\"mailto:test@test.com\"></a>"
                         "<a href=\"callto:test@test.com\"></a>"
                         "</div>")
            urls (#'core/parse-page-links base-url content)]
        (is (= urls nil))))
    (testing "process all"
      (let [content (slurp "./test/ikhilko/web_crawler/fixtures/test.html")
            urls (#'core/parse-page-links "http://http://learnxinyminutes.com/" content)]
        (is (= (count urls) 211))))))


(defn- get-request-spy
  [status]
  (case status
    "408" (throw (ConnectTimeoutException. "408"))
    "404" (throw (UnknownHostException. "404"))
    "500" (throw (Exception. "500"))
    "200" {:status 200}))

(deftest handle-errors
  (testing "Handle error"
    (testing "404"
      (with-redefs-fn {#'core/get-request get-request-spy}
                      #(let [raw-restonse (#'core/fetch-page-by-url "404")]
                        (is (= (:status raw-restonse) 404)))))
    (testing "408"
      (with-redefs-fn {#'core/get-request get-request-spy}
                      #(let [raw-restonse (#'core/fetch-page-by-url "408")]
                        (is (= (:status raw-restonse) 408)))))
    (testing "500"
      (with-redefs-fn {#'core/get-request get-request-spy}
                      #(let [raw-restonse (#'core/fetch-page-by-url "500")]
                        (is (= (:status raw-restonse) 500)))))))