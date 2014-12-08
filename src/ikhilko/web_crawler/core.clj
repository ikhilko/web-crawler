(ns ikhilko.web-crawler.core
  (:import (org.apache.http.conn ConnectTimeoutException)
           (java.net URI))
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-http.client :as client]
            [clojurewerkz.urly.core :as url]
            [net.cgrand.enlive-html :as html]))

; ------------------ HELPERS -------------------

; helper, like regular reduce but add index param
(defn reduce-indexed
  "Reduce while adding an index as the second argument to the function"
  ([f coll]
    (reduce-indexed f (first coll) 0 (rest coll)))

  ([f init coll]
    (reduce-indexed f init 0 coll))

  ([f init i coll]
    (if (empty? coll)
      init
      (let [v (first coll)
            fv (f init i v)]
        (recur f fv (inc i) (rest coll))))))

(defn str->int
  [value]
  (try (if (number? value) value (Integer/parseInt value))
       (catch NumberFormatException e nil)))

; ------------------ / HELPERS -------------------

; ------------------  TREE -------------------

(defn- create-tree-node
  [parent urls depth url state additional children]
  { :parent parent, :urls urls, :depth depth, :url url, :state state, :additional additional, :children children })

; ------------------ / TREE -------------------

; ------------------  CORE -------------------

(def ^:private http-request-options {:throw-exceptions false
                                     :conn-timeout     1000
                                     :follow-redirects false
                                     :debug            true})

(def ^:private white-statuses #{200 201 202 203 204 205 206 207 300})
(def ^:private redir-statuses #{301 302 303 307})


(defn- get-status-description
  [raw-response]
  (let [status (:status raw-response)]
    (cond
      (contains? white-statuses status) {:state "ok", :status status}
      (contains? redir-statuses status) (:state "redirect", :location (:location (:headers raw-response)),, :status status)
      :else {:state "bad", :status status})))

; get page (or 408/500 bad codes)
(defn- fetch-page-by-url
  [url]
  (try (client/get url http-request-options)
       (catch ConnectTimeoutException e {:status 408})
       (catch Exception e {:status 500})))

(defn- resolve-url
  [base-url href]
  (let [uri-href (URI. href)]
    (if (url/relative? uri-href)
      (url/resolve (URI base-url) uri-href)
      uri-href)))

(defn- parse-page-links
  [base-url body]
  (-> (html/html-snippet body)
      (html/select #{[:a]})
      (->> (reduce (fn [memo link]
                     (let [href (:href (:attrs link))]
                       (if (some? href)
                         (conj memo (resolve-url base-url href)))))
                   []))))

(defn- process-urls
  [urls]
  (pmap (fn [url]
          (if (mark-url-visited url)
            (let [raw-response (fetch-page-by-url url)
                  status-desctiption (get-status-description raw-response)
                  state (:state status-desctiption)]
              (->> (case state
                     "ok" (let [links (parse-page-links url (:body raw-response))
                                count (count links)]
                            {:urls links, :message (str "links:" count)})
                     "redirect" (let [location (resolve-url url (:location status-desctiption))]
                                  {:urls [location], :message (str "redirect to: " location)})
                     "bad" {:urls [], :message (str "bad link (" (:status status-desctiption))})
                   ))))
        urls))

; ------------------ / CORE -------------------

;------------------ FILES FUNCTIONS -------------------

; read files and split it by "\n"
(defn- file->urls
  [file-path]
  (-> (slurp file-path)
      (str/split #"\n")
      (->> (reduce #(let [line (str/trim %2)]
                     (if (str/blank? line)
                       %1
                       (conj %1 line)))
                   []))))


; check, is file and exist?
(defn- file-and-exist? [file-path]
  (let [file (io/file file-path)]
    (and (.exists file) (not (.isDirectory file)))))

; ------------------ / FILES FUNCTIONS -------------------

; ------------------ ARGUMENTS CHECKING -------------------

; helper throwing Error with "message" if condition falsy
(defn- if-false-throw-error
  [condition message]
  (if-not condition (throw (IllegalArgumentException. message)))
  condition)

; check arguments and throw exception if some check doen't valid
(defn- check-argumets
  [file-path depth]
  (dorun
    (map #(apply if-false-throw-error %)
         [[(not (str/blank? file-path)) "Not blank \"file-path\" must be provided as first argument"]
          [(file-and-exist? file-path) (str "File with name '" file-path "' doesn't exist!")]
          [(some? depth) "Depth must be a number"]
          [(>= depth 0) "Depth must be >= 0"]])))

; ------------------ / ARGUMENTS CHECKING -------------------

; entry point
(defn -main
  [file-path depth]
  (try (let [depth (str->int depth)]
         (check-argumets file-path depth)
         (println "File path: " file-path)
         (println "Depth: " depth)
         (println (file->urls file-path)))
       (catch IllegalArgumentException e (->> e (.getMessage) (println "Invalid argument:")))
       (finally (shutdown-agents))))

; (file->urls "./samples/urls.txt")