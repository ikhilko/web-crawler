(ns ikhilko.web-crawler.core
  (:import (org.apache.http.conn ConnectTimeoutException))
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-http.client :as client]
            [clojurewerkz.urly.core :as url]
            [net.cgrand.enlive-html :as html]))


(defn str->int
  [value]
  (try (if (number? value) value (Integer/parseInt value))
       (catch NumberFormatException e nil)))


; ------------------  TREE -------------------
(defn- create-tree-node
  ([urls depth]
    (create-tree-node nil urls depth "root" { :state "root" }))
  ([parent urls depth url additional]
    {:parent     parent,
     :urls       urls,
     :depth      depth,
     :url        url,
     :additional additional,
     :children   (atom ())}))

(defn- generate-indent
  [level]
  (str/join "" (take level (repeat "   "))))

(defn- print-node
  [node level]
  (let [indent (generate-indent level)
        additional (:additional node)
        state (:state additional)
        message (case state
                  "ok" (str "links: " (count (:urls node)))
                  "redirect" (str "redirect to (" (:location additional) ")")
                  "bad" (str "bad (" (:status additional) ")")
                  "visited" "already visited"
                  "another-host" "link to another host"
                  "root" "root")]
    (println indent (:url node) message)))

(defn- print-tree
  [root level]
  (print-node root level)
  (doseq [child @(:children root)] (print-tree child (inc level))))

; ------------------ / TREE -------------------

; ------------------  CORE -------------------

(def ^:private http-request-options {:throw-exceptions false
                                     :conn-timeout     1000
                                     :follow-redirects false})

(def ^:private white-statuses #{200 201 202 203 204 205 206 207 300})
(def ^:private redir-statuses #{301 302 303 307})

(def ^:private visited-urls (atom #{}))


(defn- get-status-description
  [raw-response]
  (let [status (:status raw-response)]
    (cond
      (contains? white-statuses status) {:state "ok", :status status}
      (contains? redir-statuses status) {:state "redirect", :location (:location (:headers raw-response)), :status status}
      :else {:state "bad", :status status})))

; get page (or 408/500 bad codes)
(defn- fetch-page-by-url
  [url]
  (try (client/get url http-request-options)
       (catch ConnectTimeoutException e {:status 408})
       (catch Exception e {:status 500})))

(defn- resolve-url
  [base-url href]
  (let [uri-href (url/url-like href)]
    (->> (if (not (url/absolute? uri-href))
           (url/resolve (url/url-like base-url) uri-href)
           uri-href)
         (.toString))))

(defn- prepare-href
  [href]
  (cond
    (nil? href) nil
    (nil? (re-find (re-matcher #"^((javascript|mailto|callto):|#)" href))) href
    :else nil))

(defn- parse-page-links
  [base-url body]
  (-> (html/html-snippet body)
      (html/select #{[:a]})
      (->> (reduce (fn [memo link]
                     (let [href (prepare-href (:href (:attrs link)))]
                       (if (not (nil? href))
                         (conj memo (resolve-url base-url href)))))
                   []))))

(defn- changed-host?
  [url parent-url]
  (let [host (-> (url/url-like url) (.getHost))
        parent-host (-> (url/url-like parent-url) (.getHost))]
    (println host parent-host)
    (and (not= parent-host "root") (not= host parent-host))))

(defn- process-url
  [url parent-url]
  (cond
    (contains? @visited-urls url) {:urls [], :additional {:state "visited"}}
    (changed-host? url parent-url) {:urls [], :additional {:state "another-host"}}
    :else (do (swap! visited-urls conj url)
              (let [raw-response (fetch-page-by-url url)
                    status-desctiption (get-status-description raw-response)
                    state (:state status-desctiption)
                    urls (case state
                           "ok" (parse-page-links url (:body raw-response))
                           "redirect" [(resolve-url url (:location status-desctiption))]
                           "bad" [])]
                {:urls urls, :additional status-desctiption}))))

(defn- process-node
  ([urls depth]
    (process-node (create-tree-node urls depth)))
  ([parent]
    (let [current-depth (:depth parent)
          next-depth (dec current-depth)
          urls (:urls parent)
          parent-url (:url parent)]
      (if (<= current-depth 1)
        parent
        (do (doseq [child-node (map (fn [url]
                                      (let [processed (process-url url parent-url)
                                            urls (:urls processed)
                                            node (create-tree-node parent urls next-depth url (:additional processed))]
                                        (swap! (:children parent) conj node)
                                        node))
                                    urls)]
              (process-node child-node))
            parent)))))

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
         (let [urls (file->urls file-path)
               tree (process-node urls depth)]
           (print-tree tree 0)))
       (catch IllegalArgumentException e (->> e (.getMessage) (println "Invalid argument:")))
       (finally (shutdown-agents))))