(ns ikhilko.web-crawler.core
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

;------------------ FILES FUNCTIONS -------------------

; string to hash-map (excluded last value in line)
;(defn- line->point
;  [i line]
;  (->> (str/split line #",")
;       (butlast)
;       (reduce #(conj %1 (Double/parseDouble (str/trim %2))) [])
;       (hash-map :index (inc i) :vals)))
;
;; read file to list of hash-maps
;(defn- file->points
;  [filename]
;  (->> (io/reader filename)
;       (line-seq)
;       (reduce-indexed #(if (not (str/blank? %3)) (conj %1 (line->point %2 %3)) %1) [])))

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
         (println "Depth: " depth))
       (catch IllegalArgumentException e (->> e (.getMessage) (println "Invalid argument:")))
       (finally (shutdown-agents))))