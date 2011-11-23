(ns arlib.zippers
  (:import
   (java.io File))  
  (:require [clojure.zip :as zip])
  (:require [arlib.files :as files])
  (:require [arlib.katana :as k])
  (:require [arlib.tree :as artree])
  )



(defn get-zip-from-directory-tree [directory-tree]
  {:pre [(k/not-null? directory-tree) ; non-null fields
         (seq? directory-tree)		; checks for [directory-tree]
         ], 
   }											
  "[get-zip-from-directory-tree] receives the following parameters:
   * directory-tree:   
   calculates: a zipper form a directory tree
   and outputs: a zipper
  "
  (zip/seq-zip directory-tree)
  )

  (defn get-directory-zip [path]
    "Returns a directory zipped version of the path"
    (get-zip-from-directory-tree (artree/get-directory-tree (files/get-file path))))

(defn get-directory-zip-with [path f]
  "Returns a directory zipped version of the path. f is used as a filter"
  (get-zip-from-directory-tree (artree/get-directory-tree-with (files/get-file path) f)))


(defn get-directory-node [loc]
  {:pre [(zip/branch? loc) ; non-null fields
         ], 
   }											
  "[get-directory-node] receives the following parameters:
   * loc: a zipper location
   calculates: For nodes that are branches this function returns the real directory value
   and outputs: the directory node
  "
  (first (zip/node loc))  
  )


;(branch? (next (vector-zip (zip-map (fn [n nx] (if (vector? n) n (* n 100) )) (vector-zip '[5 [10 20 30] [1 2 3] ])))))

(defn zip-map [f zipper]
  " map over a zipper applying f to every node.
    the function received has the form (f node-value loc)
    receives the node value and its location for additional processing."
  (loop [z zipper]
    (if (zip/end? z)
      (zip/seq-zip (zip/root z))
      (recur (zip/next (zip/edit z f z))))))


;; (defn tree-edit [zipper editor]
;;   (loop [loc zipper]
;;     (if (zip/end? loc)
;;       (zip/root loc)
;;       (if-let [matcher-result (matcher (zip/node loc))]
;;         (recur (zip/next (zip/edit loc (partial editor matcher-result))))
;;         (recur (zip/next loc))))))


(defn do-zip-run
  "Runs over a zipper executing some side effects.
   Receives a zipper and a function that performs the required changes
   f is of the form (f node-value loc) where node-value is the stored
   value in the node and loc is the zipper location
"
  [f zipper]
   (loop [z zipper]
    (if (zip/end? z)
      'nil
      (do
        (f (zip/node z) z)
        (recur (zip/next z))))))
                                
  


 ;; (do-zip-run (fn [node loc]
 ;;              (if (clojure.zip/branch? loc)
 ;;                (do (println "size: " (count (clojure.zip/children loc)))
 ;;                    (println "---")
 ;;                    (doseq  [c (clojure.zip/children loc)]
 ;;                      (print ":)" c))
 ;;                      (println "---")
 ;;  									(println (first node)))
 ;;                (println node))) (get-directory-zip "/home/amuller/temp/test"))

