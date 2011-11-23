(ns arlib.io
  (:require [arlib.katana :as k])
  )
;; io related tools that Arnoldo uses.

(defn substract [path1  path2]
  {:pre [(k/not-null? path1 path2) ; non-null fields
         (. (str path1) startsWith (str path2))	;
         ], 
   :post [instance? java.io.File %] ; post-condition
   }											
  "[substract] receives the following parameters:
   * path1: the main path, path2 must be a prefix of it.
   * path2:   the path that will be substracted
   calculates: substracts two paths, removes path2 from path1 (prefix)
   and outputs: path1 without the common prefix shared with path2
  "
  (new java.io.File (. (str path1) replace (str path2) ""))  
  )


(defn concat-paths [file1 file2]
  {:pre [(k/not-null? file1 file2) ; non-null fields
         ], 
   :post [(instance? java.io.File %)] ; post-condition
   }											
  "[concat] receives the following parameters:
   * file1: the first file to add
   * file2: second file to add
   calculates: concatenates two files
   and outputs: a combination of both paths
  "
  (new java.io.File (str file1) (str file2))  
  )