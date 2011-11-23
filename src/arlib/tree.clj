(ns arlib.tree
  (:require [arlib.katana :as k])
  (:require [arlib.files :as files])
  (:require [clojure.walk :as walk])
  )


(defn map-tree [f tree]
  {:pre [(k/not-null? tree f) ; non-null fields
         (seq? tree)		; checks for [tree]
         ], 
   :post [(seq? %)] ; post-condition
   }											
  "[map-tree] receives the following parameters:
   * tree: the tree to process
   * f: the function that will be applied
   calculates: maps f over every node of the tree
   and outputs: the new transformed tree
  "
  (walk/postwalk #(if (seq? %1) %1 (f %1)) tree )  
  )

(defn tree-map )

;; returns a tree filtered with the given filtering function
(defn get-directory-tree-with [path f]
  {:pre [(k/not-null? path) ; non-null fields
         (. (files/get-file path) exists)
         ], 
   :post [(seq? %)] ; post-condition
   }											

  "Returns the directory tree structure defined by the given path"
  (let [base (k/chk																								; check if expression is valid
              (files/get-file path)																; expression
              f									                    							; validation func
              "Given file does not satisfy filtering expression" 	; error message
              )
        ]
    (if (. base isDirectory)
      (cons base  (map #(if (. %1 isDirectory) (get-directory-tree-with %1 f) %1)
                       (filter f (. base listFiles))))
      (list base))
    ))

(defn get-directory-tree [path]
  (get-directory-tree-with path (fn [_] true)))

(defn head [tree] (if (seq? tree) (first tree) tree))
(defn leaf? [tree] (not (seq? tree)))
(defn branch? [tree] (seq? tree))
(defn children [tree] {:pre [(seq? tree)]}  (rest tree))


(defn flatten-with
 "Takes any nested combination of sequential things (lists, vectors,
 etc.) and returns their contents as a single, flat sequence.
 (flatten nil) returns nil. Uses f to determine when to stop flattening
 sequence? will flatten everything. Useful when you are tired of thinking."
[f x]
(filter (complement f)
(rest (tree-seq f seq x))))

(defn get-path-aux [path tree node ]
  {:pre [(k/not-null? path) ; non-null fields         
         (k/not-null? node) ]}
         
  "[get-path-aux] receives the following parameters:
   * path: 
   * tree:
   * node:   
   calculates: auxiliary function for path processing
   and outputs: the path towards the given node
  "
  (cond (nil? tree) '()
        ;(and (leaf? tree) (= tree node)) (conj path tree)
        (= (head tree) node) (conj path (head tree))
        (leaf? tree) '()
        :else
        (filter #(not (empty? %1)) (map #(get-path-aux (conj path (head tree)) %1 node) (children tree) ))
    )    
  )






(defn get-path [tree node]
  {:pre [(k/not-null? tree node) ; non-null fields
         (seq? tree)
         ], 
   }											
  "[get-path] receives the following parameters:
   * tree: tree to explore
   * node: node to search for
   calculates: the path that preceedes the given node
   and outputs: the corresponding node and the node at the end
  "
  (flatten-with seq? (get-path-aux [] tree node))
  )

(defn get-children [tree node]
  {:pre [(k/not-null? tree node) ; non-null fields
         (seq? tree)		; checks for [node]
         ], 
   :post [(seq? %)] ; post-condition
   }											
  "[get-children] receives the following parameters:
   * tree:
   * node:   
   calculates: returns the children of the given node
   and outputs: the children
  "
  (map #(map head (children %1))  (filter #(= node( head %1))
                    (filter branch?  (tree-seq seq? rest tree))))
  )


(defn get-sibblings-aux [tree node]
  {:pre [(k/not-null? tree node) ; non-null fields
         ], 
   }											
  "[get-sibblings] receives the following parameters:
   * tree: input tree
   * node: node that will be searched
   calculates: returns the sibblings of the given node
   and outputs: the sibblings of the given node
  "
  (cond (and  (branch? tree)
              (>= (count (filter #(= node %1) (children tree))) 1))
        (apply vector (map head (children tree)))
        (leaf? tree)
        []
        :else 
        (map #(get-sibblings-aux  %1 node) tree)
        )
  )


(defn get-sibblings [tree node]
  {:pre [(k/not-null? tree node) ; non-null fields
         ], 
   }											
  "[get-sibblings] receives the following parameters:
   * tree: input tree
   * node: node that will be searched
   calculates: returns the sibblings of the given node
   and outputs: the sibblings of the given node
  "
  (filter #(not (empty? %1))  (flatten-with seq? (get-sibblings-aux tree node))))

