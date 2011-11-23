(ns arlib.katana
  
  )
; Katana contains useful tools for debugging / development


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))



(defmacro chki[x cond msg]
  "Checks the condition, returns x if the condition returns true.
   the condition is independent of x"
  `(let [x# ~x
         cond# ~cond]
     (if cond# 
     x#
     (throw (IllegalArgumentException.
             (str "Expression: "  '~x " value: [> " (str x#) " <] did not satisfy: " '~cond " [> " ~msg " <]"))) )))

(defn not-null? [& value-list]
  "not-null receives:
   value-list: a list of values
   calculates: if any of the values is null
   and outputs: true if none of the input values is null
  "
  (not-any? #(nil? %1) value-list)
  )

(defmacro chk[x f msg]
    "Checks the condition, returns x if the condition returns true
   f will be called with x and it must return true if everything is fine."
    `(let [x# ~x
         ]
     (if (~f x#)
     x#
     (throw (IllegalArgumentException.
             (str "Expression: "  '~x " value: [> " (str x#) " <] did not satisfy: " '~f " [> " ~msg " <]"))) )))



