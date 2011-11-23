(ns arlib.files
  (:import
   (java.io File))  
)



(defn get-file [str]
  (if (instance? String str)
    (new File str)
    str)
  )
