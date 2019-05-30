(ns clojure.puzzle.game
   (:gen-class))
(use '[clojure.string :only (split triml)]) 
(def tresuremap " ")
(def rows 0)
(def cols 0)
(def puzzleArray [])
(def mazesolved false)
(println tresuremap)
(def rows (clojure.string/split-lines tresuremap))
(def cols (nth rows 0))

;; function responsible for solving the maze
(defn solve-now[i j]
		(cond
    (< i 0) false
    (> i (- (count rows) 1)) false
    (< j 0) false
    (> j (- (count cols) 1)) false
    (= (aget puzzleArray i j) "@") (do (def mazesolved true) true)
    ;;(= (aget puzzleArray i j) "@") (and ( true (def mazesolved true)))
    (= (aget puzzleArray i j) "#") false
    (= (aget puzzleArray i j) "+") false
    (= (aget puzzleArray i j) "-") 
			    (do
			      (aset puzzleArray i j "+")
			    		(cond
           (= (solve-now (- i 1) j) true) true
           (= (solve-now  i (+ j 1)) true) true
           (= (solve-now (+ i 1) j) true) true
           (= (solve-now i (- j 1)) true) true
           :else (aset puzzleArray i j "!")
			    		)
			    )

    :else (do 
          (if(= (aget puzzleArray i j) "!") false
             (do
              (try
              ;; assumption : will only be thrown if the unexpected charater is encountered in the path.
              ;;if a path the algorithim uses, does not contains a unexpected charater, the alogorithim 
              ;; will process as usual and no exception will be thrown.
  														(throw (Exception. "Unexpected character present in file. Use the correct input")) 
        								(catch Exception e (println "Unexpected character encountered in file. Use the correct input")
           					(System/exit 0)
  												))
             )
          )

    ))
)

(defn solveMaze[]
    
    ;; Validation with loding file in 
    (try
    		(def tresuremap (slurp "map.txt"))
    		(catch Exception e (println "Issue in loading file. Please ensure correct format is used and file is present in the directory.")
       (System/exit 0)
    ))

  ;; validation for empty file.
  (if(empty? tresuremap)
     (do 
        (println "The file is empty. Use correct input file.")
        (System/exit 0)
     )
     (do 
       (def rows (clojure.string/split-lines tresuremap))
							(def cols (nth rows 0))
							(def puzzleArray (to-array-2d(partition (count cols) (filter #(not (clojure.string/blank? %))(split tresuremap #"")))))
     )
  )

  ;; validation on extra cols and rows
  (doseq [x (range (count rows))]
  				(try
  						(if(not(= (count(nth rows x)) (count cols))) (throw (Exception. "Rows/cols have different length")) )
        (catch Exception e (println "The input file provided is not correct. Use the correct input.")
           (System/exit 0)
  				))
  )

  ;; validation for empty character
  (doseq [x (range (count rows)) 
          y (range (count cols))] 
          
        (try
          (aget puzzleArray x y )
          (if (= (aget puzzleArray x y ) " ")
          		(throw (Exception. "Blank Character in array"))
          )
        (catch Exception e (println "The input file provided is not correct. Use the correct input.")
           (System/exit 0)
        )
  ))
  
  ;; run the recursive algorithim

  (try 
     (solve-now 0 0)
     (catch Exception e (println "An unexpected error occured. Try again or contact the developer")
  ))

  ;; printing the challenge

  (println "This is my challenge:")
  (println )
  (println tresuremap)

  ;; printing the result
  (println )
  (if (= mazesolved true)
      (println "Woo hoo, I found the treasure :-)")
      (println "Uh oh, I could not find the treasure :-(")
  )

  (doseq [x (range (count rows)) 
          y (range (count cols))] 
          
        (try
          (if (= 0 y) (println ))
          (print(aget puzzleArray x y))
        (catch Exception e 
           (println "An unexpected error occured. Try again or contact the developer")
           (System/exit 0)
        )
  ))
(println )
  

  ;;end of function
 )
(solveMaze)