(ns hangman.core
  (:gen-class))

(use '[clojure.string :only (join split)])

(defn draw-progress-bar
  ; Draws a progress bar, visualizing the remaining number of guesses.
  [limit current]
  (print "[")
  (print (join (take current (repeat "*"))))
  (print (join (take (- limit current) (repeat "-"))))
  (println "]"))

(defn print-prompt 
  ; prints out the prompt on each play iteration
  [board misses]
  (println "Current: " board " Invalid Guesses: " misses)
  (print "Progress Meter: " )
  (draw-progress-bar (* 3 (count board)) (count misses))
  (print "Enter a guess: ")
  (flush))

(defn check-letters
  ; takes a word, guess, and game board and fills slots return updated board.
  [w b g]
  
  (if (= 0 (count w)) 
    ; recursive end condition is a zero length word
    ""
    ; replace curent letter if necessary, iterate again on remaining letters
    (let [new-w (join (rest w)), new-b (join (rest b))] 
      (if (= g (str (first w))) 
        (str g (check-letters new-w new-b g))
        (str (first b) (check-letters new-w new-b g))))))

(defn update-invalid-guesses
  ; Determine if guess modified board, if not update invalid guesses 
  ; returns the collection of invalid guesses 
  [guess board new-board invalid-guesses]

  (if (= new-board board) 

    ; if nothing changes, we have a miss. 
    (if (some #(= % guess) invalid-guesses)  

      ; if this guess is redundant, we do nothing. 
      invalid-guesses

      ; this is a new guess, so add it to the miss list. 
     (do 
       (println "\nSorry," guess "is not in the word.")
       (sort (cons guess invalid-guesses))))

     ; This guess made us progress. Time for positive reinforcement!
     (do (println "\nGood job...") 
       invalid-guesses)))



(defn play-game
  ; main game loop
  [w b] 

  (loop [word w board b misses '()] 

    ; print out game status
    (print-prompt board misses)

    ; read in and process the next guess
    (let [guess (str (read))] 

      (if (= word guess) 

        ; if the input matches we are done! 
        (println "The answer is [" word "] You win!")

	; if we don't have a match we have some work to do. 
        (let [new-board (check-letters word board guess)
              new-misses (update-invalid-guesses guess board new-board misses)]

          (if (= new-board word) 

            ; if the guess brought us over the finish line, we're done
            (println "The answer is [" word "] You win!") 

            (if (>= (count new-misses) (* 3 (count word))) 

              ; exceeded the number of bad guesses, player loses. 
              (println "Sorry, you snoozed, and you lose! The secret word was [" word "].")

	      ; not done yet! 
              (recur w new-board new-misses))))))))
	  

	
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "\nWelcome to Hangman Deluxe.  Hang onto your hat!\n")
  (let 

    ; Read and split in the word dictionary, each word is on a 
    ; separate line. 
    [words (split (slurp "words.txt") #"\n+"),

     ; Select a random word from the dictionary for this game session
     word (nth words (rand-int (count words)))
     
     ; Bind to a blank game board based on length of word 
     board (join (take (count word) (repeat "_")))]

    ; call the main game play engine
    (play-game word board))
 ) 
