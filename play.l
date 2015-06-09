;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assigntment 4
;; Author: Melih Elibol
;; Description: Implementation of a minimax algorithm,
;; with alpha beta pruning, for owari.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load 'owari.l)

;; The name of the player.
(setq name "_-'D E A T H A D D E R'-_")

;; The indices of the computer's holes,
;; this is reversed to align with
;; the human's index. (simplifies static-eval)
(setq c-index '(12 11 10 9 8 7))

;; The indices of the human's holes.
(setq h-index '(0 1 2 3 4 5))

;; The score of the computer and the human, respectively.
(setq game-score (cons 0 0))

;; Total stones.
(setq total-stones 36)

;; The number of movable holes each player begins with.
(setq index-length 6)

;; The index of the computer's last hole.
(setq cpu-end 12)

;; The maximum depth of the minimax algorithm.
(setq maxlevel 7)

;; A board for testing.
(setq test-board '(3 3 3 3 3 3 0 3 3 3 3 3 3 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: An object that (loosely) represents a game state. 
;; Parameters:
;;   board: An owari board
;;   turn: A game turn ('computer or 'human)
;;   index: The index of a hole that maps a preceding game state to this game state.
;;   precedent: The preceding game state that produced this game state.
;; *Note: Multiple precedences may exist for a given game state. The relationship could be
;; *represented by a graph.
;; Result: A game state object.
(defstruct (state
	   (:constructor make-state (board turn index precedent))
	    (:print-object (lambda (object stream)
			   (setq turn (state-turn object))
			   (setq data (list (state-index object) (state-value object)))
			   (terpri stream)
  			   (princ "         " stream)
			   (princ 'COMPUTER stream)
   			   (if (equal turn 'COMPUTER) (princ data stream))
			   (print-board (state-board object) stream)
  			   (princ "          " stream)
			   (princ 'HUMAN stream)
   			   (if (equal turn 'HUMAN) (princ data stream))
			   (terpri stream)
			   )))
            board
	    turn
	    index
	    precedent
           (value nil)
	   (indices (get-indices turn))
	   (score (get-score board)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: A function that returns a game state of test-board.
;; Parameters: A turn to assign the game state.
;; Result: A game state.
(defun test-state (turn) (make-state test-board turn nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Returns the complement of a turn: 'human is not 'computer.
;; Parameters: A turn.
;; Result: The complement of turn.
(defun other (turn) (if (equal turn 'computer) 'human 'computer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Returns the score of a board as a cons object.
(defun get-score (board)(cons (%c-goal-val board) (%p-goal-val board)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Returns the indices of holes.
;; Parameters: A game turn.
;; Returns: Indices of holes for a game turn.
;; *Note: The result for turn 'computer is reversed.
(defun get-indices (turn) (if (equal turn 'computer) c-index h-index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Returns an ordered list of stones in each hole of the human player.
(defun h-stones (board)(loop for i from 0 below index-length collect (nth i board)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Returns the reversed* list of stones in each hole of the computer player.
(defun c-stones (board)(loop for i from 0 below index-length collect (nth (- cpu-end i) board)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Returns either c-stones or h-stones based on the game state s.
(defun get-stones (s)
  (if (equal (state-turn s) 'computer) (c-stones (state-board s)) (h-stones (state-board s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Returns all legal moves given the game state s.
(defun legal-sub-states (s)
      (mapcan #'(lambda (i j)(if (> j 0) (list
		(make-state (%do-move (state-turn s) i (state-board s))
			    (other (state-turn s)) i s))))
	        (state-indices s) (get-stones s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: A recursive implementation of the minimax algorithm, with alpha beta pruning.
;; Parameters: A game state, a level (depth), the current alpha, the current beta, and a static evaluator.
;; Result: A game state that corresponds to stones being moved from a hole on the current board.
(defun best-state (state level alpha beta s-eval)
(if (<= level 0)
    (progn (setf (state-value state) (funcall s-eval state)) state)
    (progv `(sub-states tmp ret aState bState)
	   `(,(legal-sub-states state) nil nil nil nil)
	    ;; This makes sure that if no legal sub states of a game state exist, then we still return something.
	    ;; Which implies an end game state. If it's positive then we win!
	    (if (<= (list-length sub-states) 0)
		(return-from best-state (progn (setf (state-value state) (funcall s-eval state)) state)))
	    (dolist (i sub-states)
		    (setq tmp (best-state i (- level 1) alpha beta s-eval))
		    ;; this is done to ensure a value is returned, so tmp != nil, and best-move !=nil.
		    (setq ret i)
		    (setf (state-value ret) (state-value tmp))
		    (if (not (null tmp)) ;; now impossible to be null because of ret...
			(if (equal 'computer (state-turn state))
			    (if (< alpha (state-value tmp))
				(progn (setq alpha (state-value tmp))
				       (setq aState i)
				       (setf (state-value aState) alpha)))
			    (if (> beta (state-value tmp))
				(progn (setq beta (state-value tmp))
				       (setq bState i)
				       (setf (state-value bState) beta)))))
		    (if (<= beta alpha) (return)))
			(if (equal 'computer (state-turn state))
			    (return-from best-state (if (eq nil aState) ret aState))
			    (return-from best-state (if (eq nil bState) ret bState))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: This function starts the minimax algorithm.
;; Parameters: A game state, a level (minimax depth), and a static evaluator.
;; Result: Returns an index that corresponds to a hole; the alleged "best move".
(defun best-move (state level s-eval)(state-index (best-state state level -10000000 10000000 s-eval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: This is the overriden function that the owari game calls.
;; It will compute the best move, and print the move the computer player made before submitting the move.
(defun get-move (board)(progn
(setq game-score (get-score board))
(setq move (best-move (make-state board 'computer 0 0) maxlevel  #'static-eval-weighted))
(terpri) (princ name) (princ " moves stones from hole ") (princ move) (terpri) move))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: A weighted static evaluator.
;; Quantifies the value of a game state by weighing quantifiers.
;; Score and Control are quantified.
;; The weight of quantifiers shift as a game progresses.
;; Score carries greater weight in value toward the beginning of the game.
;; Control carries greater weight in value toward the end of the game.
;; Maximum of 20% given to control (end of game).
(defun static-eval-weighted (s)
    (progv '(score stones cpu hmn a b c d f g h i j)
	   `(,(state-score s) total-stones ,(c-stones (state-board s)) ,(h-stones (state-board s)) 0 0 0 0 0 0 0 0 0)
	    (setq g (+ (car score) (cdr score))) ;; stones captured
	    (setq f (- total-stones g)) ;; stones in play
	    (setq a (- (car score) (cdr score))) ;; difference in score
	    (setq b (- (apply '+ cpu) (apply '+ hmn))) ;; difference in control
	    (setq c (/ a (max g 1))) ;; normalized score difference
	    (setq d (/ b (max f 1))) ;; normalized control difference
	    (setq h (/ f total-stones)) ;; normalized potential control
	    (setq i (/ g total-stones)) ;; normalized lost control
	    (setq j (+ (* 0.8 c h) (* 0.2 d i))) ;; weighted decision
	    j)) ;; hopefully the compiler will optimize this!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: A simple static evaluator.
;; This is a brute force static evaluator that measures only the relative score.
;; This algorithm will perform better the more depth it's able to examine.
(defun static-eval-simple (s)
  (setq score (state-score s))
  (- (car score) (cdr score)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Prints an owari board to a given stream.
;; Parameters: An owari board, and a stream.
;; Result: None.
(defun print-board (BOARD &optional stream)
  (terpri stream)
  (princ "     " stream)
  (dolist (index (reverse %c-hole-nums))
	  (princ (nth index BOARD) stream)
	  (princ "  " stream))
  (terpri stream)
  (princ (%c-goal-val BOARD) stream)
  (princ "                        " stream)
  (princ (%p-goal-val BOARD) stream)
  (terpri stream)
  (princ "     " stream)
  (dolist (index %p-hole-nums)
	  (princ (nth index BOARD) stream)
	  (princ "  " stream))
  (terpri stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: A "clever" static evaluator (in the works...)
;; Uses information about captures to quantify value (doesn't use score).
;; This is only good with limited depth.
#| (defun static-eval-clever (s)
       (progv '(v)
	      `(0)
	       (loop for i from 0 to 5 do
	             ;basic strategy
		     (progn 
		     (if (equal (nth i o) 0) ; this is favorable, since it implies a capture
			 (+= 'v 1)
		         (+= 'v 0))
		     )
		     ;advanced strategy
		     (loop for j from i to 5 do
			   (if (equal (+ (nth i p) i) (nth j p)) ;if last moves to position
			       (if (equal (nth j p) 0) ;if position is zero
				   (if (> (nth j o) 0) ;if opposite is greater than zero
				       (+= 'v 10) ;this is a very good state!
				       (+= 'v 0))
				   (+= 'v 0))
			       (+= 'v 0)))
		 )
		 (return v))) |#
