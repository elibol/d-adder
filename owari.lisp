;;; OWARI is played on a board with 14 holes arranged as two rows of 6
;;; with two holes in the middle on the ends of the rows.  Complete rules
;;; of the game are in the accompanying assignment document.

;;; The STATE of the game is very simple to describe.  All you need
;;; to keep track of are the number of stones in each of the fourteen
;;; holes (and whose turn it is, but that can be done automatically).
;;; Let's represent the board as a list of 14 numbers.  The first
;;; six will be the player's holes, the seventh will be the
;;; player's goal hole, the next six will be the computer's holes
;;; (counting counter-clockwise around the board) and the 14th will
;;; be the computer's goal hole.  So the list 
;;;  '(1 3 0 2 2 2 6 4 2 1 0 8 1 11)
;;; would represent the board position:
;;;
;;;     1  8  0  1  2  4
;;; 11                    6
;;;     1  3  0  2  2  2
;;;
;;; For the purposes of this program, holes are numbered from 0 to 13,
;;; beginning with the player's first hole and going counterclockwise.
;;; So above, there is 1 stone in hole number 0, 3 in hole number 1,
;;; 4 in number 7, 11 in number 13, etc.

;;; Programming note: to avoid conflict with functions you might write
;;; as part of the assignment, all the functions and constants (global
;;; variables) in this document begin with the % character.  Notice that
;;; atoms beginning with the % are perfectly acceptable in LISP, unlike
;;; most programming languages.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: OWARI
;;; Takes no arguments.  OWARI is the top-level call.  It sets the
;;; board to the initial position, and then iteratively prints it,
;;; asks the player for a move, handles the player's move, asks the
;;; computer for a move, handles the computer's move, and repeats.
;;; It also checks for the end-of-game condition after every move
;;; by each side, and prints final statistics if the end is reached.
;;;
;;; It is at this top level ONLY that the real board is maintained.
;;; Although all functions manipulate "boards", unless they return
;;; their values to this top-level function, the board will not change,
;;; and this is as it should be.  Thus, BOARD is not a global variable.

(defun owari ()
  (let ((BOARD '(3 3 3 3 3 3 0 3 3 3 3 3 3 0)))
    (loop
     (%print-board BOARD)
     (setq BOARD (%player-turn BOARD))
     (if (%end-position BOARD) (return))
     (%print-board BOARD)
     (setq BOARD (%computer-turn BOARD))
     (if (%end-position BOARD) (return)))
    (%print-final-board BOARD)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A set of utility functions and constants (global variables) used to
;;; make the rest of the code more readable and understandable.  Names
;;; begining with P refer to the human player; C refers to the computer.

;;; The numbers of the goal holes

(setq %p-goal-num 6)
(setq %c-goal-num 13)

;;; The values in the goal holes

(defun %p-goal-val (board) (nth %p-goal-num board))
(defun %c-goal-val (board) (nth %c-goal-num board))

;;; The numbers of the other holes

(setq %p-hole-nums '(0 1 2 3 4 5))
(setq %c-hole-nums '(7 8 9 10 11 12))

;;; The values in the other holes

(defun %p-hole-vals (board)
  (%extract %p-hole-nums board))

(defun %c-hole-vals (board)
  (%extract %c-hole-nums board))

;;; %EXTRACT takes a list of position numbers, and a list, and returns
;;; the elements of the list which occur at each of the positions.

(defun %extract (positions list)
  (cond
   ((null positions) nil)
   (t (cons (nth (car positions) list)
	    (%extract (cdr positions) list)))))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; %END-POSITION takes a BOARD, and returns NIL if the game is not over
;;; (that is, if both players still have stones on their side), T otherwise.
;;; Since COND returns nil if none of the conditions is met, the final
;;; line of this function, (t nil), is not strictly necessary.  However,
;;; some people include this line to make it clear that the function is
;;; in fact supposed on occasion to reach that line.
;;;
;;; Programming note:  Look up the function APPLY in the textbooks.  It
;;; can be a very useful tool.  Make sure you understand how this function
;;; works.  Notice the trick:  since none of the board positions can have
;;; a negative number of stones, then as long as the sum of the values is 0,
;;; all the individual values must be zero as well.

(defun %end-position (board)
  (cond
   ((or (equal 0 (apply '+ (%p-hole-vals board)))
	(equal 0 (apply '+ (%c-hole-vals board))))
    t)
   (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; %PRINT-FINAL-BOARD prints the board, calculates the final score,
;;; and prints that as well.  It assumes that checking has already
;;; been done to ensure that the game really is over.  This function
;;; returns the atom 'PLAYER or 'COMPUTER, according to whether the
;;; player or the computer won, or 'TIE if the game was a tie.

(defun %print-final-board (board)
  (let ((p-score 0) (c-score 0))
    (terpri)
    (princ "The game is over")
    (terpri)
    (setq p-score (+ (%p-goal-val board)
		     (apply '+ (%p-hole-vals board))))
    (setq c-score (+ (%c-goal-val board)
		     (apply '+ (%c-hole-vals board))))
    (princ "Final score: ")
    (princ "Player: ")
    (princ p-score)
    (princ "    Computer: ")
    (princ c-score)
    (terpri)
    (cond
     ((equal p-score c-score) (princ "It's a tie!") 'TIE)
     ((> p-score c-score) (princ "You have won!") 'PLAYER)
     ((< p-score c-score) (princ "The computer has won!") 'COMPUTER))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: %PRINT-BOARD
;;; Takes a BOARD, and prints it nicely.  A quick-and-dirty function.
;;; Notice how dolist is used.  This function does not make best use
;;; of the game-specific utility functions.

(defun %print-board (BOARD)
  (terpri)
  (princ "     ")
  (dolist (index (reverse %c-hole-nums))
	  (princ (nth index BOARD))
	  (princ "  "))
  (terpri)
  (princ (%c-goal-val BOARD))
  (princ "                        ")
  (princ (%p-goal-val BOARD))
  (terpri)
  (princ "     ")
  (dolist (index %p-hole-nums)
	  (princ (nth index BOARD))
	  (princ "  "))
  (terpri))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; %PLAYER-TURN takes a board, asks the player for a move, verifies
;;; that the move is legal, and returns a new board by calling %do-move
;;; with the given board and the move entered from the terminal.

(defun %player-turn (BOARD)
  (let ((hole-num))
    (loop
     (princ "Move stones from which hole (0 - 5)? ")
     (setq hole-num (read))
     (if (and (member hole-num %p-hole-nums)
	      (< 0 (nth hole-num BOARD)))
	 (return (%do-move 'player hole-num BOARD))
       (princ "Illegal move! ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	    
;;; Function: %DO-MOVE
;;; Arguments:
;;;    who: an atom, either 'player or 'computer, indicating whose move
;;;         it is (for the purpose of deciding which holes to skip and
;;;         whether the move ended on the current players' side.
;;;    hole-num: the number of the hole from which to move;
;;;         %DO-MOVE assumes error-checking has already been done.
;;;    BOARD: the old board position.
;;; Returns: a new board, which is the board resulting from moving out
;;;    of hole-num on the old board.
;;; Notice that when it calls %do-move-aux, it passes it the board with
;;;    the stones already removed from the hole we start at.

(defun %do-move (who hole-num BOARD)
  (%do-move-aux who
		hole-num
		(nth hole-num BOARD)
		(%remove-stones hole-num BOARD)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; %DO-MOVE-AUX is the auxiliary function for %DO-MOVE.  It is given:
;;; whose turn it is to play, the hole number we're about to drop
;;; a stone into, how many stones are still in the player's hand, and
;;; the BOARD as it looks so far.  When there are no more stones left
;;; in the player's hand, %DO-MOVE-AUX determines whether any stones
;;; have been captured, and finally returns the new BOARD.
;;;
;;; Programming note: the formal parameter hole-num is manipulated and
;;; changed (using setq) throughout this program.  All formal parameters
;;; are local to their functions and may be changed locally without error.
;;;
;;; Style note: This function is too long to leave with just a header
;;; comment, so I should probably break it into smaller functions.  I
;;; have chosen instead to use in-line comments.

(defun %do-move-aux (who hole-num how-many BOARD)

  (cond
   
   ((equal 0 how-many)                   ;check if our hand is empty
    (%do-capture who hole-num BOARD))    ;if so, end by calling %do-capture
   (t                                    ;we still have stones, so
    (setq hole-num (1+ hole-num))        ;move to the next hole

    (if (or (and (equal who 'player)              ;if we're over the
		 (equal hole-num %c-goal-num))    ;opponent's goal hole,
	    (and (equal who 'computer)            ; ...
		 (equal hole-num %p-goal-num)))   ; ...
	(setq hole-num (1+ hole-num)))            ;skip it
    
    (if (>= hole-num 14)                  ;if we've gone all the way around
	(setq hole-num (- hole-num 14)))  ;the circle, go back to the start

    (%do-move-aux who                     ;call this function recursively
		  hole-num                ;with the new hole number we're over
		  (1- how-many)           ;and with one fewer stone, which
		  (%add-stones hole-num 1 BOARD)))))  ;we put in this hole

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; %DO-CAPTURE is called by %DO-MOVE-AUX at the point when
;;; all stones have been distributed.  At that point, if there is
;;; exactly one stone in the last hole, and if that hole is on the
;;; current player's side of the board, then any stones on the other
;;; side are captured.  The arguments are:  whose turn it is, the
;;; hole number that player finished at, and a board.  It returns a
;;; new board reflecting the results of the capture, if there is one,
;;; or else simply returns the old board.

(defun %do-capture (who hole-num board)
  (cond
   ((and (equal who 'player)
	 (member hole-num %p-hole-nums)
	 (equal 1 (nth hole-num board)))
    (%add-stones %p-goal-num
		(nth (%opposite hole-num) board)
		(%remove-stones (%opposite hole-num) board)))
   ((and (equal who 'computer)
	 (member hole-num %c-hole-nums)
	 (equal 1 (nth hole-num board)))
    (%add-stones %c-goal-num
		(nth (%opposite hole-num) board)
		(%remove-stones (%opposite hole-num) board)))
   (t board)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; %OPPOSITE is a utility function which returns the number of the
;;; hole %opposite the one it is given in its argument.

(defun %opposite (hole-num)
  (- 12 hole-num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; %REMOVE-STONES takes a hole number and a BOARD, and returns a
;;; new board with all the stones removed from the given hole number.

(defun %remove-stones (hole-num BOARD)
  (cond
   ((null BOARD) nil)
   ((equal hole-num 0) (cons 0 (cdr BOARD)))
   (t (cons (car BOARD) (%remove-stones (1- hole-num) (cdr BOARD))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; %ADD-STONES takes a hole number, a number of stones to add, and a
;;; BOARD, and returns a new board with the stones added to the given
;;; hole number.  Its algorithm is similar to that of %REMOVE-STONES

(defun %add-stones (hole-num how-many BOARD)
  (cond
   ((null BOARD) nil)
   ((equal hole-num 0) (cons (+ how-many (car BOARD)) (cdr BOARD)))
   (t (cons (car BOARD) (%add-stones (1- hole-num) how-many (cdr BOARD))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; %COMPUTER-TURN:  Asks the computer for its move by calling "GET-MOVE".
;;; GET-MOVE is the top-level function you need to write!  As a place-
;;; holder, a function GET-MOVE is included that asks for terminal input,
;;; but you need to replace that function with one of your own.  (You will
;;; probably write additional utility functions as well, but they will
;;; not interact directly with the game.)
;;;
;;; %COMPUTER-TURN checks to see if the move is legal, but if it isn't then
;;; your function is doing something wrong, since computers should never
;;; make illegal moves.  A message will be printed that the move was illegal,
;;; and the computer loses its turn.  Of course, this should not happen.

(defun %computer-turn (BOARD)
  (let ((hole-num (get-move board)))
     (cond
      ((and (member hole-num %c-hole-nums)
	    (< 0 (nth hole-num BOARD)))
       (%do-move 'computer hole-num BOARD))
      (t (princ "ERROR!  The computer tried to move from ")
	 (princ hole-num)
	 (terpri)
	 BOARD))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GET-MOVE: the place-holder function.  This is the function you
;;; must replace with your own.  This top-level function will be
;;; passed the board as its one argument.  Although you will write
;;; a set of functions to solve the problem, the top level function
;;; must be called GET-MOVE if it is to work with the rest of the
;;; program.  Notice also that you must load your functions AFTER
;;; loading this file, else your function will be overwritten by this one.

(defun get-move (board)
  (princ "Move stones from which hole (7 - 12)? ")
  (read))
