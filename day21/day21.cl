(defparameter *board-size* 10)
(defparameter *score-target* 1000)

(defun move (position dice-roll)
  (mod (+ position dice-roll) *board-size*))

(defun update-player (player rolls)
  (destructuring-bind (position . score) player
      (let ((new-position (reduce #'move rolls :initial-value position)))
        (cons
         new-position
         (+ score new-position 1)))))

(defun increment-dice (dice)
  (let ((new-dice (1+ dice)))
    (if (<= new-dice 100) new-dice 1)))

(defun play-turn (player1 player2 dice turn)
  (let* ((player (if (zerop (mod turn 2)) 'player2 'player1))
         (rolls (list
                 dice
                 (increment-dice dice)
                 (increment-dice (increment-dice dice))))
         (updated (update-player (if (eql 'player1 player) player1 player2) rolls)))
    (if (<= *score-target* (cdr updated))
        (* (* 3 turn) (cdr (if (eql 'player1 player) player2 player1)))
        (play-turn (if (eql 'player1 player) updated player1)
                   (if (eql 'player2 player) updated player2)
                   (increment-dice (increment-dice (increment-dice dice)))
                   (1+ turn)))))

(play-turn (cons (1- 4) 0) (cons (1- 8) 0) 1 1)
(play-turn (cons (1- 8) 0) (cons (1- 7) 0) 1 1)
