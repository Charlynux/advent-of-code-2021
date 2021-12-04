(require 'uiop)
(defvar day4-example (uiop:read-file-lines "~/git-repositories/advent-of-code-2021/day04/example"))
(defvar day4-input (uiop:read-file-lines "~/git-repositories/advent-of-code-2021/day04/input"))

(defun parse-drawn-numbers (input)
  (mapcar #'parse-integer (uiop:split-string (car input) :separator ",")))

(parse-drawn-numbers day4-input)

;; boards
(cddr day4-example)

;;;; loop
(defun get-first-board (result boards)
  (if boards
    (let ((line (car boards)))
      (if (equal "" line)
          (list result (cdr boards))
          ;; du fait de "cons", les lignes seront dans le mauvais ordre
          ;; a priori pas d'impact
          (get-first-board (cons line result) (cdr boards))))
    (cons result nil)))

(get-first-board nil '("1" "2" "" "3" "4"))
(get-first-board nil '("1" "2" "" "3" "4" "" "5" "6"))
(get-first-board nil '("1" "2"))

(defun split-boards (boards)
  (when boards
    (let ((result (get-first-board nil boards)))
      (cons (car result) (split-boards (cadr result))))))

(split-boards '("1" "2"))
(split-boards '("1" "2" "" "3" "4" "" "5" "6"))

(split-boards (cddr day4-example))

(defun read-boards (boards)
  (mapcar
   (lambda (board)
     (mapcar (lambda (line)
               (mapcar #'parse-integer
                       (remove-if (lambda (s) (equal "" s))
                                  (uiop:split-string line)))) board))
   boards))

(read-boards (split-boards (cddr day4-example)))

(defun row->columns (board)
  (apply #'mapcar #'list board))

(row->columns '((1 2) (3 4)))

(defun prepare-boards (boards)
  (mapcar
   (lambda (board) (append board (row->columns board)))
   boards))

(prepare-boards (read-boards (split-boards (cddr day4-example))))

(defun equal-to (n)
  (lambda (m) (= n m)))

(defun draw-number (number boards)
  (mapcar
   (lambda (board)
     (mapcar
      (lambda (numbers) (remove-if (equal-to number) numbers))
      board))
   boards))

(draw-number 1 (prepare-boards (read-boards (split-boards (cddr day4-example)))))

(defun check-winner (boards)
  (car (remove-if
        (lambda (board) (not (remove-if #'identity board)))
        boards)))

(check-winner '(((1 3) (2 4)) (() (3 4))))
(check-winner '(((1 3) (2 4)) ((5 6) (3 4))))

(defun sum-remaining (winning-board)
  (reduce #'+
          (remove-duplicates
              (reduce #'append winning-board))))

(sum-remaining '((3 5) () (3) (5)))

(defun bingo-recursion (numbers boards)
  (when numbers
    (let ((updated-boards (draw-number (car numbers) boards)))
      (let ((winner (check-winner updated-boards)))
        (print winner)
        (if winner
            (* (car numbers) (sum-remaining winner))
            (bingo-recursion (cdr numbers) updated-boards))))))

(defun day4-part1 (input)
  (let ((numbers (parse-drawn-numbers input))
        (boards (prepare-boards (read-boards (split-boards (cddr input))))))
    (bingo-recursion numbers boards)))

(day4-part1 day4-example)
;; 4512
(day4-part1 day4-input)
