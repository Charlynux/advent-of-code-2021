;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining utils function to manage association lists as Clojure Hash Maps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aget (key alist)
  (cdr (assoc key alist)))

(defun aupdate (key f alist)
  (let ((old-value (aget key alist)))
    (acons key (apply f old-value) alist)))

(defmacro comment (&rest body) nil)

(comment
 (let ((state '((:horizontal 0) (:depth 0))))
   (print (assoc :horizontal state))
   (print (acons :horizontal 42 state))
   (print (assoc :horizontal (acons :horizontal 42 state)))

   (print (aget :horizontal state))
   (print (aupdate :horizontal (lambda (n) (+ n 1)) state))
   ))

;; During the day, think about a more "Common Lisp way" of doing.

;; (horizontal depth)

(defun forward-1 (x state)
  (cons
   (+ (car state) x)
   (cdr state)))

(defun down-1 (x state)
  (cons
   (car state)
   (+ (cdr state) x)))

(defun up-1 (x state)
  (cons
   (car state)
   (- (cdr state) x)))

(defun parse (line)
  (let ((pair (uiop:split-string line :separator " ")))
    (cons (car pair) (parse-integer (cadr pair)))))

(defvar day2-example '("forward 5"
                       "down 5"
                       "forward 8"
                       "up 3"
                       "down 8"
                       "forward 2"))

(comment
 (mapcar #'parse day2-example))

(defun submarine (forward down up)
  )

(defun day2-part1 (lines)
  (let ((state
          (reduce
           (lambda (state pair)
             (cond
               ((equal (car pair) "forward") (forward-1 (cdr pair) state))
               ((equal (car pair) "down") (down-1 (cdr pair) state))
               ((equal (car pair) "up") (up-1 (cdr pair) state))
               (otherwise state)))
           (mapcar #'parse lines)
           :initial-value (cons 0 0))))
    (* (car state) (cdr state))))

(day2-part1 day2-example)

(defvar day2-input (uiop:read-file-lines "./git-repositories/advent-of-code-2021/day02/input"))
(day2-part1 day2-input)

(defun forward-2 (value state)
  (destructuring-bind (horizontal depth aim) state
    (list (+ horizontal value) (+ depth (* aim value)) aim)))

(defun down-2 (value state)
  (destructuring-bind (horizontal depth aim) state
    (list horizontal depth (+ aim value))))

(defun up-2 (value state)
  (destructuring-bind (horizontal depth aim) state
    (list horizontal depth (- aim value))))

(defun day2-part2 (lines)
  (let ((state
          (reduce
           (lambda (state pair)
             (cond
               ((equal (car pair) "forward") (forward-2 (cdr pair) state))
               ((equal (car pair) "down") (down-2 (cdr pair) state))
               ((equal (car pair) "up") (up-2 (cdr pair) state))
               (otherwise state)))
           (mapcar #'parse lines)
           :initial-value (list 0 0 0))))
    (* (car state) (cadr state))))

(day2-part2 day2-example)
(day2-part2 day2-input)
