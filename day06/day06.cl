(require 'uiop)
(defvar day6-example '(3 4 3 1 2))
(defvar day6-input (mapcar #'parse-integer (uiop:split-string "3,5,3,5,1,3,1,1,5,5,1,1,1,2,2,2,3,1,1,5,1,1,5,5,3,2,2,5,4,4,1,5,1,4,4,5,2,4,1,1,5,3,1,1,4,1,1,1,1,4,1,1,1,1,2,1,1,4,1,1,1,2,3,5,5,1,1,3,1,4,1,3,4,5,1,4,5,1,1,4,1,3,1,5,1,2,1,1,2,1,4,1,1,1,4,4,3,1,1,1,1,1,4,1,4,5,2,1,4,5,4,1,1,1,2,2,1,4,4,1,1,4,1,1,1,2,3,4,2,4,1,1,5,4,2,1,5,1,1,5,1,2,1,1,1,5,5,2,1,4,3,1,2,2,4,1,2,1,1,5,1,3,2,4,3,1,4,3,1,2,1,1,1,1,1,4,3,3,1,3,1,1,5,1,1,1,1,3,3,1,3,5,1,5,5,2,1,2,1,4,2,3,4,1,4,2,4,2,5,3,4,3,5,1,2,1,1,4,1,3,5,1,4,1,2,4,3,1,5,1,1,2,2,4,2,3,1,1,1,5,2,1,4,1,1,1,4,1,3,3,2,4,1,4,2,5,1,5,2,1,4,1,3,1,2,5,5,4,1,2,3,3,2,2,1,3,3,1,4,4,1,1,4,1,1,5,1,2,4,2,1,4,1,1,4,3,5,1,2,1" :separator ",")))

(defun day-iteration (lanternfish)
  (if (zerop lanternfish)
      (list 6 8)
      (list (1- lanternfish))))

(mapcan #'day-iteration day6-example)

(defun days-loop (lanternfishs n)
  (if (zerop n)
      lanternfishs
      (days-loop (mapcan #'day-iteration lanternfishs) (1- n))))

(length (days-loop day6-example 18))
(length (days-loop day6-example 80))

(print (length (days-loop day6-input 80)))

;; Part 2 : 256 days
;; For day6-example : 26 984 457 539

(sort (remove-duplicates day6-input) #'<)
;; From 1 to 5
(length day6-input)
;; 300

;; Rappel de l'énoncé : Every 7 days - not synchronized
(- 256 7) ;; 249
;; (defvar synchronization-list (mapcar #'- day6-input))

(length day6-example)
;; 5

;; "Synchronisation"
(=
 (length (days-loop '(8) 21))
 (length (days-loop '(5) 18))
 (length (days-loop '(0) 13)))

(defun memoizable-solution (steps)
  (if (zerop steps)
      1
      (+
       (memoizable-solution (max (- steps (1+ 6)) 0))
       (memoizable-solution (max (- steps (1+ 8)) 0)))))

(memoizable-solution 13)
(= (length (days-loop '(0) 80))
   (memoizable-solution 80))

(let ((cache (make-hash-table)))
  (defun cached-solution (steps)
    (if (gethash steps cache)
        (gethash steps cache)
        (let ((solution (memoizable-solution steps)))
          (setf (gethash steps cache) solution)
          solution))))

(print (cached-solution 256))
;; LOOOOONG -> 6703087164

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DYNAMIC PROGRAMMING !!!!!!!
;;
;; https://www.educative.io/courses/grokking-dynamic-programming-patterns-for-coding-interviews/m2G1pAq0OO0#Characteristics-of-Dynamic-Programming
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; f(n) = f(n - 7) + f(n - 9)

(defun dynamic-solution (steps)
  (let ((dp (make-hash-table)))
    ;; On pré-remplit les premières valeurs avec l'algo de base
    (loop for n from 0 to 9
          do (setf (gethash n dp)
                   (memoizable-solution n)))

    (loop for n from 10 to steps
          do (setf (gethash n dp)
                   (+ (gethash (max (- n (1+ 6)) 0) dp)
                      (gethash (max (- n (1+ 8)) 0) dp))))
    (gethash steps dp)))


(dynamic-solution 256) ;; quasi-instant

(defun day6-part2 (input steps)
  (reduce
   #'+
   (mapcar (lambda (n) (dynamic-solution (- steps n))) input )))


(day6-part2 day6-example 256)
(print (day6-part2 day6-input 256))
