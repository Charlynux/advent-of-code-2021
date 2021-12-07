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
