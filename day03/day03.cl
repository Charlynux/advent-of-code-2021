;; binary numbers -> gamma rate and epsilon rate
;; power consumption = gamma rate * epsilon rate

;; most common bit in the corresponding position

;; (parse-integer "00100" :radix 2)

(defun line->list (line)
  (map 'list (lambda (c) (parse-integer (string c))) line))

(defun list->number (xs)
  (reduce
   (lambda (acc n)
     (+ (* acc 2) n))
   xs))

(list->number (line->list "10110"))

(defun transpose (lists)
  (apply #'mapcar #'list lists))

(defvar day3-example
  '("00100"
    "11110"
    "10110"
    "10111"
    "10101"
    "01111"
    "00111"
    "11100"
    "10000"
    "11001"
    "00010"
    "01010"))

(defun most-common-bit (xs)
  (let ((ones (reduce #'+ xs)))
    (if (< ones (/ (length xs) 2))
        0
        1)))

(most-common-bit '(0 1 0 1 1 0 0 0))

;; gamma rate calculation
(list->number (mapcar #'most-common-bit (transpose (mapcar #'line->list day3-example))))
;; 22

(defun flip-bit (n) (mod (+ 1 n) 2))

;; epsilon rate calculation
(list->number (mapcar #'flip-bit (mapcar #'most-common-bit (transpose (mapcar #'line->list day3-example)))))
;; 9


(defun day3-part1 (lines)
  (let ((gamma-rates (mapcar #'most-common-bit (transpose (mapcar #'line->list lines)))))
    (* (list->number gamma-rates)
       (list->number (mapcar #'flip-bit gamma-rates)))))

(day3-part1 day3-example)
(defvar day3-input (uiop:read-file-lines "./git-repositories/advent-of-code-2021/day03/input"))
(day3-part1 day3-input)
