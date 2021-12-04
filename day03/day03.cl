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

;; PART 2
;; life support rating = oxygen generator rating * CO2 scrubber rating
;; Both values are located using a similar process that involves filtering out values until only one remains.

;; consider just the first bit

(defun filtering-values (pos bit-criteria lines)
  (if (= 1 (length lines))
      (list->number (car lines))
      (filtering-values (+ 1 pos)
                        bit-criteria
                        (funcall bit-criteria pos lines))))

(defun most-common-with-eq (xs)
  (let ((half (ceiling (length xs) 2))
        (ones (reduce #'+ xs)))
    (cond
      ((< ones half) 0)
      ((<= half ones) 1))))

(most-common-with-eq '(0 1 0))
(most-common-with-eq '(0 1))
(most-common-with-eq '(0 1 1))

(defun remove-on-pred-at-pos (pred pos lines)
  (let ((keep (funcall pred (mapcar (lambda (xs) (nth pos xs)) lines))))
    (remove-if
     (lambda (xs) (not (= keep (nth pos xs))))
     lines)))

;; oxygen generator rating
(defun oxygen-rating (pos lines)
  (remove-on-pred-at-pos #'most-common-with-eq pos lines))

(oxygen-rating 0 '((0 1 0) (1 1 1) (1 0 0)))
(oxygen-rating 1 '((0 1 0) (1 1 1) (1 0 0)))

(filtering-values
 0
 #'oxygen-rating
 (mapcar #'line->list day3-example))
;; 23


(defun least-common-with-eq (xs)
  (flip-bit (most-common-with-eq xs)))

(least-common-with-eq '(0 1 0))
(least-common-with-eq '(0 1))
(least-common-with-eq '(0 1 1))

;; CO2 scrubber rating
(defun scrubber-rating (pos lines)
  (remove-on-pred-at-pos #'least-common-with-eq pos lines))

(filtering-values
 0
 #'scrubber-rating
 (mapcar #'line->list day3-example))

(defun day3-part2 (lines)
  (let ((oxygen-rate (filtering-values
                      0
                      #'oxygen-rating
                      lines))
        (co2-rate (filtering-values
                      0
                      #'scrubber-rating
                      lines)))
    (* oxygen-rate co2-rate)))


(day3-part2 (mapcar #'line->list day3-example))
(day3-part2 (mapcar #'line->list day3-input))
