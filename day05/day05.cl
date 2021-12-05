;; hydrothermal vents

(require 'uiop)
(defvar day5-example '("0,9 -> 5,9"
                       "8,0 -> 0,8"
                       "9,4 -> 3,4"
                       "2,2 -> 2,1"
                       "7,0 -> 7,4"
                       "6,4 -> 2,0"
                       "0,9 -> 2,9"
                       "3,4 -> 1,4"
                       "0,0 -> 8,8"
                       "5,5 -> 8,2"))
(defvar day5-input (uiop:read-file-lines "~/git-repositories/advent-of-code-2021/day05/input"))

;; line of vent : x1,y1 -> x2,y2 (ex. 0,9 -> 5,9)

;; PPCRE : Package pour les regex
(ql:quickload "cl-ppcre")

(defun parse-line (line)
  (ppcre:register-groups-bind (x1 y1 x2 y2)
      ("(\\d+),(\\d+) -> (\\d+),(\\d+)" line :sharedp t)
    (cons (cons (parse-integer x1) (parse-integer y1))
          (cons (parse-integer x2) (parse-integer y2)))))

(parse-line "644,38 -> 644,265")

;; An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3
;; !!! For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2. !!!
(defun keep-straight-lines (lines)
  (remove-if
   (lambda (line)
     (destructuring-bind ((x1 . y1) . (x2 . y2)) line
       (and (not (= x1 x2)) (not (= y1 y2)))))
   lines))

;; Solution :
;; the number of points where at least two lines overlap

(defun expand-line (line)
  (destructuring-bind ((x1 . y1) . (x2 . y2)) line
    (loop for x from (min x1 x2) to (max x1 x2)
          append (loop for y from (min y1 y2) to (max y1 y2)
                       collect (cons x y)))))
(expand-line (parse-line "0,1 -> 0,3"))

(defun occurrences (lst)
  (let ((table (make-hash-table :test #'equal)))
    (loop for x in lst
          do (incf (gethash x table 0)))
    (loop for k being the hash-key of table
            using (hash-value v)
          collect (cons k v))))

(occurrences (list (cons 0 1) (cons 0 2) (cons 0 1) (cons 0 3) (cons 0 2) (cons 0 1)))

(defun count-over-one (occurrences-result)
  (loop for pair in occurrences-result
        count (> (cdr pair) 1)))

(defun day5-part1 (input)
  (let ((lines (mapcar #'parse-line input)))
    (let ((occs (occurrences (mapcan #'expand-line (keep-straight-lines lines)))))
      (count-over-one occs))))

(day5-part1 day5-example)
(day5-part1 day5-input)
