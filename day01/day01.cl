(defvar example '(199
                  200
                  208
                  210
                  200
                  207
                  240
                  269
                  260
                  263))

(defun count-increase (previous xs count)
  (if xs
      (if previous
          (let ((current (car xs))
                (rest (cdr xs)))
            (if (< previous current)
                (count-increase current rest (+ 1 count))
                (count-increase current rest count)))
          (count-increase (car xs) (cdr xs) count))
      count))

(defun solution-1 (xs)
  (count-increase nil xs 0))

(solution-1 example)
;; 7

(defvar data-day1 (mapcar (lambda (n) (parse-integer n)) (uiop:read-file-lines "./git-repositories/advent-of-code-2021/day01/input")))

(solution-1
 data-day1)
;; 1139

(defun sliding-window (xs)
  (when (<= 3 (length xs))
    (subseq xs 0 3)))

(defun convert-datas (xs result)
  (let ((window (sliding-window xs)))
    (if window
        (convert-datas (cdr xs) (append result (list window)))
        result)))

(convert-datas example nil)

(defun sum-windows (xs)
  (mapcar (lambda (window) (reduce '+ window)) xs))

(sum-windows (convert-datas example nil))

(solution-1 (sum-windows (convert-datas example nil)))
;; 5

(solution-1 (sum-windows (convert-datas data-day1 nil)))
;; 1103
