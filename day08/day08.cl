(defvar day8-input (uiop:read-file-lines "~/git-repositories/advent-of-code-2021/day08/input"))

(defun day8-parse-line-part1 (line)
  (let ((output (uiop:split-string (string-trim " " (cadr (uiop:split-string line :separator "|"))))))
    (mapcar #'length output)))

(day8-parse-line-part1 "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")


(defun easy-digitp (digit-length)
  (or
   (= digit-length 2) ;; 1
   (= digit-length 3) ;; 7
   (= digit-length 4) ;; 4
   (= digit-length 7) ;; 8
))

(length (remove-if-not #'easy-digitp (mapcan #'day8-parse-line-part1 day8-input)))
