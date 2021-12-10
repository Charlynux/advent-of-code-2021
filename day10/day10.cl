(defparameter *corresponding-open*
  (list (cons #\} #\{)
        (cons #\> #\<)
        (cons #\) #\()
        (cons #\] #\[)))

(defparameter *corrupted-score*
  (list (cons #\} 1197)
        (cons #\> 25137)
        (cons #\) 3)
        (cons #\] 57)))

(defun recur-line-part1 (pile characters)
  (if (null characters)
      0
      (let ((c (car characters)))
        (if (assoc c *corresponding-open*)
            ;; closing
            (let ((top (car pile)))
              (if (equal top (cdr (assoc c *corresponding-open*)))
                  (recur-line-part1 (cdr pile) (cdr characters))
                  (cdr (assoc c *corrupted-score*))))
            ;; opening
            (recur-line-part1 (cons c pile) (cdr characters))))))

(recur-line-part1 nil (coerce "{([(<{}[<>[]}>{[]{[(<()>" 'list))

(defun day10-part1 (lines)
  (reduce
   #'+
   (mapcar
    (lambda (line)
      (recur-line-part1 nil (coerce line 'list)))
    lines)))

(defparameter *day10-example*
  '("[({(<(())[]>[[{[]{<()<>>"
    "[(()[<>])]({[<{<<[]>>("
    "{([(<{}[<>[]}>{[]{[(<()>"
    "(((({<>}<{<{<>}{[]{[]{}"
    "[[<[([]))<([[{}[[()]]]"
    "[{[{({}]{}}([{[{{{}}([]"
    "{<[[]]>}<{[{[{[]{()[[[]"
    "[<(<(<(<{}))><([]([]()"
    "<{([([[(<>()){}]>(<<{{"
    "<{([{{}}[<[[[<>{}]]]>[]]"))

(defparameter *day10-input*
  (uiop:read-file-lines "~/git-repositories/advent-of-code-2021/day10/input"))

(day10-part1 *day10-example*)
(day10-part1 *day10-input*)
