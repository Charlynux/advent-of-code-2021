(ql:quickload "cl-ppcre")

(defun to-lisp (line)
  (ppcre:regex-replace-all
   ","
   (ppcre:regex-replace-all
    "\\]"
    (ppcre:regex-replace-all "\\[" line "(")
    ")")
   " . "))

(defun parse-line (line) (read-from-string (to-lisp line)))

(parse-line "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]")

(defun snailfish-numberp (number) (typep number 'sequence))

(defun pairp (number)
  (and (snailfish-numberp number)
       (not (snailfish-numberp (car number)))
       (not (snailfish-numberp (cdr number)))))
(pairp (cons 0 1))
(pairp (cons (cons 1 2) 1))
(pairp (cons 0 (cons 1 2)))

(pairp (cons (cons 0 7) 4))

(defparameter *explosion-depth* 4)

(defun add-leftmost (number n)
  (if (snailfish-numberp number)
      (cons
       (add-leftmost (car number) n)
       (cdr number))
      (+ number n)))

(defun add-rightmost (number n)
  (if (snailfish-numberp number)
      (cons
       (car number)
       (add-rightmost (cdr number) n))
      (+ number n)))

(defun apply-left-instructions (number instructions)
  (let ((left-instruction (assoc 'left instructions)))
    (if left-instruction
        (add-rightmost number (cdr left-instruction))
        number)))

(defun apply-right-instructions (number instructions)
  (let ((right-instruction (assoc 'right instructions)))
    (if right-instruction
        (add-leftmost number (cdr right-instruction))
        number)))

(defun insert-if-left (number-to-insert instructions instruction)
  (let ((remaining (assoc instruction instructions)))
    (if (null remaining)
        number-to-insert
        (list 'instructions
              remaining
              (cons 'insert number-to-insert)))))

(defun explode (number depth)
  (cond
    ((or (typep number 'number)
         (and (pairp number) (< depth *explosion-depth*)))
     number)
    ((and (pairp number) (<= *explosion-depth* depth))
     (list 'instructions
           (cons 'left (car number))
           (cons 'right (cdr number))
           (cons 'insert 0)))
    (t (let ((left (explode (car number) (1+ depth))))
         (cond
           ((equal left (car number))
            (let ((right (explode (cdr number) (1+ depth))))
              (if (and
                   (typep right 'sequence)
                   (eql 'instructions (car right)))
                  (insert-if-left
                   (cons
                    (apply-left-instructions (car number) (cdr right))
                    (cdr (assoc 'insert (cdr right))))
                   (cdr right)
                   'right)
                  (cons (car number) right))))
           ((eql 'instructions (car left))
            (insert-if-left
             (cons (cdr (assoc 'insert (cdr left)))
                   (apply-right-instructions (cdr number) (cdr left)))
             (cdr left)
             'left))
           (t (cons left (cdr number))))))))

(defun start-explode (number)
  (let ((result (explode number 0)))
    (if (eql 'instructions (car result))
        (cdr (assoc 'insert (cdr result)))
        result)))

(explode (cons (cons 0 7) 4) 2)
(explode 4 2)
(start-explode (parse-line "[[[[[9,8],1],2],3],4]"))
(start-explode (parse-line "[7,[6,[5,[4,[3,2]]]]]"))
(start-explode (parse-line "[[6,[5,[4,[3,2]]]],1]"))
(start-explode (parse-line "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))
(start-explode (parse-line "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))


(defun split (number) (cons (floor number 2) (ceiling number 2)))

(defun apply-split (number)
  (if (snailfish-numberp number)
      (multiple-value-bind
            (new-left updated) (apply-split (car number))
        (if (equal (car number) new-left)
            (cons new-left (apply-split (cdr number)))
            (cons new-left (cdr number))))
      (if (< number 10)
          (values number nil)
          (values (split number) t))))

(apply-split (cons 1 2))
(apply-split (apply-split (cons 11 12)))

(equal (split 10) (cons 5 5))
(equal (split 11) (cons 5 6))
(equal (split 12) (cons 6 6))

(defun apply-reduction (number)
  (let ((exploded (start-explode number)))
    (if (equal exploded number)
        (apply-split number)
        exploded)))

(defun reduction (number)
  (let ((reduced (apply-reduction number)))
    (if (equal number reduced)
        reduced
        (reduction reduced))))

(reduction (cons 1 2))

(defun addition (a b) (cons a b))

(reduction
 (addition
  (parse-line "[[[[4,3],4],4],[7,[[8,4],9]]]")
  (parse-line "[1, 1]")))

(defun parse-example (string)
  (mapcar #'parse-line
          (uiop:split-string
                        string
                        :separator '(#\Newline))))

(defun apply-reductions (numbers)
  (reduce
   (lambda (prev current)
     (reduction (addition prev current)))
   numbers))

(apply-reductions
 (parse-example "[1,1]
[2,2]
[3,3]
[4,4]
[5,5]"))

(apply-reductions
 (parse-example "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]"))

(defun magnitude (number)
  "To check whether it's the right answer, the snailfish teacher only checks the magnitude of the final sum. The magnitude of a pair is 3 times the magnitude of its left element plus 2 times the magnitude of its right element. The magnitude of a regular number is just that number."
  (if (snailfish-numberp number)
      (+ (* 3 (magnitude (car number)))
         (* 2 (magnitude (cdr number))))
      number))

(magnitude (parse-line "[[1,2],[[3,4],5]]"))
(magnitude (parse-line "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))

(magnitude
 (apply-reductions
  (parse-example
   "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")))

(defvar day18-input
  (mapcar #'parse-line
          (uiop:read-file-lines "~/git-repositories/advent-of-code-2021/day18/input")))

(magnitude (apply-reductions day18-input))
