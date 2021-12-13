;; Installation du package FSet
(ql:quickload "fset")

;; Test de la lib
(setq coords (fset:set (cons 0 1) (cons 0 2)))

(fset:contains? coords (cons 0 1))
(fset:contains? coords (cons 1 0))
(fset:contains? coords (cons 0 2))
(fset:contains? coords (cons 0 0))

(reduce
 (lambda (acc n) (fset:with acc n))
 '(1 2 3 3 4 1 2 4 5)
 :initial-value (fset:empty-set))

(defun list->set (xs)
  (reduce
   (lambda (acc e) (fset:with acc e))
   xs
   :initial-value (fset:empty-set)))

(fset:reduce
 (lambda (acc n) (fset:with acc (1+ n)))
 (fset:set 1 2 3 4 5 6 7 8)
 :initial-value (fset:empty-set))

;; 14 -> 0
;; 8 -> 7

(defun fold-y (coord n)
  (let ((y (cdr coord)))
    (cons
     (car coord)
     (if (<= y n)
         y
         (- (* n 2) y)))))

(fold-y (cons 0 8) 7)
(fold-y (cons 0 14) 7)

(defun fold-x (coord n)
  (let ((x (car coord)))
    (cons
     (if (<= x n)
         x
         (- (* n 2) x))
     (cdr coord))))

(defun parse-coords (line)
  (let ((coords (mapcar #'parse-integer (uiop:split-string line :separator ","))))
    (cons
     (car coords)
     (cadr coords))))

(ql:quickload "cl-ppcre")
(defun parse-instruction (line)
  (ppcre:register-groups-bind (direction value)
                              ("fold along (x|y)=(\\d+)" line :sharedp t)
                              (cons direction (parse-integer value))))

(defun read-input (path)
  (with-open-file (stream path)
    (cons
     (loop for line = (read-line stream nil) while (not (equal line ""))
           collect (parse-coords line))
     (loop for line = (read-line stream nil) while line
           collect (parse-instruction line)))))

(read-input "~/git-repositories/advent-of-code-2021/day13/example")

(defun folding-step (coords instruction)
  (let ((fn-instruction (if (equal "x" (car instruction))
                            (lambda (coord) (fold-x coord (cdr instruction)))
                            (lambda (coord) (fold-y coord (cdr instruction))))))
    (fset:reduce
     (lambda (acc coord) (fset:with acc (funcall fn-instruction coord)))
     coords
     :initial-value (fset:empty-set))))

(defun day13-part1 (file)
  (let ((input (read-input file)))
    (fset:size
     (folding-step (list->set (car input)) (cadr input)))))

(day13-part1 "~/git-repositories/advent-of-code-2021/day13/example")
(day13-part1 "~/git-repositories/advent-of-code-2021/day13/input")

(defun day13-part2 (file)
  (let* ((input (read-input file))
         (result (reduce
                  #'folding-step
                  (cdr input)
                  :initial-value (list->set (car input))))
         (max-x (apply #'max (mapcar #'car (fset:convert 'list result))))
         (max-y (apply #'max (mapcar #'cdr (fset:convert 'list result)))))
    (loop for y from 0 to max-y
          do (loop for x from 0 to max-x
                  do (princ (if (fset:contains? result (cons x y)) "#" ".")))
          do (princ #\NewLine))))

(day13-part2 "~/git-repositories/advent-of-code-2021/day13/example")
(day13-part2 "~/git-repositories/advent-of-code-2021/day13/input")
