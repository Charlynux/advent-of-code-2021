;; top left position -> bottom right
;; you cannot move diagonally

;; lowest total risk

;; https://www.geeksforgeeks.org/a-search-algorithm/
;; g : Cost = Sum of risks
;; h : Estimated cost = Manhattan distance ?

;; open & closed : (list coords f path)

(defun remove-point (open coords)
  (remove-if
   (lambda (node) (equal (car node) coords))
   open))

(remove-point
 (list (list (cons 0 0) 1 (list 'a 'b))
       (list (cons 1 0) 2 (list 'b 'c))
       (list (cons 0 0) 3 (list 'b 'c)))
 (cons 0 0))

(defun manhattan-distance (a b)
  (+ (abs (- (car a) (car b)))
     (abs (- (cdr a) (cdr b)))))

(manhattan-distance (cons 0 0) (cons 1 1))

(defun move (pos offset)
  (cons
   (+ (car pos) (car offset))
   (+ (cdr pos) (cdr offset))))

(defparameter *neighbors* (list (cons 0 1) (cons 1 0) (cons 0 -1) (cons -1 0)))

(defun get-pos (pos array)
  (aref array (car pos) (cdr pos)))

(defun char->int (c)
  (parse-integer (string c)))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun day9-parse-lines (lines)
  (list-to-2d-array
   (mapcar
    (lambda (line)
      (mapcar #'char->int (coerce line 'list)))
    lines)))

(defvar example-data (day9-parse-lines
                      (uiop:split-string
                       "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"
                       :separator '(#\Newline))))

(defun create-inboundp (dimensions)
  (lambda (pos) (and (<= 0 (car pos) (1- (car dimensions)))
                     (<= 0 (cdr pos) (1- (cadr dimensions))))))


(defun not-closed-and-not-best-open (open closed)
  (lambda (xs)
    (let ((pos (car xs)))
      (or (fset:lookup closed pos)
          (let ((from-open (fset:lookup open pos)))
            (and from-open
                 (< (cadr from-open)
                    (cadr xs))))))))

(defun calculate-point-definition (array current goal)
  (lambda (pos)
    (let ((pos-risk (+ (caddr current) (get-pos pos array))))
      (list pos (+ pos-risk (manhattan-distance pos goal)) pos-risk))))

(defun add-to-open (open points)
  (reduce
   (lambda (acc point)
     (fset:with acc (car point) point))
   points
   :initial-value open))

(defun find-best-candidate (open)
  (fset:first (fset:sort (fset:range open) (lambda (apoint bpoint)
                                      (<= (cadr apoint) (cadr bpoint))))))

(ql:quickload "fset")

(defun day15-part1 (array)
  (let* ((dimensions (array-dimensions array))
       (goal (cons (1- (car dimensions)) (1- (cadr dimensions))))
       (find-neighbors (lambda (pos) (remove-if-not
                                      (create-inboundp dimensions)
                                      (mapcar
                                       (lambda (offset) (move pos offset))
                                       *neighbors*)))))
  (labels ((recursion-step (open closed)
             (let ((current (find-best-candidate open)))
               (if (or (null current) (equal (car current) goal))
                   current
                   (let ((next-open (remove-if
                                     (not-closed-and-not-best-open open closed)
                                     (mapcar
                                      (calculate-point-definition array current goal)
                                      (funcall find-neighbors (car current))))))
                     (recursion-step
                      (add-to-open
                       (fset:less open (car current))
                       next-open)
                      (fset:with
                       closed
                       (car current)
                       current)))))))
    (cadr (recursion-step (fset:map ((cons 0 0) (list (cons 0 0) 0 0)))
                         (fset:map))))))

(day15-part1 example-data)

(day15-part1
 (day9-parse-lines
  (uiop:read-file-lines "~/git-repositories/advent-of-code-2021/day15/input")))

(defun increment (i) (lambda (n) (if (< (+ n i) 10)
                                     (+ n i)
                                     (- (+ n i) 9))))

(defun extend-horizontally (lines)
  (loop for line in lines
        collect (loop for i from 0 to 4
                      nconcing (mapcar
                                (increment i)
                                line))))

(defun create-full-map (lines)
  (let ((horizontal (extend-horizontally lines)))
    (loop for i from 0 to 4
          append (loop for line in horizontal
                       collect (mapcar (increment i) line)))))

(defun day15-parse (lines)
  (mapcar (lambda (line)
            (mapcar #'char->int (coerce line 'list)))
          lines))

(day15-part1
 (list-to-2d-array
  (create-full-map
   (day15-parse
    (uiop:split-string
     "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"
     :separator '(#\Newline))))))

(defvar input-full-map
  (list-to-2d-array
   (create-full-map
    (day15-parse
     (uiop:read-file-lines "~/git-repositories/advent-of-code-2021/day15/input")))))

(print (day15-part1 input-full-map))
