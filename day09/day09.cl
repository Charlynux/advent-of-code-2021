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

(defvar example-array (day9-parse-lines '("2199943210"
                                         "3987894921"
                                         "9856789892"
                                         "8767896789"
                                         "9899965678")))

(aref example-array 0 9)

(1- (cadr(array-dimensions example-array)))


(defun move (pos offset)
  (cons
   (+ (car pos) (car offset))
   (+ (cdr pos) (cdr offset))))

(defparameter *neighbors* (list (cons 0 1) (cons 1 0) (cons 0 -1) (cons -1 0)))

(defun get-pos (pos array)
  (aref array (car pos) (cdr pos)))

(defun keep-if-min-local (pos array)
  (let* ((value (get-pos pos array))
         (dimensions (array-dimensions array))
         (neighbors (loop for neighbor-offset in *neighbors*
                          collect (move pos neighbor-offset)))
         (accessible-neighbors (remove-if-not (lambda (pos1)
                                                (and (<= 0 (car pos1) (1- (car dimensions)))
                                                     (<= 0 (cdr pos1) (1- (cadr dimensions)))))  neighbors))
         (neighbors-values (mapcar (lambda (pos1) (get-pos pos1 array))
                                   accessible-neighbors)))
    (when (every (lambda (n) (< value n)) neighbors-values)
      (cons pos value))))

(defun day9-part1 (input)
  (let* ((array (day9-parse-lines input))
         (dimensions (array-dimensions array))
         (local-mins (loop for x from 0 to (1- (car dimensions))
                           append (loop for y from 0 to (1- (cadr dimensions))
                                        collect (keep-if-min-local (cons x y) array)))))
    (print local-mins)
    (reduce
     #'+
     (mapcar #'1+
             (mapcar
              #'cdr
              (remove-if #'null local-mins))))))

(day9-part1 '("2199943210"
              "3987894921"
              "9856789892"
              "8767896789"
              "9899965678"))

(defvar day9-input
 (uiop:read-file-lines "~/git-repositories/advent-of-code-2021/day09/input"))

(day9-part1 day9-input)

(defun in-bounds (dimensions pos)
  (and (<= 0 (car pos) (1- (car dimensions)))
       (<= 0 (cdr pos) (1- (cadr dimensions)))))

(defun find-neighbors (array coord closed)
  (let ((dimensions (array-dimensions array))
        (value (get-pos coord array)))
    (loop for neighbor-offset in *neighbors*
          for neighbor = (move coord neighbor-offset)
          when (and (in-bounds dimensions neighbor)
                    (not (= 9 (get-pos neighbor array)))
                    (<= value (get-pos neighbor array))
                    (set-difference (list (sxhash neighbor)) closed))
            collect neighbor)))

(defun expand-basin (array open closed)
  (let ((next (remove-duplicates
               (mapcan (lambda (coord) (find-neighbors array coord closed)) open)
               :test #'equal)))
    (if next
        (expand-basin array
                      next
                      (union
                       (mapcar #'sxhash next)
                       closed))
        closed)))


(defun day9-part2 (input)
  (let* ((array (day9-parse-lines input))
         (dimensions (array-dimensions array))
         (local-mins (loop for x from 0 to (1- (car dimensions))
                           append (loop for y from 0 to (1- (cadr dimensions))
                                        collect (keep-if-min-local (cons x y) array))))
         (local-mins-coords (mapcar #'car (remove-if #'null local-mins)))
         (sizes (mapcar #'length
                        (mapcar
                         (lambda (coord) (expand-basin array
                                                       (list coord)
                                                       (list (sxhash coord))))
                         local-mins-coords))))
    (print sizes)
    (apply #'* (subseq (sort sizes #'>) 0 3))))

(day9-part2 '("2199943210"
              "3987894921"
              "9856789892"
              "8767896789"
              "9899965678"))

(print (day9-part2 day9-input))
;; First try 485760 ;; too-low

;; (= 1 diff) -> (<= diff 1)
;; Second try 790830 ;; too-low

;; Misunderstood the rule.
;; It's simply (<= current-point neighbor), no "by 1" difference
;; Third try : 916688 SUCCESS

(let* ((dimensions (array-dimensions (day9-parse-lines day9-input)))
       (hashes (loop for x from 0 to (car dimensions)
                     append (loop for y from 0 to (cadr dimensions)
                                  collect (sxhash (cons x y))))))
  (-
   (length hashes)
   (length (remove-duplicates hashes))))
;; 0, donc a priori pas de collisions dans les hashes
