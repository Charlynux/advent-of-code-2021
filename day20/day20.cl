(defvar day20-example
  (uiop:read-file-lines "~/git-repositories/advent-of-code-2021/day20/example"))
(defvar day20-input
  (uiop:read-file-lines "~/git-repositories/advent-of-code-2021/day20/input"))

(defun char->pixel (c)
  (if (eql #\. c) 0 1))

(defun parse-enhancement-algorithm (line)
  (make-array
   512
   :initial-contents
   (mapcar #'char->pixel (coerce line 'list))))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun parse-image (lines)
  (let* ((array (list-to-2d-array
                 (mapcar
                  (lambda (line)
                    (mapcar #'char->pixel (coerce line 'list)))
                  lines)))
         (dimensions (array-dimensions array))
         (table (make-hash-table :test 'equal)))
    (loop for y from 0 to (1- (cadr dimensions))
          append (loop for x from 0 to (1- (car dimensions))
                       do (setf (gethash (cons x y) table)
                                (aref array y x))))
    table))

(defun day20-parse (input)
  (cons
   (parse-enhancement-algorithm (car input))
   (parse-image (cddr input))))

(defun inner-box (image)
  (let ((light-coords (loop for coord being the hash-keys of image
                            when (= 1 (gethash coord image))
                              collect coord)))
    (cons
     (cons
      (reduce #'min (mapcar #'car light-coords))
      (reduce #'min (mapcar #'cdr light-coords)))
     (cons
      (reduce #'max (mapcar #'car light-coords))
      (reduce #'max (mapcar #'cdr light-coords))))))

(inner-box (cdr (day20-parse day20-example)))

(inner-box
 (enhance
  (car (day20-parse day20-example))
  (cdr (day20-parse day20-example))))

(defun debug-image (image)
  (destructuring-bind
      ((min-x . min-y) . (max-x . max-y))
      (inner-box image)
    (loop for y from (- min-y 2) to (+ max-y 2)
          do (loop for x from (- min-x 2) to (+ max-x 2)
                   do (princ (if (= 1 (or (gethash (cons x y) image) 0)) "#" ".")))
          do (princ #\NewLine))))

(debug-image (cdr (day20-parse day20-example)))

(day20-parse day20-input)

(defparameter *square*
  (list
   (cons -1 -1) (cons 0 -1) (cons 1 -1)
   (cons -1 0) (cons 0 0) (cons 1 0)
   (cons -1 1) (cons 0 1) (cons 1 1)))

(defun square-around (coord)
  (loop for offset in *square*
        collect (cons (+ (car offset) (car coord))
                      (+ (cdr offset) (cdr coord)))))

(square-around (cons 5 10))

(defparameter *default-value* 0)
(defun square-value (image square)
  (format nil "~{~a~}"
          (mapcar
           (lambda (coord) (or
                            (gethash coord image)
                            *default-value*))
           square)))

(defun value-at-coord (image coord)
  (parse-integer
   (square-value image (square-around coord))
   :radix 2))

(value-at-coord
 (cdr (day20-parse day20-example))
 (cons 5 10))

(defun infinite-image-coords (image)
  (destructuring-bind
      ((min-x . min-y) . (max-x . max-y))
      (inner-box image)
    (loop for x from (- min-x 2) to (+ max-x 2)
          append (loop for y from (- min-y 2) to (+ max-y 2)
                       collect (cons x y)))))

(length (infinite-image-coords (cdr (day20-parse day20-example))))

(defun enhance (algorithm image)
  (let ((new-image (make-hash-table :test 'equal)))
    (loop for coord in (infinite-image-coords image)
          for key = (value-at-coord image coord)
          do (setf (gethash coord new-image)
                   (aref algorithm key)))
    new-image))

(enhance
 (car (day20-parse day20-example))
 (cdr (day20-parse day20-example)))

(aref
 (car (day20-parse day20-example))
 0)
(aref
 (car (day20-parse day20-example))
 511)

(aref
 (car (day20-parse day20-input))
 511)

(aref
 (car (day20-parse day20-input))
 0)

(defun count-lit-pixels (image)
  (loop for pixel being the hash-value in image
        count (= 1 pixel)))

(count-lit-pixels (cdr (day20-parse day20-example)))

(count-lit-pixels
 (enhance
  (car (day20-parse day20-example))
  (cdr (day20-parse day20-example))))

(debug-image
 (enhance
  (car (day20-parse day20-example))
  (enhance
   (car (day20-parse day20-example))
          (cdr (day20-parse day20-example)))))

(defun day20-part1 (input)
  (setf *default-value* 0)
  (destructuring-bind (algorithm . image) input
    (let ((intermediate (enhance algorithm image)))
      (setf *default-value* (aref algorithm 0))
      (let ((resulting-image (enhance algorithm intermediate)))
        (count-lit-pixels resulting-image)))))

(day20-part1 (day20-parse day20-example))

(debug-image (cdr (day20-parse day20-input)))

(day20-part1 (day20-parse day20-input))
;; 5486 - too high
;; 4942 - too high
