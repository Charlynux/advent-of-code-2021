(defparameter *start-point* (cons 0 0))
(defparameter *day17-example* (cons (cons 20 30) (cons -10 -5)))
;; target area: x=206..250, y=-105..-57
(defparameter *day17-input* (cons (cons 206 250) (cons -105 -57)))

(defun move (coord offset)
  (cons
   (+ (car coord) (car offset))
   (+ (cdr coord) (cdr offset))))

(defun velocity-update (offset)
  (destructuring-bind (offset-x . offset-y) offset
    (cons
     (cond
       ((zerop offset-x) 0)
       ((< offset-x 0) (1+ offset-x))
       ((< 0 offset-x) (1- offset-x)))
     (1- offset-y))))

(velocity-update (cons 1 1))
(velocity-update (cons -1 -3))

(defun winp (target coord)
  (destructuring-bind ((minx . maxx) . (miny . maxy)) target
    (and (<= minx (car coord) maxx)
         (<= miny (cdr coord) maxy))))

(winp *day17-example* (cons 28 -8))

(defun stoppedp (offset) (zerop (car offset)))

(defun outp (target coord offset)
  (destructuring-bind ((minx . maxx) . (miny . maxy)) target
    (or
     (and (zerop (car offset))
          (< (car coord) minx))
     (and (< (car offset) 0)
          (< maxx (car coord)))
     (and (< (cdr offset) 0)
          (< (cdr coord) miny)))))

(defun day17-step (target coord offset path)
  (cond
    ((winp target coord) (cons 'won (cons coord path)))
    ((outp target coord offset) (cons 'stopped (cons coord path)))
    (t
     (day17-step
      target
      (move coord offset)
      (velocity-update offset)
      (cons coord path)))))

(day17-step *day17-example* *start-point* (cons 12 25) nil)

(day17-step *day17-example* *start-point* (cons 7 2) nil)

(day17-step *day17-input* *start-point* (cons 7 2) nil)

(day17-step *day17-example* *start-point* (cons 6 9) nil)

(defun find-max-y (path)
  (apply #'max (mapcar #'cdr path)))

(defun find-paths (target)
  (loop for offset-x from 0 to (cdar target)
        append (loop for offset-y from (cadr target) to 200
                     for offset = (cons offset-x offset-y)
                     for result = (day17-step target
                                              *start-point*
                                              offset
                                              nil)
                     when (eql 'won (car result))
                       collect (cdr result))))

(defun day17-part1 (target)
  (let ((ys (mapcar #'find-max-y (find-paths target))))
    (apply #'max ys)))

(day17-part1 *day17-example*)

(day17-part1 *day17-input*)

(length (find-paths *day17-example*))

(length (find-paths *day17-input*))
