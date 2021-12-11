(defun char->int (c)
  (parse-integer (string c)))

(defparameter *coords*
  (loop for y from 0 to 9
        append (loop for x from 0 to 9
                     collect (cons x y))))

(defun day11-parse (input)
  (let* ((lines (uiop:split-string input :separator '(#\Newline))))
    (loop for coord in *coords*
          collect (cons coord
                        (char->int (char (nth (cdr coord) lines) (car coord)))))))

(defvar day11-example
  (day11-parse "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"))

(defvar day11-input
  (day11-parse "7232374314
8531113786
3411787828
5482241344
5856827742
7614532764
5311321758
1255116187
5821277714
2623834788"))


;; 100 octopuses' energy level

;; - increase all by 1
;; - over 9 -> increases neighbors by 1 -> loop
;;   (octopus can only flash once by step)
;; - over 9 -> 0

;; How many total flashes are there after 100 steps ?

(defun increase-all (octopuses)
  (loop for coord in *coords*
        collect (cons coord (1+ (cdr (assoc coord octopuses))))))

(increase-all day11-example)

(set-difference (list (cons 1 1)) (list (cons 1 1) (cons 2 1)) :test #'equal)

(defparameter *all-neighbors*
  (loop for y from -1 to 1
        append (loop for x from -1 to 1
                     when (not (and (zerop x) (zerop y)))
                     collect (cons x y))))

(defun move (coord offset)
  (cons
   (+ (car coord) (car offset))
   (+ (cdr coord) (cdr offset))))

(equal
 (cons 0 1)
 (cons 0 1))

(defun increase-neighbors (flashes octopuses)
  (let* ((neighbors (mapcan
                    (lambda (coord) (mapcar
                                     (lambda (offset) (move coord offset))
                                     *all-neighbors*))
                    flashes))
        (to-increase
          ;; ici le problème est que l'association list s'appuie sur eq et non equal
          ;; je dois donc toujours utiliser les coordonnées issues de *coords*
          (loop for coord in *coords*
                append (loop for neighbor in neighbors
                             when (equal coord neighbor)
                               collect coord))))
    (reduce
     (lambda (octopuses coord)
       (acons coord
              (1+ (cdr (assoc coord octopuses)))
              octopuses))
     to-increase
     :initial-value octopuses)))

(print (increase-neighbors (list (car *coords*)) day11-example))

(defun recur-flashes (flashes octopuses)
  (let* ((new-flashes (loop for coord in *coords*
                            when (and (< 9 (cdr (assoc coord octopuses)))
                                      (set-difference (list coord) flashes))
                              collect coord)))
    (if new-flashes
        (recur-flashes (union flashes new-flashes)
                       (increase-neighbors new-flashes octopuses))
        (values octopuses flashes))))

(recur-flashes nil (increase-all day11-example))
(recur-flashes nil (increase-all (increase-all day11-example)))

(defun reset-flashes (octopuses)
  (let ((reset-coords (loop for coord in *coords*
                            when (< 9 (cdr (assoc coord octopuses)))
                              collect coord)))
    (reduce
     (lambda (octopuses coord)
       (acons coord 0 octopuses))
     reset-coords
     :initial-value octopuses)))

(let ((reseted (reset-flashes (increase-all (increase-all day11-example)))))
  (loop for coord in *coords*
        do (print (assoc coord reseted))))

(defun day11-step (octopuses)
  (multiple-value-bind (updated-octopuses flashes)
      (recur-flashes nil (increase-all octopuses))
    (values
     (reset-flashes updated-octopuses)
     (length flashes))))

(day11-step (day11-step day11-example))

(defun day11-part1 (octopuses nb-flashes steps)
  (if (zerop steps)
      (values nb-flashes octopuses)
      (multiple-value-bind (updated-octopuses nb-new-flashes)
          (day11-step octopuses)
        (day11-part1 updated-octopuses
                     (+ nb-flashes nb-new-flashes)
                     (1- steps)))))

(identity (day11-part1 day11-example 0 10))

(identity (day11-part1 day11-example 0 100))

(identity (day11-part1 day11-input 0 100))

(defun day11-part2 (octopuses steps)
  (multiple-value-bind (updated-octopuses nb-flashes)
      (day11-step octopuses)
    (if (= 100 nb-flashes)
        (values steps updated-octopuses)
        (day11-part2 updated-octopuses (1+ steps)))))

(identity (day11-part2 day11-example 1))

(identity (day11-part2 day11-input 1))
