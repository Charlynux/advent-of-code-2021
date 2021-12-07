(defun parse-day7 (s)
  (mapcar #'parse-integer (uiop:split-string s :separator ",")))
(defvar day7-example (parse-day7 "16,1,2,0,4,2,7,1,2,14"))
(defvar day7-input (parse-day7 (uiop:read-file-string "~/git-repositories/advent-of-code-2021/day07/input")))

(defun frequencies (input)
  (let ((ns (remove-duplicates input))
        (freqs
          (reduce
           (lambda (acc n)
             (let ((count (or (cdr (assoc n acc)) 0)))
               (acons n (1+ count) acc)))
           input
           :initial-value nil)))
    (reduce
     (lambda (acc n) (acons n (cdr (assoc n freqs)) acc))
     ns
     :initial-value nil)))

(sort (frequencies day7-example) (lambda (a b) (> (cdr a) (cdr b))))
(car (sort (frequencies day7-input) (lambda (a b) (> (cdr a) (cdr b)))))

(defun movement-cost (ns position)
  (reduce #'+ (mapcar (lambda (n) (abs (- position n))) ns)))
(movement-cost day7-example 2)

;; Brute Force was the solution...
(defun part1-solution (input)
  (let ((max-position
          (car (sort (remove-duplicates input) #'>))))
    (reduce #'min (loop for i from 1 to max-position
                        collect (movement-cost input i)))))

(part1-solution day7-example)
(part1-solution day7-input)


(defun part2-cost (n pos)
  (let ((cost (abs (- n pos))))
    (/ (* cost (1+ cost)) 2)))

(defun movement-cost-2 (ns position)
  (reduce #'+ (mapcar (lambda (n) (part2-cost n position)) ns)))

(defun part2-solution (input)
  (let ((max-position
          (car (sort (remove-duplicates input) #'>))))
    (reduce #'min (loop for i from 1 to max-position
                        collect (movement-cost-2 input i)))))

(part2-solution day7-example)
(print (part2-solution day7-input))
