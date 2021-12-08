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

;; Part 1 Solution
(length (remove-if-not #'easy-digitp (mapcan #'day8-parse-line-part1 day8-input)))

;; 1 -> c f
;; 4 -> b c d f
;; 7 -> a c f
;; 8 -> a b c d e f g

;; 2 -> a c d e g -> c e
;; 3 -> a c d f g -> c f
;; 5 -> a b d f g -> b f

;; 0 -> a b c e f g -> c e
;; 6 -> a b d e f g -> d e
;; 9 -> a b c d f g -> c d

;;
;; longueur 6 restant = 0

(defun contient (chars refs)
  (null (set-difference refs chars)))

(contient
 (coerce "abcdef" 'list)
 (coerce "cba" 'list))

(contient
 (coerce "abdef" 'list)
 (coerce "cba" 'list))

(set-difference
 (coerce "abcdef" 'list)
 (coerce "cba" 'list))

(defun parse-part (part)
  (mapcar (lambda (s) (coerce s 'list)) (uiop:split-string (string-trim " " part))))

(defun day8-parse-line (line)
  (let ((parts (uiop:split-string line :separator "|")))
    (cons
     (parse-part (car parts))
     (parse-part (cadr parts)))))

(day8-parse-line "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe")

(defun longueur= (n) (lambda (xs) (= n (length xs))))

(defun remove-already-found (state)
  (let ((founds (loop for i from 0 to 9 collect (cdr (assoc i state)))))
    (set-difference
     (cdr (assoc 999 state))
     founds)))

(defun tri-initial (numbers)
  "Ici, on tire les nombres faciles : 1, 4, 7, 8"
  (list
   (cons 1 (car (remove-if-not (longueur= 2) numbers)))
   (cons 4 (car (remove-if-not (longueur= 4) numbers)))
   (cons 7 (car (remove-if-not (longueur= 3) numbers)))
   (cons 8 (car (remove-if-not (longueur= 7) numbers)))
   (cons 999 (remove-if-not (lambda (xs) (<= 5 (length xs) 6)) numbers))))

(tri-initial (car (day8-parse-line "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe")))

(defun est-3 (chiffre-1 numbers)
  (car (remove-if-not (lambda (s) (and (= (length s) 5)
                                       (contient s chiffre-1)))
                      numbers)))

(defun est-5 (bd numbers)
  (car (remove-if-not (lambda (s) (and (= (length s) 5)
                                       (contient s bd)))
                      numbers)))

(defun est-6 (chiffre-1 bd numbers)
  (car (remove-if-not (lambda (s) (and (= (length s) 6)
                                       (contient s bd)
                                       (= 1 (length (set-difference chiffre-1 s)))))
                      numbers)))

(defun deuxieme-tri (state)
  "longueur 5 + 2 caractères en commun avec 1  = 3"
  (let ((chiffre-1 (cdr (assoc 1 state)))
        (numbers (remove-already-found state)))
    (print (length (cdr (assoc 999 state))))
    (print (length numbers))
    (acons 3 (est-3 chiffre-1 numbers)
           (acons 999
                  numbers
                  state))))

(defun deuxieme-trib (state)
  "longueur 5 + 2 caractères en commun avec 1  = 3
4 - 1 = b d, longueur 5 + b et d = 5
4 - 1 = b d, longueur 6 + b et d = 6"
  (let ((chiffre-1 (cdr (assoc 1 state)))
        (chiffre-4 (cdr (assoc 4 state)))
        (numbers (remove-already-found state)))
    (print (length (cdr (assoc 999 state))))
    (print (length numbers))
    (let ((bd (set-difference chiffre-4 chiffre-1)))
      (print bd)
      (print numbers)
      (acons 5 (est-5 bd numbers)
             (acons 6 (est-6 chiffre-1 bd numbers)
                    (acons 999
                           numbers
                           state))))))

;; longueur 5 restant = 2
;; longueur 6, tout commun avec 5 = 9
(defun troisieme-tri (state)
  (let* ((chiffre-5 (cdr (assoc 5 state)))
         (numbers (remove-already-found state)))
    (acons 2
           (car (remove-if-not (longueur= 5) numbers))
           (acons 9
                  (car (remove-if-not (lambda (s)
                                        (contient s chiffre-5)) numbers))
                  (acons 999 numbers state)))))

(defun dernier-tri (state)
  (let* ((numbers (remove-already-found state)))
    (acons 0 (car numbers) state)))

(defun compose (&rest fns)
  (lambda (init)
    (reduce
     (lambda (v fn) (funcall fn v))
     (reverse fns)
     :initial-value init)))

(defun key-fn (s)
  (concatenate 'string (sort s #'string<)))

(key-fn '(#\b #\a #\e))

(defun reverse-state (state)
  (let ((h (make-hash-table :test 'equal)))
    (loop for n from 0 to 9
          do (print (cdr (assoc n state)))
             (setf (gethash (key-fn (cdr (assoc n state))) h) n))
    h))

(defvar digits-hash-table
  (compose #'reverse-state #'dernier-tri #'troisieme-tri #'deuxieme-tri #'deuxieme-trib #'tri-initial))

(defun day8-part2-line (line)
  (let* ((patterns (car line))
         (digits (cdr line))
         (h (funcall digits-hash-table patterns)))
    (reduce
     (lambda (total n)
       (+ (* 10 total) n))
     (mapcar (lambda (s) (gethash (key-fn s) h 0)) digits))))

(defun day8-part2 (input)
  (let ((lines (mapcar #'day8-parse-line input)))
    (reduce #'+ (mapcar #'day8-part2-line lines))))


(day8-part2
 (uiop:read-file-lines "~/git-repositories/advent-of-code-2021/day08/example"))

(day8-part2
 (uiop:read-file-lines "~/git-repositories/advent-of-code-2021/day08/input"))
