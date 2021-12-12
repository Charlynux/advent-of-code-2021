(defvar day12-example "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

;; La comparaison de strings se fait via equal et non via eq.
;; Ceci nous empêche de les utiliser pour "indexer" une association list
;; Je transforme donc les strings en keyword pour supporter "eq".
(defun make-keyword (name) (values (intern name "KEYWORD")))

(defun day12-parse (input)
  (reduce
   (lambda (acc path)
     (let ((caves (cdr (assoc (car path) acc))))
       (acons (car path) (cons (cdr path) caves) acc)))
   (mapcan
    (lambda (line)
      (let ((caves (mapcar #'make-keyword (uiop:split-string line :separator "-"))))
        (list
         (cons (car caves) (cadr caves))
         (cons (cadr caves) (car caves)))))
    (uiop:split-string input :separator '(#\NewLine)))
   :initial-value nil))

(defparameter *start-point* :|start|)
(defparameter *end-point* :|end|)

;; Depuis START je peux aller vers...
(cdr (assoc :|start| (day12-parse day12-example)))

(string :|start|)
(string :A)

(defun small-cavep (cave)
  (<= 97
      (char-code (char (string cave) 0))
      122))

(small-cavep :|he|)
(small-cavep :BD)

;; open - start-point
;; closed - nil
;; current-path nil

(defun visit-all-once (paths start closed current)
  "Solution incomplète. On visite où l'on ne visite les noeuds qu'une fois."
  (if (eq start *end-point*)
      (list current)
      (let* ((next (set-difference
                    (cdr (assoc start paths))
                    closed)))
        (if next
            (mapcan (lambda (next-start)
                      (visit-all-once
                       paths
                       next-start
                       (cons next-start closed)
                       (cons next-start current)))
                    next)
            (list current)))))

(visit-all-once (day12-parse day12-example)
                *start-point*
                (list *start-point*)
                (list *start-point*))

(defun visit-small-caves-once (paths start closed current)
  (if (eq start *end-point*)
      (list current)
      (let* ((next (set-difference
                    (cdr (assoc start paths))
                    closed)))
        (if next
            (mapcan (lambda (next-start)
                      (visit-small-caves-once
                       paths
                       next-start
                       (if (small-cavep next-start)
                           (cons next-start closed)
                           closed)
                       (cons next-start current)))
                    next)
            (list current)))))

(defun finishedp (path) (eq *end-point* (car path)))

(length (remove-if-not #'finishedp
                       (visit-small-caves-once (day12-parse day12-example)
                                               *start-point*
                                               (list *start-point*)
                                               (list *start-point*))))

(defun day12-part1 (input)
  (length (remove-if-not #'finishedp
                         (visit-small-caves-once (day12-parse input)
                                                 *start-point*
                                                 (list *start-point*)
                                                 (list *start-point*)))))

(defvar day12-larger-example
  "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

(day12-part1 day12-larger-example)

(defvar day12-input
  "ex-NL
ex-um
ql-wv
VF-fo
VF-ql
start-VF
end-tg
wv-ZQ
wv-um
NL-start
lx-ex
ex-wv
ex-fo
sb-start
um-end
fo-ql
NL-sb
NL-fo
tg-NL
VF-sb
fo-wv
ex-VF
ql-sb
end-wv")

(day12-part1 day12-input)

(defun already-small-twice (closed current)
  (or
   (remove-if
    (lambda (pair) (= 1 (cdr pair)))
    (reduce
     (lambda (acc cave)
       (let ((count (or (cdr (assoc cave acc)) 0)))
         (acons cave (1+ count) acc)))
     (remove-if-not #'small-cavep current)
     :initial-value nil))
   ;; Si une petite cave est dans la liste des visitées, mais pas dans les closeds, alors on a déjà prévu de la visiter une deuxième fois.
   (set-difference
    (remove-if-not #'small-cavep current)
    (remove-if-not #'small-cavep closed))))

(already-small-twice (list :|b|) (list :|a| :B :|b|))

(already-small-twice (list :|a| :|b|) (list :|a| :B :|b|))

(defun visit-one-small-cave-twice (paths start closed current)
  (if (eq start *end-point*)
      (list current)
      (let* ((next (set-difference
                    (cdr (assoc start paths))
                    closed)))
        (if next
            (mapcan (lambda (next-start)
                      (append
                       (when (small-cavep next-start)
                         (visit-one-small-cave-twice
                          paths
                          next-start
                          (cons next-start closed)
                          (cons next-start current)))
                       (when (or (not (small-cavep next-start))
                                 (not (already-small-twice closed current)))
                         (visit-one-small-cave-twice
                          paths
                          next-start
                          closed
                          (cons next-start current)))))
                    next)
            (list current)))))



(defun remove-duplicates-paths (paths)
  (let ((contains (lambda (acc path)
                    (reduce (lambda (found already-seen-path)
                              (or found
                                  (equal already-seen-path path)))
                            acc :initial-value nil))))
    (reduce
     (lambda (acc path)
       (if (funcall contains acc path)
           acc
           (cons path acc)))
     paths
     :initial-value nil)))

(remove-duplicates-paths (list (list :a :b :c) (list :a :b) (list :a :b :c) (list :a)))

(defun day12-part2 (input)
  (length
   (remove-duplicates-paths
    (remove-if-not #'finishedp
                   (visit-one-small-cave-twice (day12-parse input)
                                               *start-point*
                                               (list *start-point*)
                                               (list *start-point*))))))

(day12-part2 day12-larger-example)

(defvar day12-slightly-larger-example
  "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

(day12-part2 day12-slightly-larger-example)

(day12-part2 day12-input)
