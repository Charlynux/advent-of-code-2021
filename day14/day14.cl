(loop for (a b) on (coerce "NNCB" 'list)
      nconcing (list a b))
;; if match then a + match else a

;; Stockage des patterns dans un arbre
(defvar test-tree
  (list (cons #\N (list (cons #\N #\C) (cons #\B #\B)))))

(cdr (assoc #\N (cdr (assoc #\N test-tree))))

(defun find-rule (rules a b)
  (when b
    (cdr (assoc b (cdr (assoc a rules))))))

(find-rule test-tree #\B nil)
(find-rule test-tree #\N #\B)
(find-rule test-tree #\N #\n)

(defun day14-step (word rules)
  (loop for (a b) on word
        nconcing (let ((insert (find-rule rules a b)))
                   (if insert
                       (list a insert)
                       (list a)))))

(day14-step (coerce "NNCB" 'list) test-tree)

;; TODO : PARSING

(defun parse-insertion-rule (line)
  (ppcre:register-groups-bind (pattern insert)
      ("([A-Z][A-Z]) -> ([A-Z])" line :sharedp t)
    (cons pattern insert)))

(char "N" 0)

(defun make-tree (rules)
  (reduce
   (lambda (tree rule)
     (destructuring-bind (pattern . insert) rule
       (let ((subtree (cdr (assoc (char pattern 0) tree))))
         (acons (char pattern 0)
                (acons (char pattern 1) (char insert 0) subtree)
                tree))))
   rules
   :initial-value nil))

(defun parse-rules (input-rules)
  (make-tree (mapcar #'parse-insertion-rule input-rules)))

(defun day14-parse (input)
  (let ((input-lines (uiop:read-file-lines input)))
    (cons (coerce (car input-lines) 'list)
          (parse-rules (cddr input-lines)))))

(day14-parse "~/git-repositories/advent-of-code-2021/day14/example")

(defun day14-recuring-step (word rules n)
  (if (zerop n)
      word
      (day14-recuring-step
       (day14-step word rules)
       rules
       (1- n))))

(defun occurrences (lst)
  (let ((table (make-hash-table :test #'equal)))
    (loop for x in lst
          do (incf (gethash x table 0)))
    (loop for k being the hash-key of table
            using (hash-value v)
          collect (cons k v))))

(defun max-and-min (alist)
  (let ((sorted (sort (mapcar #'cdr alist)  #'>)))
    (list (car sorted)
          (car(last sorted)))))

(defun day14-generic (input steps)
  (let ((resulting-word (destructuring-bind (word . rules) (day14-parse input)
                          (day14-recuring-step word rules steps))))
    (apply #'- (max-and-min (occurrences resulting-word)))))

(defun day14-part1 (input)
  (day14-generic input 10))

(day14-part1 "~/git-repositories/advent-of-code-2021/day14/example")
(day14-part1 "~/git-repositories/advent-of-code-2021/day14/input")
