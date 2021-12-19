(defparameter *hex-to-binary* (make-hash-table))

(ql:quickload "cl-ppcre")
(loop for line in (uiop:split-string
                   "0 = 0000
1 = 0001
2 = 0010
3 = 0011
4 = 0100
5 = 0101
6 = 0110
7 = 0111
8 = 1000
9 = 1001
A = 1010
B = 1011
C = 1100
D = 1101
E = 1110
F = 1111"
                   :separator '(#\Newline))
      do (ppcre:register-groups-bind (hex binary)
             ("(.*) = (.*)" line :sharedp t)
           (setf
            (gethash (char hex 0) *hex-to-binary*)
            binary)))

(gethash #\B *hex-to-binary*)
(gethash #\9 *hex-to-binary*)

(defun hex-to-binary (hex)
  (apply #'concatenate 'string
         (mapcar
          (lambda (h) (gethash h *hex-to-binary*))
          (coerce hex 'list))))

(defun read-binary (binary)
  (parse-integer binary :radix 2))

(defun read-header (binary)
  (values
   (cons
    (read-binary (subseq binary 0 3))
    (read-binary (subseq binary 3 6)))
   (subseq binary 6)))

(read-header (hex-to-binary "D2FE28"))
(read-header (hex-to-binary "38006F45291200"))
(read-header (hex-to-binary "EE00D40C823060"))

(defun read-type-4-value (binary)
  (let ((prefix (char binary 0))
        (value (subseq binary 1 (1+ 4))))
    (if (eql prefix #\0)
        (values value (subseq binary 5))
        (multiple-value-bind (next-value rest)
            (read-type-4-value (subseq binary 5))
          (values
           (concatenate 'string value next-value)
           rest)))))

(defun read-type-4 (header binary)
  (multiple-value-bind (value rest)
      (read-type-4-value binary)
    (values
     (list header (read-binary value))
     rest)))

(defun read-packet (binary)
  (labels ((read-subpackets-length (binary length)
             (if (zerop length)
                 (values nil binary)
                 (multiple-value-bind (packet rest)
                     (read-packet binary)
                   (multiple-value-bind
                         (packets final-rest)
                       (read-subpackets-length
                        rest
                        (- length
                           (- (length binary) (length rest))))
                     (values
                      (cons packet packets)
                      final-rest)))))
           (read-subpackets-count (binary count)
             (if (zerop count)
                 (values nil binary)
                 (multiple-value-bind (packet rest)
                     (read-packet binary)
                   (multiple-value-bind
                         (packets final-rest)
                       (read-subpackets-count
                        rest
                        (1- count))
                     (values
                      (cons packet packets)
                      final-rest))))))
    (multiple-value-bind (header rest) (read-header binary)
      (if (= 4 (cdr header))
          (read-type-4 header rest)
          (multiple-value-bind
                (packets final-rest)
              (if (equal #\0 (char rest 0))
                  (read-subpackets-length
                   (subseq rest (1+ 15))
                   (read-binary (subseq rest 1 (1+ 15))))
                  (read-subpackets-count
                   (subseq rest (1+ 11))
                   (read-binary (subseq rest 1 (1+ 11)))))
            (values (cons header packets) final-rest))))))

(read-packet (hex-to-binary "38006F45291200"))
(read-packet (hex-to-binary "EE00D40C823060"))

(defun sum-all-version (packet)
  (if (typep packet 'number)
      0
      (let ((version (caar packet)))
        (reduce
         #'+
         (mapcar #'sum-all-version (cdr packet))
         :initial-value version))))

(defun day16-part1 (input)
  (sum-all-version (read-packet (hex-to-binary input))))

(= (day16-part1 "8A004A801A8002F478")
   16)

(= (day16-part1 "620080001611562C8802118E34")
   12)

(= (day16-part1 "C0015000016115A2E0802F182340")
   23)

(= (day16-part1 "A0016C880162017C3686B18A3D4780")
   31)

(defvar day16-input
  (uiop:read-file-line "~/git-repositories/advent-of-code-2021/day16/input"))

(day16-part1 day16-input)


(defun packet-value (packet)
  (labels
      ((reduction (fn)
         (reduce
          fn
          (mapcar #'packet-value (cdr packet))))
       (compare (fn)
         (destructuring-bind
             (a b)
             (mapcar #'packet-value (cdr packet))
           (if (funcall fn b a) 1 0))))
    (case (cdar packet)
      (4 (cadr packet))
      (0 (reduction #'+))
      (1 (reduction #'*))
      (2 (reduction #'min))
      (3 (reduction #'max))
      (5 (compare #'<))
      (6 (compare #'>))
      (7 (compare #'=)))))

(defun day16-part2 (input)
  (packet-value (read-packet (hex-to-binary input))))

(day16-part2 "D2FE28")
(day16-part2 "C200B40A82D2FE28")
(print(day16-part2 day16-input))
