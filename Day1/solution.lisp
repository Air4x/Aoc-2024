(require "asdf")
(asdf:load-system :uiop)

(defun read-input (file)
  "Read a file with two columns delimited by a - and
return two lists"
  (let ((l1 '())
	(l2 '())
	(in (open file)))
    (loop for line = (read-line in nil)
	  while line
	  do (let ((line (uiop:split-string line :max 2 :separator "-")))
	       (push (car line) l1)
	       (push (cadr line) l2)))
    (values l1 l2)))

(defun solve1 (l1 l2)
  "calcola al distanza totale tra due liste"
  (let ((xs (sort l1 #'<))
	(ys (sort l2 #'<)))
    (reduce #'+ (loop for x in xs for y in ys
		      collect (abs (- x y))))))

(defvar soluzione1
  (multiple-value-bind (l1 l2) (read-input "input")
    (solve1 (mapcar #'parse-integer l1) (mapcar #'parse-integer l2))))

(print soluzione1)


(defun solve2 (l1 l2)
  (reduce #'+ (loop for x in l1
	 collect (* x (count x l2)))))

(defvar soluzione2
  (multiple-value-bind (l1 l2) (read-input "input")
    (print (solve2 (mapcar #'parse-integer l1) (mapcar #'parse-integer l2)))))

(multiple-value-bind (l1 l2) (read-input "input")
  (print (solve2 (mapcar #'parse-integer l1) (mapcar #'parse-integer l2))))

(print soluzione2)
