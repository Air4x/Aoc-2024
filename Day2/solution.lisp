(require "asdf")
(asdf:load-system :uiop)

(defun read-input (file)
  (let ((in (open file)))
    (loop for line = (read-line in nil)
  	while line
  	collect line)))

(defun sorted? (l)
  "check if a list of numbers is sorted, will it be ascending or descending order"
  (or (every #'<= l (rest l)) (every #'>= l (rest l))))

(defun gradual-rule (a b)
  "check if the arguments are gradual"
  (if (and (>= (abs (- a b)) 1) (<= (abs (- a b)) 3))
      t
      nil))

(defun gradual? (l)
  "check that the numbers in l are gradualy increasing or decreasing"
  (every #'gradual-rule l (cdr l)))

(defun safe? (report)
  "Dato un report come stringa controlla che il report sia safe"
  (if (and (sorted? (map 'list #'parse-integer (uiop:split-string report))) (gradual? (map 'list #'parse-integer (uiop:split-string report))))
      1
      0))

(defun solve1 (filename)
  (reduce #'+ (mapcar #'safe? (read-input filename))))

(print (solve1 "input"))
