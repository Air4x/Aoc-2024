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
  "Check if two numbers are gradual (absolute difference between 1 and 3, inclusive)."
  (and (>= (abs (- a b)) 1)
       (<= (abs (- a b)) 3)))

(defun gradual? (rules l)
  "check that the numbers in l are graduals following rules"
  (every rules l (cdr l)))

(defun safe? (report)
  "Dato un report come stringa controlla che il report sia safe"
  (if (and (sorted? (map 'list #'parse-integer (uiop:split-string report))) (gradual? #'gradual-rule (map 'list #'parse-integer (uiop:split-string report))))
      1
      0))

(defun solve1 (filename)
  (reduce #'+ (mapcar #'safe? (read-input filename))))

(print (solve1 "input"))


;; sorted is the same, but gradual must check for bad levels

;; if not, it remove one element and check again, it does that
;; for every element in the report

(defvar l '(8 6 4 4 1))

(defun gradual-with-dampener? (rules l)
  (loop for i from 0 below (length l)
        when (gradual? rules (remove (nth i l) l :count 1))
          do (return-from gradual-with-dampener? t))
  nil)

(defun report->integers (report)
  "Convert a report to a list of integers"
  (map 'list #'parse-integer (uiop:split-string report)))

(defun safe-with-dampener? (report)
  "Given a report as a string, return 1 it safe else it return 0
This function account for the problem dampener"
  (if (and (sorted? (report->integers report))
	   (gradual-with-dampener? #'gradual-rule (report->integers report)))
      1
      0))

(defun solve2 (filename)
  "Solve the second puzzle given the filename of the input"
  (reduce #'+ (mapcar #'safe-with-dampener? (read-input filename))))

