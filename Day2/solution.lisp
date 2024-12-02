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
  (and (/= a b)  ;; Ensure the numbers are not identical
       (>= (abs (- a b)) 1)
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

;; it simply check if the report is safe
;; if not, it remove one element and check again, it does that
;; for every element in the report
(defun safe-with-dampener? (report)
  "Check if a report is safe, allowing one level to be ignored if it makes the report safe.
  Return 1 if safe, 0 otherwise."
  (let* ((levels (map 'list #'parse-integer (uiop:split-string report)))
         (is-safe
           (lambda (levels)
             (and (sorted? levels) (gradual? #'gradual-rule levels)))))
    (if (or (funcall is-safe levels)  ;; Check if the original report is safe
            (some (lambda (i)
                    (funcall is-safe (remove (nth i levels) levels)))
                  (loop for i from 0 below (length levels) collect i)))
        1  ;; Safe
        0)))


(defun solve2 (f)
  (reduce #'+ (mapcar #'safe-with-dampener? (read-input f))))

(defun test (report)
  "Check if a report is safe, allowing one level to be ignored if it makes the report safe.
  Return 1 if safe, 0 otherwise."
  (let* ((levels (map 'list #'parse-integer (uiop:split-string report)))
         (is-safe
           (lambda (levels)
             (and (or (every #'<= levels (rest levels))  ;; Allow for ascending order
                      (every #'>= levels (rest levels))) ;; Allow for descending order
                  (gradual? #'gradual-rule levels)))))
    (if (or (funcall is-safe levels)  ;; Check if the original report is safe
            (some (lambda (i)
                    (funcall is-safe (remove (nth i levels) levels)))
                  (loop for i from 0 below (length levels) collect i)))
        1  ;; Safe
        0)))
