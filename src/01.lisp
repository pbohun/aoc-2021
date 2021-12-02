;; --- Day 1: Sonar Sweep ---

(defun read-numbers (filename)
  "Returns a list of numbers read from filename"
  (with-open-file (f filename :direction :input)
	(loop for num = (read f nil) while num collect num)))

;; part 1
(defun increasing-pairs (nums)
  "Returns the number of times pairs of a list are increasing"
  (loop for (a b) on nums while b count (< a b)))

;; part 2
(defun increasing-threes (nums)
  "Returns the number of times the sum of sliding windows of size 3 increase"
  (loop for (a b c d) on nums while d count (< (+ a b c) (+ b c d))))

(time (format t "part 1: ~a~%" (increasing-pairs (read-numbers "../inputs/01.txt"))))
(time (format t "part 2: ~a~%" (increasing-threes (read-numbers "../inputs/01.txt"))))
