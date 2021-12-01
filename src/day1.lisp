;; --- Day 1: Sonar Sweep ---
;; ...
;; The first order of business is to figure out how quickly the depth increases, just so you know what you're dealing with - you never know if the keys will get carried into deeper water by an ocean current or a fish or something.
;;
;; To do this, count the number of times a depth measurement increases from the previous measurement. (There is no measurement before the first measurement.) In the example above, the changes are as follows:
;;
;; 199 (N/A - no previous measurement)
;; 200 (increased)
;; 208 (increased)
;; 210 (increased)
;; 200 (decreased)
;; 207 (increased)
;; 240 (increased)
;; 269 (increased)
;; 260 (decreased)
;; 263 (increased)
;; 
;; In this example, there are 7 measurements that are larger than the previous measurement.
;;
;; How many measurements are larger than the previous measurement?

(defun read-numbers (filename)
  "Returns a list of numbers read from filename"
  (with-open-file (f filename :direction :input)
	(loop for num = (read f nil) while num collect num)))

;; part 1
(defun increasing-pairs (nums)
  "Returns the number of times pairs of a list are increasing"
  (loop for (a b) on nums while b count (< a b)))

(time (format t "part 1:~a~%" (increasing-pairs (read-numbers "../inputs/day1.txt"))))

;; part 2
;; instead of pairs, do a sum of sliding windows of size 3
;; count the number of times the sliding window increases
(defun increasing-threes (nums)
  "Returns the number of times the sum of sliding windows of size 3 increase"
  (loop for (a b c d) on nums while d count (< (+ a b c) (+ b c d))))

(time (format t "part 2:~a~%" (increasing-threes (read-numbers "../inputs/day1.txt"))))
