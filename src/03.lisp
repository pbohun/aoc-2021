;;;; --- Day 3: Binary Diagnostic ---
(defun read-binary-nums (filename)
  (with-open-file (f filename :direction :input)
	(loop for line = (read-line f nil)
		  while line collect (parse-integer line :radix 2))))

(defun get-majority-bit (nums mask threshold)
  (let ((nbits (loop for n in nums count (> (logand mask n) 0))))
	(if (>= nbits threshold) mask 0)))

(defun majority-bits (nums bits)
  (let ((mask 1) (result 0) (threshold (/ (length nums) 2)))
	(loop for i from 0 below bits do
		  (setf result (logior result
							   (get-majority-bit nums (ash mask i) threshold))))
	result))

;; part 1
(defun power-consumption (inputs)
  (let* ((gamma (majority-bits inputs 12))
		 (epsilon (logand 4095 (lognor gamma gamma))))
	(* gamma epsilon)))

;; part 2
(defun get-popular-bit (nums shift)
  (let ((nbits (loop for n in nums count (> (logand 1 (ash n shift)) 0))))
	(if (>= nbits (- (length nums) nbits)) 1 0)))

(defun get-unpopular-bit (nums shift)
  (let ((nbits (loop for n in nums count (> (logand 1 (ash n shift)) 0))))
	(if (< nbits (- (length nums) nbits)) 1 0)))

(defun filter-unpopular (nums shift)
  (let ((pbit (get-popular-bit nums shift)))
	(remove-if-not (lambda (x) (eq (logand (ash x shift) 1) pbit)) nums)))

(defun filter-popular (nums shift)
  (let ((pbit (get-unpopular-bit nums shift)))
	(remove-if-not (lambda (x) (eq (logand (ash x shift) 1) pbit)) nums)))

(defun o2-rating (nums shift)
  (cond
	((= (length nums) 1) (car nums))
	(t (o2-rating (filter-unpopular nums shift) (1+ shift)))))

(defun co2-rating (nums shift)
  (cond
	((= (length nums) 1) (car nums))
	(t (co2-rating (filter-popular nums shift) (1+ shift)))))

(defun life-support-rating (nums)
  (let ((o2 (o2-rating nums -11)) (co2 (co2-rating nums -11)))
	(* o2 co2)))

(time (format t "Part 1: ~a~%" (power-consumption (read-binary-nums "../inputs/03.txt"))))
(time (format t "Part 2: ~a~%" (life-support-rating (read-binary-nums "../inputs/03.txt"))))

