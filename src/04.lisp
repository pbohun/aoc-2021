;;;; --- Day 4: Giant Squid --

(defun read-input (filename)
  (let* ((lines (uiop:read-file-lines filename))
		 (bingo-nums (read-nums (car lines)))
		 (boards (read-boards (cddr lines)))
		 (table (make-hash-table)))
	(setf (gethash :inputs table) bingo-nums)
	(setf (gethash :boards table) boards)
	table))

(defun read-nums (line)
  (with-input-from-string (str (format nil "(~a)" (cl-ppcre:regex-replace-all "," line " ")))
	(read str)))

(defun line-to-nums (line)
  (with-input-from-string (s (format nil "(~a)" line)) (read s)))

(defun get-row (board row)
  (nth row board))

(defun get-col (board col)
  (loop for row in board collect (nth col row)))

(defun board-as-columns (board)
  (loop for i from 0 below (length (car board)) collect (get-col board i)))

(defun read-boards (lines)
  (let ((boards '()) (tmp '()))
	(loop for line in lines do (if (> (length line) 0)
								   (push (line-to-nums line) tmp)
								   (progn (push tmp boards)
										  (setf tmp '()))))
	boards))

(defun read-board (lines)
  (loop for ln in lines collect
		(with-input-from-string (s (format nil "(~a)" ln)) (read s))))

(defun mark-board (board row col)
  (let ((val (nth col (nth row board))))
	(setf (nth col (nth row board)) (cons val '()))
	board))

(defun find-num (board num)
  (let ((location nil))
	(loop for i from 0 below (length board) do
		  (let ((col (position num (nth i board))))
			(when (not (null col)) (setf location (list i col)))))
	location))

(defun find-and-mark (board num)
  (let ((location (find-num board num)))
	(if (null location)
		board
		(mark-board board (car location) (cadr location)))))

(defun bingop (lst)
  (reduce (lambda (x y) (and x y)) (mapcar #'listp lst)))

(defun board-winp (board)
  (if (null board)
	  nil
	  (let ((row-win (reduce (lambda (x y) (or x y)) (mapcar #'bingop board)))
		(col-win (reduce (lambda (x y) (or x y))
						 (mapcar #'bingop (board-as-columns board)))))
		(or row-win col-win))))

(defun score-board (board)
  (if (null board)
	  0
	  (let ((nums (loop for row in board collect (remove-if #'listp row))))
		(reduce #'+ (apply #'append nums)))))

(defun find-winning-board (boards)
  (let ((winning-board nil))
	(loop for b in boards do
		  (when (board-winp b) (setf winning-board b)))
	winning-board))

(defun remove-winning-board (boards)
  (remove-if #'board-winp boards))

;; part 1
(defun win-bingo (table)
  (let ((winner nil)
		(score 0)
		(final-num 0)
		(inputs (gethash :inputs table))
		(boards (gethash :boards table)))
	(loop for num in inputs while (null winner) do
		  (setf boards (mapcar (lambda (x) (find-and-mark x num)) boards))
		  (setf winner (find-winning-board boards))
		  (setf score (score-board winner))
		  (setf final-num num))
	(* final-num score)))

;; part 2 
(defun lose-bingo (table)
  (let ((num-winning 0)
		(last-board-to-win nil)
		(score 0)
		(inputs (gethash :inputs table))
		(boards (gethash :boards table))
		(final-num 0))
	(loop for num in inputs while (< num-winning (length boards)) do
		  (setf boards (mapcar (lambda (x) (find-and-mark x num)) boards))
		  (setf last-board-to-win (find-winning-board boards))
		  (setf boards (remove-winning-board boards))
		  (setf score (score-board last-board-to-win))
		  (setf final-num num))
	(* final-num score)))

(time (format t "Part 1: ~a~%" (run-bingo (read-input "../inputs/04.txt"))))
(time (format t "Part 2: ~a~%" (lose-bingo (read-input "../inputs/04.txt"))))
