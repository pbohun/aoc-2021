;;;; --- Day 2: Dive! ---

(defun read-commands (filename)
  "Returns a list of commands where each command is of the form (forward 3)"
  (with-open-file (f filename :direction :input)
	(loop for line = (read-line f nil)
		  while line collect
		  (with-input-from-string (str (format nil "(~a)" line))
			(read str)))))

;; part 1
(defun multiply-position (commands)
  "Returns the product of the depth and position of the submarine"
  (let ((depth 0) (pos 0))
	(loop for cmd in commands
		  do (cond
			   ((eq 'forward (car cmd)) (incf pos (cadr cmd)))
			   ((eq 'up (car cmd)) (decf depth (cadr cmd)))
			   ((eq 'down (car cmd)) (incf depth (cadr cmd)))))
	(* depth pos)))

;; part 2
(defun submarine-aim (commands)
  "Returns the product of the depth and position of the submarine using aim"
  (let ((depth 0) (pos 0) (aim 0))
	(loop for cmd in commands
		  do (cond
			   ((eq 'forward (car cmd))
				(incf pos (cadr cmd))
				(incf depth (* aim (cadr cmd))))
			   ((eq 'up (car cmd)) (decf aim (cadr cmd)))
			   ((eq 'down (car cmd)) (incf aim (cadr cmd)))))
	(* depth pos)))

(time (format t "part 1: ~a~%" (multiply-position (read-commands "../inputs/02.txt"))))
(time (format t "part 2: ~a~%" (submarine-aim (read-commands "../inputs/02.txt"))))
