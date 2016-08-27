(in-package #:pentothal)

(defparameter *passed* nil)
(defparameter *failed* nil)
(defparameter *results* nil)
(defparameter *groups* nil)

(defun init-testing ()
  (setf *groups* nil)
  (setf *results* nil))

(defun in-group (group-symbol)
  (push nil *results*)
  (push group-symbol *groups*))

(defmacro test (name function-form comparison expected)
  `(progn 
     (push (list ',name 
		 (lambda () 
		   (if (funcall #',comparison
				,expected
				,function-form)
		       (progn (incf *passed*)
			      (format t "ok: ~a~%" ',name))
		       (progn (incf *failed*)
			      (format t
				      "NOT OK: ~a ==> got ~a, expected ~a from ~a~%"
				      ',name
				      ,function-form
				      ,expected
				      ',function-form)))))
	   (car *results*))
     nil))

(defun run-tests ()
  (setf *passed* 0)
  (setf *failed* 0)
  (loop 
     for test-pair-group in (reverse *results*)
     and group in (reverse *groups*)  
     do
       (terpri)
       (format t "~&~a~%" group)
       (loop for test-pair in test-pair-group
	  do 
	    (format t "   ")
	    (funcall (cadr test-pair))))
  (terpri)
  (format t "~&passed: ~a" *passed*)
  (format t "~&failed: ~a" *failed*)
  nil)
