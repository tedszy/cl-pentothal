(in-package #:cl-pentothal)

(defparameter *passed* nil)
(defparameter *failed* nil)
(defparameter *results* nil)

(defun init-testing ()
  (setf *results* nil))

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
	   *results*)
     nil))

(defun run-tests ()
  (setf *passed* 0)
  (setf *failed* 0)
  (loop for test-pair in (reverse *results*)
       do (funcall (cadr test-pair)))
  (format t "~&passed: ~a" *passed*)
  (format t "~&failed: ~a" *failed*)
  nil)
