(in-package #:cl-pentothal)

(defparameter *passed* nil)
(defparameter *failed* nil)
(defparameter *results* nil)

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
				      "NOT OK: ~a ==> got ~a, expected ~a in ~a~%"
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
  'done)

;; example. 

#| 

(test barf (+ 1 1 1) = 3)
(test merlin (+ 1 1 1) = 4)
(test queequeg (* 5 2) = 10)
(test zarf (+ 1 1) = 2)
(test zoo 
      (make-array 3 :initial-contents '(1 2 3)) 
      equalp 
      #(1 2 3))

;; CL-PENTOTHAL> (run-tests)
;; ok: BARF
;; NOT OK: MERLIN ==> got 3, expected 4 in (+ 1 1 1)
;; ok: QUEEQUEG
;; ok: ZARF
;; ok: ZOO
;; passed: 4
;; failed: 1
;; DONE

|#
