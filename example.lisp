(in-package #:cl-pentothal)

(init-testing)

(in-group 'mathematics)

(test add (+ 1 1) = 2)
(test mul (* 1 2 3) = 6)

(in-group 'logic)

(test and (and t nil) eq nil)
(test or (or t nil) eq t)

(run-tests)
