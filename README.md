# cl-pentothal
Find the TRUTH about your code!

CL-pentothal is a unit testing framework distinguished by its lack of features and lack of abilities. You can pound your head against a wall trying to learn someone else's Common Lisp testing framework by reading the (mostly) non-existent documentation... or you can write your own in half the time. CL-pentothal is an example of the latter.

##Setup
The easiest and probably best way to use ```cl-pentothal``` is to just copy the code right into your project and use it. But if you want it as a seperate package, here is what to do. I assume you have Emacs, SBCL and Quicklisp set up. Git clone ```cl-pentothal``` into ```local-projects,``` then make a new project that will use ```cl-pentothal``` for testing. We will call it ```flaming-guava.``` This project can be created automatically by Quickproject, but let's do it manually here, just to see how that is done.

```
$ cd quicklisp/local-projects
$ git clone https://github.com/tedszy/cl-pentothal.git
$ mkdir flaming-guava
$ cd flaming-guava
$ touch flaming-guava.asd flaming-guava.lisp package.lisp
```

In the system definition file ```flaming-guava.asd``` put the ```cl-pentothal``` dependency.

```common-lisp
(asdf:defsystem #:flaming-guava
    :description "Test project."
    :author "Your name <your@email.com"
    :license "None yet."
    :serial t
    :depends-on (#:cl-pentothal)
    :components ((:file "package")
		         (:file "flaming-guava")))
```

In ```package.lisp,``` specify that you want to use the ```cl-pentothal``` namespace

```common-lisp
(defpackage #:flaming-guava
  (:use #:cl
	    #:cl-pentothal)
  (:export #:nothing-yet))
```

Everything is set up at this point, except that Quicklisp does not yet know about the existence of these two projects. You have to register them.

```common-lisp
CL-USER> (ql:register-local-projects)
NIL
```

Now we are ready to write tests in ```flaming-guava.lisp.``` 

```common-lisp
(init-testing)

(test barf (+ 1 1 1) = 3)
(test merlin (+ 1 1 1) = 4) ;; make this one fail just to see.
(test queequeg (* 5 2) = 10)
(test zarf (+ 1 1) = 2)
(test zoo 
      (make-array 3 :initial-contents '(1 2 3)) 
      equalp 
      #(1 2 3))

```

Load ```flaming-guava``` into SBCL and run the tests like so:

```common-lisp
CL-USER> (ql:quickload :flaming-guava)
To load "flaming-guava":
  Load 1 ASDF system:
    flaming-guava
; Loading "flaming-guava"

CL-USER> (in-package :flaming-guava)
#<PACKAGE "FLAMING-GUAVA">

FLAMING-GUAVA> (run-tests)
ok: BARF
NOT OK: MERLIN ==> got 3, expected 4 from (+ 1 1 1)
ok: QUEEQUEG
ok: ZARF
ok: ZOO
passed: 4
failed: 1
NIL

```

