# pentothal
Find the TRUTH about your code!

Pentothal is a unit testing framework distinguished by its lack of features and lack of abilities. You can pound your head against a wall trying to learn someone else's Common Lisp testing framework by reading the (mostly) non-existent documentation... or you can write your own in half the time. Pentothal is an example of the latter.

##Setup
The easiest and probably best way to use ```pentothal``` is to just copy the code right into your project and use it. But if you want it as a seperate package, here is what to do. I assume you have Emacs, SBCL and Quicklisp set up. Git clone ```pentothal``` into ```local-projects,``` then make a new project that will use ```pentothal``` for testing. We will call it ```flaming-guava.``` This project can be created automatically by Quickproject, but let's do it manually here, just to see how that is done.

```
$ cd quicklisp/local-projects
$ git clone https://github.com/tedszy/pentothal.git
$ mkdir flaming-guava
$ cd flaming-guava
$ touch flaming-guava.asd flaming-guava.lisp package.lisp
```

In the system definition file ```flaming-guava.asd``` put the ```pentothal``` dependency.

```common-lisp
(asdf:defsystem #:flaming-guava
    :description "Test project."
    :author "Your name <your@email.com"
    :license "None yet."
    :serial t
    :depends-on (#:pentothal)
    :components ((:file "package")
		         (:file "flaming-guava")))
```

In ```package.lisp,``` specify that you want to use the ```pentothal``` namespace

```common-lisp
(defpackage #:flaming-guava
  (:use #:cl
	    #:pentothal)
  (:export #:nothing-yet))
```

Everything is set up at this point, except that Quicklisp does not yet know about the existence of these two projects. You have to register them.

```common-lisp
CL-USER> (ql:register-local-projects)
NIL
```

Now we are ready to write tests in ```flaming-guava.lisp.``` 

##Testing

Some testing systems have innumerable little assertion and checking functions: ```assert-true```, ```assert-less-than```, ```check-equal-in-some-way.``` We avoid this. Why is that necessary when Common Lisp already has loads of specific equality and comparison functions? And if that's not enough, write your own. We just pass them in to the ```test``` macro. 

```common-lisp
(test descriptive-name function-form comparison expected-result)
```
Descriptive name is just anything you like as long as it's a legal symbol name. Function form is the thing you are testing: an expression that returns a result. You supply the comparison operator. For example, if you want to compare vectors, use ```equalp.``` The expected result is what the function form should return if it is working correctly.

Don't forget to initialize testing! Tests can be grouped according to topic using ```in-group```. Pass ```in-group``` a symbol that describes your test group.

```common-lisp
(init-testing)

(in-group 'math)
(test barf (+ 1 1 1) = 3)
(test merlin (+ 1 1 1) = 4) ;; make this one fail just to see.

(in-group 'more-math)
(test queequeg (* 5 2) = 10)
(test zarf (+ 1 1) = 2)
(test zoo (make-array 3 :initial-contents '(1 2 3)) equalp #(1 2 3))

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

MATH
   NOT OK: MERLIN ==> got 3, expected 4 from (+ 1 1 1)
   ok: BARF

MORE-MATH
   ok: ZOO
   ok: ZARF
   ok: QUEEQUEG

passed: 4
failed: 1
NIL
```

