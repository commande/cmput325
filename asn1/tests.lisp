;;;; Reference: http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html

;;; Helpers

(defvar *test-name* nil)

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
    ,@body))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call other test functions or use 'check' to run individual test cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)


;;; Tests

(deftest test-xmember ()
  (check
    (eq (xmember '(1) '1) T)
    (eq (xmember '((1) 2 3) '1) NIL)
    (eq (xmember '((1) 2 3) '(1)) T)
    (eq (xmember nil nil) NIL)
    (eq (xmember '(nil) nil) T)
    (eq (xmember '((nil)) nil) NIL)
    (eq (xmember '(1 2 3 (nil)) '(nil)) T)
    (eq (xmember '(nil) '(nil)) NIL)))

(deftest test-flatten ()
  (check
    (equal (flatten '(((((a))((((((b)))))))))) '(a b))
    (equal (flatten '(a (b c) (d ((e)) f))) '(a b c d e f))
    (equal (flatten '(a (b c) (((d)) e))) '(a b c d e))))

(deftest test-mix ()
  (check
    (equal (mix '(d e f) '(a b c)) '(a d b e c f))
    (equal (mix '(a) '(1 2 3)) '(1 a 2 3))
    (equal (mix '(d e f g h) '((a) (b c))) '((a) d (b c) e f g h))
    (equal (mix nil '(1 2 3)) '(1 2 3))
    (equal (mix '(nil) '(1 2 3)) '(1 nil 2 3))))

(defun test-asn1 ()
  (combine-results
    (test-xmember)
    (test-flatten)
    (test-mix)))
