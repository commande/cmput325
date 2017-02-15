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
(deftest test-primitives ()
  (check
    ;; First
    (equal (fl-interp '(first (1 2 3)) nil) '1)
    ;; Rest
    (equal (fl-interp '(rest (1 2 (3))) nil) '(2 (3)))
    ;; First and Rest
    (equal (fl-interp '(first (rest (1 (2 3)))) nil) '(2 3))))

(defun test-asn2 ()
  (combine-results
    (test-primitives)))
