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
    ;; First and Rest
    (equal (fl-interp '(first (1 2 3)) nil) '1)
    (equal (fl-interp '(rest (p 1 2 (3))) nil) '(1 2 (3)))
    (equal (fl-interp '(rest (1 2 (3))) nil) '(2 (3)))
    (equal (fl-interp '(first (rest (1 (2 3)))) nil) '(2 3))
    ;; If
    (equal (fl-interp '(if (> 1 0) (+ 1 2) (+ 2 3)) nil) '3)
    (equal (fl-interp '(if (> 1 0) (if (eq 1 2) 3 4) 5)  nil) '4)
    ;; Others
    (equal (fl-interp '(eq (< 3 4) (eq (+ 3 4) (- 2 3))) nil) 'nil)
    (equal (fl-interp '(cons (first (1 2 3))  (cons a nil)) nil) '(1 a))
    (equal (fl-interp '(and (or t nil) (> 3 4)) nil) 'nil)
    (equal (fl-interp '(eq (1 2 3) (1 2 3)) nil) 'nil)
    (equal (fl-interp '(equal (1 2 3) (1 2 3)) nil) 't)))

(defun test-asn2 ()
  (combine-results
    (test-primitives)))
