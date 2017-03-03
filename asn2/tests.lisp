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
    ;;; Basic
    (equal (fl-interp '(+ 10 5) nil) '15)
    (equal (fl-interp '(- 12 8) nil) '4)
    (equal (fl-interp '(* 5 9) nil) '45)
    (equal (fl-interp '(> 2 3) nil) 'nil)
    (equal (fl-interp '(< 1 131) nil) 't)
    (equal (fl-interp '(= 88 88) nil) 't)
    (equal (fl-interp '(and nil t) nil) 'nil)
    (equal (fl-interp '(or t nil) nil) 't)
    (equal (fl-interp '(not t) nil) 'nil)
    (equal (fl-interp '(isnumber 354) nil) 't)
    (equal (fl-interp '(equal (3 4 1) (3 4 1)) nil) 't)
    (equal (fl-interp '(if nil 2 3) nil) '3)
    (equal (fl-interp '(null ()) nil) 't)
    (equal (fl-interp '(atom (3)) nil) 'nil)
    (equal (fl-interp '(eq x x) nil) 't)
    (equal (fl-interp '(first (8 5 16)) nil) '8)
    (equal (fl-interp '(rest (8 5 16)) nil) '(5 16))
    (equal (fl-interp '(cons 6 3) nil) '(6 . 3))

    ;;; Complex
    (equal (fl-interp '(+ (* 2 2) (* 2 (- (+ 2 (+ 1 (- 7 4))) 2))) nil) '12)
    (equal (fl-interp '(and (> (+ 3 2) (- 4 2)) (or (< 3 (* 2 2))) (not (= 3 2))) nil) 't)
    (equal (fl-interp '(or (= 5 (- 4 2)) (and (not (> 2 2)) (< 3 2))) nil) 'nil)
    (equal (fl-interp '(if (not (null (first (a c e)))) (if (isnumber (first (a c e))) (first (a c e)) (cons (a c e) d)) (rest (a c e))) nil) '((a c e) . d))

    ;;; Custom
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

(deftest test-user-defined ()
  (check
    ;; Helpers
    (equal (user-defined 'greater '((greater x y = (if (> x y) x (if (< x y) y nil))))) '2)
    (equal (user-defined 'square '((square x = (* x x)))) '1)
    (equal (user-defined 'simpleinterest '((simpleinterest x y z = (* x (* y z))))) '3)
    (equal (user-defined 'xor '((xor x y = (if (equal x y) nil t)))) '2)))

    ; ;; Basic = 4 total
    ; (equal (fl-interp '(greater 3 5) '((greater x y = (if (> x y) x (if (< x y) y nil))))) '5)
    ; (equal (fl-interp '(square 4) '((square x = (* x x))))) ; > '16
    ; (equal (fl-interp '(simpleinterest 4 2 5) '((simpleinterest x y z = (* x (* y z))))) '40)
    ; (equal (fl-interp '(xor t nil) '((xor x y = (if (equal x y) nil t)))) 't)
    ;
    ; ;; More complex = 6 total
    ; (equal (fl-interp '(last (s u p)) '((last x = (if (null (rest x)) (first x) (last (rest x)))))) 'p)
    ; (equal (fl-interp '(push (1 2 3) 4) '((push x y = (if (null x) (cons y nil) (cons (first x) (push (rest x) y)))))) '(1 2 3 4))
    ; (equal (fl-interp '(pop (1 2 3)) '((pop x = (if (atom (rest (rest x))) (cons (first x) nil) (cons (first x)(pop (rest x))))))) '(1 2))
    ; (equal (fl-interp '(power 4 2) '((power x y = (if (= y 1) x (power (* x x) (- y 1)))))) '16)
    ; (equal (fl-interp '(factorial 4) '((factorial x = (if (= x 1) 1 (* x (factorial (- x 1))))))) '24)
    ; (equal (fl-interp '(divide 24 4) '((divide x y = (div x y 0)) (div x y z = (if (> (* y z) x) (- z 1) (div x y (+ z 1)))))))) '6)

(defun test-asn2 ()
  (combine-results
    ; (test-primitives)
    (test-user-defined)))
