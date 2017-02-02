;;;; 1255891.lisp
;;;; Deliverable for Assignment 1

;;;; Brett Commandeur
;;;; 1255891
;;;; CMPUt 325 Wi17 - NON-PROCEDURAL PROG LANGUAGES Combined LAB LEC Wi17

#|--- TODO: Remove this comment

Acceptable built-in functions:

(atom x)
(null x)
(eq x y)
(equal x y)
(numberp x)
(append x y)
(car x)
- (cadr ...)
(cdr x)
- (cdaar ...)
(cons x y)
(if x y z)
(cond ...)
(let ((x y) (u v)) z)
(let* ((x y) (u v)) z)
(defun ...)
(quote x) and its short form 'x
(list x1 x2 ...)
(print ...)
(sort L fun) % this is useful for the last problem
(copy-list L) % useful in conjunction with sort
- http://clhs.lisp.se/Body/f_cp_lis.htm#copy-list
- (sort (copy-list L) sort-predicate)

(+ x y)
(- x y)
(* x y)
(/ x y)
(< x y)
(> x y)
(= x y)
(<= x y)
(>= x y)
(and x y)
(or x y)
(not x)

|#

;;; Question 1

; Solution tests y against each element of x until a match
; or the end of x is found. It uses 3 base cases for first element of x with
; recursion on the remaining elements: (1) a fail case when x is nil and thus
; can't contain y, (2) a success case when the x element matches y, and (3)
; a fail case when x contains no more elements after not matching y.

(defun xmember (X Y)
  "Returns t if argument Y is a member of the argument list X and nil otherwise."

  (cond
    ;; base case: list is empty (fail)
    ((null X) nil)

    ;; base case: list contains Y (success)
    ((equal (car X) Y) t)

    ;; base case: end of list (fail)
    ((null (cdr X)) nil)

    ;; recursion to iterate over remaining elements of list
    (t (xmember (cdr X) Y))))

;;; Question 2

(defun flatten (x)
  "Returns list of only the atoms appearing in list x and its contained nested sublists in order of appearance."

  (collect-atoms x nil)) ; invoke accumulating parameters technique

(defun collect-atoms (x y)
  "Searches list x and sublists in x for atoms and captures them in list y"

  (if (null x) y ; end of list, return found atoms
    (collect-atoms ; else
      (cdr x) ; iterate over list
      (if (atom (car x)) ; element is atom?
        (append y (list (car x))) ; if so, add it to the return list
        (append y (collect-atoms (car x) nil)))))) ; if not, search it for atoms

;;; Question 3

(defun mix (L2 L1)
  (cond
    ((null L2) L1)
    ((null L1) L2)
    (t (append (list (car L1)) (list (car L2)) (mix (cdr L2) (cdr L1))))))
