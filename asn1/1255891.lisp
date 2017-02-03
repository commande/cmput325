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
    ;; Base Cases
    ((null X) nil) ; list is empty (fail)
    ((equal (car X) Y) t) ; list contains Y (success)
    ((null (cdr X)) nil) ; end of list (fail)

    ;; Recursion
    (t
      (xmember (cdr X) Y)))) ; test remaining elements for match


;;; Question 2

; Solution uses a helper function following accumulating parameters technique
; to "gather" atoms in a return list. The helper uses recursion to not only
; search the input list for atoms by
; iterating over each element, but also search within list elements encountered
; for atoms inside them.

(defun flatten (x)
  "Returns list of only the atoms appearing in list x and its contained nested sublists in order of appearance."

  ;; Use accumulating parameters helper
  (collect-atoms x nil))

(defun collect-atoms (x y)
  "Searches list x and sublists in x for atoms and captures them in list y"

  (if
    ;; Base Case
    (null x) y ; end of list, return found atoms

    ;; Recursion
    (collect-atoms
      (cdr x) ; iterate over remaining elements in list
      (if (atom (car x)) ; but first check if this element is an atom
        (append y (list (car x))) ; if it is, add it to the return list
        (append y (collect-atoms (car x) nil)))))) ; if it's not, search within it for atoms


;;; Question 3

; Solution uses recursion with 2 base cases, one for either input list being
; empty. It takes the first element of each list, joins them, and continues
; doing so with what remains in both lists.

(defun mix (L2 L1)
  "Mixes the elements of L1 and L2 into a single list, by choosing elements from L1 and L2 alternatingly."

  (cond
    ;; Base Cases
    ((null L2) L1) ; Nothing left in L2, return what's left in L1
    ((null L1) L2) ; Nothing left in L1, return what's left in L2

    ;; Recursion
    (t
      (append ; Join the following in order:(li
        (list (car L1)) ; 1st element of L1,
        (list (car L2)) ; 1st element of L2,
        (mix (cdr L2) (cdr L1)))))) ; remaining elements of both



;;; Question 4

; Try every second element and then what remains. Actually dont...
; accumulating params...

(defun split (L)
  (split-into L (nil nil)))

(defun split-into (L R)
  "R is a list containing two sublists"

  (cond
    ;; Base Cases
    ((null L) R)

    ;; Recursion
    (t ()
      (split-into L R))))
