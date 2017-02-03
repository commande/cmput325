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

;;; ----------
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

;;; ----------
;;; Question 2

; Solution uses a helper function following accumulating parameters technique
; to "gather" atoms in a return list. The helper uses recursion to not only
; search the input list for atoms by iterating over each element, but also
; search within list elements encountered for atoms inside them.

; A second helper function is defined to help construct return list.

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
        (append y (pluck x)) ; if it is, add it to the return list
        (append y (collect-atoms (car x) nil)))))) ; if it's not, search within it for atoms


(defun pluck (L)
  "Returns first element of list L as a one-element list or nil if L is empty"

  (if (null L) nil ; ensures nil input returns 'nil not '(nil)
    (list (car L)))) ; first element of L as a list



;;; ----------
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
        (pluck L1) ; 1st element of L1,
        (pluck L2) ; 1st element of L2,
        (mix (cdr L2) (cdr L1)))))) ; remaining elements of both



;;; ----------
;;; Question 4

; Solution iterates two elements at a time through the input list using an
; accumulating paramters helper function to capture and return the alternating
; elements.

(defun split (L)
  (split-into L '(nil nil)))

(defun split-into (L R)
  "Appends elements of list L alternatingly to the two sublists in list R"

  (if
    ;; Base Case
    (null L) R ; input list is empty

    ;; Recursion
    (split-into
      (cddr L) ; discard the first two elements of L
      (list ; but first capture them into R[0] and R[1] respectively
        (append (car R) (pluck L)) ; add L[0] to R[0]
        (append (cadr R) (pluck (cdr L))))))) ; add L[1] to R[1]


;;; ----------
;;; Question 5

; 5.1: No, not when L1 is shorter than L2

; 5.2: Yes, because (car (split L)) is the [even-indexed elements of L]
; 	and (cadr (split L)) is the [odd-indexed elements of L].
;		Therefore, (mix (cadr (split L)) (car (split L))) is essentially
;		(mix [odd-indexed elements of L] [even-indexed elements of L])
;		and mix by its definition will return the first even-indexed element (L[0])
;		followed by the first even-indexed element (L[1]) until both lists are
;		exhausted. In other words, that mix call returns (L[0] L[1] ... L[n-1] L[n])
;		where n is the number of elements in L.

;;; ----------
;;; Question 6

; http://www.geeksforgeeks.org/dynamic-programming-subset-sum-problem/

; isSubsetSum(set, n, sum) = isSubsetSum(set, n-1, sum) ||
;                            isSubsetSum(set, n-1, sum - set[n-1])
; Base Cases:
; isSubsetSum(set, n, sum) = false, if sum > 0 and n == 0
; isSubsetSum(set, n, sum) = true, if sum == 0

(defun is-subset-sum (S L)
  "TODO: DOCUMENTATION"

  (cond
    ;; Base Cases
    ((and (> S 0) (null L)) nil) ; false, if sum > 0 and n == 0
    ((= S 0) T) ; true, if sum == 0
    ((> (car L) S) (is-subset-sum S (cdr L))) ; number too big

    (t
      (or
        (is-subset-sum
          S
          (cdr L))
        (is-subset-sum
          (- S (car L))
          (cdr L))))))

(defun subsetsum (S L)
  (is-subset-sum S (sort L #'>)))
