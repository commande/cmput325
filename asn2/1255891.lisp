;;;; 1255891.lisp
;;;; Deliverable for Assignment 2
;;;; CMPUT 325 Wi17 LEC B1
;;;; Brett Commandeur

(defun fl-interp (expr prog)
  " Interprets FL expression with given program of user-defined functions "

  ;; Interpret atoms [including nil]
  (if
    (atom expr) expr ; simply return immediately

    ;; Interpret Functions
    (let*
      (
        (func (car expr))   ; function name
        (args (cdr expr))   ; function arguments
        (e1 (car args))     ; first arg (for primitives)
        (e2 (cadr args))    ; second "
        (e3 (caddr args)))  ; third "

      (cond

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;; Primitive Functions ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;; One Argument Primitives
        (
          (xmember func '(null atom isnumber not first rest)) ; function names
          (let ; evaluate the argument
            (
              (ev-e1 (fl-interp e1 prog)))
            (cond                                   ; expected functions:
              ((eq func 'null) (null ev-e1))          ; (null x)
              ((eq func 'atom) (atom ev-e1))          ; (atom x)
              ((eq func 'isnumber) (numberp ev-e1))   ; (isnumber x)
              ((eq func 'not) (not ev-e1))            ; (not x)
              ((eq func 'first) (car ev-e1))          ; (first x)
              ((eq func 'rest) (cdr ev-e1)))))        ; (rest x)

        ;; Two Argument Primitives
        (
          (xmember func '(cons eq equal + - * > < = and or)) ; function names
          (let ; evaluate the arguments
            (
              (ev-e1 (fl-interp e1 prog))
              (ev-e2 (fl-interp e2 prog)))
            (cond                                     ; expected functions:
              ((eq func 'cons) (cons ev-e1 ev-e2))    ; (cons x y)
              ((eq func 'eq) (eq ev-e1 ev-e2))        ; (eq x y)
              ((eq func 'equal) (equal ev-e1 ev-e2))  ; (equal x y)
              ((eq func '+) (+ ev-e1 ev-e2))          ; (+ x y)
              ((eq func '-) (- ev-e1 ev-e2))          ; (- x y)
              ((eq func '*) (* ev-e1 ev-e2))          ; (* x y)
              ((eq func '/) (/ ev-e1 ev-e2))          ; (/ x y)
              ((eq func '>) (> ev-e1 ev-e2))          ; (> x y)
              ((eq func '<) (< ev-e1 ev-e2))          ; (< x y)
              ((eq func '=) (= ev-e1 ev-e2))          ; (= x y)
              ((eq func 'and) (and ev-e1 ev-e2))      ; (and x y)
              ((eq func 'or) (or ev-e1 ev-e2)))))    ; (or x y)

        ;; Three Argument Primitives
        (
          (eq func 'if)
          (if (fl-interp e1 prog)
            (fl-interp e2 prog)
            (fl-interp e3 prog)))

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;; User Defined Funcitons ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; evaluate the arguments
        ;; apply f to the evaluated arguments (applicative order reduction)

        (
          (user-defined func prog)
          (let )

        ;; Undefined Functions
        (t expr)))))

(defun xmember (X Y)
  "Returns t if X is a member of the list Y and nil otherwise."

  (cond
    ((null Y) nil) ; list is empty (fail)
    ((equal (car Y) X) (expected-args)) ; list contains Y (success)
    ((null (cdr Y)) nil) ; end of list (fail)
    (t (xmember X (cdr Y))))) ; test remaining elements for match

(defun user-defined (name prog)
  "Returns number of expected args for function [name] if it is defined in prog and nil otherwise"

  (cond
    ( ; prog is empty, fail
      (null prog) nil)
    ( ; first function name in prog matches, success
      (equal (caar prog) name) (expected-args (caar prog) nil))
    ( ; end of list, fail
      (null (cdr prog)) nil)
    (t ; recursion
      (user-defined name (cdr prog))))) ; test remaining definitions for match

(defun expected-args (def ret)
  "Accumulates number of expected args in ret for function definition [def] in prog and nil if unable to find"

  (cond
    ( ; def is empty, fail
      (def) nil)
    ( ; end of param list found (as marked by =), return count
      (equal (car def) '=) ret)
    ( ; end of list, fail
      (null (cdr def)) nil)
    (t ; recursion
      (if (null ret)
        (expected-args (cdr def) 0) ; skip first element (function name)
        (expected-args (cdr def) (+ ret 1)))))) ; count each following element
