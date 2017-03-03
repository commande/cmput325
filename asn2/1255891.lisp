;;;; 1255891.lisp
;;;; Deliverable for Assignment 2
;;;; CMPUT 325 Wi17 LEC B1
;;;; Brett Commandeur

(defun fl-interp (expr prog)
  " Interprets FL expression with given program of user-defined functions "

  ;; Interpret atoms [including nil]
  (if
    (atom expr) expr ; simply return immediately

    ;; Functions
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

        ;; context: single list with (n . v) pairs for locality of access

        ;; Undefined Functions
        (t expr)))))

(defun xmember (X Y)
  "Returns t if argument X is a member of the argument list Y and nil otherwise."

  (cond
    ((null Y) nil) ; list is empty (fail)
    ((equal (car Y) X) t) ; list contains Y (success)
    ((null (cdr Y)) nil) ; end of list (fail)
    (t (xmember X (cdr Y))))) ; test remaining elements for match
