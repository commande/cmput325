;;;; 1255891.lisp
;;;; Deliverable for Assignment 2

;;;; Brett Commandeur
;;;; 1255891
;;;; CMPUT 325 Wi17 LEC B1 - NON-PROCEDURAL prog LANGUAGES Combined LAB LEC Wi17

(defun fl-interp (expr prog)
  (let*
    (
      (func (car expr))
      (args (cdr expr))
      (e1 (car args))
      (e2 (cadr args))
      (e3 (caddr args)))

    (if
      ;; Atoms and Nil
      (atom expr) expr ; simply return immediately as is

        ;; Functions
        (cond

          ;; One Argument Primitives
          (
            (xmember func '(null atom not first rest)) ; function names
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
            (xmember func '(cons eq equal + - * > < = and or))
            (let ; evaluate the arguments
              (
                (ev-e1 (fl-interp e1 prog))
                (ev-e2 (fl-interp e2 prog)))
              (cond                                     ; expected functions:
                ((eq func 'null) (null ev-e1))          ; (null x)
                ((eq func 'atom) (atom ev-e1))          ; (atom x)
                ((eq func 'isnumber) (numberp ev-e1))   ; (isnumber x)
                ((eq func 'not) (not ev-e1))            ; (not x)
                ((eq func 'first) (car ev-e1))          ; (first x)
                ((eq func 'rest) (cdr ev-e1))))))       ; (rest x)


      ;;; (cons x y)
      (
        (eq func 'cons)
        (cons (fl-interp e1 prog)))

      ;;; (eq x y)
      (
        (eq func 'eq)
        (eq (fl-interp e1 prog) (fl-interp e2 prog)))

      ;;; (equal x y)
      (
        (eq func 'equal)
        (equal (fl-interp e1 prog) (fl-interp e2 prog)))

      ;;; (+ x y)
      (
        (eq func '+)
        (+ (fl-interp e1 prog) (fl-interp e2 prog)))

      ;;; (- x y)
      (
        (eq func 'equal)
        (equal (fl-interp e1 prog) (fl-interp e2 prog)))

      ;;; (* x y)

      ;;; (> x y)

      ;;; (< x y)

      ;;; (= x y)

      ;;; (and x y)

      ;;; (or x y)

      ;;; (if x y z)
      (
        (eq func 'if)
        (if (fl-interp (car args) prog)
          (fl-interp (cadr args) prog)
          (fl-interp (caddr args) prog))

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;; User Defined Funcitons ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;; evaluate the arguments
        ;; apply f to the evaluated arguments (applicative order reduction)

        ;; context: single list with (n . v) pairs for locality of access

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;; Undefined Funcitons ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (t expr)))))

(defun xmember (X Y)
  "Returns t if argument Y is a member of the argument list X and nil otherwise."

  (cond
    ((null X) nil) ; list is empty (fail)
    ((equal (car X) Y) t) ; list contains Y (success)
    ((null (cdr X)) nil) ; end of list (fail)
    (t (xmember (cdr X) Y)))) ; test remaining elements for match
