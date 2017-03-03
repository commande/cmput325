;;;; 1255891.lisp
;;;; Deliverable for Assignment 2

;;;; Brett Commandeur
;;;; 1255891
;;;; CMPUT 325 Wi17 LEC B1 - NON-PROCEDURAL prog LANGUAGES Combined LAB LEC Wi17

(defun fl-interp (expr prog)
  (if (atom expr) expr ; simply atom or nil
    (let ((func (car expr)) (args (cdr expr)))
      (cond
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;; Primitive Functions ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;; (first x)
        (
          (eq func 'first)
          (car (fl-interp (car args) prog)))

        ;;; (rest x)
        (
          (eq func 'rest)
          (cdr (fl-interp (car args) prog)))

        ;;; (if x y z)
        (
          (eq func 'if)
          (if (fl-interp (car args) prog)
            (fl-interp (cadr args) prog)
            (fl-interp (caddr args) prog)))


        ;;; (null x)
        ;;; (atom x)
        ;;; (eq x y)

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;; User Defined Funcitons ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;; evaluate the arguments
        ;; apply f to the evaluated arguments (applicative order reduction)

        ;; context: single list with (n . v) pairs for locality of access

        ;; f is undefined, expr is returned as if it is quoted in lisp
        (t expr)))))

(defun fl-eval (expr ctx)
  nil)
