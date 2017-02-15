;;;; 1255891.lisp
;;;; Deliverable for Assignment 2

;;;; Brett Commandeur
;;;; 1255891
;;;; CMPUT 325 Wi17 LEC B1 - NON-PROCEDURAL PROG LANGUAGES Combined LAB LEC Wi17

(defun fl-interp (Expr Prog)
  (if ((atom Expr) Expr) ; this includes the case where expr is nil
    (let ( (func (car Expr))  (args (cdr Expr)))
      (cond
        ;;; Built in Functions

        ;; First
        ((eq func 'first
          (car (fl-interp (car args) Prog))))

        ;; CASE: f is a user-defined function --> evaluate the arguments and
          ;; apply f to the evaluated arguments (applicative order reduction)


        ;; f is undefined --> E is returned as if it is quoted in lisp
        (t (E))))))
