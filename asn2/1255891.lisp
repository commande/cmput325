;;;; 1255891.lisp
;;;; Deliverable for Assignment 2

;;;; Brett Commandeur
;;;; 1255891
;;;; CMPUT 325 Wi17 LEC B1 - NON-PROCEDURAL prog LANGUAGES Combined LAB LEC Wi17

(defun fl-interp (expr prog)
  (if (atom expr) expr ; this includes the case where expr is nil
    (let ((func (car expr)) (args (cdr expr)))
      (cond
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;; Primitive Functions ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;; First
        ((eq func 'first) (car (fl-interp (car args) prog)))
        ((eq func 'rest) (cdr (fl-interp (car args) prog)))

        ;; CASE: f is a user-defined function --> evaluate the arguments and
          ;; apply f to the evaluated arguments (applicative order reduction)


        ;; f is undefined --> E is returned as if it is quoted in lisp
        (t expr)))))
