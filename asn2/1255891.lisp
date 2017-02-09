;;;; 1255891.lisp
;;;; Deliverable for Assignment 2

;;;; Brett Commandeur
;;;; 1255891
;;;; CMPUT 325 Wi17 LEC B1 - NON-PROCEDURAL PROG LANGUAGES Combined LAB LEC Wi17

(defun fl-interp (E P)
  (cond
    ((atom E) E)   ; this includes the case where expr is nil
    (t
      (let ( (f (car E))  (arg (cdr E)))
        (cond
          ;; f is a built-in function
          ((eq f 'first)  (car (fl-interp (car arg) P)))

          ;; f is a user-defined function --> evaluate the arguments and
          ;; apply f to the evaluated arguments (applicative order reduction)

          ;; f is undefined --> E is returned as if it is quoted in lisp
          (t (E)))))))
