(in-package :objective-lisp)

(defmacro with-gensyms (syms &body body)
  "Helps avoid variable capture in macro definitions."
  `(let ,(loop for s in syms collect `(,s (gensym)))
     ,@body))

(defmacro unless-empty (seq &body body)
  (with-gensyms (s)
    `(let ((,s ,seq))
       (if (emptyp ,s) ,s
	   (progn ,@body)))))

(defun list-all-symbols (&optional package)
  (let ((lst ())
        (package (find-package package)))
    (if package
	(do-symbols (s (find-package package)) (push s lst))
	(do-all-symbols (s lst) (push s lst)))
    lst))