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