;;;; objective-lisp.lisp
(in-package #:objective-lisp)

(defparameter false nil)
(defparameter true :true)

(defmethod nullp ((obj list)) (cl:null obj))
(defmethod nullp ((obj string)) (= "" obj))
(defmethod nullp ((obj array)) (= (vector) obj))

(defgeneric memberp (object collection &key key)
  (:documentation "Checks if [object] is a member of [collection]. Searches by [key] if passed in."))

(defmethod memberp (item (collection list) &key key)
  (cl:member item list :key key :test #'=))

;;;;;;;;;; Arithmetic primitives
(defmethod + ((num number) &rest nums)
  (apply #'cl:+ (cons num nums)))

(defmethod - ((num number) &rest nums)
  (apply #'cl:- (cons num nums)))

(defmethod * ((num number) &rest nums)
  (apply #'cl:* (cons num nums)))

(defmethod / ((num number) &rest nums)
  (apply #'cl:/ (cons num nums)))

;;;;;;;;;; Sequence primitives
(defmethod length (seq)
  (cl:length seq))

(defmethod map ((fn function) (l list) &rest lists)
  (apply #'cl:mapcar fn (cons l lists)))

(defmethod concatenate ((str string) &rest strings)
  (apply #'cl:concatenate 'string (cons  str strings)))

(defmethod first ((seq cons)) (cl:first seq))
(defmethod first ((seq string)) (aref seq 0))
(defmethod first ((seq array)) (aref seq 0))

(defmethod rest (seq) (subseq seq 1))

(defmethod nth ((index integer) (seq cons)) (cl:nth index seq))
(defmethod nth ((index integer) (seq string)) (aref seq index))
(defmethod nth ((index integer) (seq array)) (aref seq index))
;; init
;; last

;;;;;;;;;; Comparison primitives
(defmethod = (a b) false)
(defmethod = ((a number) (b number)) (cl:= a b))
(defmethod = ((a string) (b string)) (cl:string= a b))
(defmethod = ((a character) (b character)) (cl:char= a b))
(defmethod = ((a symbol) (b symbol)) (cl:eq a b))
(defmethod = ((a cons) (b cons)) (cl:equalp a b))
(defmethod = ((a array) (b array)) (cl:equalp a b))
(defmethod = ((a structure-object) (b structure-object)) (cl:equalp a b))

(defmethod <= ((a number) (b number)) (cl:<= a b))
(defmethod <= ((a string) (b string)) (cl:string<= a b))
(defmethod <= ((a character) (b character)) (cl:char<= a b))
(defmethod <= ((a symbol) (b symbol)) (<= (symbol-name a) (symbol-name b)))
;; (defmethod <= ((a cons) (b cons)) (cl:equalp a b))
;; (defmethod <= ((a array) (b array)) (cl:equalp a b))
;; (defmethod <= ((a structure-object) (b structure-object)) (cl:equalp a b))

;; >=
;; >
;; <
;; /=

;;;;;;;;;; Introspection
(defun list-all-symbols (&optional package)
  (let ((lst ())
        (package (find-package package)))
    (if package
	(do-symbols (s (find-package package)) (push s lst))
	(do-all-symbols (s lst) (push s lst)))
    lst))