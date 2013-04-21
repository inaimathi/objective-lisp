;;;; objective-lisp.lisp
(in-package #:objective-lisp)

(defparameter false nil)
(defparameter true :true)

(defun nullp (obj) (cl:null obj))

(defmethod emptyp ((obj list)) (nullp obj))
(defmethod emptyp ((obj string)) (= "" obj))
(defmethod emptyp ((obj array)) (= (vector) obj))

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
(defmethod map ((fn function) (seq array) &rest arrays)
  (apply #'cl:map 'vector fn seq arrays))
;;; ODD BEHAVIOR NOTE: (map #'char-code "Hello there") gives type error (the results are ints rather than chars)
(defmethod map ((fn function) (seq string) &rest strings)
  (apply #'cl:map 'string fn seq strings))
;;; hash


(defmethod concatenate ((seq string) &rest strings)
  (apply #'cl:concatenate 'string (cons seq strings)))
(defmethod concatenate ((seq cons) &rest conses)
  (apply #'cl:concatenate 'cons (cons seq conses)))
(defmethod concatenate ((seq array) &rest arrays)
  (apply #'cl:concatenate 'vector (cons seq arrays)))
;;; hash

(defmethod nth ((index integer) (seq cons)) (cl:nth index seq))
(defmethod nth ((index integer) (seq string)) (aref seq index))
(defmethod nth ((index integer) (seq array)) (aref seq index))
;;; hash?

(defmethod first (seq) 
  (unless-empty seq (nth 0 seq)))
;;; hash?

(defmethod rest (seq) 
  (unless-empty seq (subseq seq 1)))
;;; hash?

(defmethod init ((seq list)) (cl:butlast seq))
(defmethod init ((seq string)) (subseq seq 0 (max 0 (- (length seq) 1))))
(defmethod init ((seq array)) (subseq seq 0 (max 0 (- (length seq) 1))))
;;; hash?

(defmethod last ((seq list)) (cl:last seq))
(defmethod last ((seq string))
  (unless-empty seq 
    (let ((len (length seq)))
      (subseq seq (max 0 (- len 1)) len))))
(defmethod last ((seq array))
  (unless-empty seq
    (let ((len (length seq)))
      (subseq seq (max 0 (- len 1)) len))))
;;; hash?

;;;;;;;;;; Comparison primitives
(defmethod = (a b) false)
(defmethod = ((a number) (b number)) (cl:= a b))
(defmethod = ((a string) (b string)) (cl:string= a b))
(defmethod = ((a character) (b character)) (cl:char= a b))
(defmethod = ((a symbol) (b symbol)) (cl:eq a b))
(defmethod = ((a cons) (b cons)) (cl:equalp a b))
(defmethod = ((a array) (b array)) (cl:equalp a b))
(defmethod = ((a structure-object) (b structure-object)) (cl:equalp a b))

(defmethod /= (a b) true)
(defmethod /= ((a number) (b number)) (cl:/= a b))
(defmethod /= ((a string) (b string)) (cl:string/= a b))
(defmethod /= ((a character) (b character)) (cl:char/= a b))
(defmethod /= ((a symbol) (b symbol)) (/= (symbol-name a) (symbol-name b)))
(defmethod /= ((a cons) (b cons)) (not (= a b)))
(defmethod /= ((a array) (b array)) (not (= a b)))
(defmethod /= ((a structure-object) (b structure-object)) (not (= a b)))

(defmethod <= ((a number) (b number)) (cl:<= a b))
(defmethod <= ((a string) (b string)) (cl:string<= a b))
(defmethod <= ((a character) (b character)) (cl:char<= a b))
(defmethod <= ((a symbol) (b symbol)) (<= (symbol-name a) (symbol-name b)))
(defmethod <= ((a sequence) (b sequence)) (<= (first a) (first b)))

(defmethod >= ((a number) (b number)) (cl:>= a b))
(defmethod >= ((a string) (b string)) (cl:string>= a b))
(defmethod >= ((a character) (b character)) (cl:char>= a b))
(defmethod >= ((a symbol) (b symbol)) (>= (symbol-name a) (symbol-name b)))
(defmethod >= ((a sequence) (b sequence)) (>= (first a) (first b)))

(defmethod > ((a number) (b number)) (cl:> a b))
(defmethod > ((a string) (b string)) (cl:string> a b))
(defmethod > ((a character) (b character)) (cl:char> a b))
(defmethod > ((a symbol) (b symbol)) (> (symbol-name a) (symbol-name b)))
(defmethod > ((a sequence) (b sequence)) (> (first a) (first b)))

(defmethod < ((a number) (b number)) (cl:< a b))
(defmethod < ((a string) (b string)) (cl:string< a b))
(defmethod < ((a character) (b character)) (cl:char< a b))
(defmethod < ((a symbol) (b symbol)) (< (symbol-name a) (symbol-name b)))
(defmethod < ((a sequence) (b sequence)) (< (first a) (first b)))

;;;;;;;;;; Introspection
(defun list-all-symbols (&optional package)
  (let ((lst ())
        (package (find-package package)))
    (if package
	(do-symbols (s (find-package package)) (push s lst))
	(do-all-symbols (s lst) (push s lst)))
    lst))