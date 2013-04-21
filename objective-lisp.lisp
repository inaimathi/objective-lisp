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
;;;                    What's a reasonable behavior here? Can we meet expectations?
(defmethod map ((fn function) (seq string) &rest strings)
  (apply #'cl:map 'string fn seq strings))
(defmethod map ((fn function) (seq hash-table) &rest tables)
  (let* ((ts (cons seq tables))
	 (res (make-hash-table :test (hash-table-test seq) :size (* (hash-table-size seq) (length ts)))))
    (loop for tbl in ts
       do (loop for k being the hash-keys of tbl
	     for v being the hash-values of tbl
	     do (multiple-value-bind (new-k new-v) (funcall fn k v)
		  (setf (gethash new-k res) new-v))))
    res))

(defmethod concatenate ((seq string) &rest strings)
  (apply #'cl:concatenate 'string (cons seq strings)))
(defmethod concatenate ((seq cons) &rest conses)
  (apply #'cl:concatenate 'cons (cons seq conses)))
(defmethod concatenate ((seq array) &rest arrays)
  (apply #'cl:concatenate 'vector (cons seq arrays)))
(defmethod concatenate ((seq hash-table) &rest tables)
  (apply #'map (lambda (k v) (values k v)) (cons seq tables)))

;;; TODO: Try to make sure that `setf`ing the results of all these functions has reasonable results. 
;;; Not sure what the reasonable behavior would be for strings and arrays
(defmethod nth ((index integer) (seq cl:null)) nil)
(defmethod nth ((index integer) (seq cons)) (cl:nth index seq))
(defmethod nth ((index integer) (seq string)) (unless-empty seq (aref seq index)))
(defmethod nth ((index integer) (seq array)) (unless-empty seq (aref seq index)))

(defmethod lookup ((k integer) (seq cl:null) &optional default) default)
(defmethod lookup ((k integer) (seq cons) &optional default) (or (cl:nth k seq) default))
(defmethod lookup ((k symbol) (seq cons) &optional default) 
  (if (keywordp k)
      (getf seq k default)
      (cdr (assoc k seq))))
(defmethod lookup ((k integer) (seq string) &optional default) (aref seq k))
(defmethod lookup ((k integer) (seq array) &optional default) (aref seq k))
(defmethod lookup (k (seq hash-table) &optional default) (gethash key seq default))

(defmethod first (seq) 
  (unless-empty seq (nth 0 seq)))
;;; TODO: hash?

(defmethod rest (seq) 
  (unless-empty seq (subseq seq 1)))
;;; TODO: hash?

(defmethod init ((seq list)) (cl:butlast seq))
(defmethod init ((seq string)) (subseq seq 0 (max 0 (- (length seq) 1))))
(defmethod init ((seq array)) (subseq seq 0 (max 0 (- (length seq) 1))))
;;; TODO: hash?

(defmethod last ((seq list)) (cl:last seq))
(defmethod last ((seq string))
  (unless-empty seq 
    (let ((len (length seq)))
      (subseq seq (max 0 (- len 1)) len))))
(defmethod last ((seq array))
  (unless-empty seq
    (let ((len (length seq)))
      (subseq seq (max 0 (- len 1)) len))))
;;; TODO: hash?

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

;;;;;;;;;; Hash table support attempt
;;; TODO De-stub this hash macro. It should deal with literals and return a new hash table
(defmacro hash ((&key test size rehash-size rehash-threshold hash-function weakness synchronized) &rest k/v-pairs)
  nil)