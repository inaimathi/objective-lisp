
;;;; package.lisp

(defpackage #:objective-lisp
  (:nicknames #:ol)
  (:use #:cl)
  (:shadow #:+ #:- #:* #:/ #:<= #:>= #:> #:<
           #:member #:null
           #:= #:string= #:eq #:eql #:equal #:equalp #:/=
           #:length #:map #:mapcar #:concatenate
	   #:first #:last #:rest #:nth)
  (:export #:+ #:- #:* #:/ #:<= #:>= #:> #:<
	   #:false #:true
           #:memberp #:nullp
           #:= #:string= #:eq #:eql #:equal #:equalp #:/=
           #:length #:map #:mapcar #:concatenate #:loop
	   #:first #:last #:init #:rest #:nth
	   #:setf

	   #:list #:vector #:hash #:nil

	   ;;; Introspection
	   #:in-package #:describe #:documentation #:apropos-list #:list-all-packages #:list-all-symbols

	   ;;; General utility
	   #:with-gensyms

	   ;;; All the defs
	   #:lambda #:defclass #:defconstant #:defgeneric #:define-compiler-macro 
	   #:define-condition #:define-method-combination #:define-modify-macro 
	   #:define-setf-expander #:define-symbol-macro #:defmacro #:defmethod #:defpackage #:defparameter 
	   #:defsetf #:defstruct #:deftype #:defun #:defvar
	   
	   ;;; All the dynamic goodnes
	   #:funcall #:apply #:eval))