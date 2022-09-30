
;; -----------------------------------------------------------------------------
;;                          ~/lisp/javascript.lisp
;; -----------------------------------------------------------------------------
;;
;; simple translator from subset of common lisp to javascript 
;;
;; mainly so i dont need to write so much javascript infix code
;;
;; -----------------------------------------------------------------------------
;; many pass conversion
;;
;; 1st pass - convert basic types
;;
;; integer
;; string
;; symbol  variables ...
;;
;; typical infix operators such as + 
;;        (+ 2 3)     becomes  2 + 3  rather than +(2,3)
;;
;;  (return 3)     becomes return 3;  rather than return(3)
;;
;;  (break)                           break;
;;  (continue)                        continue;  
;;
;;  quick hack
;;
;;  "use strict";
;; 
;;  (if a b)       translates to stringified :   if (a) { b }
;;                                 
;;  (def f 3)                let f = 3
;;  
;;
;; 
;; -----------------------------------------------------------------------------
;;


;; (convert-if '(if a))


;; if translation

;; (if condition action else-action)


;; if (condition) {
;;   action
;; }
;; else {
;;   else-action
;; }


;; write a simple lisp interpreter in javascript
;; then done with javascript
;; just write lisp code
;; be similar to common lisp as no tail call optimisation
;;

;; tokeniser
;; parser
;; evaluator
;;
;; some way to manipulate the common lisp reader so it preserves the symbol case
;; or even turns it into an object like fake-symbol
;;

(defpackage "PS-TUTORIAL"
  ;;(:use "COMMON-LISP" "HUNCHENTOOT" "CL-WHO" "PARENSCRIPT" "CL-FAD")
  (:use "COMMON-LISP" "PARENSCRIPT"))

(in-package "PS-TUTORIAL")

;; (if)
;; ()
;; (if a)
;; (if a b)
;; (if a b c)
;; simple translator

;; conditional if expression
;;
(defun arg-1(exp) (second exp))

(defun arg-2(exp) (third exp))

(defun arg-3(exp) (fourth exp))


;; conversion phase will read the s expression and build up some knowledge about the
;; expression ...

(defun js-string(exp) (cons 'js-string exp))
(defun js-stringp(exp) (and (consp exp) (eq (car exp) 'js-string)))

(defun js-apply(exp) (cons 'js-apply exp))
(defun js-applyp(exp) (and (consp exp) (eq (car exp) 'js-apply)))

(defun js-definition(exp) (cons 'js-definition exp))
(defun js-definitionp(exp) (and (consp exp) (eq (car exp) 'js-definition)))

;; local variable declarations
(defun js-let(exp) (cons 'js-let exp))
(defun js-letp(exp) (and (consp exp) (eq (car exp) 'js-let)))

;; argument lists
(defun js-arglist-nil() (cons 'js-arglist-nil nil))
(defun js-arglist-nilp(exp) (and (consp exp) (eq (car exp) 'js-arglist-nil)))

(defun js-arglist(exp) (cons 'js-arglist exp))
(defun js-arglistp(exp) (and (consp exp) (eq (car exp) 'js-arglist)))

;; integers small presumably
(defun js-integer(exp) (cons 'js-integer exp))
(defun js-integerp(exp) (and (consp exp) (eq (car exp) 'js-integer)))

(defun js-seq(exp) (cons 'js-seq exp))
(defun js-seqp(exp) (and (consp exp) (eq (car exp) 'js-seq)))

(defun js-symbol(exp) (cons 'js-symbol exp))
(defun js-symbolp(exp) (and (consp exp) (eq (car exp) 'js-symbol)))
(defun js-symbol-nilp(exp) (and (consp exp) (eq (car exp) 'js-symbol) (eq (cdr exp) nil)))



(defun js-if(exp) (cons 'js-if exp))
(defun js-if2(a b) (list 'js-if a b))
(defun js-if3(a b c) (list 'js-if a b c))

(defun js-ifp(exp) (and (consp exp) (eq (car exp) 'js-if)))

(defun js-or(exp) (cons 'js-or exp))
(defun js-orp(exp) (and (consp exp) (eq (car exp) 'js-or)))


;; convert-if : sexp -> string 
(defun convert-if(exp)
  (cond
    ((and (consp exp) (= (length exp) 3))
     (let ((a (arg-1 exp)) (b (arg-2 exp)))
       (js-if2 (convert a) (convert b))))
    ((and (consp exp) (= (length exp) 4))
     (let ((a (arg-1 exp)) (b (arg-2 exp)) (c (arg-3 exp)))
       (js-if3 (convert a) (convert b) (convert c))))
    (t (error (list exp "convert-if now what")))))


(defun display-if(exp)
  ;;(format t "display-if : ~a~%" exp)
  (cond
    ((and (consp exp) (= (length exp) 3))
     (let ((a (arg-1 exp)) (b (arg-2 exp)))
       (format nil "if (~a){ ~a }~%" (display a) (display b))))
    ((and (consp exp) (= (length exp) 4))
     (let ((a (arg-1 exp)) (b (arg-2 exp)) (c (arg-3 exp)))
       (format nil "if (~a){ ~a }~%else{ ~a }" (display a) (display b) (display c))))
    (t (error (list exp "display-if now what")))))
  

;; ------------------------------------
;;   fn   anonymous function  \lambda
;;   equivalent
;; ------------------------------------

;;(defun display-js-arglist-nil() ... )

;; sequence seq






;; =========================================================
;; (let a b c d e f)
;; let a = b , c =d , e =f  ;
;; let (a 1 b 2 c 3) a b c
(defun convert-let(exp)
  (cons 'js-let (cons (mapcar #'convert (car (cdr exp)))
		      (mapcar #'convert (cdr (cdr exp))))))







  ;; ;;(format t "convert-let : ~a~%" exp)
  ;; (cond
  ;;   ((and (consp exp) (= (length exp) 3))
  ;;    (let ((a (arg-1 exp)) (b (arg-2 exp)))
  ;;      (js-if2 (convert a) (convert b))))
  ;;   ((and (consp exp) (= (length exp) 4))
  ;;    (let ((a (arg-1 exp)) (b (arg-2 exp)) (c (arg-3 exp)))
  ;;      (js-if3 (convert a) (convert b) (convert c))))
  ;;   (t (error (list exp "convert-let now what")))))


(defun display-let(exp)
  ;;(format t "display-let : ~a~%" exp)
  (cond
    ((and (consp exp) (>= (length exp) 2))
     (let ((exprs (car (cdr exp))))
       (format nil "let ~a ~% ~a~%" 
	     (let ((rest-args exprs)
		   (rest-str ""))
	       (loop while (not (null rest-args)) do
		 (let ((tmp (if
			     (null (cdr (cdr rest-args)))
			     (format nil "~a = ~a;"
				     (display (car rest-args))
				     (display (car (cdr rest-args))))
			     (format nil "~a = ~a ,"
				     (display (car rest-args))
				     (display (car (cdr rest-args)))))))
		   (setq rest-str (concatenate 'string rest-str tmp))
		   (setq rest-args (cdr (cdr rest-args)))))
	       rest-str)
	     
	     ;; progn (cdr (cdr exp))
	     (let ((rest-args (cdr(cdr exp)))
		   (rest-str ""))
	       (dolist (p rest-args)
		 (let ((tmp (format nil "~a;~%" (display p))))
		   (setq rest-str (concatenate 'string rest-str tmp))))
	       rest-str))))

    (t (error (list exp "display-let now what")))))




;; =========================================================




;; -----------------------------------
;;  (def f 3)
;;  (def f a 1 2 3)
;;  (def f (a) 1 2 3)
;;  (def f (a b c) 1 2 3 4 5)
;; -----------------------------------
(defun convert-definition(exp)
  (cond
    ;; (def f 3)
    ((and (= (length exp) 3) (symbolp (arg-1 exp)))
     (list 'js-definition (convert (arg-1 exp)) (convert (arg-2 exp))))
    
    ;; (def f () 3 ...)
    ((and (>= (length exp) 3) (null (arg-2 exp))) 
     (cons 'js-definition (cons (convert (arg-1 exp))
	   (cons (js-arglist-nil)
		 (mapcar #'convert (cdr (cdr (cdr exp))))))))

    ;; (def f (a...) 3 ...)
    ((and (>= (length exp) 3) (consp (arg-2 exp))) 
     (cons 'js-definition (cons
			   (convert (arg-1 exp))
			   (cons (cons 'js-arglist (mapcar #'convert (arg-2 exp)))
		 (mapcar #'convert (cdr (cdr (cdr exp))))))))
    (t 
     (js-definition (mapcar #'convert (cdr exp))))))


;;
;; (js-definition (js-symbol . f) (js-integer 3))     :  const f = 3
;;
;; (def f () 4 5 6)    :  const f = () => { 4 5 6 }
(defun display-definition(exp)
  (format t "display-definition : ~a~%" exp)
  (cond
    ((and (consp exp) (= (length exp) 3))
     (let ((a (arg-1 exp)) (b (arg-2 exp)))
       (format nil "const ~a = ~a;~%" (display a) (display b))))
    ((and (consp exp) (>= (length exp) 4))
     (let ((a (arg-1 exp)) (b (arg-2 exp))(c (arg-3 exp)))
       (cond
	 ((js-arglist-nilp b)
	  (format nil "const ~a = () => { ~a };~%" (display a)
		  (let ((rest-args (cdr(cdr(cdr exp))))
			(rest-str ""))
		    (dolist (p rest-args)
		      (let ((tmp (format nil "~a;~%" (display p))))
		        (setq rest-str (concatenate 'string rest-str tmp))))
		    rest-str)))
	 ((js-symbolp b)
	  (format nil "const ~a = (~a) => { ~a };~%" (display a) (display b)
		  (let ((rest-args (cdr(cdr(cdr exp))))
			(rest-str ""))
		    (dolist (p rest-args)
		      (let ((tmp (format nil "~a;~%" (display p))))
		        (setq rest-str (concatenate 'string rest-str tmp))))
		    rest-str)))
	 (t
	  (format nil "const.sanity ~a = (~a) => { ~a };~%" (display a) (display b)
		  (let ((rest-args (cdr(cdr(cdr exp))))
			(rest-str ""))
		    (dolist (p rest-args)
		      (let ((tmp (format nil "~a;~%" (display p))))
		        (setq rest-str (concatenate 'string rest-str tmp))))
		    rest-str))))))
    (t (error (list exp "display-apply now what")))))



;;=========== sequence seq ======================

(defun convert-seq(exp)
  (cons 'js-seq (mapcar #'convert (cdr exp))))



(defun display-seq(exp)
  ;;(format t "seq = ~a~%" exp)  
  (format nil "~a"
	  (let ((rest-args (cdr exp))
		(rest-str ""))
	    (dolist (p rest-args)
	      (let ((tmp (format nil "~a;~%" (display p))))
		(setq rest-str (concatenate 'string rest-str tmp))))
	    rest-str)))



;; ================================================




(defun convert-apply(exp)
  (js-apply (mapcar #'convert exp)))


(defun display-apply(exp)
  (format t "display-apply : ~a~%" exp)
  (cond
    ((and (consp exp) (= (length exp) 2))
     (let ((a (arg-1 exp)))
       (format nil "~a()" (display a))))
    
    ;; ((and (consp exp) (= (length exp) 3))
    ;;  (let ((a (arg-1 exp)) (b (arg-2 exp)))
    ;;    (format nil "~a(~a)" (display a) (display b))))
    ;; ((and (consp exp) (= (length exp) 4))
    ;;  (let ((a (arg-1 exp)) (b (arg-2 exp))(c (arg-3 exp)))
    ;;    (format nil "~a(~a,~a)" (display a) (display b)(display c))))
    ((and (consp exp) (> (length exp) 2))
     (format nil "~a(~a)" (display (arg-1 exp)) 
	     (let ((rest-args (cdr (cdr exp)))
		   (rest-str ""))
	       (loop while (not (null rest-args)) do
		 (let ((tmp (if
			     (null (cdr rest-args))
			     (format nil "~a" (display (car rest-args)))
			     (format nil "~a," (display (car rest-args))))))
		   (setq rest-str (concatenate 'string rest-str tmp))
		   (setq rest-args (cdr rest-args))))
	       rest-str)))
    (t (error (list exp "display-apply now what")))))




  


(defun convert(exp)
  (cond
    ((stringp exp) (js-string exp))
    ((integerp exp) (js-integer exp))
    ((symbolp exp) (js-symbol exp))
    ((and (consp exp) (eq (car exp) 'if) (or (= (length exp) 3) (= (length exp) 4)))
     (convert-if exp))

    ;; sequence seq  ... aka progn , begin 
    ((and (consp exp)
	  (eq (car exp) 'seq))
     (convert-seq exp))
    
    ;; (let a 1 b 2 c 3)
    ;; let a = 1 let b = 2 let c = 3  or let a = 1 , b = 2 ,c=3
    ;; matching number of 
    ((and (consp exp)
	  (eq (car exp) 'let)
	  (>= (length exp) 2)
	  (zerop (mod (length (car (cdr exp))) 2)))
     (convert-let exp))

    ((and (consp exp) (eq (car exp) 'def) (>= (length exp) 3))
     (convert-definition exp))
    ((consp exp)
     (convert-apply exp))
    (t (error (list exp "convert: exp not recognised as string,int,symbol or list")))))



(defun display(exp)
  (cond
    ;; strings
    ((js-stringp exp) (format nil "\"~a\"" (cdr exp)))
    ;; integers
    ((js-integerp exp) (format nil "~a" (cdr exp)))
    ;; symbols
    ((js-symbolp exp) (format nil "~a" (cdr exp)))
    ;; argument lists
    ((js-arglist-nilp exp) (format nil "()"))
    ((js-arglistp exp) (let ((rest-args (cdr exp))
			     (rest-str ""))
			 (loop while (not (null rest-args)) do
			   (let ((tmp (if
				       (null (cdr rest-args))
				       (format nil "~a" (display (car rest-args)))
				       (format nil "~a," (display (car rest-args))))))
		             (setq rest-str (concatenate 'string rest-str tmp))
			     (setq rest-args (cdr rest-args))))
			 rest-str))
    ;; conditionals
    ((js-ifp exp) (display-if exp))
    ;; let local definition
    ((js-letp exp) (display-let exp))
    ;; seq sequence
    ((js-seqp exp) (display-seq exp))
    ;; definition
    ((js-definitionp exp)
     (display-definition exp))
    ;; application
    ((js-applyp exp)
     (display-apply exp))
    ;; hands up in the air
    (t (error (list exp "display: exp not recognised as string,int,symbol or list")))))




(convert '(if a b))

(convert '(if a b c))

(convert '(> 2 3))

(convert '(def f 3))

(display (convert '(def f 3)))

(format t (display (convert '(def f 3))))

(display (convert '(> 2 3)))

(display (convert '(if a b)))

(display(convert '(if a b c)))

(format t (display(convert '(if a b))))

(format t (display(convert '(if a b c))))

(format t (display(convert '(> a b))))

(format t (display(convert '(cons 3 (add a b)))))

(format t (display(convert '(def f (x) (+ x 1)))))

(display (convert '(if a b)))

(display (convert '(if a b c)))

(display (convert '(if a b (if c d e))))

(display (convert '(if a b (if c d (if e f g)))))

(display (convert '(if a b (if c d (if e f (if g h i ))))))









