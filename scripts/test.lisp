(defun hello-world ()
  (format t "Hello, worlds!!"))
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))
(defun make-comparisons-list (fields)
  (loop while fields ;;seems that collecting only works within a loop construct. 
     collecting (make-comparison-expr (pop fields) (pop fields))))
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))
; make a function for doing the jumble newspaper puzzle.
(defun read-dict (fid pzl)
	"a function for doing the jumble newspaper puzzle."
	(loop for line = (read-line fid nil)
			 (loop for c across line

(defun copy-til (g g2 stop)
	(if (eql (length g) 0)
			g2
			(if (char= (car g) stop) 
					g2 
					(copy-til (rest g) (push (car g) g2) stop))))

(defun list-eql (l1 l2)
	(defun zlen (l3)
		(eql (length l3) 0))
	(if (and (zlen l1) (zlen l2))
			t
			(let ((a1 (pop l1))
						 (a2 (pop l2)))
				 (if (eql a1 a2)
						 (list-eql (rest l1) (rest l2))
						 ()))))

(defun check-jumble (pzl g)
	(let* ((p (sort (coerce pzl 'list) 'char-lessp))
				 (g2 (copy-til (coerce g 'list) () #\/))
				 (g3 (sort g2 'char-lessp)))
		(format t "p ~a~%" p)
		(format t "g3 ~a~%" g3)
		;; solution found if p eq g3
		(list-eql p g3)))

;; generate a random lambda fn using macros? 
;(defun mk-expr ()
(defun rnd-thing (things)
	(let ((x (random (length things))))
		(nth x things)))
		
(defmacro bogus (funame) ;; you can then call the function by name..
;;	(let ((body '(,(rnd-thing '(a b c d)) ,(rnd-thing '(+ - / *)) ,(rnd-thing '(a b c d)))))
;; later, it occurs to me that you can get the same effect with eval()
	(let ((p1 (gensym)))
		(format t "p1 ~a~%" p1) ;;
	`(defun ,funame (,p1) (,(rnd-thing '(+ - * /)) ,p1 2))))
;; okay, got the syntax working for this. 
;; now what -- need targeted modification of a list? 
;; code that can take the partial derivative of other code? yea. 
;; otherwise we have to represent the code as something other than s-expr
;; which would suck and basically bring us back to the lua solution. 
;; (basically need compression as well as expansion)

(defun traverse (body from to)
;; this is not at all the most efficient way of doing things .. still learning!
			(format t "body ~a ~%" body)
			(if (atom body)
					(progn
						(format t "atom ~a~%" body)
						(if (eql body from)
								(setf body to)))
					(progn (traverse (first body) from to)
								 (let ((r (rest body)))
									 (if r (traverse r from to))))))

(defun list-of-symbols-p (x)
  (and (listp x)
       (every #'symbolp x)))

;; really want a multiple-value let. 
;; something like 
(mlet a b c d (list 1 2 3 4) body)
;; which expands to
(let ((p (gensym)))
			`(let* ((,p (list 1 2 3 4))
							(,(nth 0 args) (nth 0 ,p))
							(,(nth 1 args) (nth 1 ,p))
							(,(nth 2 args) (nth 2 ,p))
							(,(nth 3 args) (nth 3 ,p)))
				 body))
			('p (list 1 2 3 4))
			(a (nth 'p
;; (don't want to do multiple values .. not clear how that's used.)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro mlet (params val &body body)
;; lists are fundamental, and much easier to manipulate than multiple value 
;; returns.  hence, rather than having a function return (values 1 2)
;; use (list 1 2) , which mlet will allow you to assign to new variables. 
;; this is very much canonical Ocaml - perhaps there is a more lispy way of proceeding, 
;; but i don't know how (now)
;; example:
;; (mlet (a b c) (list 11 12 13) (format t "a:~a b:~a c:~a ~%" a b c))
	(with-gensyms (l)
		`(let* ((,l ,val)
						,@(loop for p in params collect `(,p (pop ,l))))
			 ,@body)))

(defun tree-count-instance (body y)
	;; iterate through a tree, count the instances that match y
	;; comparison function is tree-equal
	(if (tree-equal body y)
			1
			(if (consp body)
					(+ (tree-count-instance (car body) y) (tree-count-instance (cdr body) y))
					0)))

(defun rand (a)
	(if (< a 1) 0 (rand a)))

(defun mutate (body from to)
	;;change a random instance of from to to in the tree body.
	;;works on a copy of the tree.
	(let* ((n (tree-count-instance body from))
				 (r (if (> n 0) (random n) 0))
				 (c 0)) ;; only change the instance if c == r
		(defun tate (b)
			(let ((fnd (tree-equal b from)))
				(when fnd (incf c))
				(if (and (= c (+ r 1)) fnd)
							to
						(if (consp b)
								(cons (tate (car b)) (tate (cdr b)))
								b))))
		(if (> n 0)
				(tate body)
				())))
;;alright, this works ... need to close the loop on the eval function though. 
;;for that, I need documentation -- aka the internet! 
;; or generate test-funs myself .. - echt, that's a lot of work.

(defun traverse2 (body from to found)
;; take a tree, modify one random instance of 'from'
;; return a duplicate of the tree.
	(mlet (nu fnd) (if (and (tree-equal body from) (not found))
										(list to t)
										(list body nil))
		(format t "body: ~a fnd:~a ~%" body fnd)
		(if (consp body)
				(cons (traverse2 (car body) from to fnd) (traverse2 (cdr body) from to fnd))
				nu)))
		 
(defmacro mutate (bodd)
	;; why do we need to write this as a macro? 
	;; just need a function that converts a list to a function. 
	;; anyway, only want to mutate one operation. 
	

;;for computing fibonacci numbers. do loop example
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))

;; from chapter 9 of gigamonkeys
(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

Conrad Barski - Land of Lisp
