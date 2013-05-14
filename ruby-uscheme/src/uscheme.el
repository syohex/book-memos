(defvar primitive-functions
  '((:+ . (:prim . (lambda (x y) (+ x y))))
    (:- . (:prim . (lambda (x y) (- x y))))
    (:* . (:prim . (lambda (x y) (* x y))))))

(defun _eval (exp)
  (if (not (listp exp))
      (if (immediate-value-p exp)
          exp
        (lookup-primitive-function exp))
    (let ((fun (_eval (car exp)))
          (args (eval-list (cdr exp))))
      (_apply fun args))))

(defun eval-list (exp)
  (mapcar (lambda (e) (_eval e)) exp))

(defun lookup-primitive-function (exp)
  (assoc-default exp primitive-functions))

(defun immediate-value-p (exp)
  (let ((type (type-of exp)))
    (or (eq type 'integer) (eq type 'float))))

(defun _apply (fun args)
  (apply-primitive-function fun args))

(defun apply-primitive-function (fun args)
  (let ((func (cdr fun)))
    (apply func args)))

(defun test ()
  (_eval '(:+ 1 2)))
