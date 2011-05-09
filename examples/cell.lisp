(defactor cell (c) (message)
  (cond ((get? message) (send (cust message)  c))
	((set? message) (setf c (contents message))))
  next)

(defun make-set (val) `("set" ,val))
(defun make-get (cust) `("get" ,cust)) 

(defun get? (message) (equal (first message) "get"))
(defun set? (message) (equal (first message) "set"))

(defun cust (message) (second message))
(defun contents (message) (second message))
	     
  
