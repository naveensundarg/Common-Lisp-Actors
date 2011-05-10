(in-package #:cl-actors)

; Create a behavior that can be attached to any actor
(defmacro behav (state vars  &body body)
  `(let ,state
       (labels ((me ,(append vars `(&key self  (next #'me next-supplied-p))) 
		     (setf next (curry next :self self))
		  ,@body))
	#'me)))

; Macro for creating actors with the behavior specified by body
(defmacro defactor (name state vars &body body)
 `(defun ,name (&key (self) ,@state)
    (labels ((me ,(append vars `(&key (next #'me next-supplied-p))) 
	       (if next-supplied-p
		  (setf next (curry next :self self)))
	       ,@body))
      (setf self (make-actor #'me)) self)))

; The shell of an actor
(defun make-actor (behav)
  (let (self 
	(lock (make-lock)) 
	(messages '())
	(cv (make-condition-variable)))
    (labels ((add (m) 
	       (with-lock-held (lock) 
		 (push m messages))
	       (condition-notify cv))
	     (run-actor () (loop
			      (thread-yield)
			      (with-lock-held (lock)
				(if (not (null messages))
				    (setf behav (apply behav 
						       (pop messages)))
				    (condition-wait cv lock )))))) 
	(setf self
	      (list (make-thread #'run-actor)
		#'add
		messages)))))

(defun send (actor &rest message)
  (make-thread #'(lambda () (funcall (get-add actor) message))) 
  (values))

(defun stop-actor (actor) (destroy-thread (get-thread actor)))

(defun if-single (x) (if (eq (length x) 1) (car x) x))
(defun get-thread (actor) (first actor))
(defun get-add (actor) (second actor))
(defun sink (&rest args) (declare (ignore args)) #'sink)

;Currying. 
(defun curry (f &rest args)
	   (lambda (&rest rem)
	     (apply f (append rem args) )))

;Easy priting to repl from threads.
(defun pr (x) (print x *standard-output*))

;A printing actor 
(defactor printer () (x) (pr x) next)
