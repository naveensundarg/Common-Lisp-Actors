(in-package #:cl-actors)

;; ----------------------------------------------------------------------------
(defclass actor()
  ((name :initarg :name
         :initform (error ":name must be specified")
         :accessor name
         :documentation "Hold the name of actor")
   (add :initarg :add
         :initform (error ":add must be specified")
         :accessor add
         :documentation "holds handle to add method")
   (main :initarg :main
         :initform (error ":main must be specified")
         :accessor main
         :documentation "Main Thread")
   (messages :initarg :messages
         :initform (error ":messages must be specified")
         :accessor messages
         :documentation "Message stream sent to actor")
   thread))

;; ----------------------------------------------------------------------------
(defmethod initialize-instance :after((self actor) &key)
  "Uses the main functiona name to create a thread"
  (with-slots (name main thread) self
    (setf thread
          (bt:make-thread main :name name))))

;; ----------------------------------------------------------------------------
(defmethod send ((self actor) &rest message)
  (bt:make-thread #'(lambda () (funcall (get-add self) message)))
  (values))

;; ----------------------------------------------------------------------------
;; (defun stop-actor (actor) (destroy-thread (get-thread actor)))
(defmethod stop-actor ((self actor))
  "Stops the actor thread"
  (with-slots (thread) self
    (destroy-thread  thread)))

;; ----------------------------------------------------------------------------
;; (defun get-thread (actor) (first actor))
(defmethod get-thread ((self actor))
  "Returns the handle of a thread"
  (with-slots (thread) self
    thread))

;; ----------------------------------------------------------------------------
;; (defun get-add (actor) (second actor))
(defmethod get-add ((self actor))
  "Returns handle to the add function"
  (with-slots (add) self
    add))

;; ----------------------------------------------------------------------------
;; Create a behavior that can be attached to any actor
(defmacro behav (state vars  &body body)
  `(let ,state
     (labels ((me ,(append vars `(&key self  (next #'me next-supplied-p)))
                (setf next (curry next :self self))
             x   ,@body))
       #'me)))

;; ----------------------------------------------------------------------------
;; Macro for creating actors with the behavior specified by body
(defmacro defactor (name state vars &body body)
  `(defun ,name (&key (self) ,@state)
     (labels ((me ,(append vars `(&key (next #'me next-supplied-p)))
                (if next-supplied-p
                    (setf next (curry next :self self)))
                ,@body))
       (setf self (make-actor #'me ,(string name))) self)))

;; ----------------------------------------------------------------------------
;; The shell of an actor
(defun make-actor (behav name)
  (let (self
        (lock (bt:make-lock))
        (messages '())
        (cv (bt:make-condition-variable)))
    (labels ((add (m)
               (with-lock-held (lock)
                 (setf messages (nconc messages (list m))))
               (condition-notify cv))
             (run-actor () (loop
                              (thread-yield)
                              (with-lock-held (lock)
                                (if (not (null messages))
                                    (setf behav (apply behav
                                                       (pop messages)))
                                    (condition-wait cv lock ))
                                (unless behav (return))))))
      (setf self (make-instance 'actor
                                :name (concatenate 'string "Actor: " name)
                                :main #'run-actor
                                :add #'add
                                :messages messages)))))

;; ----------------------------------------------------------------------------
(defun if-single (x)
  (if (eq (length x) 1)
      (car x)
      x))

;; ----------------------------------------------------------------------------
(defun sink (&rest args)
  (declare (ignore args)) #'sink)

;; ----------------------------------------------------------------------------
;; Currying.
(defun curry (f &rest args)
  (lambda (&rest rem)
    (apply f (append rem args) )))

;; ----------------------------------------------------------------------------
;; Easy priting to repl from threads.
(defun pr (x)
  (print x *standard-output*)
  (format t "~%"))

;; ----------------------------------------------------------------------------
;; A printing actor
(defactor printer ()
    (x)
  (pr x) next)
