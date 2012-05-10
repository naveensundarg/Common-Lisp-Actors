(in-package #:cl-actors)

;; ----------------------------------------------------------------------------
(defclass actor()
  ((name :initarg :name
         :initform (error ":name must be specified")
         :accessor name
         :documentation "Hold the name of actor")
   (behavior :initarg :behavior
         :initform (error ":behav must be specified")
         :accessor behavior
         :documentation "Behavior")
   (messages :initform '()
         :accessor messages
         :documentation "Message stream sent to actor")
   (lock :initform (bt:make-lock)
         :accessor lock
         :documentation
         "The lock is used when adding a message to the message queue")
   (cv :initarg :cv
         :initform (bt:make-condition-variable)
         :accessor cv
         :documentation "conditional variable used by the thread")
   thread))

;; ----------------------------------------------------------------------------
(defmethod initialize-instance :after((self actor) &key)
  "Uses the main functiona name to create a thread"
  (with-slots (name thread) self
    (setf thread
          (bt:make-thread #'(lambda() (main self))
                          :name name))))

;; ----------------------------------------------------------------------------
(defmethod send ((self actor) &rest message)
  "
Creates a message sending thread which
1. Holds lock to the message (queue)
2. Appends messages (queue) with incoming message
3. Releases lock
4. Notifies the waiting thread that there is a message
"
  (with-slots (messages lock cv) self
    (bt:make-thread #'(lambda ()
                        (with-lock-held (lock)
                          (setf messages (nconc messages (list message))))
                        (condition-notify cv)))
    (values)))

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
;; The main which is started as a thread from the constructor I think that this
;; should be more of an internal function than a method (experiment with
;; funcallable-standard-class)
(defmethod main((self actor))
  (with-slots (lock cv (behav behavior) messages) self
    (loop
       (thread-yield)
       (with-lock-held (lock)
         (if (not (null messages))
             (setf behav (apply behav
                                (pop messages)))
             (condition-wait cv lock ))
         (unless behav (return))))))

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
  (make-instance 'actor
                 :name (concatenate 'string "Actor: " name)
                 :behavior behav))

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
