This is a simple and easy to use Actor system in Common Lisp. 

## Set-Up

Requires Bordeaux threads. http://common-lisp.net/project/bordeaux-threads/ 2. Just load actors.lisp and start using it. 
If you have Quicklisp, ```(ql:quickload "cl-actors")```



## Usage 
An small manual can be found at : 
http://www.cs.rpi.edu/~govinn/actors.pdf

- Creating an actor class or template
  ```lisp
	(defactor Actor-Class
            (state)
	    (message-vars)
	    behavior)
	```


- Creating an actor instance 
     
	```lisp 
        (setq my-actor (Actor-Class (:state-var_1 value_1 ...
   	 	  	        :state-var_n value_n)))
	```

-  Sending a message
     ```lisp 
        (send my-actor message_args)
	```


## Features 

1. Concurrency using the actors model.
2. Dynamic behavior change of actors.



# Examples 

- A ticker: Keeps printing out a count every 2 seconds, starting from 0 and incrementing it every 2 seconds. 

  ```lisp
;create the ticker template
(defactor ticker ((counter 0)) (m) 
	     (sleep 2) (pr counter)
	        (incf counter) (send self nil) next)
; Create an instance
(setf t1 (ticker))
; send a message (async)
(send t1 nil)
; to stop use
(stop-actor t1)
```

- A print actor: Prints the message which was sent to it. A very useful utility actor. 

  ```lisp
	;create the actor template
(defactor print-actor () (val) (pr val) next)
; initialize a new instance
(setf printer (print-actor))
;send values for printing
(send printer "hello, world")
```

- A factorial computing actor : The name says it all :)

  ```lisp
;create the template
(defactor fact ((temp 1)) (n cust) 
	     (if (equal 1 n) 
	            (progn (send cust (* temp 1))
                      (setf temp 1) next)
		             (progn (setf temp (* n temp))
                      (send self (- n 1) cust) next)))
;create a new instance 
(setf f (fact))
; send a value
(send f 4 print-actor)
```

- A nagger for fun : Works only in Mac OS X. Keeps saying out aloud "please work" every 10 seconds :)

   ```lisp
(defactor nagger () () 
    (sleep 10)
       (trivial-shell:shell-command "say please work")
          (send self) next)
; anonymous actor , no way to stop the nagging 
(send (nagger))
	```


## More Resources

1. [Meta-Circular Adventures in Functional Abstraction –Challenging Clojure in Common Lisp] (http://chriskohlhepp.wordpress.com/metacircular-adventures-in-functional-abstraction-challenging-clojure-in-common-lisp/)
