(defactor Bouncer (name) (other) 
	   (send other self) 
	   (pr (concatenate 'string name " : " "  bouncing")) 
	   (sleep 3)
	   next)

(setq b1 (Bouncer :name "Bouncer 1"))
(setq b2 (Bouncer :name "Bouncer 2"))

(send b1 b2)
