;;;; --------------------------------------------------------------------------
;;;; @file   package.lisp
;;;; @author Nikhil J. Shetty <nikhil.j.shetty@gmail.com> 
;;;; @date   Thu May  6 00:32:51 2010
;;;;
;;;; @brief  Package definitions
;;;; --------------------------------------------------------------------------
(in-package #:cl-user)

(defpackage #:cl-actors
  (:use #:cl
        #:bordeaux-threads)
  (:export :defactor
           :next
           :behav
           :send
           :stop-actor
           :printer))
