;;;;===========================================================================
;;;; @file   cl-actors.asd
;;;; @author Nikhil Shetty <nikhil.j.shetty@gmail.com>
;;;; @date   Thu May  6 00:19:07 2010
;;;; @author Naveen Sundar G. <naveensundarg@gmail.com>
;;;; @date   Thu Apr 5 2012
;;;; @brief asdf-install package file for cl-actors
;;;;===========================================================================

(defpackage #:cl-actors-asd (:use #:asdf #:cl))
(in-package :cl-actors-asd)

(defsystem cl-actors
  :author      "Naveen Sundar G. <naveensundarg@gmail.com>"
  :version     "1.1.0"
  :description "A simple common lisp actors library."
  :license "BSD"
  :depends-on (:bordeaux-threads)
  :components ((:file "package")
               (:file "actors" :depends-on ("package"))))


