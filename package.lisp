;;;; package.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(defpackage g2i
  (:use #:cl #:cffi))

(defpackage #:gphoto2
  (:use #:cl #:g2i )
  (:nicknames :gp2)
  (:export
   #:version
   #:setting-get
   #:setting-set
   #:list-cameras
   #:create-context
   #:create-camera
   ))
