;;;; package.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(defpackage g2i
  (:use #:cl #:cffi )
  (:export #:gp-result-as-string
           #:gp-setting-get
           #:gp-setting-set
           #:gp-library-version
           #:gp-context-new

           #:gp-port-info-list-new
           #:gp-port-info-list-free
           #:gp-port-info-list-load
           #:gp-port-info-list-count
           #:gp-port-info-list-get-info
           #:gp-port-info-list-lookup-path

           #:gp-list-new
           #:gp-list-free
           #:gp-list-count
           #:gp-list-get-name
           #:gp-list-get-value

           #:gp-abilities-list-new
           #:gp-abilities-list-free
           #:gp-abilities-list-load
           #:gp-abilities-list-detect
           #:gp-abilities-list-count
           #:gp-abilities-list-lookup-model
           #:gp-abilities-list-get-abilities


           #:gp-camera-new
           #:gp-camera-set-abilities
           #:gp-camera-set-port-info
           #:gp-camera-init

           #:camera-abilities
           #:port-info
           ))

(defpackage #:gphoto2
  (:use #:cl #:g2i)
  (:nicknames :gp2)
  (:export
   #:version
   #:setting-get
   #:setting-set
   #:list-cameras
   #:create-context
   #:create-camera
   ))


