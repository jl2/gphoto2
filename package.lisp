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
           #:gp-port-info-list-lookup-name

           #:gp-list-new
           #:gp-list-free
           #:gp-list-count
           #:gp-list-get-name
           #:gp-list-get-value

           #:gp-port-new
           #:gp-port-free
           
           #:gp-abilities-list-new
           #:gp-abilities-list-free
           #:gp-abilities-list-load
           #:gp-abilities-list-detect
           #:gp-abilities-list-count
           #:gp-abilities-list-lookup-model
           #:gp-abilities-list-get-abilities


           #:gp-camera-new
           #:gp-camera-free
           #:gp-camera-set-abilities
           #:gp-camera-set-port-info
           #:gp-camera-get-summary
           #:gp-camera-init
           #:gp-camera-exit
           #:gp-camera-get-config
           #:gp-widget-get-child-by-name
           #:gp-widget-get-name
           #:gp-widget-get-label
           #:gp-widget-get-id
           #:gp-widget-get-type
           #:gp-widget-set-value
           #:gp-camera-setconfig
           #:gp-camera-get-config-by-name

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
   #:enable-capture
   #:cleanup-camera
   ))


