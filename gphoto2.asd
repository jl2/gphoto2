;;;; gphoto2.asd
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(asdf:defsystem #:gphoto2
  :description "Describe gphoto2 here"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi #:cffi-libffi)
  :components ((:file "package")
               (:file "gphoto2")))
