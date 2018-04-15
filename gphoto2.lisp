;;;; gphoto2.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:g2i)

(cffi:load-foreign-library "libgphoto2.so")

(autowrap:c-include "/usr/include/gphoto2/gphoto2-version.h"
                    :sysincludes '("/usr/local/include/" "/usr/include/")
                    :trace-c2ffi t
                    :include-definitions t
                    :include-sources t)
(autowrap:c-include "/usr/include/gphoto2/gphoto2.h"
                    :sysincludes '("/usr/local/include/" "/usr/include/")
                    :trace-c2ffi t
                    :include-definitions t
                    :include-sources t)


(in-package #:gphoto2)
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun err-check (rval)
  (when (< rval 0)
    (error (g2i:gp-result-as-string rval)))
  rval)

(defun version ()
  (let ((vstring (cffi:foreign-string-to-lisp (cffi:mem-ref (g2i:gp-library-version 1) :pointer))))
    vstring))

(defun setting-get (id key)
  (cffi:with-foreign-strings ((gp-id id)
                              (gp-key key))
    (let ((rval (cffi:foreign-alloc :char :count 2048))
          (lisp-rval nil))
      (g2i:gp-setting-get gp-id gp-key rval)
      (setf lisp-rval (cffi:foreign-string-to-lisp rval))
      (cffi:foreign-free rval)
      lisp-rval)))

(defun setting-set (id key value)
  (cffi:with-foreign-strings ((gp-id id)
                              (gp-key key)
                              (gp-value value))
    (g2i:gp-setting-set gp-id gp-key gp-value)))

(defun create-context ()
  (g2i:gp-context-new))

;; (defstruct gphoto-session
;;   (context :initform nil)
;;   (ability-list :initform nil)
;;   (port-list :initform nil)
;;   (xlist :initform nil)
;;   (camera-list nil))

;; (Defun create-gphoto-session ()
;;   (let ((context (g2i:gp-context-new))
;;         (camera-list nil))
;;     (cffi:foreign-object-
;;     (cffi:with-foreign-objects ((pi-list :pointer)
;;                                 (ab-list :pointer)
;;                                 )
;;     (err-check (g2i:gp-port-info-list-new pi-list))
;;     (err-check (g2i:gp-port-info-list-load (cffi:mem-ref pi-list :pointer)))
    

;;     (err-check (g2i:gp-abilities-list-new ab-list))
;;     (err-check (g2i:gp-abilities-list-load (cffi:mem-ref ab-list :pointer) context))
    
;;     (g2i:gp-list-new xlist)
;;     (g2i:gp-abilities-list-detect (cffi:mem-ref ability-list :pointer)
;;                                   (cffi:mem-ref pi-list :pointer)
;;                                   (cffi:mem-ref xlist :pointer)
;;                                   context)
;;     (setf cam-count (g2i:gp-list-count (cffi:mem-ref xlist :pointer)))
;;     (dotimes (i cam-count)
;;       (cffi:with-foreign-objects ((name :pointer)
;;                                   (value :pointer))
        
;;         (err-check (g2i:gp-list-get-name (cffi:mem-ref xlist :pointer) i name))
;;         (err-check (g2i:gp-list-get-value (cffi:mem-ref xlist :pointer) i value))
;;         (push (cons (cffi:foreign-string-to-lisp (cffi:mem-ref name :pointer))
;;                     (cffi:foreign-string-to-lisp (cffi:mem-ref value :pointer)))
;;               camera-list))))
;;     (make-gphoto-session :context context
;;                          :port-list pi-list
;;                          :ability-list ab-list
;;                          :camera-list camera-list)))

;; (defun destroy-gphoto-session (session)
  
;;     (g2i:gp-abilities-list-free (cffi:mem-ref ability-list :pointer)))


(defun list-cameras (&optional context)
  (cffi:with-foreign-object ( pi-list :pointer)
    (let ((rval nil)
          (cams nil)
          (ctxt (if context context (create-context))))
      
      (err-check (g2i:gp-port-info-list-new pi-list))
      (err-check (g2i:gp-port-info-list-load (cffi:mem-ref pi-list :pointer)))

      (cffi:with-foreign-object ( ability-list :pointer)
        (err-check (g2i:gp-abilities-list-new ability-list))
        (err-check (g2i:gp-abilities-list-load (cffi:mem-ref ability-list :pointer) context))
        (err-check (g2i:gp-abilities-list-count (cffi:mem-ref ability-list :pointer)))
        (cffi:with-foreign-object ( xlist :pointer)
          (g2i:gp-list-new xlist)

          (g2i:gp-abilities-list-detect (cffi:mem-ref ability-list :pointer)
                                        (cffi:mem-ref pi-list :pointer)
                                        (cffi:mem-ref xlist :pointer)
                                        ctxt)

          (setf rval (g2i:gp-list-count (cffi:mem-ref xlist :pointer)))
          (dotimes (i rval)
            (cffi:with-foreign-objects ((name :pointer)
                                        (value :pointer))
              
              (err-check (g2i:gp-list-get-name (cffi:mem-ref xlist :pointer) i name))
              (err-check (g2i:gp-list-get-value (cffi:mem-ref xlist :pointer) i value))
              (push (cons (cffi:foreign-string-to-lisp (cffi:mem-ref name :pointer))
                          (cffi:foreign-string-to-lisp (cffi:mem-ref value :pointer)))
                    cams))))
        (g2i:gp-abilities-list-free (cffi:mem-ref ability-list :pointer)))
      cams)))

(defun create-camera (description &optional context)
  (let ((ctxt (if context context (create-context)))
        (ab-idx nil)
        (model (car description))
        (port (cdr description))
        (gp-camera (autowrap:alloc :pointer)))
    (g2i:gp-camera-new gp-camera)

    (cffi:with-foreign-objects ((ability-list :pointer)
                                ;;(pi-list :pointer)
                                )
      (err-check (g2i:gp-abilities-list-new ability-list))
      (err-check (g2i:gp-abilities-list-load (cffi:mem-ref ability-list :pointer) ctxt))
      (setf ab-idx (err-check (g2i:gp-abilities-list-lookup-model (cffi:mem-ref ability-list :pointer) model)))

      (autowrap::c-let ((cam-ability camera-abilities :free t))
        (err-check (g2i:gp-abilities-list-get-abilities (cffi:mem-ref ability-list :pointer) ab-idx cam-ability))
        (err-check (g2i:gp-camera-set-abilities gp-camera cam-ability)))

      ;; (c-let ((port-info gp-port-info)
      ;; (pi-idx 0))
      ;;   (err-check (g2i:gp-port-info-list-new pi-list))
      ;;   (err-check (g2i:gp-port-info-list-load (cffi:mem-ref pi-list :pointer)))
        ;; (setf pi-idx (err-check (g2i:gp-port-info-list-lookup-path pi-list (cffi:liport))))
        ;; (err-check (g2i:gp-port-info-list-get-info pi-list pi-idx port-info))
        ;; (err-check (g2i:gp-camera-set-port-info gp-camera (autowrap:ptr port-info)))
        ;; (err-check (g2i:gp-camera-init gp-camera context))
      )
    gp-camera))

;;     (err-check (g2i:gp-abilities-list-get-abilities ability-list 

;; (err-check (g2i:gp-camera-new 
