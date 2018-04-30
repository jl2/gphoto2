;;;; gphoto2.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:g2i)

(cffi:define-foreign-library gphoto-lib
  (:darwin (:or "libgphoto2.dylib" (:framework "gphoto2")))
  (:windows "gphoto2.dll")
  (:unix "libSDL_gfx.so"))

(cffi:use-foreign-library gphoto-lib)
 
(cffi:defcenum camera-driver-status
  :production
  :testing
  :experimental
  :deprecated)

(cffi:defcenum camera-operation
  (:none 0)
  (:capture-image 1)
  (:capture-video #.(ash 1 1))
  (:capture-audio #.(ash 1 2))
  (:capture-preview #.(ash 1 3))
  (:config #.(ash 1 4))
  (:trigger-capture #.(ash 1 5)))

(cffi:defcenum port-type
  (:none 0)
  (:serial 0)
  (:usb #.(ash 1 2))
  (:disk #.(ash 1 3))
  (:ptpip #.(ash 1 4))
  (:usb-disk-direct #.(ash 1 5))
  (:usb-scsi #.(ash 1 6)))

(cffi:defcenum file-operations
  (:none 0)
  (:delete #.(ash 1 1))
  (:preview #.(ash 1 3))
  (:raw #.(ash 1 4))
  (:audio #.(ash 1 5))
  (:exif #.(ash 1 6)))

(cffi:defcenum folder-operations
  (:none 0)
  (:delete-all #.(ash 1 0))
  (:put-file #.(ash 1 1))
  (:make-dir #.(ash 1 2))
  (:remove-dir #.(ash 1 3)))

(cffi:defcenum device-type
  (:still-camera 0)
  (:audio-player #.(ash 1 0)))

(cffi:defcenum camera-widget-type
  :window
  :section
  :text
  :range
  :toggle
  :radio
  :menu
  :button
  :date)

(cffi:defcstruct camera-abilities
  (model :int)
  (camera-driver-status camera-driver-status :offset 128)
  (port :int)
  (speed :int)
  (camera-operations :uint16 :offset #.(+ 128 4 64))
  (file-operations :uint16)
  (folder-operations :uint16)
  (usb-vendor :int)
  (usb-product-id :int)
  (usb-class :int)
  (usb-subclass :int)
  (usb-protocol :int)
  (library :int )
  (id :int :offset 1440) ;; :uint8 #.(/ 1024 1))
  (device-type device-type)
  (reserved2 :int)
  (reserved3 :int)
  (reserved4 :int)
  (reserved5 :int)
  (reserved6 :int)
  (reserved7 :int)
  (reserved8 :int))

(cffi:defcstruct port-info-obj
  (type :uint)
  (name :string)
  (path :string) 
 (library-filename :string))

(cffi:defctype port-info (:pointer (:struct port-info-obj)))

(cffi:defcfun "gp_library_version" (:pointer :string)
  (verbosity :int))

(cffi:defcfun "gp_setting_get" :int
  (id :string) (key :string) (value (:pointer :string)))

(cffi:defcfun "gp_setting_set" :int (id :string) (key :string) (value :string))

(cffi:defcfun "gp_context_new" :pointer)
(cffi:defcfun "gp_result_as_string" :string (err :int))

(cffi:defcfun "gp_port_new" :int (info-list (:pointer :pointer)))
(cffi:defcfun "gp_port_free" :int (info-list :pointer))

(cffi:defcfun "gp_port_info_list_new" :int (info-list (:pointer :pointer)))
(cffi:defcfun "gp_port_info_list_free" :int (info-list :pointer))
(cffi:defcfun "gp_port_info_list_load" :int (info-list :pointer))
(cffi:defcfun "gp_port_info_list_count" :int (info-list :pointer))
(cffi:defcfun "gp_port_info_list_get_info" :int (info-list :pointer) (index :int) (info :pointer))
(cffi:defcfun "gp_port_info_list_lookup_path" :int (info-list :pointer) (path :string))
(cffi:defcfun "gp_port_info_list_lookup_name" :int (info-list :pointer) (path :string))

(cffi:defcfun "gp_list_new" :int (gp-list (:pointer :pointer)))
(cffi:defcfun "gp_list_free" :int (gp-list :pointer))
(cffi:defcfun "gp_list_count" :int (gp-list :pointer))
(cffi:defcfun "gp_list_get_name" :int (gp-list :pointer) (index :int) (name (:pointer :string)))
(cffi:defcfun "gp_list_get_value" :int (gp-list :pointer) (index :int) (value (:pointer :string)))
(cffi:defcfun "gp_list_set_name" :int (gp-list :pointer) (index :int) (name :string))
(cffi:defcfun "gp_list_set_value" :int (gp-list :pointer) (index :int) (value :string))

(cffi:defcfun "gp_abilities_list_new" :int (ability-list (:pointer :pointer)))
(cffi:defcfun "gp_abilities_list_free" :int (ability-list :pointer))
(cffi:defcfun "gp_abilities_list_load" :int (ability-list :pointer)(context :pointer))
(cffi:defcfun "gp_abilities_list_detect" :int (ability-list :pointer) (port-info-list :pointer) (camera-list :pointer) (context :pointer))
(cffi:defcfun "gp_abilities_list_count" :int (ability-list :pointer))
(cffi:defcfun "gp_abilities_list_lookup_model" :int (ability-list :pointer) (model :string))
(cffi:defcfun "gp_abilities_list_get_abilities" :int (ability-list :pointer) (index :int) (abilities :pointer))

(cffi:defcfun "gp_camera_new" :int (camera (:pointer :pointer)))
(cffi:defcfun "gp_camera_free" :int (camera :pointer))

(cffi:defcfun "gp_camera_init" :int (camera :pointer) (context :pointer))
(cffi:defcfun "gp_camera_exit" :int (camera :pointer))

(cffi:defcfun "gp_camera_get_summary" :int (camera :pointer) (summary :pointer) (context :pointer))

(cffi:defcfun "gp_camera_set_abilities" :int (camera :pointer) (abilities (:struct camera-abilities)))
(cffi:defcfun "gp_camera_set_port_info" :int (camera :pointer) (port-info :pointer))
(cffi:defcfun "gp_camera_get_config" :int (camera :pointer) (root-config :pointer) (context :pointer))
;;(cffi:defcfun "gp_camera_get_config_by_name" :int (config :pointer) (name :string) (child-config :pointer)))
(cffi:defcfun "gp_widget_get_child_by_name" :int (root :pointer) (child-name :string) (child :pointer))
(cffi:defcfun "gp_widget_get_name" :int (widget :pointer) (widget-info :pointer))
(cffi:defcfun "gp_widget_get_label" :int (widget :pointer) (widget-label :pointer))
(cffi:defcfun "gp_widget_get_id" :int (widget :pointer) (widget-id (:pointer :int)))
(cffi:defcfun "gp_widget_get_type" :int (widget :pointer) (widget-type :pointer))
(cffi:defcfun "gp_widget_set_value" :int (widget :pointer) (widget-type :pointer))
(cffi:defcfun "gp_camera_set_config" :int (camera :pointer) (config :pointer) (context :pointer))

(in-package #:gphoto2)
(declaim (optimize (speed 0) (safety 3) (debug 3)))


(defun err-check (rval)
  (format t "Returned ~a~%" rval)
  (when (< rval 0)
    (error (g2i:gp-result-as-string rval)))
  rval)

(defun version ()
  (let ((vstring (cffi:mem-ref (g2i:gp-library-version 1) :string)))
    vstring))

(defun setting-get (id key)
  (cffi:with-foreign-strings ((gp-id id)
                              (gp-key key))
    (let ((rval (cffi:foreign-alloc :char :count 2048))
          (lisp-rval nil))
      (unwind-protect
           (progn
             (g2i:gp-setting-get gp-id gp-key rval)
             (setf lisp-rval (cffi:foreign-string-to-lisp rval)))
        (cffi:foreign-free rval))
      lisp-rval)))

(defun setting-set (id key value)
  (cffi:with-foreign-strings ((gp-id id)
                              (gp-key key)
                              (gp-value value))
    (g2i:gp-setting-set gp-id gp-key gp-value)))

(defun create-context ()
  (g2i:gp-context-new))


(defun list-cameras (&optional context)
  (cffi:with-foreign-object ( pi-list :pointer)
    (let ((rval nil)
          (cams nil)
          (ctxt (if context context (create-context))))
      
      (err-check (g2i:gp-port-info-list-new pi-list))
      (err-check (g2i:gp-port-info-list-load (cffi:mem-ref pi-list :pointer)))

      (cffi:with-foreign-object ( ability-list :pointer)
        (err-check (g2i:gp-abilities-list-new ability-list))
        (err-check (g2i:gp-abilities-list-load (cffi:mem-ref ability-list :pointer) ctxt))
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
      (g2i:gp-port-info-list-free (cffi:mem-ref pi-list :pointer))
      cams)))

(defun create-camera (description &optional context)
  (let ((ctxt (if context context (create-context)))
        (ab-idx nil)
        (model (car description))
        (port (cdr description))
        (pi-idx -1)
        (gp-camera (cffi:foreign-alloc :pointer))
        (cam-ability (cffi:foreign-alloc '(:struct g2i:camera-abilities))))

    (format t "In create-camera model: ~a port ~a~%" model port)
    (err-check (g2i:gp-camera-new gp-camera))
    
    (cffi:with-foreign-objects ((pi-list :pointer)
                                (port-info :pointer)
                                (ability-list :pointer)
                                
                                )
        (err-check (g2i:gp-abilities-list-new ability-list))
        (err-check (g2i:gp-abilities-list-load (cffi:mem-ref ability-list :pointer) ctxt))
        (setf ab-idx (err-check (g2i:gp-abilities-list-lookup-model (cffi:mem-ref ability-list :pointer) model)))
        (format t "ab-idx: ~a~%" ab-idx)
        (err-check (g2i:gp-abilities-list-get-abilities (cffi:mem-ref ability-list :pointer) ab-idx cam-ability))
        (format t "Setting abilities.~%")
        ;;(format t "~a~%" cam-ability)
        (err-check (g2i:gp-camera-set-abilities
                    (cffi:mem-ref gp-camera :pointer)
                    (cffi:mem-ref cam-ability '(:struct g2i:camera-abilities))))
        

        (err-check (g2i:gp-port-info-list-new pi-list))
        (err-check (g2i:gp-port-info-list-load (cffi:mem-ref pi-list :pointer)))
        (setf pi-idx (err-check (g2i:gp-port-info-list-lookup-path (cffi:mem-ref pi-list :pointer) port)))

        (format t "idx: ~a~%" pi-idx)
        (err-check (g2i:gp-port-new port-info))
        (err-check (g2i:gp-port-info-list-get-info (cffi:mem-ref pi-list :pointer) pi-idx port-info))
        (format t "Got port-info: ~a~%" port-info)
        ;;(format t "Got port info: ~a~%" (cffi:mem-ref port-info '(:struct g2i:port-info)))
        ;; 
        (err-check (g2i:gp-camera-set-port-info (cffi:mem-ref gp-camera :pointer)
                                                (cffi:mem-ref port-info :pointer)
                                                ))
        (format t "calling gp-port-free.")
        (err-check (g2i:gp-port-free (cffi:mem-ref port-info :pointer)))
        

        (format t "Calling camera-init.~%")
        ;; (cffi:with-foreign-objects ((outs (cffi:foreign-string-alloc (make-string (* 32 1024)))))
        ;;   (err-check (g2i:gp-camera-get-summary (cffi:mem-ref gp-camera :pointer) outs ctxt))
        ;;   (format t "~a~%" (cffi:foreign-string-to-lisp  outs)))
        (err-check (g2i:gp-camera-init (cffi:mem-ref gp-camera :pointer) ctxt))
        (enable-capture gp-camera ctxt))
        


    ;; (g2i:gp-port-info-list-free (cffi:mem-ref pi-list :pointer))
        ;; (g2i:gp-abilities-list-free (cffi:mem-ref ability-list :pointer)))
        ;; (err-check (g2i:gp-camera-init (cffi:mem-ref gp-camera :pointer) ctxt)))
    
    

      ;; (c-let ((port-info gp-port-info)
      ;; (pi-idx 0))
        ;; 
    ;; (err-check (g2i:gp-camera-init gp-camera ctxt))
    (Values gp-camera ctxt)))

;;     (err-check (g2i:gp-abilities-list-get-abilities ability-list 

;; (err-check (g2i:gp-camera-new 


(defun enable-capture (camera &optional context)
  (let ((ctxt (if context context (create-context)))
        (root-config (cffi:foreign-alloc :pointer))
        (main-config (cffi:foreign-alloc :pointer))
        (settings-config (cffi:foreign-alloc :pointer))
        (capture-config (cffi:foreign-alloc :pointer)))
    (format t "In enable-capture.~%")
    (cffi:with-foreign-objects (;; (root-config :pointer)
                                ;; (main-config :pointer)
                                ;; (settings-config :pointer)
                                ;; (capture-config :pointer)
                                (widget-info :pointer)
                                (widget-label :pointer)
                                (widget-id :pointer)
                                (widget-type :pointer)
                                )
      (format t "Calling gp-camera-get-config~%")
      (err-check  (g2i:gp-camera-get-config (cffi:mem-ref camera :pointer) root-config (cffi:mem-ref ctxt :pointer)))
      (format t "Calling gp-widget-get-child-by-name~%")
      (err-check  (g2i:gp-widget-get-child-by-name (cffi:mem-ref root-config :pointer)  "main" main-config))
      ;; (err-check  (g2i:gp-widget-get-child-by-name (cffi:mem-ref main-config :pointer)  "settings" settings-config))
      ;; (err-check  (g2i:gp-widget-get-child-by-name (cffi:mem-ref settings-config :pointer) "capture" capture-config))
      )))

(defun cleanup-camera (camera &optional context)
  (g2i:gp-camera-exit (cffi:mem-ref camera :pointer))
  (g2i:gp-camera-free (cffi:mem-ref camera :pointer)))
;;     gp_widget_get_name(capture, &widgetinfo);

;;     gp_widget_get_label(capture, &widgetlabel);

;;     int widgetid;
;;     gp_widget_get_id(capture, &widgetid);

;;     CameraWidgetType widgettype;
;;     gp_widget_get_type(capture, &widgettype);

;;     int one=1;
;;     herr(gp_widget_set_value(capture, &one), "gp_widget_set_value", __LINE__);

;;     herr(gp_camera_set_config(camera, actualrootconfig, cameracontext), "gp_camera_set_config", __LINE__);
;; )
