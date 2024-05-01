(in-package :gfx)

(defclass texture ()
  ((texture-type :initarg :texture-type :reader texture-type)
   (texure-id :initarg :texture-id :reader texture-id)
   (texture-target :initarg :texture-target :reader texture-target)
   (data :initarg :data :reader data)))

(defmethod update-texture (texture-type texture)
  (gl:bind-texture (texture-target texture) (texture-id texture)))

(defmethod release-texture (texture-type texture)
  (gl:delete-texture (texture-id texture)))


;; String
(defmethod init-texture ((texture-type string) texture-info)
  (let* ((image-path (namestring (truename texture-type))))
    (unless (probe-file image-path)
      (format t "can't find image file ~s" texture-type)
      (return-from init-texture))
    (let* ((texture (gl:gen-texture))
	   (image (imago:read-image image-path))
	   (width (imago:image-width image))
	   (height (imago:image-height image))
	   (pointer (cffi:foreign-alloc :unsigned-int :count (* width height)))
	   (filter (getf texture-info :filter))
	   (wrap (getf texture-info :wrap)))
      (loop for y below height
	    do (loop for x below width
		     do (setf (cffi:mem-aref pointer :unsigned-int (+ (* y width) x))
			  (imago:image-pixel image x y))))
      (gl:bind-texture :texture-2d texture)
      (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0 :bgra :unsigned-byte pointer)
      (gl:tex-parameter :texture-2d :texture-mag-filter (if filter filter :linear))
      (gl:tex-parameter :texture-2d :texture-min-filter (if filter filter :linear))
      (gl:tex-parameter :texture-2d :texture-wrap-s (if wrap wrap :clamp-to-edge))
      (gl:tex-parameter :texture-2d :texture-wrap-t (if wrap wrap :clamp-to-edge))
      (gl:bind-texture :texture-2d 0)
      (cffi:foreign-free pointer)
      (make-instance 'texture :texture-type texture-type
		     :texture-target :texture-2d
		     :texture-id texture))))


;; previous frame
(defmethod init-texture ((texture-type (eql :previous-frame)) texture-info)
  (let ((texture (gl:gen-texture))
	(target :texture-2d)
	(renderer *running-renderer*))
    (gl:bind-texture target texture)
    (gl:tex-parameter target :texture-mag-filter :linear)
    (gl:tex-parameter target :texture-min-filter :linear)
    (gl:tex-parameter target :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter target :texture-wrap-t :clamp-to-edge)
    (gl:bind-texture  target 0)
    (make-instance 'texture :texture-type texture-type
		   :texture-target target
		   :texture-id texture 
		   :data (list :width 0 :height 0 :renderer renderer))))


(defmethod update-texture ((texture-type (eql :previous-frame)) texture)
  (gl:bind-texture (texture-target texture) (texture-id texture))
  (let* ((data (data texture))
	 (renderer (getf data :renderer))
	 (width (getf data :width))
	 (height (getf data :height)))
    (when (or (/= width (width renderer))
	      (/= height (height renderer)))
      (setf width (width renderer)
	    height (height renderer))
      (gfx:with-fbo ((output-fbo (framebuffer renderer)))
	(gl:copy-tex-image-2d (texture-target texture) 0 :rgba8 0 0 width height 0))
      (setf (getf data :width) width
	    (getf data :height) height))))




