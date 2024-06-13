(in-package :gfx)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((uniform-vars '(glsl::ichannel0 glsl::ichannel1 glsl::ichannel2 glsl::ichannel3
			glsl::icontrol0 glsl::icontrol1 glsl::icontrol2 glsl::icontrol3
			glsl::ivolume0 glsl::itime glsl::iresolution)))
    (loop for var in uniform-vars
	  do (import var)
	     (export var :glsl))
    (import '(gfx::camera gfx::lookat) :glsl)
    (export '(gfx::camera gfx::lookat) :glsl)))



(defun clear-pipeline (&optional remove-uniforms)
  "Initialize global state of shader program.
Clear user-defined variables, functions, macros, structures."
  (gfx:reinit-shader-system)
  (loop for pipeline being the hash-values of gfx::*all-pipeline-table*
 	do (setf (gfx::%pipeline-used-funcs pipeline) nil))
  (unless remove-uniforms 
    (loop for chan in '(ichannel0 ichannel1 ichannel2 ichannel3)
	  do (gfx::add-uniform chan :sampler-2d))
    (loop for cont in '(icontrol0 icontrol1 icontrol2 icontrol3)
	  do (gfx::add-uniform cont :float))
    (gfx::add-uniform 'ivolume0 :float)
    (gfx::add-uniform 'itime :float)
    (gfx::add-uniform 'iresolution :vec2)
    (values)))



;; ================================================================================
;; Renderer
;; 

(defvar *running-renderer* nil)

(defclass renderer ()
  ((framebuffer :accessor framebuffer)
   (scene-ratio :initarg :scene-ratio :accessor scene-ratio)
   (width :accessor width)
   (height :accessor height)
   (mailbox   :initform (sb-concurrency:make-mailbox) :reader mailbox)
   (shader-name    :initarg :shader-name :accessor shader-name)
   (textures  :initform nil :accessor textures)
   (gl-canvas :initform nil :accessor gl-canvas)
   (close-fn  :initform nil :accessor close-fn)
   (quad-stream :allocation :class
		:initform (gfx:make-gpu-stream '((pos :vec2) (coord :vec2))
					       '(-1.0 -1.0 0.0 0.0
						 1.0 -1.0  1.0 0.0
						 -1.0 1.0  0.0 1.0
						 -1.0 1.0  0.0 1.0
						 1.0 -1.0  1.0 0.0
						 1.0 1.0  1.0 1.0))
		:reader quad-stream)
   (fov :initarg :fov :accessor fov)
   (camera :initform (make-instance 'gfx:camera) :reader camera)
   (key-modifier :initform nil :accessor key-modifier))
  (:documentation "An object responsible for all graphics processing."))


(defclass gl-canvas (gfx:shader-environment)
  ((renderer :initarg :renderer :reader renderer)
   (width :accessor width)
   (height :accessor height))
  (:documentation "Objects for using traditional rasterize methods in renderer.
You should be define generic function to init, draw, and release for each step of the drawing process."))




;; define generic functions for renderer and gl-canvas.
(defmethod projection-matrix ((renderer renderer))
  (kit.math:perspective-matrix (fov renderer) (/ (width renderer) (height renderer)) .1 10000.0))

(defmethod projection-matrix ((canvas gl-canvas))
  (projection-matrix (renderer canvas)))


(defmethod view-matrix ((renderer renderer))
  (gfx:eval-camera (camera renderer)))

(defmethod view-matrix ((canvas gl-canvas))
  (gfx:eval-camera (camera (renderer canvas))))


(defmethod init ((canvas gl-canvas)))
(defmethod draw ((canvas gl-canvas)))
(defmethod release ((canvas gl-canvas)))
(defmethod release :after ((canvas gl-canvas))
  (gfx:release-environment canvas))



(defgeneric texture-type (texture))
(defgeneric init-texture (texture-type texture-info))
(defgeneric update-texture (texture-type texture))
(defgeneric release-texture (texture-type texture))




(defun cleanup (renderer)
  "Cleanup function. Called when the renderer receives a request(mail) for a state change."
  (alexandria:when-let ((gl-canvas (gl-canvas renderer)))
    (handler-case (release gl-canvas)
      (error (c) (break "Error ~a on release gl-canvas~%" c)))
    (setf (gl-canvas renderer) nil))
  (dolist (texture (textures renderer))
    (release-texture (texture-type texture) texture))
  (setf (textures renderer) nil))


(defun watch-mailbox (renderer)
  "Handles state change requests to the renderer."
  (let* ((mail (sb-concurrency:receive-message-no-hang (mailbox renderer))))
    (when mail
      ;; cleanup
      (when (getf mail :cleanup)
	(cleanup renderer))
      ;; change size
      (alexandria:when-let ((size (getf mail :size)))
	(glfw:set-window-size (first size) (second size)))
      ;; change shader
      (alexandria:when-let ((name (getf mail :shader))) 
	(setf (shader-name renderer) name)
	(glfw:set-window-title (string-upcase (shader-name renderer))))
      ;; change gl-canvas
      (alexandria:when-let ((gl-canvas (getf mail :gl-canvas)))
	(let* ((new-gl-canvas (make-instance gl-canvas
				:renderer renderer)))
	  (handler-case (init new-gl-canvas)
	    (error (c) (break "Error ~a on init gl-canvas~%" c)))
	  (setf (gl-canvas renderer) new-gl-canvas)))
      ;; change textures 
      (alexandria:when-let ((texture-infos (getf mail :textures)))
	(setf (textures renderer)
	  (loop for texture-info in texture-infos
		collect (init-texture (car texture-info) (cdr texture-info))))))))


(defun resize-framebuffer (renderer)
  "Called on change to Window/Renderer size. Refreshes the renderer's internal framebuffer."
  (let* ((size (glfw:get-window-size))
	 (width (first size))
	 (height (second size))
	 (framebuffer (framebuffer renderer))
	 (scene-ratio (scene-ratio renderer)))
    (when (or (/= (* width scene-ratio) (gfx:width framebuffer))
	      (/= (* height scene-ratio) (gfx:height framebuffer)))
      (gfx:release-fbo framebuffer)
      (setf (framebuffer renderer) (gfx:make-fbo (* width scene-ratio)
						 (* height scene-ratio)
						 :multisample t)))))


(defun render (renderer environment)
  "Called on every render."
  (resize-framebuffer renderer)
  (let* ((framebuffer (framebuffer renderer))
	 (w (gfx:width framebuffer))
	 (h (gfx:height framebuffer))
	 (camera (camera renderer)))
    (setf (width renderer) w
	  (height renderer) h)
    (gl:viewport 0 0 w h)
    (gl:clear-color .0 .0 .0 1.0)
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:enable :depth-test)
    (loop for texture in (textures renderer)
	  for tex-enum in '(:texture0 :texture1 :texture2 :texture3)
	  do (when texture
	       (gl:active-texture tex-enum)
	       (update-texture (texture-type texture) texture)))
    (with-shader (environment (shader-name renderer) (quad-stream renderer))
      (gfx:set-uniform 'projection-matrix (projection-matrix renderer))
      (gfx:set-uniform 'view-matrix (view-matrix renderer))
      (gfx:set-uniform 'iresolution (list w h))
      (gfx:set-uniform 'itime (get-internal-seconds))
      (gfx:set-uniform 'camera (list (eye-x camera)
				     (eye-y camera)
				     (eye-z camera)))
      (gfx:set-uniform 'lookat (list (center-x camera)
				     (center-y camera)
				     (center-z camera)))
      (gfx:set-uniform 'ivolume0 (av:get-data  :ivolume0))
      (gfx:set-uniform 'icontrol0 (av:get-data :icontrol0))
      (gfx:set-uniform 'icontrol1 (av:get-data :icontrol1))
      (gfx:set-uniform 'icontrol2 (av:get-data :icontrol2))
      (gfx:set-uniform 'icontrol3 (av:get-data :icontrol3))
      (gfx:set-uniform 'ichannel0 0)
      (gfx:set-uniform 'ichannel1 1)
      (gfx:set-uniform 'ichannel2 2)
      (gfx:set-uniform 'ichannel3 3)
      (gl:draw-arrays :triangles 0 (gfx:gpu-stream-length (quad-stream renderer))))
    (gl:disable :depth-test)
    (alexandria:when-let ((gl-canvas (gl-canvas renderer)))
      (setf (width gl-canvas) w
	    (height gl-canvas) h)
      (handler-case (draw gl-canvas)
	(error (c) (break "Error ~a on draw gl-canvas~%" c))))
    (loop for texture in (textures renderer)
	  for tex-enum in '(:texture0 :texture1 :texture2 :texture3)
	  do (gl:active-texture tex-enum)
	     (when (eql (texture-type texture) :previous-frame)
	       (with-fbo ((output-fbo (framebuffer renderer)))
		 (gl:copy-tex-sub-image-2d :texture-2d 0 0 0  0 0 w h)))
	     (gl:bind-texture (texture-target texture) 0))))


(defun release-renderer (renderer)
  "Called when rendering ends. If the renderer's close-fn property has a value, will call it."
  (cleanup renderer)
  (alexandria:when-let ((fn (close-fn renderer)))
    (funcall fn)))




;; ================================================================================
;; drawing renderer
;; The task of drawing the renderer into the actual glfw window
;; 
(gfx:defpipeline draw-framebuffer ((frame :sampler-2d))
  (:vertex (:in ((pos :vec2) (coord :vec2))
	    :out ((v-coord :vec2)))
	   (setf v-coord coord)
	   (v! (* 1.0 pos) .0 1.0))
  (:fragment (:in ((v-coord :vec2)))
	     (texture frame v-coord)))


(defun draw-framebuffer (renderer environment)
  (let* ((size (glfw:get-window-size)))
    (gl:viewport 0 0 (first size) (second size))
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gfx:with-shader (environment 'draw-framebuffer (quad-stream renderer))
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d (gfx:output-texture (framebuffer renderer)))
      (gfx:set-uniform 'frame 0)
      (gl:draw-arrays :triangles 0 (gfx:gpu-stream-length (quad-stream renderer)))
      (gl:bind-texture :texture-2d 0))))



;; ================================================================================
;; Uitlities
;;


(defun move-camera (&key (eye-x 0.0) (eye-y 0.0) (eye-z 5.0)
		      (center-x 0.0) (center-y 0.0) (center-z 0.0))
  "Reset the state of camera in renderer."
  (when *running-renderer*
    (gfx:reset-camera (camera *running-renderer*)
		      :eye-x (float eye-x 1f0) :eye-y (float eye-y 1f0) :eye-z (float eye-z 1f0)
		      :center-x (float center-x 1f0) :center-y (float center-y 1f0) :center-z (float center-z 1f0))
    t))





;; ================================================================================
;; Setup GLFW3
;;


(glfw:def-key-callback keyinput-callback (window key scancode action mod-keys)
  (declare (ignore window scancode))
  (setf (key-modifier *running-renderer*) mod-keys)
  (cond ((and (eq key :escape) (eq action :press)) (glfw::set-window-should-close))))

(glfw:def-scroll-callback  mouse-scroll-callback (window xoffset yoffset)
  (declare (ignore window))
  (let* ((x (float (if (> .004 (abs xoffset)) .0 xoffset) 1.0))
	 (y (float (if (> .004 (abs yoffset)) .0 yoffset) 1.0))
	 (camera (camera *running-renderer*))
	 (key-modifier (key-modifier *running-renderer*)))
    (cond ((find :control key-modifier) (gfx:track-mouse-zoom camera (- x) (- y) .1))
	  ((find :shift key-modifier) (gfx:track-mouse-pan camera (- x) y .1))
	  (t (gfx:track-mouse-spin camera (- x) y .1)))))



(defun create-glfw-window (renderer width height scene-ratio)
  (trivial-main-thread:with-body-in-main-thread ()
    (float-features:with-float-traps-masked (:invalid :overflow)
      (glfw:with-init
	#+darwin (%glfw:window-hint #x00023001 0) 
	(glfw:with-window (:title "" :width width :height height :resizable t
			   :context-version-major 3
			   :context-version-minor 3
			   :opengl-profile :opengl-core-profile
			   :opengl-forward-compat t)
	  (let* ((environment (make-instance 'gfx:shader-environment)))
	    (glfw:set-key-callback 'keyinput-callback)
	    (glfw:set-scroll-callback 'mouse-scroll-callback)
	    (setf (framebuffer renderer) (gfx:make-fbo (* width scene-ratio) (* height scene-ratio) :multisample t))
	    (loop until (glfw:window-should-close-p)
		  do (handler-case (watch-mailbox renderer)
		       (error (c) (break "Error ~a on initialize renderer~%" c)))
		     (gfx:with-fbo ((framebuffer renderer))
		       (render renderer environment))
		     (draw-framebuffer renderer environment)
		  do (glfw:swap-buffers)
		  do (glfw:poll-events))
	    (gfx:release-environment environment)
	    (release-renderer renderer)
	    (setf *running-renderer* nil)))))))



;; ================================================================================
;; Front End
;;

(defmacro define-shader (name &body body)
  "Define the shader program for raymarching. By default, the following uniform variables are defined:
  iresolution(vec2): Resolution of render.
  itime(float): Rendering time.
  camera(vec3): Position the camera's eye.
  lookat: Position the camera is looking at.
  ichannelN(sampler-2d): Texture unit input to the shader program. Currently 0 to 3 are provided.
  ivolume0(float): Audio data(main volume) from scsynth.
  icontrolN(float): Control data from scsynth. Currently 0 to 3 are provided. "
  `(progn
     (gfx:defpipeline ,name  ((projection-matrix :mat4) (view-matrix :mat4) (iresolution :vec2) (itime :float)
			      (camera :vec3) (lookat :vec3)
			      (ivolume0 :float)
			      (ichannel0 :sampler-2d) (ichannel1 :sampler-2d) (ichannel2 :sampler-2d) (ichannel3 :sampler-2d)
			      (icontrol0 :float) (icontrol1 :float) (icontrol2 :float) (icontrol3 :float))
       (:vertex (:in ((pos :vec2) (coord :vec2))
		 :out ((vfuv :vec2)))
		(progn
		  (setf vfuv pos)
		  (v! pos .0 1.0)))
       (:fragment (:in ((vfuv :vec2)))
		  (progn ,@body)))
     ',name))


(defmacro start-shader (name &key (size '(800 600)) (scene-ratio 1) textures gl-canvas (fov 45.0))
  "Create a glfw window(Renderer) and start the shader program. If there is already a running glfw window, it changes the state of the renderer. Provides the following arguments for the state of the renderer:
  size: Resolution of glfw's window.
  scene-ratio: Size ratio between glwfw window and renderer.
  textures: Parameters for creating texture objects.
  gl-canvas: Class name based on gl-canvas for custom drawing within the renderer."
  `(progn
     (assert (gethash ',name *all-pipeline-table*) nil "Can't found shader ~a" ',name)
     (unless *running-renderer*
       (let* ((renderer (make-instance 'renderer :shader-name ',name :fov ,fov
				       :scene-ratio ,scene-ratio)))
	 (setf *running-renderer* renderer)
	 (create-glfw-window renderer ,(first size) ,(second size) ,scene-ratio)))
     (setf (scene-ratio *running-renderer*) ,scene-ratio)
     (sb-concurrency:send-message (mailbox *running-renderer*)
				  (list :cleanup t
					:shader ',name
					:size ',size
					:textures ',textures
					:gl-canvas ',gl-canvas))))




;; ================================================================================
;; 
;;

(export '(gfx::clear-pipeline
	  gfx::width
	  gfx::height
	  gfx::projection-matrix
	  gfx::view-matrix
	  gfx::gl-canvas
	  gfx::init
	  gfx::draw
	  gfx::release
	  gfx::move-camera
	  gfx::define-shader
	  gfx::start-shader
	  gfx::vfuv
	  gfx::iresolution
	  gfx::itime
	  gfx::camera
	  gfx::lookat))
