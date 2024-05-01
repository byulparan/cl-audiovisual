;;
;; cl-audiovisual is crossplatform audio-visual library.
;; It based on cl-collider, cl-glfw3 and cl-glsl
;; It should be run on macOS/Windows/Linux.
;; 

(ql:quickload :cl-audiovisual)


;;
;; setup the package
;; 
(in-package :sc-user)
(named-readtables:in-readtable :sc)
(use-package :sc-extensions)
(use-package :glsl)


;;
;; play synth
;;
(av:run-cl-collider)

(proxy :ambient
  (mix
   (loop with noise = (pink-noise.ar .001)
	 for gate in (mod (+ [0 1] (cnt .125)) 2)
	 for note = (+ 60 (ti-rand.kr 0 12 gate))
	 collect (pan2.ar (* (dyn-klank.ar [(midicps [48 55 note])] noise)
			     (asr.kr 4 1 4 gate))))))


(proxy :bass
  (let* ((freq (midicps 36))
	 (freq (env-gen.kr (env [freq freq (* freq .5)] [.0 .1]) :gate (tr .25)))
	 (sig (pulse.ar freq (r+ (lf-tri.kr .23) .2 .8) 
			(env.kr .0 .6 .1 (rand-tr (tr .25) .7) .2))))
    (out.kr 0 (peak.ar sig (impulse.kr 20)))
    (pan2.ar (rlpf.ar sig (r+ (lf-tri.kr .31) 400 500)))))





;; Provide the prepared uniform variable for interaction with audio data / user event,
;; utility function for shader programming(in shader-lib package).

(gfx:define-shader demo1
  (v! .4 .2 .1 1.0))

(gfx:start-shader demo1)


;; 
;; The graphics part of cl-audiovisual uses two methods: raymarching (similar to shadertoy)
;; and traditional rasterization rendering. And you can also combine the two methods.
;;


;; ================================================================================
;; Raymaching
;; 
(gfx:move-camera :eye-x 7.0 :eye-y 10.0 :eye-z 20.0 :center-y 8.0) ;; reset camera position

(gfx:clear-pipeline) ;; clear user-defined variables, functions, structures in shader program


;; define variables in shader
(gfx:defvar-g eps .001)
(gfx:defvar-g max-depth 100.0)

;; define functions in shader
(gfx:defun-g sdf ((p :vec3))
  (let* ((pos (* (sl:yrot (+ itime (sl:rand11 (floor (x p))) (* (x p) icontrol0)))
		 (sl:zrot (+ itime (sl:rand11 (floor (z p))) (* (z p) icontrol0)))
		 (v! (sin (x p)) (y p) (cos (z p)))))
	 (sz (+ .5 icontrol0 (* 1.7 ivolume0)))
	 (r (* sz .7)))
    (sl:op-u
     (sl:sd-sphere (- p (v! .0 10.0 .0)) (+ 2.0 (* icontrol0 4.0)))
     (sl:op-s
      (sl:sd-box pos (vec3 sz))
      (sl:sd-box pos (v! 10.0 r r))
      (sl:sd-box pos (v! r 10.0 r))
      (sl:sd-box pos (v! r  r 10.0))))))

(gfx:defun-g get-normal ((p :vec3))
  (normalize (v! (- (sdf p) (sdf (- p (v! eps .0 .0))))
		 (- (sdf p) (sdf (- p (v! .0 eps .0))))
		 (- (sdf p) (sdf (- p (v! .0 .0 eps)))))))


;; define shader
(gfx:define-shader cl-audiovisual
  (sl:with-raymarch (uv rd (ro camera) (ta lookat))
    (let* ((md .0)
	   (col (vec3 .0)))
      (dotimes (i 100)
	(let* ((p (+ ro (* rd md)))
	       (d (sdf p)))
	  (when (< d eps) (break))
	  (incf md d)
	  (when (> md max-depth) (break))))
      (when (< md max-depth)
	(let* ((p (+ ro (* rd md)))
	       (li (v! 10.0 10.0 10.0))
	       (l (normalize (- li p)))
	       (n (get-normal p)))
	  (setf col (* (if (> (y p) 2.0)
			   (xyz (sl:texture! ichannel0 (* .3 (v! (+ .5 (x p))
								 (+ 1.4 (y p))))
					     t))
			 (v! .2 .2 (* 4.0 ivolume0)))
		       (dot n l)))))
      (v! col 1.0))))


(gfx:start-shader cl-audiovisual
  :textures (("./john-mccarthy.jpeg" :wrap :repeat)))




;; ================================================================================
;; Rasterize or custom drawing
;; 

(defclass canvas (gfx:gl-canvas)
  ())


;; setup to VBO and VAO
(defparameter *stream* (gfx:make-gpu-stream '((pos :vec3))
					    (list 0.0 0.0 0.0)))



;; define shader program
(gfx:defpipeline draw-stream ((p-matrix :mat4) (v-matrix :mat4) (itime :float) (ichannel0 :sampler-2d))
  (:vertex (:in ((pos :vec3)))
	   (setf gl-point-size (* gl-instance-id .06))
	   (let* ((mv-matrix (* v-matrix
				(sl:translate (v! .0 10.0 .0))
				(sl:rotate .0 (v! 1.0 .0 .0))))
		  (pos (v! (* 4.0 (sin (+ itime (* .126 gl-instance-id))))
			   (* -10.0 (tan (* .05 (+ itime  gl-instance-id))))
			   (* 4.0 (cos (+ itime (* .21 gl-instance-id)))))))
	     (setf v-pos pos
		   v-id gl-instance-id)
	     (* p-matrix mv-matrix (v! pos 1.0))))
  (:fragment (:in ((v-pos :vec3) (v-id :float)))
	     (* (v! (abs (sin (* .04 v-id))) .2 .2 1.0)
		2.3
		(sl:texture! ichannel0 gl-point-coord))))


(defmethod gfx:init ((canvas canvas))
  "called once when create gl-canvas object"
  (uiop:println "init"))


(defmethod gfx:draw ((view canvas))
  (let* ((w (gfx:width view))
	 (h (gfx:height view))
	 (time (gfx:get-internal-seconds)))
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:enable :program-point-size)
    (gl:enable :depth-test)
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one)
    (gl:depth-mask nil)
    ;; run shaderprogram 
    (gfx:with-shader (view 'draw-stream *stream*)
      (gfx:set-uniform 'p-matrix (gfx:projection-matrix view))
      (gfx:set-uniform 'v-matrix (gfx:view-matrix view))
      (gfx:set-uniform 'ichannel0 1)
      (gfx:set-uniform 'itime time)
      (gl:draw-arrays-instanced :points 0 (gfx:gpu-stream-length *stream*) 1000))
    (gl:depth-mask t)
    (gl:disable :depth-test)
    (gl:disable :blend)))

(defmethod gfx:release ((canvas canvas))
  "called once when release gl-canvas object"
  (uiop:println "release"))



;; release previous gl-canvas and create newer gl-canvas
(gfx:start-shader cl-audiovisual
  :gl-canvas canvas
  :textures (("./john-mccarthy.jpeg" :wrap :repeat)
	     ("./spark1.png")))




;; ================================================================================
;; Mix raymarhcing with rasterize 
;; 

(gfx:define-shader cl-audiovisual
  (let* ((md .0)
	 (col (vec3 .0)))
    (sl:with-hybrid (ro rd depth)
      (dotimes (i 100)
	(let* ((p (+ ro (* rd md)))
	       (d (sdf p)))
	  (when (< d eps) (break))
	  (incf md d)
	  (when (> md max-depth) (break))))
      (when (< md max-depth)
	(let* ((p (+ ro (* rd md)))
	       (li (v! 10.0 10.0 10.0))
	       (l (normalize (- li p)))
	       (n (get-normal p)))
	  
	  (setf col (* (dot n l) (if (> (y p) 4.0)
				     (xyz (sl:texture! ichannel0 (* .2 (v! (+ 1.4 (x p))
									   (+ 2.2 (y p)))) t))
				   (v! .3 .4 (* 4.0 ivolume0)))))))
      (setf depth md)) ;; write to depth buffer
    (+
     (v! col 1.0)
     (sl:with-uv (uv uvn)
       (* .8 (sl:texture! ichannel2 uvn))))))


(defmethod gfx:draw ((view canvas))
  (let* ((w (gfx:width view))
	 (h (gfx:height view))
	 (time (gfx:get-internal-seconds)))
    (gl:enable :program-point-size)
    (gl:enable :depth-test)
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one)
    (gl:depth-mask nil)
    (gfx:with-shader (view 'draw-stream *stream*)
      (gfx:set-uniform 'p-matrix (gfx:projection-matrix view))
      (gfx:set-uniform 'v-matrix (gfx:view-matrix view))
      (gfx:set-uniform 'ichannel0 1)
      (gfx:set-uniform 'itime time)
      (gl:draw-arrays-instanced :points 0  (gfx:gpu-stream-length *stream*) 1000))
    (gl:depth-mask t)
    (gl:disable :depth-test)
    (gl:disable :blend)))



(gfx:start-shader cl-audiovisual
  :gl-canvas canvas
  :textures (("john-mccarthy.jpeg" :wrap :repeat)
	     ("spark1.png")
	     (:previous-frame)))





