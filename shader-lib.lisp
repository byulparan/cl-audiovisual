(in-package #:shader-lib)


(define-macro-library with-uv ((uv &optional (normal-uv 'uvn)) &body body)
  `(let* ((,normal-uv (/ (xy gl-frag-coord) (xy iresolution)))
	  (,uv (- (* ,normal-uv 2.0) 1.0)))
     (setf (x ,uv) (* (x ,uv) (/ (x iresolution) (y iresolution))))
     ,@body))


(define-macro-library with-raymarch ((uv-var rd-var ro ta &optional (uvn-var 'uvn) (dist 2.0)) &body body)
  (alexandria:with-gensyms (cp cw cu cv var)
    `(with-uv (,uv-var ,uvn-var)
       (let* (,ro
	      ,ta)
	 (let* ((,cp (v! .0 1.0 .0))
		(,cw (normalize (- ,(car ta) ,(car ro))))
		(,cu (normalize (cross ,cw ,cp)))
		(,cv (normalize (cross ,cu ,cw)))
		(,var (m! ,cu ,cv ,cw))
		(,rd-var (* ,var (normalize (v! ,uv-var ,dist)))))
	   ,@body)))))

(define-macro-library with-hybrid ((ro rd depth) &body body)
  `(let* ((uv gfx::vfuv)
	  (,ro (let* ((ro (- (xyz (aref gfx:view-matrix 3)))))
		 (incf (x ro) (x (aref gfx:projection-matrix 2)))
		 ro))
	  (,rd (let* ((aspect (/ (x iresolution) (y iresolution)))
		      (u-fovy (/ 1.0 (y (aref gfx:projection-matrix 1))))
		      (dir (v! (* (x uv) u-fovy  aspect)
			       (* (y uv) u-fovy)
			       -1.0)))
		 (normalize dir)))
	  (,depth 9999.0))
     (incf (x ,rd) (x (aref gfx:projection-matrix 2)))
     (setf ,rd (normalize ,rd))
     (setf ,ro (* ,ro (mat3 gfx:view-matrix)))
     (setf ,rd (* ,rd (mat3 gfx:view-matrix)))
     ,@body
     (let* ((eye-fwd (* (v! 0.0 0.0 -1.0) (mat3 gfx:view-matrix)))
	    (eye-hit-z (* (- ,depth) (dot ,rd eye-fwd)))
	    (p10 (z (aref gfx:projection-matrix 2)))
	    (p11 (z (aref gfx:projection-matrix 3)))
	    (ndc-depth (+ (- p10) (/ (- p11) eye-hit-z)))
	    (dep (/ (+ (* (s~ gl-depth-range "diff" :float) ndc-depth)
		       (s~ gl-depth-range "near" :float)
		       (s~ gl-depth-range "far" :float))
		    2.0)))
       (setf gl-frag-depth dep))))


(define-macro-library texture! (texture uv &optional flip)
  (let* ((target (glsl::compile-form texture))
	 (size (ecase (glsl::code-type target)
		 (:sampler-2d 1.0)
		 (:sampler-2d-rect `(texture-size ,texture)))))
    (if flip
	(alexandria:once-only (uv)
	  `(texture ,texture (* (v! (x ,uv) (- 1.0 (y ,uv))) ,size)))
      `(texture ,texture (* ,uv ,size)))))



(gfx:clear-pipeline)








