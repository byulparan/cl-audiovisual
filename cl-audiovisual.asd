
(asdf:defsystem :cl-audiovisual
  :serial t
  :depends-on (#:cl-glsl
	       #:cl-glfw3
	       #:cl-opengl
	       #:sb-cga
	       #:mathkit
	       #:cl-collider
	       #:sc-extensions
	       #:trivial-main-thread
	       #:float-features
	       #:sb-concurrency
	       #:imago)
  :components ((:file "package")
	       (:file "audio")
	       (:file "renderer")
	       (:file "textures")
	       (:file "shader-lib")))
