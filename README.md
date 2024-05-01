# CL-AUDIOVISUAL
cl-audiovisual is crossplatform audio-visual library for Common Lisp.  
It based on cl-collider and cl-glfw / cl-glsl.  
It should be run on macOS / Windows / Linux.  
  
The graphics part of cl-audiovisual uses two methods: raymarching (similar to shadertoy)
and traditional rasterization rendering. And you can also combine the two methods.

Provide the prepared uniform variable for interaction with audio data / user event, utility function for shader programming(in shader-lib package).
  

## Dependencies
- Quicklisp  
- CL-GLSL(https://github.com/byulparan/cl-glsl/)
	

## TODO
  1. more Texture Sources
  2. Post Processing


## ISSUE
in macOS, Don't click close button on window. If you want close window, just click ESC.  
It's cl-glfw3's known issue.


## Supported Texture Sources
  1. image
  2. previous-frame


## API
- AV:RUN-CL-COLLIDER  
  boot scsynth and set bpm(60) then call function `av:add-monitor`.
  
- AV:ADD-MONITOR  
  add monitor synth and event handler to scsynth for get audio/control data.
  
- AV:GET-AUDIO  
  get audio/control data from cl-collider's audio server(scsynth).
  support params:  
    - :ivolume0 - volume data to mix of 0 and 1 bus.
    - :icontrolN(0..3) - data of control bus N(from 0 to 3)
	
- AV:STOP-CL-COLLIDER  
  just server-quit to scsynth

- GFX:DEFINE-SHADER  
  Define the shader program for raymarching. By default, the following uniform variables are defined:  
  - iresolution(vec2): Resolution of render.  
  - itime(float): Rendering time.  
  - camera(vec3): Position the camera's eye.  
  - lookat: Position the camera is looking at.  
  - ichannelN(sampler-2d): Texture unit input to the shader program. Currently 0 to 3 are provided.  
  - ivolume0(float): Audio data(main volume) from scsynth.  
  - icontrolN(float): Control data from scsynth. Currently 0 to 3 are provided.   

   ```lisp
   (gfx:define-shader view
	 (sl:with-uv (uv)
	   (let* ((c (length uv)))
		 (setf c (smoothstep .2 .1 c))
		 (v! (vec3 c) 1.0))))
   ```

- GFX:START-SHADER  
  Create a glfw window(Renderer) and start the shader program. If there is already a running glfw window, it changes the state of the renderer. Provides the following arguments for the state of the renderer:  
  - size: Resolution of glfw's window.
  - scene-ratio: Size ratio between glwfw window and renderer.
  - textures: Parameters for creating texture objects.
  - gl-canvas: Class name based on gl-canvas for custom drawing within the renderer.
  
   ```lisp
   (gfx:start-shader view
	 :size (800 600)
	 :scene-ratio 1.0
	 :textures ((:previous-frame)
			("PATH/TO/IMAGE")))
   ```
   
- GFX:CLEAR-PIPELINE  
  Initialize global state of shader program. Clear user-defined variables, functions, macros, structures.

- GFX:MOVE-CAMERA  
  Reset the state of camera in renderer.
