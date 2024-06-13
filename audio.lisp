(in-package #:av)


(defvar *audio-signal-data*
  (list :ivolume0 0.0
	:icontrol0 0.0
	:icontrol1 0.0
	:icontrol2 0.0
	:icontrol3 0.0))


(defun get-data (param)
  "get audio/control data from cl-collider's audio server(scsynth).
support params:
  :ivolume0 - volume data to mix of 0 and 1 bus.
  :icontrolN(0..3) - data of control bus N(from 0 to 3)"
  (getf *audio-signal-data* param))

(defun add-monitor ()
  "add monitor synth and event handler to scsynth for get audio/control data."
  (sc:add-reply-responder
   "/signal-monitor"
   (lambda (node id volume control0 control1 control2 control3)
     (setf (getf *audio-signal-data* :ivolume0) volume
	   (getf *audio-signal-data* :icontrol0) control0
	   (getf *audio-signal-data* :icontrol1) control1
	   (getf *audio-signal-data* :icontrol2) control2
	   (getf *audio-signal-data* :icontrol3) control3)))
  (sc:proxy :signal-monitor-synth
    (let* ((trig (sc:impulse.kr 30)))
      (sc:send-reply.kr trig "/signal-monitor"
			(list (sc:peak.ar (sc:mix (sc:in.ar 0 2)) trig)
			      (sc:in.kr 0)
			      (sc:in.kr 1)
			      (sc:in.kr 2)
			      (sc:in.kr 3))))
    :pos :tail
    :to 0))

(defun run-cl-collider (&key (port 57140))
  "boot scsynth and set bpm(60) then call function `av:add-monitor'."
  (unless sc:*s*
    (setf sc:*s* (sc:make-external-server :audio-visaul :port port)))
  (unless (sc:boot-p sc:*s*)
    (sc:server-boot sc:*s*)
    (sc-extensions:bpm 60.0)
    (add-monitor)))

(defun stop-cl-collider ()
  "just server-quit to scsynth"
  (sc:server-quit sc:*s*))



