#!/usr/bin/guile  -s
!#
(use-modules (sdl2)
             (sdl2 render)
             (sdl2 surface)
             (sdl2 video)
			 (sdl2 image)
			 (sdl2 events)
			 (sdl2 input keyboard))

(define (draw renderer)
  (set-renderer-draw-color! renderer 255 255 255 255)

  (let* ((surface (load-image "guile.png"))
         (texture (surface->texture renderer surface)))
	
	(do ((event (poll-event) (poll-event)))
		((eq? '#t (quit-event? event)))
	  (clear-renderer renderer)
	  (render-copy renderer texture)
	  (present-renderer renderer))))

(sdl-init)

(call-with-window (make-window)
  (lambda (window)
    (call-with-renderer (make-renderer window) draw)))

(sdl-quit)
