(ql:quickload :cl-cffi-gtk)

; Main window
(defvar window (make-instance 'gtk:gtk-window :type :toplevel :title "GTK Test"))
(defvar vbox (make-instance 'gtk:gtk-box :orientation :vertical
                                         :spacing 10
                                         :margin 10))

(defvar text-panel-box (make-instance 'gtk:gtk-box :orientation :horizontal :spacing 1 :margin 10))
(defvar panel-left-window (make-instance 'gtk:gtk-scrolled-window :height-request 500 :width-request 100))
(defvar panel-left (make-instance 'gtk:gtk-text-view :height-request 300 :width-request 100 :wrap-mode :word-char :vscroll-policy :natural))
(defvar panel-right-window (make-instance 'gtk:gtk-scrolled-window :height-request 500 :width-request 300))
(defvar panel-right (make-instance 'gtk:gtk-text-view :height-request 300 :width-request 300 :wrap-mode :word-char :vscroll-policy :natural))

(gtk:gtk-box-pack-start vbox text-panel-box)
(gtk:gtk-container-add panel-left-window panel-left)
(gtk:gtk-box-pack-start text-panel-box panel-left-window)
(gtk:gtk-container-add panel-right-window panel-right)
(gtk:gtk-box-pack-end text-panel-box panel-right-window)

(gtk:within-main-loop
  ; Quit program when window closed
  (gobject:g-signal-connect window "destroy" (lambda (widget)
    (declare (ignore widget))
    (gtk:leave-gtk-main)))
  ; Display GUI
  (gtk:gtk-container-add window vbox)
  (gtk:gtk-widget-show-all window))

(loop
  (sleep 3)
(setf (gtk:gtk-text-buffer-text (gtk:gtk-text-view-buffer panel-right)) (format nil "~a~a" (gtk:gtk-text-buffer-text (gtk:gtk-text-view-buffer panel-right)) "
Hello, world!
")))
