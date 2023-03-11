(define-module (barra infinite-loop))

(define-syntax infinite-loop
  (syntax-rules ()
	((infinite-loop procedure)
	 (do () (#f)
	   procedure))))
	

(export infinite-loop)
