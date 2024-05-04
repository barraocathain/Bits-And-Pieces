;;;; Simple CD Database program:
(defvar *cd-database* nil "The current state of the CD database.")

(defun make-cd (title artist rating ripped)
  "Creates a CD record."
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-cd (cd)
  "Adds a CD record to the database."
  (push cd *cd-database*) (setf *cd-database* (sort *cd-database* #'cd-sort-by-title)))

(defun print-cd-database (&optional database)
  "Prints the current CD database, or, optionally, a passed in database."
  (if database
      (dolist (cd database)
	(format t "~{~a: ~10t~a~%~}~%" cd))
      (dolist (cd *cd-database*)
	(format t "~{~a: ~10t~a~%~}~%" cd))))
  
(defun prompt-read (prompt)
  "Prompts for a single line of user input."
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  "Creates a CD record using user input."
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  "Adds one or more CDs to the database with user input."
  (loop (add-cd (prompt-for-cd))
	(if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun cd-sort-by-title (a b)
  "Sorts two CD records alphabetically."
  (string< (getf a :title) (getf b :title)))

(defun save-cds (filename)
  "Saves the CD database to a file."
  (with-open-file 
      (file-output filename
	   :direction :output
	   :if-exists :supersede)
    (with-standard-io-syntax
      (print *cd-database* file-output))))

(defun read-cds (filename)
  "Loads a CD database from a file."
  (with-open-file
      (file-input filename)
    (with-standard-io-syntax
      (print-cd-database (setf *cd-database* (read file-input))))))

(defun delete-cds (selector-function)
  "Deletes CD records according to a selector function."
  (print-cd-database (setq *cd-database* (remove-if selector-function *cd-database*))))

(defun select (selector-function)
  "Select records from a database based on a selector function."
  (print-cd-database (remove-if-not selector-function *cd-database*)))

(defun update (selector-function &key title artist rating (ripped nil ripped-p))
  "Update selected records in a database."
  (setf *cd-database*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-function row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *cd-database*)))

(defun make-comparison-expression (field value)
  "Create a comparison function for a field and a value in a plist."
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  "Create a list of plist comparison functions."
  (loop while fields
     collecting (make-comparison-expression (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  "Create an expression where all plist comparisons must be true."
  `#'(lambda (row) (and ,@(make-comparisons-list clauses))))

;; Load up the CD database on startup:
(read-cds "~/.cds.db")
(print-cd-database)
