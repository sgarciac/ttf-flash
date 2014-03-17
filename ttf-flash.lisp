(in-package #:cl-user)

(defpackage #:ttf-flash
    (:use #:cl #:gordon)
      (:export
	#:generate-flash-font   
	#:generate-flash-font-file
	      ))

(in-package #:ttf-flash)

(defun glyph-to-swf-contours (glyph ratio)
  (let ((contours '()
	  ))
    (labels ((x-trans (x) (floor (* x ratio)))
	     (y-trans (y) (* -1 (floor (* y ratio))))
	     )
      (zpb-ttf:do-contours (contour glyph)
	(setf contours
	      (nconc contours
		     (list
		      (let ((points '()
			      ))
			(zpb-ttf:do-contour-segments (s c e) contour
			  (setf points
				(nconc
				 points
				 (list
				  (if c
				      (list
				       (x-trans  (- (zpb-ttf:x c)
						    (zpb-ttf:x s)))
				       
				       (y-trans (- (zpb-ttf:y c)
						   (zpb-ttf:y s)))

				       (x-trans 
					(- (zpb-ttf:x e)
					   (zpb-ttf:x c)))
				       
				       (y-trans 
					(- (zpb-ttf:y e)
					   (zpb-ttf:y c)))
				       )
				      (list
				       (x-trans 
					(- (zpb-ttf:x e)
					   (zpb-ttf:x s)
					   ))
				       (y-trans 
					(- (zpb-ttf:y e)
					   (zpb-ttf:y s))))
				      )))))
		      
			(close-contour  (cons (list (x-trans (zpb-ttf:x (aref contour 0))) (y-trans (zpb-ttf:y (aref contour 0))))  points))
			))))))
    contours
    ))


(defun generate-flash-font (ttf-path name default-id &optional (max-glyphs 10000))
  (zpb-ttf:with-font-loader (loader ttf-path)
       (let ((ratio (/ 1024 (zpb-ttf:units/em loader)))
	     (unicodes (sort 
			(loop for i from 0 upto (min max-glyphs (1- (zpb-ttf:glyph-count loader)))
			      when (zpb-ttf:index-glyph i loader) collect (zpb-ttf:code-point (zpb-ttf:index-glyph i loader))) #'<)))
	 
	 
     (make-instance 'tag-define-font-2
      :id default-id
      :name (zpb-ttf:name-entry-value :FULL-NAME loader)
      :shapes (loop for code in unicodes
		    for glyph = (zpb-ttf:find-glyph code loader) then (zpb-ttf:find-glyph code loader)
	      collect (make-instance 'shape :records (mapcan (lambda (x) (make-records-from-contour x :linestyle 0)) (glyph-to-swf-contours glyph ratio))))
      :codes unicodes
      :ascent (floor (* ratio (zpb-ttf:ascender loader))) 
      :descent (floor (* ratio -1 (zpb-ttf:descender loader)))
      :advance (loop for code in unicodes
		     for glyph = (zpb-ttf:find-glyph code loader) then (zpb-ttf:find-glyph code loader)
		     collect (floor (* ratio (zpb-ttf:advance-width glyph))) 
		     )
      ))))


(defun generate-flash-font-file (ttf-path flash-font-path name default-id &optional (max-glyphs 1000000))
  (with-open-file (stream flash-font-path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-sequence 
     (flat-marshall-data (marshall (generate-flash-font ttf-path name default-id max-glyphs)))
     stream
     )))

;
;(ttf-flash:generate-flash-font-file "/home/sergio/dev/flash/ttf-flash2/freeserif.ttf" "radio.fo" "_sans" 64000 500)

