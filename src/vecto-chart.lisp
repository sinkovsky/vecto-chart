(cl:defpackage :vecto-chart
  (:use :cl :vecto :flexi-streams)
  (:export #:render-png-stream #:add-data #:with-line-chart #:draw-axis #:draw-grid))

(in-package :vecto-chart)

(defvar *current-chart* nil)

(defclass chart ()
  ((width :accessor chart-width :initarg :width :initform 400)
   (height :accessor chart-height :initarg :height :initform 300)
   (data :accessor chart-data :initarg :data :initform nil)
   (offset :accessor chart-offset :initarg :offset :initform 0)))

(defclass line-chart (chart)
  ((have-x-y :accessor chart-have-x-y :initform nil)
   (min-x :accessor chart-data-min-x :initform 0)
   (min-y :accessor chart-data-min-y :initform 0)
   (max-x :accessor chart-data-max-x :initform 0)
   (max-y :accessor chart-data-max-y :initform 0)
   (ratio-x :accessor chart-ratio-x :initform 1)
   (ratio-y :accessor chart-ratio-y :initform 1)
   (label-x :accessor chart-label-x :initform "t")
   (label-y :accessor chart-label-y :initform "value")
   (draw-labels-p :accessor chart-draw-labels-p :initform t)
   (line-width :accessor chart-line-width :initform 3)
   (draw-grid-p :initarg :draw-grid :accessor chart-draw-grid-p :initform nil)
   (draw-axis-p :initarg :draw-axis :accessor chart-draw-axis-p :initform t)))

(defclass pie-chart (chart)
  ())


(defmacro with-pie-chart ((&key width height) &body body)
 `(let ((*current-chart* (make-instance 'pie-chart
				      :width ,width
				      :height ,height)))
	(with-canvas (:width ,width :height ,height)
	  ,@body)))

(defmacro with-line-chart ((&key width height draw-grid draw-axis)
						   &body body)
  (let ((width-value-name (gensym))
		(height-value-name (gensym)))
	`(let ((,width-value-name ,width)
		   (,height-value-name ,height))
	   (let ((*current-chart*
			  (make-instance 'line-chart
							 :width ,width-value-name
							 :height ,height-value-name
							 :offset (* ,height-value-name 0.08)
							 :draw-grid ,draw-grid
							 :draw-axis ,draw-axis)))
		 (with-canvas (:width ,width-value-name :height ,height-value-name)
		   ,@body)))))


(defun hex->rgb (hex-color)
  (unless (string= (elt hex-color 0) "#")
	(error 'wrong-hex-color-format))
  (when (< (length hex-color) 7)
	(error 'wrong-hex-color-format))
  (list 
   (/ (parse-integer (subseq hex-color 1 3) :radix 16) 255.0)
   (/ (parse-integer (subseq hex-color 3 5) :radix 16) 255.0)
   (/ (parse-integer (subseq hex-color 5 7) :radix 16) 255.0)))


(defparameter +series-colors+
  (list "#edc240" "#afd8f8" "#cb4b4b" "#4da74d" "#9440ed"))

(defun draw-axis ()
  (draw-axis* *current-chart*))

(defun set-labels (labels)
  (set-labels* *current-chart* labels))

(defmethod set-labels* ((chart line-chart) labels)
  (with-slots (label-x label-y) chart
	(setf label-x (car labels))
	(setf label-y (cdr labels))))

(defmethod draw-axis* ((chart line-chart))
  (with-slots (width height offset draw-labels-p label-x label-y) chart

	;; drawing background
    (set-rgb-fill 0.97 0.97 0.97)
    (rectangle 0 0 width height)
    (fill-path)

	;; drawing axis
	(set-rgb-fill 0 0 0)
    (set-line-cap :round)
	(set-line-width 1)
    (move-to offset offset)
    (line-to offset (- height offset))
    (move-to offset offset)
    (line-to (- width offset) offset)

	;; drawing axis labels
	(when draw-labels-p
	  (let ((font (get-font "times.ttf")))
		(set-rgb-fill 0 0 0)
		(set-font font 15)
		(draw-string 3 (- height 15) label-y)
		(draw-string (- width 15) 3 label-x)))
	
	(stroke)))

(defun draw-grid ()
  (draw-grid* *current-chart*))

(defmethod draw-grid* ((chart line-chart))
  (with-slots (offset have-x-y min-x min-y max-x max-y data height width) chart
	  (let ((x-step nil)
			(y-step nil)
			(top-margin (- height offset))
			(right-margin (- width offset)))
		(if have-x-y
			(setf x-step (/ (- max-x min-x) 10.0))
			(setf x-step 2))
		(setf y-step (/ (- max-y min-y) 8.0))
		(with-graphics-state
		  (set-line-width 0.4)
		  (set-dash-pattern #(5 5) 5)

		  ;; drawing x-grid
		  (do ((step 1 (1+ step)))
			  ((>= step (/ max-x x-step)))
			(let (x)
			  (setf x (dx->x chart (+ min-x (* step x-step))))
			  (move-to x offset)
			  (line-to x top-margin)))

		  ;; drawing y-grid
		  (do ((step 1 (1+ step)))
			  ((>= step (/ max-y y-step)))
			(let (y)
			  (setf y (dy->y chart (+ min-y (* step y-step))))
			  (move-to offset y)
			  (line-to right-margin y)))
		  
		  (stroke)))))

(define-condition wrong-argument-structure (condition)
  ())

;;; helper that uses lexical variable as a chart instance
(defun add-data (data)
  (add-data* *current-chart* data))

;;; CHART method to add more data to chart
;;; It validates data for consistency and
;;; pushes it to data slot
(defmethod add-data* ((chart chart) data)
  ;; add data validation here

  ;; 1) checking if data is empty
  (if (eq data nil)
	  (cond ((every #'numberp data)
			 (setf (chart-have-x-y chart) nil))
			((every #'consp data)
			 (setf (chart-have-x-y chart) T))))

  ;; 2) checking what we have list of numbers or list of conses
  ;; and checking if this is consistent with data that we have
  (if (or 
	   (and (every #'numberp data) (chart-have-x-y chart))
	   (and (every #'consp data) (not (chart-have-x-y chart)))
	   (not (or (not (every #'numberp data)) (not (every #'consp data)))))
	  (error 'wrong-argument-structure))
  
  (push data (chart-data chart)))


;;; Macro to avoid lot of boilerplate code
;;; that generates CHART class method
(defmacro calculate-min-max* (&body body)
  `(defmethod calculate-min-max ((chart line-chart))
	 (unless (chart-have-x-y chart)
	   ,@(loop for fn in '(min max)
			  collect
			  `(setf (,(intern (format nil "CHART-DATA-~A-Y" fn)) chart)
					 (apply #',fn (apply #'append (chart-data chart)))))
	   (setf (chart-data-min-x chart) 1)
	   ;; maximum X is max length of data series
	   (setf (chart-data-max-x chart)
			 (apply #'max
					(mapcar #'length (chart-data chart)))))
	 (when (chart-have-x-y chart)
	   ,@(loop for fn in '(min max)
		   collect 
			 `(setf (,(intern (format nil "CHART-DATA-~A-X" fn)) chart)
					(apply #',fn
						   (apply #'car (apply #'append (chart-data chart)))))
		   collect
			 `(setf (,(intern (format nil "CHART-DATA-~A-Y" fn)) chart)
					 (apply #',fn
							(apply #'cdr
								   (apply #'append (chart-data chart)))))))))

;; generating method
(calculate-min-max*)

;;; VECTO method that calculates ratios for x and y axis
(defmethod calculate-ratios ((chart line-chart))
  (with-slots (width height offset ratio-x ratio-y min-x min-y max-x max-y) chart
	(setf ratio-x
		  (/ (- width
				(* 2 offset) ; from left and right
				10) ; offset from the end of X axis
			 (- max-x min-x)))
	(setf ratio-y
		  (/ (- height
				(* 2 offset) ; from top and bottom
				10) ; from the end of Y axis
			 (- max-y min-y )))))


;;; Method to convert data values to pixel values for X axis
(defmethod dx->x ((chart line-chart) x)
  (+ (chart-offset chart) 
	 (* (chart-ratio-x chart)
		(- x (chart-data-min-x chart)))))

;;; Method to convert data values to pixel values for Y axis
(defmethod dy->y ((chart line-chart) y)
  (+ (chart-offset chart)
	 (* (chart-ratio-y chart)
		(- y (chart-data-min-y chart)))))


;;; short helper
(defun render-png-stream ()
  (render-png-stream* *current-chart*))

;;; Main drawing method
;;; returns png data stream
(defmethod render-png-stream* ((chart line-chart))
  (with-slots (data have-x-y line-width draw-grid-p draw-axis-p) chart
	(let ((line-offset (/ line-width 2))
		  (next-color 0))
	  
	  ;; calculating min-max-values for all data
	  (calculate-min-max chart)
	  ;; calculate ratios for given chart
	  (calculate-ratios chart)

	  (when draw-axis-p
		(draw-axis))

	  (when draw-grid-p
		(draw-grid))

	  ;; iterating through data and drawing line
	  (loop for dataset in data do
		   (if have-x-y
			   (move-to
				(+ line-offset (dx->x chart (caar dataset)))
				(+ line-offset (dy->y chart (cadr dataset))))
			   (move-to
				(+ line-offset (dx->x chart 1))
				(+ line-offset (dy->y chart (car dataset)))))
		   (pop dataset)
		   (apply #'set-rgb-stroke
				  (hex->rgb (nth next-color +series-colors+)))
		   (incf next-color)
		   (set-line-width line-width)
		   (set-line-join :round)
		   (set-line-cap :round)
		   (let ((current-x 2))
			 (if have-x-y
				 (loop for item in dataset do
					  (line-to
					   (dx->x chart (car item))
					   (dy->y chart (cdr item))))
				 (loop for item in dataset do
					  (line-to
					   (dx->x chart current-x)
					   (dy->y chart item))
					(incf current-x))))
		   (stroke)))
	(flexi-streams:with-output-to-sequence (png-stream)
	  (save-png-stream png-stream)
	  png-stream)))

