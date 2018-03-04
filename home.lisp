(defpackage home
  (:use :cl :ppmimage))
(in-package home)

(defconstant +white+ (make-color 255 255 255 255))
(defconstant +red+ (make-color 255 0 0 255))
(defconstant +asset-dir+ "/home/kevin/git_repos/tinyrenderer/")

(defun line (x0 y0 x1 y1 image color)
  (let ((steep nil))
    (when (< (abs (- x0 x1))
           (abs (- y0 y1))) ; if the line is steep, transpose
        (setf steep t)
        (rotatef x0 y0)
        (rotatef x1 y1))
    (when (> x0 x1) ; make it left-to-right
        (rotatef x0 x1)
        (rotatef y0 y1))
    (loop for x from x0 to x1
          do 
          (let* ((i (/ (- x x0)
                       (- x1 x0)))
                 (y (floor (+ (* y0 (- 1 i)) (* y1 i)))))
            (if steep
                (setf (ppm-image-get image y x) color)
                (setf (ppm-image-get image x y) color))))))

(defun main ()
  (let ((image (make-ppm-image 100 100 :RGB)))
    (line 13 20 80 40 image +white+)
    (line 20 13 40 80 image +red+)
    (line 80 40 13 20 image +red+)
    (flip-vertically image) ; so that origin is at the left bottom corner...
    (write-ppm-image image +asset-dir+ "output.ppm")))

(main)

#|
(time
  (let ((image (make-ppm-image 100 100 :RGB)))
    (loop for i upto 1000000 do
          (line 13 20 80 40 image +white+)
          (line 20 13 40 80 image +red+)
          (line 80 40 13 20 image +red+))))

Evaluation took:
  126.117 seconds of real time
  126.249065 seconds of total run time (125.896683 user, 0.352382 system)
  [ Run times consist of 3.001 seconds GC time, and 123.249 seconds non-GC time. ]
  100.10% CPU
  336,071,662,843 processor cycles
  38,016,118,672 bytes consed

My CPU is Intel i7 9200 at 2.79 ghz.
This is without enabling any SBCL optimizations / declaring types.
I don't think it's too bad... so I won't continue optimizing at the moment.
#|
