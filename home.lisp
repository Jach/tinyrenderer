(defpackage home
  (:use :cl :ppmimage))
(in-package home)

(defconstant +white+ (make-color 255 255 255 255))
(defconstant +red+ (make-color 255 0 0 255))
(defconstant +asset-dir+ "/home/kevin/git_repos/tinyrenderer/")

(defun line (x0 y0 x1 y1 image color)
  (loop for i from 0 to 1 by 0.01
       do 
       (let ((x (floor (+ (* x0 (- 1 i)) (* x1 i))))
             (y (floor (+ (* y0 (- 1 i)) (* y1 i)))))
         (setf (ppm-image-get image x y) color))))

(defun main ()
  (let ((image (make-ppm-image 100 100 :RGB)))
    (line 1 1 90 40 image +white+)
    (flip-vertically image) ; so that origin is at the left bottom corner...
    (write-ppm-image image +asset-dir+ "output.ppm")))

(main)
