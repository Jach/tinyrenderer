(defpackage home
  (:use :cl :ppmimage))
(in-package home)

(defconstant +white+ (make-color 255 255 255 255))
(defconstant +red+ (make-color 255 0 0 255))
(defconstant +asset-dir+ "/home/kevin/git_repos/tinyrenderer/")

(defun main ()
  (let ((image (make-ppm-image 100 100 :RGB)))
    (setf (ppm-image-get image 52 41) +red+)
    (setf (ppm-image-get image 0 0) +white+)
    (flip-vertically image) ; so that origin is at the left bottom corner...
    (write-ppm-image image +asset-dir+ "output.ppm")))

(main)
