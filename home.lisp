(defpackage home
  (:use :cl :ppmimage :model))
(in-package home)

(defconstant +white+ (make-color 255 255 255 255))
(defconstant +red+ (make-color 255 0 0 255))
(defconstant +asset-dir+ "/home/kevin/git_repos/tinyrenderer/")

(defconstant +width+ 800)
(defconstant +height+ 800)

(defparameter *model* (format nil "~A~A" +asset-dir+ "obj/african_head.obj"))

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

    (let* ((dx (- x1 x0))
           (dy (- y1 y0))
           (derror2 (* 2 (abs dy)))
           (error2 0)
           (y y0))
      (loop for x from x0 to x1
            do 
            (if steep
                (setf (ppm-image-get image y x) color)
                (setf (ppm-image-get image x y) color))
            (incf error2 derror2)
            (when (> error2 dx)
              (if (> y1 y0)
                  (incf y)
                  (decf y))
              (decf error2 (* 2 dx)))))))

(defun draw-model (model image)
  (loop for i below (model-nfaces model)
        do
        (let ((face (model-face model i))
              (w/2 (ash +width+ -1))
              (h/2 (ash +height+ -1)))
          (loop for j below 3
                do
                (let* ((v0 (model-vert model (elt face j)))
                       (v1 (model-vert model (elt face (mod (1+ j) 3))))
                       (x0 (floor (* (1+ (vec3-x v0)) w/2)))
                       (y0 (floor (* (1+ (vec3-y v0)) h/2)))
                       (x1 (floor (* (1+ (vec3-x v1)) w/2)))
                       (y1 (floor (* (1+ (vec3-y v1)) h/2))))
                  (line x0 y0 x1 y1 image +white+))))))

(defun main ()
  (let ((image (make-ppm-image +width+ +height+ :RGB)))
    (draw-model (make-model *model*) image)
    (flip-vertically image) ; so that origin is at the left bottom corner...
    (write-ppm-image image +asset-dir+ "output.ppm")))

(main)

