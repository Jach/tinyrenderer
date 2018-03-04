(defpackage ppmimage
  (:use :cl)
  (:export :make-color
           :make-ppm-image
           :ppm-image-get
           :flip-vertically
           :write-ppm-image))
(in-package ppmimage)

(defstruct (color
  (:constructor make-color (red green blue alpha))
  (:type (vector unsigned-byte)))
  (red)
  (green)
  (blue)
  (alpha))

(deftype image-foramt ()
  '(member :grayscale :rgb :rgba))

(defclass ppm-image ()
  ((data :initarg :data :type (vector))
   (width :initarg :width :type (integer 0 *))
   (height :initarg :height :type (integer 0 *))
   (format :initarg :format :type image-format)))

(defun make-ppm-image (width height format)
  ; note: internal array's width-height must be 1+, sigh...
  (make-instance 'ppm-image
                 :data (make-array `(,(1+ width) ,(1+ height)) :initial-element (make-color 0 0 0 0))
                 :width width
                 :height height
                 :format format))

(defmethod ppm-image-get ((image ppm-image) x y)
  (declare (type (integer 0 *) x)
           (type (integer 0 *) y))
  (with-slots (data) image
    (aref data x y)))

;(remove-method #'ppm-image-get (find-method #'ppm-image-get '() (list (find-class 'ppm-image) t t)))

(defmethod (setf ppm-image-get) (color (image ppm-image) x y)
  (declare (type (integer 0 *) x)
           (type (integer 0 *) y))
  (with-slots (data) image
    (setf (aref data x y) color)))

(defmethod flip-vertically ((image ppm-image))
  (with-slots (width height) image
    (loop for y below (floor (/ height 2))
          do
          (loop for x below width
                do
                (rotatef (ppm-image-get image x y)
                         (ppm-image-get image x (- height y 1)))))))


(defmethod write-ppm-image ((image ppm-image) write-dir write-file)
  (with-slots (width height) image
    (let ((path-str (format nil "~A/~A" write-dir write-file)))
      (with-open-file (file path-str :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
        (write-byte (char-code #\P) file)
        (write-byte (char-code #\6) file)
        (write-byte (char-code #\space) file)
        (write-sequence (map 'list #'char-code (write-to-string width)) file) ; width
        (write-byte (char-code #\space) file)
        (write-sequence (map 'list #'char-code (write-to-string height)) file) ; height
        (write-byte (char-code #\space) file)
        (write-byte (char-code #\2) file) ; max color val
        ; > 0, < 65536. If < 256, then rgb vals below must take up 1 byte each, otherwise 2 bytes.
        ; max of 1 is easy black and white, where pixel vals 0 is black and 1 is white
        ; max of 255 is what we'll use for now.
        (write-byte (char-code #\5) file)
        (write-byte (char-code #\5) file)
        (write-byte (char-code #\newline) file)

        (loop for y below height
              do
              (loop for x below width
                    do
                    (let ((color (ppm-image-get image x y)))
                      (write-byte (color-red color) file)
                      (write-byte (color-green color) file)
                      (write-byte (color-blue color) file))))))))

(map 'list #'char-code "bacon")

#| 
(defparameter *im* (make-ppm-image 5 5 :RGBA))
(ppm-image-get *im* 0 1)
(setf (ppm-image-get *im* 0 1) (make-color 255 255 255 255))
(setf (ppm-image-get *im* 4 4) (make-color 0 255 0 255))
*im*

(defconstant +asset-dir+ "/home/kevin/git_repos/tinyrenderer/")
(write-ppm-image *im* +asset-dir+ "test.ppm")
|#

(defmethod print-image ((image ppm-image))
  (with-slots (width height) image
    (loop for y below height
          do
          (loop for x below width
                do
                (princ (ppm-image-get image x y))
                (princ " "))
          (terpri))))
