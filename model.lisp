(defpackage model
  (:use :cl)
  (:export vec3-x
           vec3-y
           vec3-z
           make-model
           model-nfaces
           model-face
           model-vert))
(in-package model)

(defstruct vec3
  x
  y
  z)

(defclass model ()
  ((nfaces :accessor model-nfaces :initarg :nfaces :initform 0)
   (faces :accessor model-faces :initarg :faces)
   (verts :accessor model-verts :initarg :verts)))

(defun parse-vert (line)
  "Expects line in format 'v some-float some-float some-float',
   returns a vec3 of the three floats."
  (let ((next-start 0)
        (vert (make-vec3)))
    (setf next-start (nth-value 1 (read-from-string line))) ; initial 'v'
    (multiple-value-bind (x next) 
      (read-from-string line nil nil :start next-start)
      (setf (vec3-x vert) x)
      (setf next-start next))
    (multiple-value-bind (y next) 
      (read-from-string line nil nil :start next-start)
      (setf (vec3-y vert) y)
      (setf next-start next))
    (multiple-value-bind (z next) 
      (read-from-string line nil nil :start next-start)
      (setf (vec3-z vert) z)
      (setf next-start next))
    vert))

(defun parse-face (line)
  ; I don't know why, but for some reason each number must be decreased by 1...
  ; is the thing that saves it not 0-is-first-index based?
  (let ((next-start 0)
        (face (make-array 3)))
    (setf next-start (nth-value 1 (read-from-string line))) ; initial 'f'
    (multiple-value-bind (first next)
      (read-from-string line nil nil :start next-start)
      (setf next-start next)
      (let* ((as-str (princ-to-string first))
             (section-div-idx (position #\/ as-str :test #'equal)))
        (setf (aref face 0) (1- (parse-integer (subseq as-str 0 section-div-idx))))))
    (multiple-value-bind (second next)
      (read-from-string line nil nil :start next-start)
      (setf next-start next)
      (let* ((as-str (princ-to-string second))
             (section-div-idx (position #\/ as-str :test #'equal)))
        (setf (aref face 1) (1- (parse-integer (subseq as-str 0 section-div-idx))))))
    (multiple-value-bind (third next)
      (read-from-string line nil nil :start next-start)
      (setf next-start next)
      (let* ((as-str (princ-to-string third))
             (section-div-idx (position #\/ as-str :test #'equal)))
        (setf (aref face 2) (1- (parse-integer (subseq as-str 0 section-div-idx))))))
    face))

(defun parse-vert-or-face (line verts faces)
  (when (plusp (length line))
    (case (char line 0)
      (#\v (vector-push-extend (parse-vert line) verts))
      (#\f (vector-push-extend (parse-face line) faces)))))

(defun make-model (obj-path)
  (let ((verts (make-array 1 :fill-pointer 0 :adjustable t))
        (faces (make-array 1 :fill-pointer 0 :adjustable t)))
    (with-open-file (file obj-path :direction :input :if-does-not-exist nil)
      (when file
        (loop for line = (read-line file nil)
              while line do
              (parse-vert-or-face line verts faces))))
    (make-instance 'model
                   :nfaces (length faces)
                   :faces faces
                   :verts verts)))

(defmethod model-face ((model model) face-idx)
  (elt (model-faces model) face-idx))

(defmethod model-vert ((model model) vert-idx)
  (elt (model-verts model) vert-idx))

