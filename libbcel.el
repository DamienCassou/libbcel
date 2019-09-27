;;; libbcel.el --- Library to connect to basecamp 3 API -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://gitlab.petton.fr/bcel/libbcel
;; Package-requires: ((emacs "26.1") (request "0.3.1"))
;; Version: 0.4.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides a bunch of functions and structures to
;; connect to Basecamp 3 API.  The connection is handled by
;; libbcel-proxy.el and the JS files in the proxy/ directory.

;;; Code:

(require 'libbcel-oauth)
(require 'libbcel-client)
(require 'libbcel-structs)
(require 'libbcel-nav)


;; Configuration

(defgroup libbcel nil
  "Configure libbcel to integrate Basecamp."
  :group 'external)


;;; Private variables



;;; Private functions



(defun libbcel--async-mapcar (mapfn list callback)
  "Apply MAPFN to each element of LIST and pass result to CALLBACK.

MAPFN is a function taking 2 arguments: the element to map and a
callback to call when the mapping is done."
  (if (not list)
      (funcall callback nil)
    (let ((result (make-vector (length list) nil))
          (count 0))
      (cl-loop for index below (length list)
               for item in list
               do (let ((index index) (item item))
                    (funcall
                     mapfn
                     item
                     (lambda (item-result)
                       (setf (seq-elt result index) item-result)
                       (cl-incf count)
                       (when (eq count (length list))
                         ;; use `run-at-time' to ensure that CALLBACK is
                         ;; consistently called asynchronously even if MAPFN is
                         ;; synchronous:
                         (run-at-time
                          0 nil
                          callback
                          (seq-concatenate 'list result))))))))))

(defun libbcel--async-mapc (mapfn list callback)
  "Same as `navigel-async-mapcar' but for side-effects only.

MAPFN is a function taking 2 arguments: an element of LIST and a
callback.  MAPFN should call the callback with no argument when
done computing.

CALLBACK is a function of no argument that is called when done
computing for the all elements of LIST."
  (libbcel--async-mapcar
   (lambda (item callback) (funcall mapfn item (lambda () (funcall callback nil))))
   list
   (lambda (_result) (funcall callback))))


;;; Public functions

(defun libbcel-completing-read (prompt entities &optional transformer)
  "PROMPT user to select one entity among ENTITIES.

Transform each entity to a string with TRANSFORMER,
`libbcel-name' if nil."
  (let* ((transformer (or transformer #'libbcel-name))
         (map (make-hash-table :test 'equal :size (length entities)))
         (entity-strings (mapcar (lambda (entity) (funcall transformer entity)) entities)))
    (cl-mapcar (lambda (entity entity-string)
                 (puthash entity-string entity map))
               entities entity-strings)
    (let ((entity-string (completing-read prompt entity-strings nil t)))
      (gethash entity-string map))))

(defun libbcel-completing-read-entity (function prompt entity &optional transformer)
  "Call FUNCTION after prompting for a child of ENTITY.

Pass PROMPT, the elements of ENTITY and TRANSFORMER to
`libbcel-completing-read'."
  (libbcel-nav-children
   entity
   (lambda (entities)
     (funcall function
              (libbcel-completing-read prompt entities transformer)))))

(cl-defmethod libbcel-nav-children ((entities list) callback)
  (libbcel--async-mapcar
   #'libbcel-nav-children
   entities
   callback))

(provide 'libbcel)
;;; libbcel.el ends here
