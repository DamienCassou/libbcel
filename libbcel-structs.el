;;; libbcel-structs.el --- Define Basecamp data structures  -*- lexical-binding: t; -*-

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

;; Define the structures and their accessors

;;; Code:

(require 'map)

(cl-defstruct (libbcel-entity
               (:constructor libbcel--entity-create)
               (:conc-name libbcel--entity-))
  (id nil :read-only t)
  (name nil :read-only t)
  (url nil :read-only t)
  (type nil :read-only t)
  (alist nil :read-only t))

(cl-defstruct (libbcel-project
               (:include libbcel-entity)
               (:constructor libbcel-project-create)
               (:conc-name libbcel-project-))
  (description nil :read-only t)
  (tools nil
         :read-only t
         :alist-key-name dock
         :alist-transformer (lambda (tools-data)
                              (libbcel-structs-create-instances-from-data #'libbcel-structs--tool-type tools-data))))

(cl-defstruct (libbcel-tool
               (:include libbcel-entity)
               (:constructor libbcel-tool-create)
               (:conc-name libbcel-tool-))
  (enabled nil
           :read-only t
           :alist-transformer (lambda (data) (not (eq data :json-false)))))

(cl-defstruct (libbcel-message-board
               (:include libbcel-tool)
               (:constructor libbcel-message-board-create)
               (:conc-name libbcel-message-board-)))

(cl-defstruct (libbcel-todoset
               (:include libbcel-tool)
               (:constructor libbcel-todoset-create)
               (:conc-name libbcel-todoset-)))

(cl-defstruct (libbcel-message
               (:include libbcel-entity
                         (name nil :alist-key-name subject))
               (:constructor libbcel-message-create)
               (:conc-name libbcel-message-))
  (content nil :read-only t)
  (comments-count 0 :read-only t :alist-key-name comments_count))

(cl-defstruct (libbcel-todolist
               (:include libbcel-entity)
               (:constructor libbcel-todolist-create)
               (:conc-name libbcel-todolist-))
  (todos-url nil
             :read-only t
             :alist-key-name todos_url))

(cl-defstruct (libbcel-todo
               (:include libbcel-entity
                         (name nil :alist-key-name title))
               (:constructor libbcel-todo-create)
               (:conc-name libbcel-todo-))
  (description nil :read-only t))

(cl-defmethod libbcel-name ((entity libbcel-entity))
  (libbcel--entity-name entity))

(cl-defmethod libbcel-id ((entity libbcel-entity))
  (libbcel--entity-id entity))

(cl-defmethod libbcel-structs-url ((entity libbcel-entity))
  (libbcel--entity-url entity))

(defun libbcel-structs-create-instance-from-data (struct-type entity-data)
  "Return an instance of a STRUCT-TYPE from ENTITY-DATA, an alist.

If STRUCT-TYPE is a function, pass it the current entity-data.
The return value must be a symbol representing the structure type
to instantiate."
  (let ((struct-type (if (functionp struct-type)
                         (funcall struct-type entity-data)
                       struct-type)))
    (when struct-type
      (apply
       #'record
       struct-type
       (mapcar
        (lambda (slot-info)
          (let* ((alist-key (or (plist-get slot-info :alist-key-name)
                                (car slot-info)))
                 (alist-value (if (eq alist-key 'alist)
                                  entity-data
                                (map-elt entity-data alist-key)))
                 (transformer (or (plist-get slot-info :alist-transformer)
                                  #'identity)))
            (funcall transformer alist-value)))
        (cdr (cl-struct-slot-info struct-type)))))))

(defun libbcel-structs-create-instances-from-data (struct-type entities-data)
  "Return a list of instances of a STRUCT-TYPE from ENTITIES-DATA.
ENTITIES-DATA is a list of alists.

STRUCT-TYPE is passed unchanged to
`libbcel--create-instance-from-data'."
  (seq-remove #'null
              (mapcar (lambda (entity-data)
                        (libbcel-structs-create-instance-from-data struct-type entity-data))
                      entities-data)))

(defun libbcel-structs--tool-type (tool-data)
  "Return a struct type to instanciate TOOL-DATA."
  (let ((type (intern (map-elt tool-data 'name))))
    (pcase type
      ('message_board 'libbcel-message-board)
      ('todoset 'libbcel-todoset)
      (_ nil))))

(provide 'libbcel-structs)
;;; libbcel-structs.el ends here