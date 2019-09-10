;;; libbasecampel.el --- Library to connect to basecamp 3 API -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://gitlab.petton.fr/basecampel/libbasecampel
;; Package-requires: ((emacs "26.1") (json-process-client "0.2.0"))
;; Version: 0.2.0

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
;; libbasecampel-proxy.el and the JS files in the proxy/ directory.

;;; Code:

(require 'libbasecampel-oauth)
(require 'libbasecampel-client)


;; Configuration

(defgroup libbasecampel nil
  "Configure libbasecampel to integrate Basecamp."
  :group 'external)

(defcustom libbasecampel-client-id nil
  "Set your basecamp client id here."
  :type 'string)

(defcustom libbasecampel-client-secret nil
  "Set your basecamp client secret here."
  :type 'string)

(defcustom libbasecampel-account-id nil
  "The account id to connect to.
This is the first number appearing after basecamp.com in the URL
when you are on the basecamp website."
  :type 'string)


;; Structures

(cl-defstruct (libbasecampel-entity
               (:constructor libbasecampel--entity-create)
               (:conc-name libbasecampel--entity-))
  (id nil :read-only t)
  (name nil :read-only t)
  (url nil :read-only t)
  (type nil :read-only t)
  (alist nil :read-only t))

(cl-defstruct (libbasecampel-project
               (:include libbasecampel-entity)
               (:constructor libbasecampel-project-create)
               (:conc-name libbasecampel-project-))
  (description nil :read-only t)
  (tools nil
         :read-only t
         :alist-key-name dock
         :alist-transformer (lambda (tools-data)
                              (libbasecampel--create-instances-from-data 'libbasecampel-tool tools-data))))

(cl-defstruct (libbasecampel-tool
               (:include libbasecampel-entity)
               (:constructor libbasecampel-tool-create)
               (:conc-name libbasecampel-tool-))
  (enabled nil
           :read-only t
           :alist-transformer (lambda (data) (not (eq data :json-false)))))

(cl-defstruct (libbasecampel-message
               (:include libbasecampel-entity
                         (name nil :alist-key-name subject))
               (:constructor libbasecampel-message-create)
               (:conc-name libbasecampel-message-))
  (content nil :read-only t))

(cl-defstruct (libbasecampel-todolist
               (:include libbasecampel-entity)
               (:constructor libbasecampel-todolist-create)
               (:conc-name libbasecampel-todolist-))
  (todos-url nil
             :read-only t
             :alist-key-name todos_url))

(cl-defstruct (libbasecampel-todo
               (:include libbasecampel-entity
                         (name nil :alist-key-name title))
               (:constructor libbasecampel-todo-create)
               (:conc-name libbasecampel-todo-))
  (description nil :read-only t))

(cl-defmethod libbasecampel-name ((entity libbasecampel-entity))
  (libbasecampel--entity-name entity))

(cl-defmethod libbasecampel-id ((entity libbasecampel-entity))
  (libbasecampel--entity-id entity))


;;; Private variables

(defvar libbasecampel--oauth-store nil
  "Remembers the OAuth authentication data.")


;;; Private functions

(defun libbasecampel--oauth-store ()
  "Return the OAuth authentication data."
  (or libbasecampel--oauth-store
      (setq libbasecampel--oauth-store (libbasecampel-oauth-get-store libbasecampel-client-id libbasecampel-client-secret))))

(defun libbasecampel--async-mapcar (mapfn list callback)
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

(defun libbasecampel--async-mapc (mapfn list callback)
  "Same as `navigel-async-mapcar' but for side-effects only.

MAPFN is a function taking 2 arguments: an element of LIST and a
callback.  MAPFN should call the callback with no argument when
done computing.

CALLBACK is a function of no argument that is called when done
computing for the all elements of LIST."
  (libbasecampel--async-mapcar
   (lambda (item callback) (funcall mapfn item (lambda () (funcall callback nil))))
   list
   (lambda (_result) (funcall callback))))

(defun libbasecampel--create-instance-from-data (struct-type entity-data)
  "Return an instance of a STRUCT-TYPE from ENTITY-DATA, an alist."
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
    (cdr (cl-struct-slot-info struct-type)))))

(defun libbasecampel--create-instances-from-data (struct-type entities-data)
  "Return a list of instances of a STRUCT-TYPE from ENTITIES-DATA, a list of alists."
  (mapcar (lambda (entity-data) (libbasecampel--create-instance-from-data struct-type entity-data))
          entities-data))

(defun libbasecampel-get-path (path &optional callback)
  "Execute CALLBACK with the result of a GET call to PATH."
  (libbasecampel-oauth-get-access-token
   (libbasecampel--oauth-store)
   (lambda (access-token)
     (libbasecampel-client-get-path access-token libbasecampel-account-id path callback))))

(defun libbasecampel-get-url (url callback)
  "Do a GET request on URL and evaluate CALLBACK with the result."
  (libbasecampel-oauth-get-access-token
   (libbasecampel--oauth-store)
   (lambda (access-token)
     (libbasecampel-client-get-url access-token url callback))))


;;; Public functions

(cl-defgeneric libbasecampel-children (entity callback)
  "Execute CALLBACK with the children of ENTITY as parameter.")

(cl-defmethod libbasecampel-children ((_entity (eql projects)) callback)
  "Execute CALLBACK with the list of all projects as parameter."
  (libbasecampel-get-path
   "/projects.json"
   (lambda (projects-data)
     (funcall callback
              (libbasecampel--create-instances-from-data
               'libbasecampel-project
               projects-data)))))

(cl-defmethod libbasecampel-children ((project libbasecampel-project) callback)
  (funcall
   callback
   (seq-filter
    #'libbasecampel-tool-enabled
    (libbasecampel-project-tools project))))

(defun libbasecampel--tool-child-struct-type (tool)
  "Return a struct-type to instanciate children of TOOL."
  (let ((type (libbasecampel-name tool)))
    (cond
     ((string= type "message_board") 'libbasecampel-message)
     ((string= type "todoset") 'libbasecampel-todolist)
     (t (user-error "Libbasecampel: unknown tool type `%s" type)))))

(defun libbasecampel--tool-child-url-key (tool)
  "Return the URL association key to fetch children of TOOL."
  (let ((type (libbasecampel-name tool)))
    (cond
     ((string= type "message_board") 'messages_url)
     ((string= type "todoset") 'todolists_url)
     (t (user-error "Libbasecampel: unknown tool type `%s" type)))))

(cl-defmethod libbasecampel-children ((tool libbasecampel-tool) callback)
  (libbasecampel-get-url
   (libbasecampel-tool-url tool)
   (lambda (tool-data)
     (libbasecampel-get-url
      (map-elt tool-data (libbasecampel--tool-child-url-key tool))
      (lambda (children-data)
        (funcall callback
                 (libbasecampel--create-instances-from-data
                  (libbasecampel--tool-child-struct-type tool)
                  children-data)))))))

(cl-defmethod libbasecampel-children ((todolist libbasecampel-todolist) callback)
  (libbasecampel-get-url
   (libbasecampel-todolist-todos-url todolist)
   (lambda (todos-data)
     (funcall callback (libbasecampel--create-instances-from-data 'libbasecampel-todo todos-data)))))

(cl-defmethod libbasecampel-children ((entities list) callback)
  (libbasecampel--async-mapcar
   #'libbasecampel-children
   entities
   callback))

(defun libbasecampel-completing-read (prompt entities &optional transformer)
  "PROMPT user to select one entity among ENTITIES.

Transform each entity to a string with TRANSFORMER,
`libbasecampel-name' if nil."
  (let* ((transformer (or transformer #'libbasecampel-name))
         (map (make-hash-table :test 'equal :size (length entities)))
         (entity-strings (mapcar (lambda (entity) (funcall transformer entity)) entities)))
    (cl-mapcar (lambda (entity entity-string)
                 (puthash entity-string entity map))
               entities entity-strings)
    (let ((entity-string (completing-read prompt entity-strings nil t)))
      (gethash entity-string map))))

(defun libbasecampel-completing-read-entity (function prompt entity &optional transformer)
  "Call FUNCTION after prompting for a child of ENTITY.

Pass PROMPT, the elements of ENTITY and TRANSFORMER to
`libbasecampel-completing-read'."
  (libbasecampel-children
   entity
   (lambda (entities)
     (funcall function
              (libbasecampel-completing-read prompt entities transformer)))))

(provide 'libbasecampel)
;;; libbasecampel.el ends here
