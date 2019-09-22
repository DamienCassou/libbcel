;;; libbcel.el --- Library to connect to basecamp 3 API -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://gitlab.petton.fr/bcel/libbcel
;; Package-requires: ((emacs "26.1") (request "0.3.1"))
;; Version: 0.3.0

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


;; Configuration

(defgroup libbcel nil
  "Configure libbcel to integrate Basecamp."
  :group 'external)

(defcustom libbcel-account-id nil
  "The account id to connect to.
This is the first number appearing after basecamp.com in the URL
when you are on the basecamp website."
  :type 'string)


;; Structures

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
                              (libbcel--create-instances-from-data 'libbcel-tool tools-data))))

(cl-defstruct (libbcel-tool
               (:include libbcel-entity)
               (:constructor libbcel-tool-create)
               (:conc-name libbcel-tool-))
  (enabled nil
           :read-only t
           :alist-transformer (lambda (data) (not (eq data :json-false)))))

(cl-defstruct (libbcel-message
               (:include libbcel-entity
                         (name nil :alist-key-name subject))
               (:constructor libbcel-message-create)
               (:conc-name libbcel-message-))
  (content nil :read-only t))

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


;;; Private variables

(defvar libbcel--oauth-store nil
  "Remembers the OAuth authentication data.")


;;; Private functions

(defun libbcel--oauth-store ()
  "Return the OAuth authentication data."
  (or libbcel--oauth-store
      (setq libbcel--oauth-store (libbcel-oauth-get-store))))

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

(defun libbcel--create-instance-from-data (struct-type entity-data)
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

(defun libbcel--create-instances-from-data (struct-type entities-data)
  "Return a list of instances of a STRUCT-TYPE from ENTITIES-DATA, a list of alists."
  (mapcar (lambda (entity-data) (libbcel--create-instance-from-data struct-type entity-data))
          entities-data))

(defun libbcel-get-path (path &optional callback)
  "Execute CALLBACK with the result of a GET call to PATH."
  (libbcel-oauth-get-access-token
   (libbcel--oauth-store)
   (lambda (access-token)
     (libbcel-client-get-path access-token libbcel-account-id path callback))))

(defun libbcel-get-url (url callback)
  "Do a GET request on URL and evaluate CALLBACK with the result."
  (libbcel-oauth-get-access-token
   (libbcel--oauth-store)
   (lambda (access-token)
     (libbcel-client-get-url access-token url callback))))


;;; Public functions

(cl-defgeneric libbcel-children (entity callback)
  "Execute CALLBACK with the children of ENTITY as parameter.")

(cl-defmethod libbcel-children ((_entity (eql projects)) callback)
  "Execute CALLBACK with the list of all projects as parameter."
  (libbcel-get-path
   "/projects.json"
   (lambda (projects-data)
     (funcall callback
              (libbcel--create-instances-from-data
               'libbcel-project
               projects-data)))))

(cl-defmethod libbcel-children ((project libbcel-project) callback)
  (funcall
   callback
   (seq-filter
    #'libbcel-tool-enabled
    (libbcel-project-tools project))))

(defun libbcel--tool-child-struct-type (tool)
  "Return a struct-type to instanciate children of TOOL."
  (let ((type (libbcel-name tool)))
    (cond
     ((string= type "message_board") 'libbcel-message)
     ((string= type "todoset") 'libbcel-todolist)
     (t (user-error "Libbcel: unknown tool type `%s" type)))))

(defun libbcel--tool-child-url-key (tool)
  "Return the URL association key to fetch children of TOOL."
  (let ((type (libbcel-name tool)))
    (cond
     ((string= type "message_board") 'messages_url)
     ((string= type "todoset") 'todolists_url)
     (t (user-error "Libbcel: unknown tool type `%s" type)))))

(cl-defmethod libbcel-children ((tool libbcel-tool) callback)
  (libbcel-get-url
   (libbcel-tool-url tool)
   (lambda (tool-data)
     (libbcel-get-url
      (map-elt tool-data (libbcel--tool-child-url-key tool))
      (lambda (children-data)
        (funcall callback
                 (libbcel--create-instances-from-data
                  (libbcel--tool-child-struct-type tool)
                  children-data)))))))

(cl-defmethod libbcel-children ((todolist libbcel-todolist) callback)
  (libbcel-get-url
   (libbcel-todolist-todos-url todolist)
   (lambda (todos-data)
     (funcall callback (libbcel--create-instances-from-data 'libbcel-todo todos-data)))))

(cl-defmethod libbcel-children ((entities list) callback)
  (libbcel--async-mapcar
   #'libbcel-children
   entities
   callback))

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
  (libbcel-children
   entity
   (lambda (entities)
     (funcall function
              (libbcel-completing-read prompt entities transformer)))))

(provide 'libbcel)
;;; libbcel.el ends here
