;;; libbcel-nav.el --- Code to navigate Basecamp entities  -*- lexical-binding: t; -*-

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

;; This file contains code to go from an entity to another one.  For
;; example, the file contains `libbcel-nav-children' to list the
;; direct children of an entity (e.g., the messages of a message
;; board).

;;; Code:

(require 'libbcel-structs)
(require 'libbcel-client)
(require 'libbcel-util)

(cl-defgeneric libbcel-nav-children (entity callback)
  "Execute CALLBACK with the children of ENTITY as parameter.")

(cl-defmethod libbcel-nav-children ((_entity (eql projects)) callback)
  "Execute CALLBACK with the list of all projects as parameter."
  (libbcel-client-get-path
   "/projects.json"
   (lambda (projects-data)
     (funcall callback
              (libbcel-structs-create-instances-from-data
               'libbcel-project
               projects-data)))))

(cl-defmethod libbcel-nav-children ((project libbcel-project) callback)
  (funcall
   callback
   (seq-filter
    #'libbcel-tool-enabled
    (libbcel-project-tools project))))

(cl-defmethod libbcel-nav-children ((tool libbcel-tool) callback)
  (libbcel-client-get-url
   (libbcel-entity-url tool)
   (lambda (tool-data)
     (libbcel-client-get-url
      (map-elt tool-data (libbcel-nav--tool-child-url-key tool))
      (lambda (children-data)
        (funcall callback
                 (libbcel-structs-create-instances-from-data
                  (libbcel-nav--tool-child-struct-type tool)
                  children-data)))))))

(cl-defmethod libbcel-nav-children ((todolist libbcel-todolist) callback)
  (libbcel-util-async-mapcar
   (lambda (params partial-callback)
     (libbcel-client-get-url
      (libbcel-todolist-todos-url todolist)
      (lambda (todos-data)
        (funcall partial-callback (libbcel-structs-create-instances-from-data 'libbcel-todo todos-data)))
      params))
   (list nil '((completed . "true")))
   (lambda (todos)
     (funcall callback (apply #'seq-concatenate 'list todos)))))


;;; Private functions

(cl-defgeneric libbcel-nav--tool-child-struct-type (tool)
  "Return a symbol representing the structure type to instanciate for children of TOOL.")

(cl-defmethod libbcel-nav--tool-child-struct-type ((_tool libbcel-message-board))
  'libbcel-message)

(cl-defmethod libbcel-nav--tool-child-struct-type ((_tool libbcel-todoset))
  'libbcel-todolist)

(cl-defgeneric libbcel-nav--tool-child-url-key (tool)
  "Return a symbol representing which field of TOOL contains the url to fetch its children.")

(cl-defmethod libbcel-nav--tool-child-url-key ((_tool libbcel-message-board))
  'messages_url)

(cl-defmethod libbcel-nav--tool-child-url-key ((_tool libbcel-todoset))
  'todolists_url)

(provide 'libbcel-nav)
;;; libbcel-nav.el ends here
