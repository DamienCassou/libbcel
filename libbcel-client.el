;;; libbcel-client.el --- Handles connection to the Basecamp 3 API  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://gitlab.petton.fr/bcel/libbcel
;; Package-requires: ((emacs "26.1"))
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

;; This file takes care of communicating with Basecamp 3 API.  The
;; authentication part is handled by `libbcel-oauth.el'.

;;; Code:

(require 'request)
(require 'json)

(require 'libbcel-oauth)

(defcustom libbcel-client-account-id nil
  "The account id to connect to.
This is the first number appearing after basecamp.com in the URL
when you are on the basecamp website."
  :type 'string
  :group 'libbcel)

(defvar libbcel-client--oauth-store nil
  "Remembers the OAuth authentication data.")

(defun libbcel-client--oauth-store ()
  "Return the OAuth authentication data."
  (or libbcel-client--oauth-store
      (setq libbcel-client--oauth-store (libbcel-oauth-get-store))))

(defun libbcel-client--get-url-from-token (access-token url &optional callback)
  "Do a GET query to Basecamp 3 API at URL.

ACCESS-TOKEN is found in the result of the OAUTH2 authentication.
See `libbcel-oauth-get-access-token'.

When CALLBACK is non-nil, evaluate it with the response."
  (request
   url
   :type "GET"
   :headers `(("User-Agent" . "bcel (damien@cassou.me)")
              ("Authorization" . ,(format "Bearer %s" access-token)))
   :parser #'json-read
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (funcall callback data)))))

(defun libbcel-client--get-path-from-token (access-token account-id path &optional callback)
  "Execute CALLBACK with the result of the GET call to PATH.

ACCESS-TOKEN can be retrieved with `libbcel-oauth-get-access-token'.

ACCOUNT-ID is the first number appearing after basecamp.com in
the URL when you are on the basecamp website."
  (libbcel-client--get-url-from-token
   access-token
   (format "https://3.basecampapi.com/%s/%s"
           account-id
           path)
   callback))

(defun libbcel-client-get-path (path &optional callback)
  "Execute CALLBACK with the result of a GET call to PATH."
  (libbcel-oauth-get-access-token
   (libbcel-client--oauth-store)
   (lambda (access-token)
     (libbcel-client--get-path-from-token access-token libbcel-client-account-id path callback))))

(defun libbcel-client-get-url (url callback)
  "Do a GET request on URL and evaluate CALLBACK with the result."
  (libbcel-oauth-get-access-token
   (libbcel-client--oauth-store)
   (lambda (access-token)
     (libbcel-client--get-url-from-token access-token url callback))))

(provide 'libbcel-client)
;;; libbcel-client.el ends here
