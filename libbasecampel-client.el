;;; libbasecampel-client.el --- Handles connection to the Basecamp 3 API  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://gitlab.petton.fr/basecampel/libbasecampel
;; Package-requires: ((emacs "26.1"))
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

;; This file takes care of communicating with Basecamp 3 API.  The
;; authentication part is handled by `libbasecampel-oauth.el'.

;;; Code:

(require 'request)
(require 'json)

(defun libbasecampel-client-get-url (access-token url &optional callback)
  "Do a GET query to Basecamp 3 API at URL.

ACCESS-TOKEN is found in the result of the OAUTH2 authentication.
See `libbasecampel-oauth-get-access-token'.

When CALLBACK is non-nil, evaluate it with the response."
  (request
   url
   :type "GET"
   :headers `(("User-Agent" . "basecampel (damien@cassou.me)")
              ("Authorization" . ,(format "Bearer %s" access-token)))
   :parser #'json-read
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (funcall callback data)))))

(defun libbasecampel-client-get-path (access-token account-id path &optional callback)
  "Execute CALLBACK with the result of the GET call to PATH.

ACCESS-TOKEN can be retrieved with `libbasecampel-oauth-get-access-token'.

ACCOUNT-ID is the first number appearing after basecamp.com in
the URL when you are on the basecamp website."
  (libbasecampel-client-get-url
   access-token
   (format "https://3.basecampapi.com/%s/%s"
           account-id
           path)
   callback))

(provide 'libbasecampel-client)
;;; libbasecampel-client.el ends here
