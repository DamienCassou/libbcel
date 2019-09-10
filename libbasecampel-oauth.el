;;; libbasecampel-oauth.el --- Connects to basecamp API through oauth  -*- lexical-binding: t; -*-

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

;; This file implements
;; https://github.com/basecamp/api/blob/master/sections/authentication.md#oauth-2-from-scratch.

;;; Code:

(require 'request)
(require 'json)


;;; Configuration

(defgroup libbasecampel-oauth nil
  "Group for OAuth authentication to Basecamp."
  :group 'libbasecampel)

(defcustom libbasecampel-oauth-store-filename (concat user-emacs-directory "libbasecampel-oauth.store")
  "Filename where Basecamp 3 OAuth tokens are stored."
  :type 'file)

(defcustom libbasecampel-oauth-local-http-port 9321
  "The port number used for the redirect uri.

This number should be specified when defining the integration on
the basecamp website."
  :type 'number)


;;; OAuth2 client protocol

(defun libbasecampel-oauth--kill-process (process)
  "Terminate the network PROCESS."
  (stop-process process)
  (delete-process process))

(defun libbasecampel-oauth--make-http-server (client-id client-secret callback)
  "Create a network process listening for HTTP connections.

The port the server listens to is
`libbasecampel-oauth-local-http-port'.

CLIENT-ID and CLIENT-SECRET are provided by basecamp for each
integration.

CALLBACK is executed with the authentication data if the OAUTH
authentication terminates successfully."
  (let ((http-server-process))
    (let ((kill-process-fn (lambda () (libbasecampel-oauth--kill-process http-server-process))))
      (setq http-server-process
            (make-network-process
             :server t
             :name "libbasecampel-oauth-http-server"
             :service libbasecampel-oauth-local-http-port
             :buffer (generate-new-buffer "*libbasecampel-oauth-http-server*")
             :filter (apply-partially
                      #'libbasecampel-oauth--http-server-filter
                      client-id
                      client-secret
                      (libbasecampel-oauth--redirect-uri)
                      kill-process-fn
                      callback))))))

(defun libbasecampel-oauth--open-browser (client-id redirect-uri)
  "Open the user's favorite web browser so s·he can authorize libbasecampel.

CLIENT-ID is provided by basecamp for each integration.

REDIRECT-URI is specified when creating a new integration.  It
should be a string such as \"http://localhost:9321\"."
  (browse-url
   (format "https://launchpad.37signals.com/authorization/new?type=web_server&client_id=%s&redirect_uri=%s"
           client-id
           redirect-uri)))

(defun libbasecampel-oauth--http-server-filter (client-id client-secret redirect-uri kill-process-fn callback process data)
  "Analyze DATA and continue the OAUTH process if DATA has a code.

CLIENT-ID and CLIENT-SECRET are provided by basecamp for each
integration.

REDIRECT-URI is specified when creating a new integration.  It
should be a string such as \"http://localhost:9321\".

KILL-PROCESS-FN is a function to be called to kill the HTTP server.

CALLBACK is executed with the authentication data if the OAUTH
authentication terminates successfully.

PROCESS is the HTTP process created to communicate with the HTTP
client which opened the connection."
  (save-match-data
    (with-temp-buffer
      (erase-buffer)
      (insert data)
      (setf (point) (point-min))
      (when (re-search-forward (rx bol "GET /?code=" (group-n 1 (+ (not (any " ")))) " ") nil t)
        (let ((code (match-string 1)))
          (libbasecampel-oauth--send-auth-request
           `((type . "web_server")
             (client_id . ,client-id)
             (redirect_uri . ,redirect-uri)
             (client_secret . ,client-secret)
             (code . ,code))
           (lambda (data)
             (funcall callback data)
             ;; stop the connection to the client:
             (stop-process process)
             (delete-process process)
             ;; prevent the server from
             ;; accepting new connections:
             (funcall kill-process-fn))
           kill-process-fn))))))

(defun libbasecampel-oauth--refresh-access-token (store callback)
  "Execute CALLBACK with a refreshed access token from STORE."
  (let* ((client-id (map-elt store :client-id))
         (client-secret (map-elt store :client-secret))
         (refresh-token (map-elt store :refresh-token)))
    (libbasecampel-oauth--send-auth-request
     `((type . "refresh")
       (refresh_token . ,refresh-token)
       (client_id . ,client-id)
       (redirect_uri . ,(libbasecampel-oauth--redirect-uri))
       (client_secret . ,client-secret))
     (lambda (data)
       (funcall callback data))
     (lambda ()
       (user-error "Failed to refresh basecamp access token")
       (funcall callback)))))

(defun libbasecampel-oauth--send-auth-request (params success failure)
  "Do a POST request with PARAMS on Basecamp auth URL.
Execute SUCCESS with data upon success, or FAILURE."
  (request
   "https://launchpad.37signals.com/authorization/token"
   :type "POST"
   :data ""
   :params params
   :parser #'json-read
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (funcall success data)))
   :error (cl-function (lambda (&rest _args)
                         (funcall failure)))))

(defun libbasecampel-oauth--redirect-uri ()
  "Generate a local redirect-uri from `libbasecampel-oauth-local-http-port'.

REDIRECT-URI is specified when creating a new integration.  It is
a string such as \"http://localhost:9321\"."
  (concat "http://localhost:" (number-to-string libbasecampel-oauth-local-http-port)))

(defun libbasecampel-oauth--fetch (store callback)
  "Get new tokens using credentials in STORE and pass them to CALLBACK."
  (let* ((client-id (map-elt store :client-id))
         (client-secret (map-elt store :client-secret)))
    (libbasecampel-oauth--make-http-server client-id client-secret callback)
    (libbasecampel-oauth--open-browser client-id (libbasecampel-oauth--redirect-uri))))


;;; Token storage

(defun libbasecampel-oauth--store-has-access-token-p (store)
  "Return non-nil if STORE has an access token."
  (map-contains-key store :access-token))

(cl-defun libbasecampel-oauth--store-save (store &key auth-token client-id client-secret)
  "Save AUTH-TOKEN within STORE."
  (map-put store :expires-in (or (map-elt auth-token 'expires_in)
                                 (map-elt store :expires-in)))
  (map-put store :access-token (or (map-elt auth-token 'access_token)
                                   (map-elt store :access-token)))
  (map-put store :refresh-token (or (map-elt auth-token 'refresh_token)
                                    (map-elt store :refresh-token)))
  (map-put store :client-id (or client-id
                                (map-elt store :client-id)))
  (map-put store :client-secret (or client-secret
                                    (map-elt store :client-secret)))
  (with-current-buffer (find-file-noselect libbasecampel-oauth-store-filename)
    (erase-buffer)
    (insert (format "%S" store))
    (save-buffer)))


;;; Public function

(defun libbasecampel-oauth-get-store (client-id client-secret)
  "Return a `store' where Basecamp tokens should be saved.

CLIENT-ID and CLIENT-SECRET are provided by basecamp for each
integration."
  (let ((store (if (file-readable-p libbasecampel-oauth-store-filename)
                   (with-current-buffer (find-file-noselect libbasecampel-oauth-store-filename)
                     (setf (point) (point-min))
                     (read (current-buffer)))
                 (make-hash-table :size 10))))
    (map-put store :client-id client-id)
    (map-put store :client-secret client-secret)
    store))

(defun libbasecampel-oauth-get-access-token (store callback)
  "Execute CALLBACK with an access-token using the credentials saved in STORE.
To create STORE, call `libbasecampel-oauth-get-store'."
  (let ((auth-token-callback
         (lambda (auth-token)
           (message "auth-token: %s" auth-token)
           (libbasecampel-oauth--store-save store :auth-token auth-token)
           (funcall callback (map-elt store :access-token)))))
    (if (libbasecampel-oauth--store-has-access-token-p store)
        (libbasecampel-oauth--refresh-access-token store auth-token-callback)
      (libbasecampel-oauth--fetch store auth-token-callback)))
  t)

(provide 'libbasecampel-oauth)
;;; libbasecampel-oauth.el ends here
