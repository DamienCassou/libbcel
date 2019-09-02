;;; libbasecampel-proxy.el --- Part of libbasecampel discussing with the JS proxy  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://gitlab.petton.fr/basecampel/libbasecampel
;; Package-requires: ((emacs "26.1"))
;; Version: 0.1.0

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

;; Interact with the JS proxy in the proxy/ directory.

;;; Code:

(require 'subr-x)
(require 'json-process-client)

(defgroup libbasecampel-proxy nil
  "Configure how to connect to the JS proxy."
  :group 'libbasecampel)

(defun libbasecampel-proxy--find-executable ()
  "Return the basecamp executable file."
  (if-let ((lisp-filename (or load-file-name (buffer-file-name))))
      (let ((executable (thread-last
                            (file-name-directory lisp-filename)
                          (expand-file-name "proxy")
                          (expand-file-name "bin")
                          (expand-file-name "basecamp-proxy"))))
        (when (file-executable-p executable)
          executable))))

(defcustom libbasecampel-proxy-executable (libbasecampel-proxy--find-executable)
  "Process executable."
  :type 'file)

(defcustom libbasecampel-proxy--process-port 1234
  "The port to which Emacs should connect to the JS proxy."
  :type 'number)

(defvar libbasecampel-proxy--application nil
  "The client connection as returned by `json-process-client-start'.")

(defun libbasecampel-proxy-start (account-id callback)
  "Start a basecamp process and store it as the client process.
Pass ACCOUNT-ID to the process.  Evaluate CALLBACK once the proxy is started."
  (interactive (list (lambda () (message "libbasecampel-proxy is started"))))
  (when (libbasecampel-proxy-process-live-p)
    (user-error "A basecamp process is already running"))
  (let ((executable (executable-find libbasecampel-proxy-executable)))
    (unless executable
      (user-error "Cannot find the basecamp executable"))
    (setq libbasecampel-proxy--application
          (json-process-client-start-with-id
           :name "libbasecampel"
           :executable executable
           :port libbasecampel-proxy--process-port
           :started-regexp "Basecamp proxy listening"
           :tcp-started-callback callback
           :exec-callback #'libbasecampel-proxy--handle-message
           :debug "*libbasecampel-debug-log*"
           :args (list (number-to-string libbasecampel-proxy--process-port) account-id)))))

(defun libbasecampel-proxy-stop ()
  "Stop the libbasecampel process."
  (interactive)
  (json-process-client-stop libbasecampel-proxy--application)
  (setq libbasecampel-proxy--application nil))

(defun libbasecampel-proxy--handle-message (data callback)
  "Handle a proxy message with DATA.
If DATA is a successful response to a previously-sent message,
evaluate CALLBACK with the payload."
  (let-alist data
    (pcase .type
      ("error" (libbasecampel-proxy--handle-error .payload))
      ("success" (libbasecampel-proxy--handle-response .payload callback)))))

(defun libbasecampel-proxy--handle-error (payload)
  "Handle an error from the proxy.
PAYLOAD is an alist containing the details of the error."
  (let-alist payload
    (message "Libbasecampel proxy error: %s" .error)))

(defun libbasecampel-proxy--handle-response (payload callback)
  "Handle a response to a client request.
PAYLOAD contains the data of the response.

If CALLBACK is non-nil, evaluate it with the PAYLOAD."
  (when callback
    (funcall callback payload)))

(defun libbasecampel-proxy-process-live-p ()
  "Return non-nil if the proxy is running."
  (json-process-client-process-live-p libbasecampel-proxy--application))

(defun libbasecampel-proxy-send (message &optional callback)
  "Send MESSAGE to the proxy.
When CALLBACK is non-nil, evaluate it with the process response."
  (json-process-client-send
   libbasecampel-proxy--application
   message
   callback))

(defun libbasecampel-proxy-get-url (url callback)
  "Do a GET request on URL and evaluate CALLBACK with the result."
  (libbasecampel-proxy-send
   `(("type" . "meta")
     ("action" . "get")
     ("url" . ,url))
   callback))

(provide 'libbasecampel-proxy)
;;; libbasecampel-proxy.el ends here
