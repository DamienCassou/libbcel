;;; libbcel-util-tests.el --- Tests for libbcel-util  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Damien Cassou

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

;; Tests for libbcel-util.

;;; Code:

(require 'libbcel-util)
(require 'ert)

(ert-deftest libbcel-util-tests-site-url-to-api-path ()
  (should (equal
           "/buckets/11111111/todos/1111111111.json"
           (libbcel-util-site-url-to-api-path
            "https://3.basecamp.com/1111111/buckets/11111111/todos/1111111111"))))

(provide 'libbcel-util-tests)
;;; libbcel-util-tests.el ends here
