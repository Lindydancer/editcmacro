;;; editcmacro-test-setup.el --- Setup and execute all tests.

;; Copyright (C) 2018 Anders Lindgren

;; Author: Anders Lindgren

;; This file is part of the `editcmacro' package.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package sets up a suitable enviroment for testing
;; `editcmacro', and executes the tests.
;;
;; Usage:
;;
;;   emacs -q -l editcmacro-test-setup.el
;;
;; Note that this package assumes that some packages are located in
;; specific locations.

;;; Code:

(setq inhibit-startup-screen t)

(defvar editcmacro-test-setup-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(dolist (dir '("." ".." "../../faceup"))
  (add-to-list 'load-path (concat editcmacro-test-setup-directory dir)))

(require 'editcmacro)
(require 'editcmacro-test-basic)

(if noninteractive
    (ert-run-tests-batch t)
  (ert t))

;;; editcmacro-test-setup.el ends here
