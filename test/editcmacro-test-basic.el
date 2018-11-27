;;; editcmacro-test-basic.el --- Basic tests for editcmacro.el

;; Copyright (C) 2018  Anders Lindgren

;; Author: Anders Lindgren

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

;; Basic tests for editcmacro.el

;; Manual test steps.
;;
;; Unfortunately, some things are hard to test automatically.  Before
;; releasing this package, test the following:
;;
;; - Check that the location of the cursor is retained when starting
;;   to edit a macro, and when writing the macro back.  Test this with
;;   a frame with a single window, a frame with two windows containing
;;   the source buffer, and a frame with two windows containing two
;;   different buffers.

;;; Code:

(require 'ert)

(require 'faceup)

(defun editcmacro-test-keys-to-events (keys)
  (mapcar (lambda (key)
            ;; `kbd' returns "" when applied to " ".
            (let ((k (if (equal key " ")
                         32
                       (kbd key))))
              (cons t
                    (cond ((stringp k)
                           (string-to-char k))
                          ((vectorp k)
                           (aref k 0))
                          (t
                           k)))))
          keys))

(defun editcmacro-test-edit-macro (macro value)
  (with-temp-buffer
    (c-mode)
    (setq-local indent-tabs-mode nil)
    (insert macro)
    (goto-char (point-min))
    (let ((edit-buffer (save-excursion
                         (editcmacro-edit-current-macro))))
      (faceup-test-equal (with-current-buffer edit-buffer
                           (buffer-string))
                         value))))
(faceup-defexplainer editcmacro-test-edit-macro)


(defun editcmacro-test-write-back-macro (macro &optional result &rest keys)
  (unless result
    (setq result macro))
  (with-temp-buffer
    (c-mode)
    (setq-local indent-tabs-mode nil)
    (insert macro)
    (let ((edit-buffer (save-excursion
                         (editcmacro-edit-current-macro))))
      (with-current-buffer edit-buffer
        (let ((unread-command-events
               (append
                (editcmacro-test-keys-to-events keys)
                ;; Uncomment the following line to stay in recursive edit.
                ;;
                ;; This is a vector, but after being appled to `append' only
                ;; the elements remain.
                (kbd "C-M-c")
                unread-command-events)))
          (recursive-edit))
        (editcmacro-write-back))
      (faceup-test-equal (buffer-string) result))))
(faceup-defexplainer editcmacro-test-write-back-macro)


(ert-deftest editcmacro-test-edit ()
  (should (editcmacro-test-edit-macro "#define NAME VALUE"
                                      "VALUE\n"))
  (should (editcmacro-test-edit-macro "#define NAME VALUE\n"
                                      "VALUE\n"))
  (should (editcmacro-test-edit-macro "#define NAME VALUE\n// other stuff"
                                      "VALUE\n"))
  (should (editcmacro-test-edit-macro "#define NAME     \\
MULTI \\
LINE"
                                      "MULTI\nLINE\n"))
  (should (editcmacro-test-edit-macro "\
#define NAME X   \\
             Y"
                                      "X\nY\n"))
  (should (editcmacro-test-edit-macro "\
#define NAME X   \\
        Y"
                                      "X\nY\n"))
  (should (editcmacro-test-edit-macro "\
#define NAME X   \\
                 Y"
                                      "X\n    Y\n"))
  (should (editcmacro-test-edit-macro "\
#define NAME                                    \\
  while (1)                                     \\
  {                                             \\
    g(3);                                       \\
  }"
                                      "\
while (1)
{
  g(3);
}\n"))
  nil)


(ert-deftest editcmacro-test-write-back ()
  (should (editcmacro-test-write-back-macro
           "#define NAME VALUE"
           "#define NAME VALUE"))
  (should (editcmacro-test-write-back-macro
           "#define NAME VALUE"
           "#define NAME VALUEX"
           "X"))
  (should (editcmacro-test-write-back-macro
           "#define NAME VALUE"
           "#define NAME XVALUE"
           "C-a"
           "X"))
  ;; Blank lines at the end should be ignored.
  (should (editcmacro-test-write-back-macro
           "#define NAME VALUE"
           "#define NAME VALUE"
           "RET"
           "RET"
           " "
           "RET"
           " "))
  ;; Handle empty macros.
  (should (editcmacro-test-write-back-macro
           "#define NAME"
           "#define NAME"))
  (should (editcmacro-test-write-back-macro
           "#define NAME"
           "#define NAME X"
           "X"))
  (should (editcmacro-test-write-back-macro
           "#define NAME"
           "#define NAME XX"
           "X"
           "X"))
  (should (editcmacro-test-write-back-macro
           "#define NAME"
           "\
#define NAME X                                  \\
  X"
           "X"
           "RET"
           "X"))
  ;; Handle backslashes.
  (should (editcmacro-test-write-back-macro
           "\
#define FOO \\
  this is a long line \\
  short \\
  the end"
           "\
#define FOO                                     \\
  this is a long line                           \\
  short                                         \\
  the end"))
  nil)


(provide 'editcmacro-test-basic)

;;; editcmacro-test-basic.el ends here
