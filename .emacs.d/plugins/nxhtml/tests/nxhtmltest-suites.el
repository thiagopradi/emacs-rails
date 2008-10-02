;;; nxhtmltest-suites.el --- Test suites for mumamo / nXhtml
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-07-08T20:17:36+0200 Tue
;; Version: 0.11
;; Last-Updated: 2008-08-08T03:26:35+0200 Fri
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Defines `nxhtmltest-run-ert'.  When (getenv "nxhtmltest-run-Q")
;; returns non-nil also runs this function.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; Added code from Christian Ohler for writing ert tests.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))

(setq debug-on-error t)

(defvar nxhtmltest-bin
  (file-name-directory (if load-file-name load-file-name buffer-file-name)))

(pushnew nxhtmltest-bin load-path)
(require 'nxhtmltest-helpers)
(require 'ert)

(defvar nxhtmltest-files-root
  (let* ((this-dir nxhtmltest-bin)
         (root (expand-file-name "in/" this-dir)))
    (unless (file-accessible-directory-p root)
          (error (if (file-exists-p root)
                     "Can't read files in test directory %s"
                   "Can't find test directory %s")
                 root))
    root))

(let ((distr-in "c:/EmacsW32/nxml/tests/in/"))
  (when (file-directory-p distr-in)
    (setq nxhtmltest-files-root distr-in)))

(setq nxhtmltest-update-method
      ;;'font-lock-wait
      'font-lock-run-timers
      ;;'font-lock-fontify-buffer
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define tests using ert.el

(ert-deftest nxhtml-ert-opened-modified ()
  "Test if buffer get modified when opening a file."
  (let ((file1 (nxhtml-get-test-file-name "cvd-080805-ac.php"))
        (file2 (nxhtml-get-test-file-name "cvd-080805-cc.php"))
        buf1
        buf2)
    ;; Ensure we open the files
    (setq buf1 (find-buffer-visiting file1))
    (when buf1 (kill-buffer buf1))
    (setq buf2 (find-buffer-visiting file2))
    (when buf2 (kill-buffer buf2))
    ;; Open file 1
    (setq buf1 (find-file file1))
    (nxhtmltest-fontify-default-way 2 "mod")
    (nxhtmltest-should-no-mumamo-errors)
    ;; Open file 2
    (setq buf2 (find-file file2))
    (nxhtmltest-fontify-default-way 2 "mod")
    (nxhtmltest-should-no-mumamo-errors)
    (ert-should
     (not (or (buffer-modified-p buf1)
              (buffer-modified-p buf2))))
    (kill-buffer buf1)
    (kill-buffer buf2)))

(ert-deftest nxhtml-ert-wiki-strange-hili-080629 ()
  "From a report on EmacsWiki."
  (nxhtmltest-with-persistent-buffer "wiki-strange-hili-080629.html"
    (assert (not font-lock-mode) t "%s %s" "font-lock on before")
    (nxhtml-mumamo)
    (assert (not font-lock-mode) t "%s %s" "font-lock on after")
    (nxhtmltest-fontify-default-way 2 "hili")
    (goto-char 44)
    (nxhtmltest-should-no-mumamo-errors)
    (ert-should
     (eq (get-text-property 44 'face)
         'font-lock-function-name-face))))

(ert-deftest nxhtml-ert-wiki-080708-ind-problem ()
  (nxhtmltest-with-persistent-buffer "wiki-080708-ind-problem.rhtml"
    (require 'ruby-mode nil t)
    (if (not (featurep 'ruby-mode))
        ;; Fix-me: ert should maybe have some way to just display
        ;; informational messages?
        (error "ruby-mode not available, skipping test")
      (assert (not font-lock-mode))
      (eruby-nxhtml-mumamo)
      (assert (not font-lock-mode))
      (nxhtmltest-fontify-default-way 2 "ind")
      (mark-whole-buffer)
      (indent-for-tab-command)
      (goto-line 3)
      (nxhtmltest-should-no-mumamo-errors)
      (ert-should (= (current-indentation) 0)))))

(ert-deftest nxhtml-ert-wiki-080708-ind-problem-a ()
  (nxhtmltest-with-persistent-buffer "wiki-080708-ind-problem.rhtml"
    (require 'ruby-mode nil t)
    (if (not (featurep 'ruby-mode))
        (error "ruby-mode not available, skipping test")
      (assert (not font-lock-mode))
      (eruby-nxhtml-mumamo)
      (assert (not font-lock-mode))
      (nxhtmltest-fontify-default-way 2 "ind")
      (insert "  ")
      (mark-whole-buffer)
      (indent-for-tab-command)
      (goto-line 3)
      ;; Test
      (nxhtmltest-should-no-mumamo-errors)
      (ert-should (= (current-indentation) 2)))))

(ert-deftest nxhtml-ert-sheit-2007-12-26 ()
  (nxhtmltest-with-persistent-buffer "sheit-2007-12-26.php"
    (assert (not font-lock-mode))
    (nxhtml-mumamo)
    (assert (not font-lock-mode))
    (nxhtmltest-fontify-default-way 2 "sheit")
    (nxhtmltest-should-no-mumamo-errors)
    (ert-should
     (and
      (eq (get-text-property 21 'face)
          'font-lock-comment-face)
      (eq (get-text-property 22 'face)
          'font-lock-comment-face)
      (eq (get-text-property 35 'face)
          'font-lock-comment-face)))))


;; Now some tests with a big file. Jumping backwards can fail.

(defun nxhtml-ert-nxhtml-changes-jump-back-2 (pos)
 (assert (not font-lock-mode))
  (nxhtml-mumamo)
  (assert (not font-lock-mode))
  (goto-char (- (point-max) (- 64036 63869)))
  (nxhtmltest-fontify-default-way 2)
  (nxhtmltest-should-no-mumamo-errors)
  (ert-should
   (eq (get-text-property (point) 'face)
       'font-lock-variable-name-face))
  (goto-char pos)
  (nxhtmltest-fontify-default-way 2)
  (nxhtmltest-should-no-mumamo-errors)
  (ert-should
   (eq (get-text-property (point) 'face)
       'font-lock-function-name-face)))

(ert-deftest nxhtml-ert-nxhtml-changes-jump-back-7033-2 ()
  "this is a docstring.
wonder how that works now ..."
  (nxhtmltest-with-persistent-buffer "../../nxhtml/doc/nxhtml-changes.html"
    (nxhtml-ert-nxhtml-changes-jump-back-2 7033)))

(ert-deftest nxhtml-ert-nxhtml-changes-jump-back-10547-2 ()
  (nxhtmltest-with-persistent-buffer "../../nxhtml/doc/nxhtml-changes.html"
    (nxhtml-ert-nxhtml-changes-jump-back-2 10547)))

(ert-deftest nxhtml-ert-nxhtml-changes-jump-2 ()
  (nxhtmltest-with-persistent-buffer "../../nxhtml/doc/nxhtml-changes.html"
    (assert (not font-lock-mode))
    (nxhtml-mumamo)
    (assert (not font-lock-mode))
    (goto-char 10549)
    (nxhtmltest-fontify-default-way 2 "jump-2")
    (nxhtmltest-should-no-mumamo-errors)
    (ert-should
     (eq (get-text-property (point) 'face)
         'font-lock-variable-name-face))))

;;; Indentation tests

(ert-deftest nxhtml-ert-php-indent-bug-1 ()
  "Test indentation in php only file.
The indentation on line 7 should be 0."
  (nxhtmltest-with-persistent-buffer "only-php.php"
    (nxhtml-mumamo)
    ;; No fontification needed for indentation.
    (goto-line 7)
    (indent-for-tab-command)
    (nxhtmltest-should-no-mumamo-errors)
    (ert-should
     (= 0
        (current-indentation)))))

;;; Scroll tests

(ert-deftest nxhtml-ert-scroll-jump-test ()
  "Test if `scroll-conservatively' eq 1 works."
  (nxhtmltest-with-persistent-buffer "../../nxhtml/doc/nxhtml-changes.html"
    (assert (not font-lock-mode))
    (nxhtml-mumamo)
    (assert (not font-lock-mode))
    (nxhtmltest-fontify-default-way 2 "jump-2")
    (let ((scroll-conservatively 1)
          (ws (list (window-start)))
          (xi (loop for ii from 1 to 100 by 1
                    do
                    (next-line)
                    (sit-for 0.01)
                    collect (list (window-start)
                                  (let ((here (point)))
                                    (goto-char (window-start))
                                    (prog1 (line-end-position)
                                      (goto-char here)))
                                  (point))
                    ))
          (jumps 0)
          prev-win-start
          prev-win-start-le
          )
      (loop for xx in xi
            do
            (message "xx=%s" xx)
            (let ((win-start (nth 0 xx))
                  (win-start-le (nth 1 xx))
                  (cur-point (nth 2 xx)))
              (unless (or (not prev-win-start)
                          (= prev-win-start win-start)
                          (= (1+ prev-win-start-le) win-start))
                (setq jumps (1+ jumps)))
              (setq prev-win-start win-start)
              (setq prev-win-start-le win-start-le)
              )
            )
      (ert-should (= 0 jumps))
      )))

(defvar ert-error-on-test-redefinition nil)

;;; End of test definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun nxhtmltest-run-ert ()
  "Run test with ert library."
  (ert-run-tests-interactively "nxhtml-ert"))

(defun nxhtmltest-run ()
  "Run all tests defined for nXhtml.
Currently there are only tests using ert.el defined.

Note that it is currently expected that the following tests will
fail (they corresponds to known errors in nXhtml/Emacs):

  `nxhtml-ert-nxhtml-changes-jump-back-10549'
  `nxhtml-ert-nxhtml-changes-jump-back-7033'
"
  (interactive)
  (setq message-log-max t)
  (when (called-interactively-p)
    (nxhtmltest-get-fontification-method))
  (nxhtmltest-run-ert))

(when (getenv "nxhtmltest-run-Q")
  ;;(global-font-lock-mode -1)
  (nxhtmltest-run))

(provide 'nxhtmltest-suites)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtmltest-suites.el ends here
