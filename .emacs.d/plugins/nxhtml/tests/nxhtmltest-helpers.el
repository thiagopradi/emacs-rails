;;; nxhtmltest-helpers.el --- Helper functions for testing
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-07-08T19:10:54+0200 Tue
;; Version: 0.1
;; Last-Updated: 2008-07-08T19:11:22+0200 Tue
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   Cannot open load file: test-helpers.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
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

(defun nxhtmltest-mumamo-error-messages ()
  (ert-get-messages "^MuMaMo error"))

(defun nxhtmltest-should-no-mumamo-errors ()
  (ert-should (not (nxhtmltest-mumamo-error-messages))))

(defun nxhtmltest-be-really-idle (seconds &optional prompt-mark)
  (unless prompt-mark (setq prompt-mark ""))
  (with-timeout (4 (message "<<<< %s - not really idle any more at %s"
                            prompt-mark
                            (format-time-string "%H:%M:%S")))
    (let ((prompt (format
                   ">>>> %s Starting beeing really idle %s seconds at %s"
                   prompt-mark
                   seconds
                   (format-time-string "%H:%M:%S ..."))))
      (message "%s" prompt)
      (read-minibuffer prompt)
      (redisplay))))

;;(nxhtmltest-be-really-idle 4 "HERE I AM!!")

(defmacro* nxhtmltest-with-temp-buffer (file-name-form &body body)
  (declare (indent 1) (debug t))
  (let ((file-name (gensym "file-name-")))
    `(let ((,file-name (nxhtml-get-test-file-name ,file-name-form)))
       (with-temp-buffer
         ;; Give the buffer a name that allows us to switch to it
         ;; quickly when debugging a failure.
         (rename-buffer (format "Test input %s"
                                (file-name-nondirectory ,file-name))
                        t)
         (insert-file-contents ,file-name)
         (save-window-excursion
           ;; Switch to buffer so it will show immediately when
           ;; debugging a failure.
           (switch-to-buffer (current-buffer))
           ,@body)))))

;; Fix-me: This does not work as I intended. A lot of buffers lying
;; around ...
(defvar nxhtmltest-bufnum 0)

(defmacro* nxhtmltest-with-persistent-buffer (file-name-form &body body)
  (declare (indent 1) (debug t))
  (let ((file-name (gensym "file-name-")))
    `(let* ((,file-name (nxhtml-get-test-file-name ,file-name-form))
            ;; Give the buffer a name that allows us to switch to it
            ;; quickly when debugging a failure.
            (temp-buf-name
             (format "Test input %s, %s"
                     (setq nxhtmltest-bufnum (1+ nxhtmltest-bufnum))
                     (file-name-nondirectory ,file-name)
                     ;; Fix-me: I would like to have the test name
                     ;; here. Is that possible?
                     ))
            (temp-buf (get-buffer temp-buf-name)))
       (unless (file-readable-p ,file-name)
         (if (file-exists-p ,file-name)
             (error "Can't read %s" ,file-name)
           (error "Can't find %s" ,file-name)))
       (message "Testing with file %s" ,file-name)
       (when temp-buf (kill-buffer temp-buf))
       (setq temp-buf (get-buffer-create temp-buf-name))
       (with-current-buffer temp-buf
         ;; Avoid global font lock
         (set (make-local-variable 'font-lock-global-modes) nil)
         ;; Fix-me: Can't see how to avoid this in a simple way.  It
         ;; will probably do no harm, but be aware that it has been
         ;; done!
         (put 'font-lock-global-modes 'permanent-local t)
         ;; Turn off font lock in buffer
         (font-lock-mode -1)
         (assert (not font-lock-mode) t "%s %s" "in nxhtmltest-with-persistent-buffer")
         (insert-file-contents ,file-name)
         (save-window-excursion
           ;; Switch to buffer so it will show immediately when
           ;; debugging a failure.
           (switch-to-buffer-other-window (current-buffer))
           ,@body)
         (kill-buffer temp-buf)))))


(defun nxhtml-get-test-file-name (file-name)
  (expand-file-name file-name nxhtmltest-files-root))


;;; Fontification methods

(defvar nxhtmltest-default-fontification-method nil)

(defun nxhtmltest-get-fontification-method ()
  "Ask user for default fontification method."
  (let* ((collection
          '(
            ("Fontify as usual (wait)" fontify-as-usual)
            ("Fontify by calling timer handlers" fontify-w-timer-handlers)
            ("Call fontify-buffer" fontify-buffer)
            ))
         (hist (mapcar (lambda (rec)
                         (car rec))
                       collection))
         (method-name (completing-read "Default fontification method: "
                                       collection nil t
                                       (car (nth 1 collection))
                                       'hist)))
    (setq nxhtmltest-default-fontification-method
          (nth 1 (assoc method-name collection)))))

(defun nxhtmltest-fontify-as-usual (seconds prompt-mark)
  (font-lock-mode 1)
  (font-lock-wait (nxhtmltest-be-really-idle seconds prompt-mark)))

(defun nxhtmltest-fontify-w-timers-handlers ()
    (dolist (timer (copy-list timer-idle-list))
      (timer-event-handler timer))
    (redisplay t))

(defun nxhtmltest-fontify-buffer ()
  (font-lock-fontify-buffer)
  (redisplay t))

(defun nxhtmltest-fontify-default-way (seconds &optional pmark)
  ;;(assert (not font-lock-mode))
  (case nxhtmltest-default-fontification-method
    (fontify-as-usual         (nxhtmltest-fontify-as-usual seconds pmark))
    (fontify-w-timer-handlers (nxhtmltest-fontify-w-timers-handlers))
    (fontify-buffer           (nxhtmltest-fontify-buffer))
    (t (error "Unrecognized default fontification method: %s"
              nxhtmltest-default-fontification-method))))


(provide 'nxhtmltest-helpers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtmltest-helpers.el ends here
