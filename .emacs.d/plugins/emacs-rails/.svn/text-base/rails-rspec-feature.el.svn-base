;;; rails-rspec-feature.el --- RSpec support

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://rubyforge/var/svn/emacs-rails/trunk/rails-snippets.el $
;; $Id: rails-snippets.el 155 2007-04-01 17:37:48Z dimaexe $

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(defvar rails-rspec-feature:generators-list
  '("rspec" "rspec_controller" "rspec_model" "rspec_resource"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Scripts
;;

(defun rails-script:generate-rspec ()
  "Generate the rspec."
  (interactive)
  (rails-script:run-generate "rspec"))

(defun rails-script:destroy-rspec ()
  "Run destroy the rspec."
  (interactive)
  (rails-script:run-destroy "rspec"))

(defun rails-script:generate-rspec-controller (&optional controller-name actions)
  "Generate a controller and open the controller file."
  (interactive (list
                (completing-read "Controller name (use autocomplete) : "
                                 (list->alist (rails-core:controllers-ancestors)))
                (read-string "Actions (or return to skip): ")))
  (when (string-not-empty controller-name)
    (rails-script:run-generate "rspec_controller" controller-name actions)))

(rails-script:gen-generate-function "rspec-resource")
(rails-script:gen-generate-function "rspec-model" rails-core:models-ancestors)

(rails-script:gen-destroy-function "rspec-controller" rails-core:controllers t)
(rails-script:gen-destroy-function "rspec-resource")
(rails-script:gen-destroy-function "rspec-model" rails-core:models)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Install function
;;

(defun rails-rspec-feature:install ()
  (setq rails-script:generators-list
        (append rails-script:generators-list rails-rspec-feature:generators-list))
  (setq rails-script:destroy-list
        (append rails-script:generators-list rails-rspec-feature:generators-list))
  (dolist (script '(destr gen))
    (dolist (what rails-rspec-feature:generators-list)
      (let* ((menu (vconcat [menu-bar rails scr] (vector script) (vector (intern what))))
             (title (capitalize (replace-regexp-in-string "_" " " what)))
             (func-name (cond
                         ((eq script 'destr) "destroy")
                         ((eq script 'gen) "generate")))
             (func (format "rails-script:%s-%s"
                           func-name
                           (replace-regexp-in-string "_" "-" what))))
        (define-key-after rails-minor-mode-map menu (cons title (intern func))) 'resource)
      )))

(provide 'rails-rspec-feature)
