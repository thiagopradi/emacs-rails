(setq user-full-name "nofxx")
(setq user-mail-address "user@user.com")
(prefer-coding-system 'utf-8)

(setq load-path
        (append (list nil "~/.emacs.d"
                         "~/.emacs.d/plugins"
                         "~/.emacs.d/includes"
                         "~/.emacs.d/plugins/color-theme"
                         "~/.emacs.d/plugins/nxhtml"
                         "~/.emacs.d/plugins/rinari"
                         "~/.emacs.d/plugins/rinari/rhtml"
                         "~/.emacs.d/plugins/emacs-rails"
                         "~/.emacs.d/plugins/eieio"
                         "~/.emacs.d/plugins/semantic"
                         "~/.emacs.d/plugins/speedbar"
                         "~/.emacs.d/plugins/ecb"
                         "~/.emacs.d/plugins/jump.el")
                         load-path))

; Color Theme
(require 'color-theme)
(color-theme-initialize)
;(color-theme-twilight)
(color-theme-arjen)
;(load-file "~/.emacs.d/includes/twit.el")


; Configurações dos Snippets
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
(setq yas/window-system-popup-function 'yas/x-popup-menu-for-template)

;;; rhtml mode
(add-to-list 'load-path "~/.emacs.d/includes/rhtml-mode")
(require 'rhtml-mode)

; CSS Mode
(autoload 'css-mode "css-mode" "Major mode for editing css files." t)
(setq auto-mode-alist  (cons '(".css$" . css-mode) auto-mode-alist))

; JavaScript Mode
(autoload 'js2-mode "js2" "Major mode for editing javascript scripts." t)
(setq auto-mode-alist  (cons '(".js$" . js2-mode) auto-mode-alist))

; Ruby Mode
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".erb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".god$" . ruby-mode) auto-mode-alist))

(add-hook 'ruby-mode-hook
  (lambda()
    (add-hook 'local-write-file-hooks
      '(lambda()
        (save-excursion
              (untabify (point-min) (point-max))
              (delete-trailing-whitespace)
                )
          )
        )
        (set (make-local-variable 'indent-tabs-mode) 'nil)
        (set (make-local-variable 'tab-width) 2)
        (imenu-add-to-menubar "IMENU")
        (define-key ruby-mode-map "\C-m" 'newline-and-indent)
        (require 'ruby-electric)
        (ruby-electric-mode t)
  )
)

;;; starter-kit-ruby.el --- Some helpful Ruby code
;;
;; Part of the Emacs Starter Kit

(eval-after-load 'ruby-mode
  '(progn
;;     (require 'ruby-compilation)
     (setq ruby-use-encoding-map nil)
     (add-hook 'ruby-mode-hook 'inf-ruby-keys)
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)
     (define-key ruby-mode-map (kbd "C-c l") "lambda")))

(global-set-key (kbd "C-h r") 'ri)

;; Rake files are ruby, too, as are gemspecs.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

;;; Rake

(defun pcomplete/rake ()
  "Completion rules for the `ssh' command."
  (pcomplete-here (pcmpl-rake-tasks)))

(defun pcmpl-rake-tasks ()
   "Return a list of all the rake tasks defined in the current
projects. I know this is a hack to put all the logic in the
exec-to-string command, but it works and seems fast"
   (delq nil (mapcar '(lambda(line)
      (if (string-match "rake \\([^ ]+\\)" line) (match-string 1 line)))
     (split-string (shell-command-to-string "rake -T") "[\n]"))))

(defun rake (task)
  (interactive (list (completing-read "Rake (default: default): "
                                      (pcmpl-rake-tasks))))
  (shell-command-to-string (concat "rake " (if (= 0 (length task)) "default" task))))


;; Clear the compilation buffer between test runs.
;;(eval-after-load 'ruby-compilation
;;  '(progn
;;     (defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist))
;;       (let ((comp-buffer-name (format "*%s*" name)))
;;         (when (get-buffer comp-buffer-name)
;;           (with-current-buffer comp-buffer-name
;;             (delete-region (point-min) (point-max))))))
;;     (ad-activate 'ruby-do-run-w/compilation)))

(add-hook 'ruby-mode-hook 'coding-hook)

;;; Flymake

(eval-after-load 'ruby-mode
  '(progn
     (require 'flymake)

     ;; Invoke ruby with '-c' to get syntax checking
     (defun flymake-ruby-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "ruby" (list "-c" local-file))))

     (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

     (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)
           flymake-err-line-patterns)

     (add-hook 'ruby-mode-hook
               (lambda ()
                 (when (and buffer-file-name
                            (file-writable-p
                             (file-name-directory buffer-file-name))
                            (file-writable-p buffer-file-name))
                   (local-set-key (kbd "C-c d")
                                  'flymake-display-err-menu-for-current-line)
                   (flymake-mode t))))))

;;(eval-after-load 'haml-mode
  ;;(if (functionp 'whitespace-mode)
   ;;   (add-hook 'haml-mode-hook 'whitespace-mode)))

;; TODO: set up ri
;; TODO: electric

(provide 'starter-kit-ruby)
;; starter-kit-ruby.el ends here


; YAML Mode
(autoload 'yaml-mode "yaml-mode" "Major mode for editing yaml files." t)
(setq auto-mode-alist  (cons '(".yml$" . yaml-mode) auto-mode-alist))

; XHTML Mode
(load "autostart.el")
(require 'mumamo-fun)

(setq
  nxhtml-global-minor-mode t
  mumamo-chunk-coloring 'submode-colored
  nxhtml-skip-welcome t
  indent-region-mode t
  rng-nxml-auto-validate-flag nil
  nxml-degraded t
)
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo))
(add-to-list 'auto-mode-alist '("\.html.erb$" . html-mode))


; Php Mode
(require 'php-mode)
;(setq auto-mode-alist  (cons '(".php$" . php-mode) auto-mode-alist))
;(setq auto-mode-alist  (cons '(".inc$" . php-mode) auto-mode-alist))
;(setq auto-mode-alist  (cons '(".phtml$" . php-mode) auto-mode-alist))
;(setq auto-mode-alist  (cons '(".tpl$" . php-mode) auto-mode-alist))
(define-key php-mode-map (kbd "RET") 'newline-and-indent)
(defun wicked/php-mode-init ()
  "Set some buffer-local variables."
;  (setq case-fold-search t)
  (setq indent-tabs-mode nil)
;  (setq show-paren-mode t)
  (setq fill-column 78))
;  (setq c-basic-offset 2)
;  (c-set-offset 'arglist-cont 0)
;  (c-set-offset 'arglist-intro '+)
;  (c-set-offset 'case-label 2)
;  (c-set-offset 'arglist-close 0))
(add-hook 'php-mode-hook 'wicked/php-mode-init)


; Emacs-Rails
(load "snippet")
(load "find-recursive")
(require 'rails)

; Rinari (RAILS) Configurações
(require 'rinari)
(setq rinari-tags-file-name "TAGS")

; Interactively Do Things
(require 'ido)
(ido-mode t)

; ECB Config
(setq semantic-load-turn-everything-on t)
(require 'semantic-load)
(require 'ecb)
(require 'ecb-autoloads)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(display-time-mode t)
 '(ecb-enlarged-compilation-window-max-height (quote best))
 '(ecb-eshell-auto-activate nil)
 '(ecb-layout-name "town")
 '(ecb-layout-nr 9)
 '(ecb-layout-window-sizes (quote (("town" (0.21794871794871795 . 0.4878048780487805) (0.21794871794871795 . 0.4878048780487805)) ("left-analyse" (0.21794871794871795 . 0.43902439024390244) (0.21794871794871795 . 0.34146341463414637) (0.21794871794871795 . 0.0975609756097561) (0.21794871794871795 . 0.0975609756097561)))))
 '(ecb-non-semantic-parsing-function nil)
 '(ecb-options-version "2.32")
 '(ecb-other-window-behavior (quote edit-and-compile))
 '(ecb-other-window-jump-behavior (quote edit-and-compile))
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
 '(ecb-source-path (quote ("/home/" "Home" ("/home/nofxx/git/rtrac" "rtrac") (#("/home/nofxx/git/imoby" 0 21 (help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu")) "imoby") ("/home/nofxx/git/farm" "farm"))))
 '(ecb-tip-of-the-day nil)
 '(flymake-js-off t)
 '(flymake-php-off t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode t)
 '(php-mode-force-pear t)
 '(php-mode-hook (quote (wicked/php-mode-init)))
 '(scroll-bar-mode (quote right))
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(twit-pass "")
 '(twit-user ""))



; Configurando o sistema de backup do Emacs
(setq backup-by-copying t               ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/backups"))      ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(add-hook 'ruby-mode-hook
          '(lambda ()
             (make-variable-buffer-local 'yas/trigger-key)
             (setq yas/trigger-key [tab])))

(require 'tabbar)
(tabbar-mode)
  (autoload 'mode-compile "mode-compile"
   "Command to compile current buffer file based on the major mode" t)
  (global-set-key "\C-cc" 'mode-compile)
  (autoload 'mode-compile-kill "mode-compile"
   "Command to kill a compilation launched by `mode-compile'" t)
  (global-set-key "\C-ck" 'mode-compile-kill)

(require 'rspec-mode)

(setq default-truncate-lines t)

(require 'linum)
(global-linum-mode)

(require 'haml-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(require 'magit)

(defun autotest ()
(interactive)
(let ((buffer (shell "autotest")))
(comint-send-string buffer "autotest\n")))

(defun autotest-switch ()
(interactive)
(if (equal "autotest" (buffer-name))
(previous-buffer)
(switch-to-buffer "autotest")))
; add to ruby mode hook:
(define-key ruby-mode-map "\C-c\C-s" 'autotest-switch)

; nofxx
(global-set-key "\C-x\C-g" 'magit-status)
(global-set-key "\M-/" 'comment-or-uncomment-region)
(global-set-key "\M-[" 'indent-region)
(global-set-key "\M-]" 'indent-according-to-mode)
(global-set-key "\M-s" 'save-buffer)
(global-set-key "\M-t" 'ido-find-file)
(global-set-key "\M-q" 'kill-this-buffer)
(global-set-key "\M-a" 'magit-status)
(global-set-key "\M-r" 'query-replace)
(global-set-key "\M-w" 'ido-switch-buffer)

(autoload 'findr "findr" "Find file name." t)
(define-key global-map [(meta control S)] 'findr)

(autoload 'findr-search "findr" "Find text in files." t)
(define-key global-map [(meta control s)] 'findr-search)

(autoload 'findr-query-replace "findr" "Replace text in files." t)
(define-key global-map [(meta control r)] 'findr-query-replace)

;(define-key shell-mode-map "\C-c\C-a" 'autotest-switch)
