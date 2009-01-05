(setq user-full-name "Thiago Pradi")
(setq user-mail-address "thiago@townconnect.com")
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
(color-theme-twilight)
(load-file "~/.emacs.d/includes/twit.el")


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
        (define-key ruby-mode-map "C-m" 'newline-and-indent)
        (require 'ruby-electric)
        (ruby-electric-mode t)
  )
)

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
 '(ecb-source-path (quote (("/home/tchandy/Townconnect/townconnect2" "townconnect2") ("/home/tchandy/Townconnect/project/novo" "Old Townconnect") ("/home/tchandy/Townconnect/newtownconnect2/segundo_branch" "Segundo Branch") "/home/tchandy" ("/home/tchandy/Projetos/ProjetoImoveis" "Projetos") ("/" "Root") ("/usr/lib/ruby" "Ruby 1.8") ("/home/tchandy/Townconnect/reportsgogo/townconnect" "Town Velha") ("/home/tchandy/Projetos" "Projetos") (#("/home/tchandy/Townconnect/townconnect2/newbranch/townconnect-2-0" 0 64 (help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu")) "New Townconnect"))))
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


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "#F8F8F8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 106 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(bm-persistent-face ((((class color) (background light)) (:background "MistyRose" :foreground "Black"))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "grey64" :slant italic :family "-*-helvetica-medium-*-*-*-12-*-*-*-*-*-*-*")))))


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
