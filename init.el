;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/") t)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(flycheck . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(flycheck-clojure . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Define he following variables to remove the compile-log warnings
;; when defining ido-ubiquitous
;; (defvar ido-cur-item nil)
;; (defvar ido-default-item nil)
;; (defvar ido-cur-list nil)
;; (defvar predicate nil)
;; (defvar inherit-input-method nil)

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-completing-read+

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit

    flycheck-clojure

    use-package
    ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clojure-indent-style (quote align-arguments))
 '(coffee-tab-width 2)
 '(package-selected-packages
   (quote
    (dashboard-project-status transient parseclj queue spinner magit sesman clojure-snippets flycheck-pos-tip flycheck-clojure clj-refactor tagedit rainbow-delimiters projectile smex ido-completing-read+ cider clojure-mode-extra-font-locking clojure-mode paredit exec-path-from-shell use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun my-hs-minor-mode ()
  "Turn on 'hs-minor-mode' and related key bindings."
  (hs-minor-mode)
  (local-set-key (kbd "C-+") 'hs-show-all)
  (local-set-key (kbd "C-_") 'hs-hide-all)
  (local-set-key (kbd "C-=") 'hs-show-block)
  (local-set-key (kbd "C--") 'hs-hide-block)
  (local-set-key (kbd "C-.") 'hs-toggle-hiding))

;; 80 character flag
;; https://emacs.stackexchange.com/a/14066/21012
(setq-default whitespace-line-column 80
              whitespace-style '(face lines-tail))

(require 'clj-refactor)
(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (my-hs-minor-mode)
    (whitespace-mode)  ; 80 char visual mark
    (if (eq system-type 'darwin) (text-scale-increase 1))
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; flycheck-clojure
(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)

;; flycheck-pos-tip
(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(global-linum-mode -1)

(column-number-mode 1)

(toggle-frame-fullscreen)

(global-set-key [C-tab] 'other-window)

(tool-bar-mode -1)

(menu-bar-mode 1)

;; cider
;; ---
(use-package cider
  :ensure t :defer t
  :config
  (setq
    cider-repl-history-file ".cider-repl-history"  ;; not squiggly-related, but I like it
    nrepl-log-messages t)                          ;; not necessary, but useful for trouble-shooting
  (flycheck-clojure-setup))  ;; run setup *after* cider load

(use-package flycheck-clojure
  :defer t
  :commands (flycheck-clojure-setup)               ;; autoload
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck :ensure t)
(use-package flycheck-pos-tip :ensure t
  :after flycheck)
  
;; save before compile
(setq cider-save-file-on-load t)
;; Don't warn me about the dangers of clj-refactor, fire the missiles!
(setq cljr-warn-on-eval nil)

;; projectile
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(server-start) ;; to support emacsclient calls

(toggle-truncate-lines)

(setq-default tab-width 4)

;;; switch to last buffer
(global-set-key (kbd "C-c b") #'mode-line-other-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abbreviations
;; http://pragmaticemacs.com/emacs/use-abbreviations-to-expand-text/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default abbrev-mode t)
;; save abbreviations upon exiting xemacs
(setq save-abbrevs t)
;; set the file storing the abbreviations
(setq abbrev-file-name "~/.emacs.d/docs/my-abbreviations.el")
;; reads the abbreviations file on startup
(quietly-read-abbrev-file)

;;; init.el ends here
