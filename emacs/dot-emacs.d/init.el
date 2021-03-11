;; NOTE: This file is generated from init.org.

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'default-frame-alist '(height . 36)) ; frame height
(add-to-list 'default-frame-alist '(width . 80))  ; frame width

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil))

(setq-default inhibit-startup-screen t)
(tool-bar-mode -1)                      ; hide toolbar
(scroll-bar-mode -1)                    ; hide scrollbar

(setq inhibit-compacting-font-caches t)
(set-face-attribute 'default nil :font "Fira Code-14")
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "STKaiti")))
(setq face-font-rescale-alist '(("STKaiti" . 1.3)))

(display-battery-mode t)                ; battary
(column-number-mode t)                  ; column number
(setq-default display-time-interval 30)
(setq-default display-time-default-load-average nil)
(setq-default display-time-format "%Y-%m-%d %a %H:%M")
(display-time-mode t)                   ; datetime

(use-package all-the-icons
  :ensure t) ;; and M-x all-the-icons-install-fonts

(use-package doom-themes
  :init (load-theme 'doom-solarized-light t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-encoding nil))

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(setq-default cursor-type 'bar)

(setq-default show-trailing-whitespace t)

(setq-default show-paren-delay 0)
(show-paren-mode 1)                     ; parentheses matching

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package visual-fill-column
  :config
  (setq-default visual-fill-column-width 120
                visual-fill-column-center-text t)
  (global-visual-fill-column-mode))

(global-display-line-numbers-mode)
(global-hl-line-mode t)                 ; line highlighting
(global-visual-line-mode t)             ; line wrap

(setq-default make-backup-files nil)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

(global-set-key (kbd "C-c m f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c m m") 'toggle-frame-maximized)
(global-set-key (kbd "C-c m 0") 'text-scale-adjust)
(global-set-key (kbd "C-c m g") 'goto-line)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "s-，") 'customize)
(global-set-key (kbd "M-【") 'previous-buffer)
(global-set-key (kbd "M-】") 'next-buffer)

(defun my/add-auctex-file-variables ()
  (interactive)
  (if (and (not buffer-read-only)
           (string= (file-name-extension (buffer-file-name)) "tex"))
      (progn
        ;; (add-file-local-variable 'mode 'latex)
        (add-file-local-variable 'TeX-engine 'xetex)
        (goto-char (point-min)))))
;; (add-hook 'LaTeX-mode-hook 'my/add-auctex-file-variables)

(use-package org
  :ensure t
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  :init
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook 'org-babel-tangle nil t))) ; tangle on save
  :config
  (setq org-adapt-indentation nil) ; prevent demoting heading also shifting text inside sections
  (setq org-tags-column 60)        ; set position of tags
  (setq org-habit-graph-column 50) ; set position of habit graph

  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-modules 'org-attach-git)

  (setq org-agenda-files '("~/ea/schedule/"))
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)          ; record close time for todo item
  (setq org-duration-format 'h:mm)   ; time format
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-python-command "python3")
  (org-babel-do-load-languages
   'org-babel-load-languages '((R . t)
                               (C . t)
                               (python . t)
                               (shell . t)
                               (ruby . t)
                               (haskell . t)
                               (scheme . t)
                               (awk . t)
                               (octave . t)
                               (lua . t)
                               (js . t)))

  (setq org-default-notes-file "~/ea/refile.org")
  (setq org-capture-templates
        '(("i" "Idea" entry
           (file "~/ea/roam/ideas.org")
           "* %U%?\n%i\n")
          ("t" "Task" entry
           (file "~/ea/schedule/tasks.org")
           "* TODO %?\n %i\n %a")
          ("c" "Clipboard" entry
           (file+headline org-default-notes-file "Clipboard")
           "* %?\n%i\n%a")))

  (setq org-export-backends
        '(ascii beamer html icalendar latex man md odt texinfo))
  (setq org-export-coding-system 'utf-8)
  (setq org-latex-listings 'listings)

  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
  )

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/ea/roam")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph)
               ("C-c n r" . org-roam-random-note)
               ("C-c n j" . org-roam-jump-to-index))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate)))
  :config
  (setq org-roam-graph-executable "/usr/local/bin/dot")
  (setq org-roam-index-file "~/ea/roam/index.org"))

(require 'appt)
(setq appt-time-msg-list nil)    ;; clear existing appt list
(setq appt-display-interval '5)  ;; warn every 5 minutes from t - appt-message-warning-time
(setq
 appt-message-warning-time '15  ;; send first warning 15 minutes before appointment
 appt-display-mode-line nil     ;; don't show in the modeline
 appt-display-format 'window)   ;; pass warnings to the designated window function
(setq appt-disp-window-function (function ct/appt-display-native))

(appt-activate 1)                ;; activate appointment notification
                                      ; (display-time) ;; Clock in modeline

(defun ct/send-notification (title msg)
  (let ((notifier-path (executable-find "alerter")))
    (start-process
     "Appointment Alert"
     "*Appointment Alert*" ; use `nil` to not capture output; this captures output in background
     notifier-path
     "-message" msg
     "-title" title
     "-sender" "org.gnu.Emacs"
     "-activate" "org.gnu.Emacs")))

(defun ct/appt-display-native (min-to-app new-time msg)
  (ct/send-notification
   (format "Appointment in %s minutes" min-to-app) ; Title
   (format "%s" msg)))                             ; Message/detail text


;; Agenda-to-appointent hooks
(org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
(run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (setq yas-indent-line 'fixed))

(use-package magit
  :ensure t
  :init
  :bind
  ("C-x g" . magit-status))

(use-package ibuffer
  :ensure t
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("planner" (or
                       (name . "^\\*Calendar\\*$")
                       (name . "^diary$")))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")))
           ("emacs-config" (or (filename . ".emacs.d")
                               (filename . "init.el")))
           ("magit" (or
                     (name . "magit\*")
                     (mode . Magit)
                     ))
           ("dired" (mode . dired-mode))
           ("org" (mode . org-mode))
           ("manual" (or
                      (name . "\\*Man")
                      (name . "\\*info\\*"))))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default"))))

(setq dired-listing-switches "-alh")

(use-package ivy
  :ensure t)

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t
  :bind
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook #'latex-extra-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package flycheck
  :hook (after-init . global-flycheck-mode))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package htmlize
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/multimarkdown"))

(use-package yaml-mode)

(use-package dockerfile-mode)

(use-package cmake-mode)

(use-package geiser
  :config
  (setq geiser-default-implementation 'mit))

(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "/usr/local/bin/ccl64"))

(use-package sml-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package racket-mode
  :ensure t)

(use-package python
  :config
  (setq python-shell-interpreter "python3"))

(use-package python-mode)

(use-package virtualenvwrapper
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(use-package php-mode)

(use-package typescript-mode)

(use-package lua-mode)

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(use-package js
  :config
  (setq js-indent-level 2))
