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

(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq-default inhibit-startup-screen t)
(tool-bar-mode -1)                      ; hide toolbar
(scroll-bar-mode -1)                    ; hide scrollbar

(use-package doom-themes
  :init (load-theme 'doom-solarized-light t))

(add-to-list 'default-frame-alist '(height . 36)) ; frame height
(add-to-list 'default-frame-alist '(width . 80))  ; frame width

(use-package all-the-icons
  :ensure t) ;; and M-x all-the-icons-install-fonts

(display-battery-mode t)                ; battary
(column-number-mode t)                  ; column number
(setq-default display-time-interval 30)
(setq-default display-time-default-load-average nil)
(setq-default display-time-format "%Y-%m-%d %a %H:%M")
(display-time-mode t)                   ; datetime
(use-package doom-modeline
  :init (doom-modeline-mode 1))

(setq-default cursor-type 'bar)
(setq-default show-trailing-whitespace t)
(setq-default show-paren-delay 0)
(show-paren-mode 1)                     ; parentheses matching
(global-hl-line-mode t)                 ; line highlighting
(global-linum-mode 1)                   ; line number
(global-visual-line-mode t)             ; line wrap

(set-face-attribute 'default nil :font "Fira Code-14")
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "STKaiti")))
(setq face-font-rescale-alist '(("STKaiti" . 1.3)))

(setq-default make-backup-files nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

(when (memq window-system '(mac ns))
  ;; Frame
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil)
  ;; PATH
  (setenv "PATH" (concat "/Library/TeX/texbin" ":" (getenv "PATH")))
  (setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))
  (setenv "PATH" (concat "/usr/local/smlnj/bin" ":" (getenv "PATH")))
  (setq exec-path (cons "/usr/local/smlnj/bin"  exec-path))
  )

(global-set-key (kbd "C-c m f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c m m") 'toggle-frame-maximized)
(global-set-key (kbd "C-c m n") 'my/open-notes-directory)
(global-set-key (kbd "C-c m n") 'my/open-init-file)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "s-，") 'customize)
(global-set-key (kbd "M-【") 'previous-buffer)
(global-set-key (kbd "M-】") 'next-buffer)

;; https://emacs.stackexchange.com/questions/27109/how-can-i-automatically-add-some-local-variables-info-to-a-c-x-c-f-new-tex-fi
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
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-babel-tangle))) ; tangle on save
  :config
  (setq org-adapt-indentation nil) ; prevent demoting heading also shifting text inside sections
  (setq org-tags-column 40)        ; set position of tags
  (setq org-habit-graph-column 50) ; set position of habit graph

  ;; --- todo ---
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-modules 'org-attach-git)
  (setq org-agenda-files '("~/ea/schedule/"))
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)          ; record close time for todo item
  (setq org-duration-format 'h:mm)   ; time format
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; --- babel ---
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
                               (octave . t)))

  ;; --- capture ---
  (setq org-capture-templates
        '(("i" "Idea" entry
           (file+headline "~/ea/refile/refile.org" "Ideas")
           "* %U%?\n%i\n")
          ("t" "Task" entry
           (file+headline "~/ea/refile/refile.org" "Tasks")
           "* TODO %?\n %i\n %a")
          ("c" "Clipboard" entry
           (file+headline "~/ea/refile/refile.org" "Clipboard")
           "* %?\n%i\n%a")))

  ;; --- export ---
  (setq org-export-backends
        '(ascii beamer html icalendar latex man md odt texinfo))
  (setq org-export-coding-system 'utf-8)
  (setq org-latex-listings 'listings)

  ;; --- tempo ---
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
               ("C-c n g" . org-roam-graph-show)
               ("C-c n j" . org-roam-jump-to-index))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate)))
  :config
  (setq org-roam-graph-executable "/usr/local/bin/dot")
  (setq org-roam-index-file "~/ea/roam/index.org"))

(use-package htmlize
  :ensure t)

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

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-sidebar
  :ensure t
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/multimarkdown"))

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

(use-package python-mode
  :ensure t
  :config
  (setq python-shell-interpreter "python3"))

(use-package php-mode)
