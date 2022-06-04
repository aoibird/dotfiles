;; -*- lexical-binding: t; -*-
;; NOTE: This file is generated from init.org.

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)) ; install use-package if it does not exist
(require 'use-package)
(setq use-package-always-ensure nil)

(defun my/packages-installed-p ()
  (let ((ret t))
    (dolist (pkg package-selected-packages)
      (when (not (package-installed-p pkg)) (setq ret nil)))
    ret))

(setq package-selected-packages
      '(all-the-icons ;; and M-x all-the-icons-install-fonts
        doom-themes doom-modeline
        exec-path-from-shell
        org org-roam org-bullets org-journal org-noter org-alert org-ref
        magit yasnippet ibuffer ivy counsel swiper
        projectile flycheck flycheck-haskell
        visual-fill-column all-the-icons-dired rainbow-delimiters
        markdown-mode yaml-mode dockerfile-mode cmake-mode nix-mode bison-mode csv-mode
        geiser geiser-chez slime racket-mode sml-mode clojure-mode rust-mode haskell-mode lua-mode
        typescript-mode php-mode web-mode go-mode
        git-annex magit-annex auctex try vlf pdf-tools zotxt telega elfeed elfeed-org alert dashboard htmlize
        activity-watch-mode
        ))
(setq package-pinned-packages '((telega . "melpa-stable")))

;; Install packages
(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (package-install-selected-packages))

(add-to-list 'default-frame-alist '(height . 36)) ; frame height
(add-to-list 'default-frame-alist '(width . 80))  ; frame width

(setq-default inhibit-startup-screen t)
(tool-bar-mode -1)                      ; hide toolbar
(scroll-bar-mode -1)                    ; hide scrollbar

(use-package doom-themes
  :init (load-theme 'doom-tomorrow-day t))

(display-battery-mode t)                ; battary
(column-number-mode t)                  ; column number
(setq-default display-time-interval 30)
(setq-default display-time-default-load-average nil)
(setq-default display-time-format "%Y-%m-%d %a %H:%M")
(display-time-mode t)                   ; datetime

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-encoding nil))

(setq inhibit-compacting-font-caches t)
(set-face-attribute 'default nil :font "Cascadia Code-14")
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "方正楷体_GBK")))
(setq face-font-rescale-alist '(("方正楷体_GBK" . 1.12)))

(setq-default make-backup-files nil)

(setq mac-right-option-modifier 'none)  ; option + s --> ß

(global-display-line-numbers-mode)
(global-hl-line-mode t)                 ; line highlighting
(global-visual-line-mode t)             ; line wrap

(setq ring-bell-function 'ignore)

(setq-default
   tab-width 4
   indent-tabs-mode nil
   c-basic-offset 4
   cursor-type 'bar
   show-trailing-whitespace t
   show-paren-delay 0
   major-mode 'org-mode)

(show-paren-mode 1)                     ; parentheses matching

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (setq ns-use-proxy-icon  nil)
  (setq ns-pop-up-frames nil)
  (setq frame-title-format nil))

(defun my/open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.org"))

(defun my/switch-to-org-agenda-file ()
  (interactive)
  (let ((file (completing-read "File: " (org-agenda-files))))
    (find-file file)))

(defun my/add-auctex-file-variables ()
  (interactive)
  (if (and (not buffer-read-only)
           (string= (file-name-extension (buffer-file-name)) "tex"))
      (progn
        ;; (add-file-local-variable 'mode 'latex)
        (add-file-local-variable 'TeX-engine 'xetex)
        (goto-char (point-min)))))
;; (add-hook 'LaTeX-mode-hook 'my/add-auctex-file-variables)

(defun my/dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

(global-set-key (kbd "C-c m f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c m m") 'toggle-frame-maximized)
(global-set-key (kbd "C-c m 0") 'text-scale-adjust)
(global-set-key (kbd "C-c m g") 'goto-line)
(global-set-key (kbd "C-c m r") 'revert-buffer)
(global-set-key (kbd "C-c m v") 'add-file-local-variable-prop-line)
(global-set-key (kbd "C-c m o") 'org-clock-out)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "s-，") 'customize)
(global-set-key (kbd "M-【") 'previous-buffer)
(global-set-key (kbd "M-】") 'next-buffer)
(global-set-key (kbd "C-<tab>") 'other-window)

(global-set-key (kbd "C-c m i") 'my/open-init-file)
(global-set-key (kbd "C-c m a") 'my/switch-to-org-agenda-file)

(use-package exec-path-from-shell
  ;; :if (memq window-system '(mac ns))
  :init
  (exec-path-from-shell-initialize))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package visual-fill-column
  :hook
  (org-mode . visual-fill-column-mode)
  :config
  (setq-default visual-fill-column-width 120
                visual-fill-column-center-text t)
  ;; (global-visual-fill-column-mode)
  )

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              (("?" . my/dired-get-size)))
  :config
  (setq dired-listing-switches "-avlh --time-style=long-iso --group-directories-first"))

(use-package diff
  :config
  (setq diff-switches "-u -r"))

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
  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-+]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-crypt)
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-modules 'org-attach-git)
  (setq org-habit-graph-column 50) ; set position of habit graph
  (setq org-agenda-tags-column 80)
  (setq org-agenda-files '("~/hub/sched/"))
  (setq org-agenda-file-regexp "\\`[^.].*\\.org\\(\\.gpg\\)?\\'") ; ".org" or ".org.gpg"
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)          ; record close time for todo item
  (setq org-duration-format 'h:mm)   ; time format
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-agenda-start-on-weekday nil)
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
                               (js . t)
                               (dot . t)))
  (setq org-default-notes-file "~/hub/sched/refile.gpg")
  (setq org-capture-templates
        '(("i" "Idea" entry
           (file+headline org-default-notes-file "Ideas")
           "* %U%?\n%i\n")
          ("t" "Task" entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n %i\n %a")
          ("c" "Clipboard" entry
           (file+headline org-default-notes-file "Clipboard")
           "* %?\n%i\n%a")
          ("l" "Clock" entry
           (file+headline org-default-notes-file "Clock")
           "** %?\n" :clock-in t :clock-keep t)))
  (setq org-export-backends
        '(ascii beamer html icalendar latex man md odt texinfo))
  (setq org-export-coding-system 'utf-8)
  (setq org-latex-listings 'listings)
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-head-include-default-style nil)
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
  (setq org-publish-project-alist
        '(
          ("kb-html-org"
           :base-directory "~/hub/kb"
           :base-extension "org"
           :publishing-directory "~/hub/kb_html"
           :eval never-export
           :recursive t
           :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"org.css\" />"
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :author "author"
           :email "email"
           :with-latex t
           :with-drawer t
           :with-timestamps t
           :with-email t
           :html-postamble auto
           :auto-sitemap t
           :sitemap-sort-files alphabetically
           :sitemap-filename "sitemap.org"
           :sitemap-title "Sitemap")
          ("kb-html-static"
           :base-directory "~/hub/kb"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/hub/kb_html"
           :recursive t
           :exclude "\\*proj\\*"
           :publishing-function org-publish-attachment)
          ("kb-html" :components ("kb-html-org" "kb-html-static"))))
  (setq org-attach-preferred-new-method 'id)
  (setq org-attach-store-link-p t)
  (setq org-attach-dir-relative t)
  (setq org-attach-git-use-annex nil)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key nil)
  (setq auto-save-default nil)
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/hub/kb"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode))

(use-package org-journal
  :ensure t
  :defer t
  :config
  (setq org-journal-dir "~/hub/journal")
  (setq org-journal-date-format "%Y-%m-%d")
  (setq org-journal-file-format "journal")
  (setq org-journal-encrypt-journal t)
  (setq org-journal-file-type 'still)
  (defun org-journal-file-header-func (time)
    "Custom function to create journal header."
    (concat
     (pcase org-journal-file-type
       (`daily "# -*- mode: org -*-\n#+TITLE: Daily Journal\n#+STARTUP: showeverything")
       (`weekly "# -*- mode: org -*-\n#+TITLE: Weekly Journal\n#+STARTUP: folded")
       (`monthly "# -*- mode: org -*-\n#+TITLE: Monthly Journal\n#+STARTUP: folded")
       (`yearly "# -*- mode: org -*-\n#+TITLE: Yearly Journal\n#+STARTUP: folded")
       (`still "# -*- mode: org -*-\n#+TITLE: Journal\n#+STARTUP: folded"))))
  (setq org-journal-file-header 'org-journal-file-header-func))

(use-package org-alert
  :config
  (setq alert-default-style 'libnotify))

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config
  (yas-global-mode)
  (setq yas-indent-line 'fixed))

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
           ("schedule" (and (filename . "sched/")
                            (mode . org-mode)))
           ("kb" (and (filename . "kb/")
                        (or (mode . org-mode)
                            (mode . prog-mode))))
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
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package counsel
  :ensure t
  :config
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

(use-package swiper
  :ensure t
  :config
  ;; enable this if you want `swiper' to use it
  (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper))

(use-package magit
  :ensure t
  :init
  :bind
  ("C-x g" . magit-status))

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook #'latex-extra-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex))

(use-package epa-file
  :ensure nil
  :config
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-height)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1))))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  (setq projectile-project-search-path '(("~/hub". 4)))
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package flycheck
  :hook
  (after-init . global-flycheck-mode)
  (haskell-mode-hook . flycheck-haskell-setup)
  :config
  (setq flycheck-haskell-hlint-executable "~/.local/share/cabal/bin/hlint")
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package telega
  :config
  (setq telega-directory "~/.local/share/telega")
  (setq telega-database-dir "~/.local/share/telega")
  ;; (define-key global-map (kbd "C-c t") telega-prefix-map)
  (add-hook 'telega-load-hook
            (lambda ()
              (define-key global-map (kbd "C-c t") telega-prefix-map)))
  (add-hook 'telega-root-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  (add-hook 'telega-chat-mode-hook (lambda () (setq show-trailing-whitespace nil)))
)

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory "~/.local/share/elfeed"))

(use-package elfeed-org
  :config
  (setq rmh-elfeed-org-files (list "~/hub/feeds.org")))

(use-package activity-watch-mode
  :config
  (global-activity-watch-mode))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t))

(use-package geiser
  :config
  (setq geiser-chez-binary "scheme")
  (setq geiser-default-implementation 'chez))

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl"))

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
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-part-padding 0)
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0))

(use-package js
  :config
  (setq js-indent-level 2))
