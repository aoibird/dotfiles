;; -*- lexical-binding: t; -*-
;; NOTE: This file is generated from init.org.

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)) ; install use-package if it does not exist
(require 'use-package)
(setq use-package-always-ensure nil)

(setq my/packages
  '(all-the-icons ;; and M-x all-the-icons-install-fonts
    doom-themes
    doom-modeline
    exec-path-from-shell
    org
    org-roam org-roam-server org-bullets org-journal
    magit yasnippet ibuffer
    ivy counsel swiper
    auctex
    projectile
    flycheck
    visual-fill-column
    all-the-icons-dired
    rainbow-delimiters
    htmlize
    markdown-mode yaml-mode dockerfile-mode cmake-mode nix-mode
    geiser slime
    racket-mode sml-mode clojure-mode
    rust-mode haskell-mode lua-mode
    typescript-mode php-mode web-mode
    git-annex magit-annex
    try
    ))
(defun my/packages-installed-p ()
  (let ((ret t))
    (dolist (pkg my/packages)
      (when (not (package-installed-p pkg)) (setq ret nil)))
    ret))

;; Install packages
(setq package-selected-packages my/packages)
(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (package-install-selected-packages))

(add-to-list 'default-frame-alist '(height . 36)) ; frame height
(add-to-list 'default-frame-alist '(width . 80))  ; frame width

(setq-default inhibit-startup-screen t)
(tool-bar-mode -1)                      ; hide toolbar
(scroll-bar-mode -1)                    ; hide scrollbar

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (setq ns-use-proxy-icon  nil)
  (setq ns-pop-up-frames nil)
  (setq frame-title-format nil))

(global-display-line-numbers-mode)
(global-hl-line-mode t)                 ; line highlighting
(global-visual-line-mode t)             ; line wrap

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

(setq-default make-backup-files nil)

(setq mac-right-option-modifier 'none)  ; option + s --> ß

(setq-default cursor-type 'bar)
(setq-default show-trailing-whitespace t)
(setq-default show-paren-delay 0)
(show-paren-mode 1)                     ; parentheses matching

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (exec-path-from-shell-initialize))

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

(if (memq window-system '(mac ns))
  (setq my/chinese-font-family "Kaiti SC")
  (setq my/chinese-font-family "AR PL UKai CN"))

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family my/chinese-font-family)))
(setq face-font-rescale-alist '((my/chinese-font-family . 1.2)))

(defvar my/ligature-cascadia-code-ligatures '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                              ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                              "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                              "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                              "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                              "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                              "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                              "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                              ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                              "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                              "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                              "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                              "\\\\" "://"))

(use-package ligature
  :load-path "ligature.el"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'org-mode my/ligature-cascadia-code-ligatures)
  (ligature-set-ligatures 'python-mode '("www" "__" "!=" "=="))
  ;; enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(global-set-key (kbd "C-c m f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c m m") 'toggle-frame-maximized)
(global-set-key (kbd "C-c m 0") 'text-scale-adjust)
(global-set-key (kbd "C-c m g") 'goto-line)
(global-set-key (kbd "C-c m r") 'revert-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "s-，") 'customize)
(global-set-key (kbd "M-【") 'previous-buffer)
(global-set-key (kbd "M-】") 'next-buffer)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-c m v") 'add-file-local-variable-prop-line)

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

  (setq org-agenda-files '("~/hub/schedule/"))
  (setq org-agenda-file-regexp "\\`[^.].*\\.org\\(\\.gpg\\)?\\'") ; ".org" or ".org.gpg"
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)          ; record close time for todo item
  (setq org-duration-format 'h:mm)   ; time format
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

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

  (setq org-default-notes-file "~/hub/refile.org")
  (setq org-capture-templates
        '(("i" "Idea" entry
           (file org-default-notes-file)
           "* %U%?\n%i\n")
          ("t" "Task" entry
           (file org-default-notes-file)
           "* TODO %?\n %i\n %a")
          ("c" "Clipboard" entry
           (file+headline org-default-notes-file "Clipboard")
           "* %?\n%i\n%a")))

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
          ("roam-html-org"
           :base-directory "~/hub/roam"
           :base-extension "org"
           :publishing-directory "~/hub/roam_html"
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
          ("roam-html-static"
           :base-directory "~/hub/roam"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/hub/roam_html"
           :recursive t
           :exclude "\\*proj\\*"
           :publishing-function org-publish-attachment)
          ("roam-html" :components ("roam-html-org" "roam-html-static"))))
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/hub/roam")
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
  (setq org-roam-index-file "~/hub/roam/index.org"))

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(use-package org-journal
  :ensure t
  :defer t
  :config
  (setq org-journal-dir "~/hub/")
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

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config
  (yas-global-mode)
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
  :hook (after-init . global-flycheck-mode)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package epa-file
  :ensure nil
  :config
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback))

(use-package git-annex)

(use-package magit-annex)

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              (("?" . my/dired-get-size)))
  :config
  (setq dired-listing-switches "-avlh"))

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

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/multimarkdown"))

(use-package geiser
  :config
  (setq geiser-mit-binary "mit-scheme")
  (setq geiser-active-implementations '(mit chez guile))
  (setq geiser-default-implementation 'mit))

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
