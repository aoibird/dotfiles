;; -*- lexical-binding: t; -*-
;; NOTE: This file is generated from init.org.

(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
     ("http" . "127.0.0.1:7890")
     ("https" . "127.0.0.1:7890")))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; install use-package if it does not exist
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure nil)

(defun my/packages-installed-p ()
  (let ((ret t))
    (dolist (pkg package-selected-packages)
      (when (not (package-installed-p pkg)) (setq ret nil)))
    ret))

(setq package-selected-packages
      '(;; look and feel
        all-the-icons ;; and M-x all-the-icons-install-fonts
        doom-themes
        doom-modeline ;; and M-x nerd-icons-install-fonts
        visual-fill-column all-the-icons-dired rainbow-delimiters
        ;; org
        org org-roam org-bullets org-journal org-noter org-alert org-ql
        org-ref org-drill org-roam-ui org-fragtog org-contacts
        ;; utilities
        magit yasnippet ibuffer ivy counsel swiper projectile flycheck flycheck-haskell
        exec-path-from-shell activity-watch-mode try vlf pangu-spacing
        git-annex magit-annex pdf-tools zotxt telega alert dashboard htmlize simple-httpd crux deft
        ;; markup
        markdown-mode yaml-mode dockerfile-mode cmake-mode nix-mode bison-mode csv-mode auctex
        ;; programming
        numpydoc pyvenv ein matlab-mode typescript-mode php-mode web-mode go-mode ess
        geiser geiser-chez slime racket-mode sml-mode clojure-mode rust-mode haskell-mode lua-mode
        ))
(setq package-pinned-packages '((telega . "melpa-stable")))

;; Install packages
(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (package-install-selected-packages))

(add-to-list 'default-frame-alist '(height . 36)) ; frame height
(add-to-list 'default-frame-alist '(width . 80))  ; frame width
(tool-bar-mode -1)                                ; hide toolbar
(scroll-bar-mode -1)                              ; hide scrollbar
(setq-default
 inhibit-startup-screen t               ; hide startup screen
 tab-width 8
 indent-tabs-mode nil
 c-basic-offset 4
 cursor-type 'bar
 show-trailing-whitespace t
 show-paren-delay 0
 ;; major-mode 'org-mode
 line-spacing 0.1)
;; (fringe-mode '(0 . 0))
(fringe-mode '(1 . 1))
(show-paren-mode 1)                     ; parentheses matching
(setq ring-bell-function 'ignore)
(setq auth-sources '("~/.emacs.d/authinfo" "~/.emacs.d/authinfo.gpg" "~/.emacs.d/netrc"))

(use-package doom-themes
  :init (load-theme 'doom-tomorrow-day t))

(display-battery-mode t)
(column-number-mode t)
(display-time-mode t)
(setq-default display-time-interval 30)
(setq-default display-time-default-load-average nil)
(setq-default display-time-format "%Y-%m-%d %a %H:%M")
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-enable-word-count t))

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
  :load-path "git/ligature.el"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'org-mode my/ligature-cascadia-code-ligatures)
  (ligature-set-ligatures 'prog-mode my/ligature-cascadia-code-ligatures)
  (ligature-set-ligatures 'markdown-mode '(("=" (rx (+ "=") (? (| ">" "<"))))
                                           ("-" (rx (+ "-")))))
  ;; (ligature-set-ligatures 'haskell-mode my/ligature-cascadia-code-ligatures)
  ;; (ligature-set-ligatures 'rust-mode my/ligature-cascadia-code-ligatures)
  ;; (ligature-set-ligatures 'python-mode '("www" "__" "!=" "=="))
  ;; enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(setq word-wrap-by-category t)          ; make Emacs respect kinsoku rules when wrapping lines visually.
;; (set-face-attribute 'default nil :font "Fira Code-14")
;; (set-face-attribute 'italic nil :font "Cascadia Code-14" :slant 'italic)
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset (font-spec :family "方正楷体_GBK" :size 20)))
;; (setq face-font-rescale-alist '(("方正楷体_GBK" . 1.3)))

;; Derived from Centaur Emacs
(set-face-attribute 'default nil :family "Fira Code" :height 140)
(set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'prepend)
(set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend)
(setq face-font-rescale-alist '(("方正楷体_GBK" . 1.28)))
(set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family "方正楷体_GBK"))

(custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Merriweather" :height 1.0))))
   '(fixed-pitch ((t (:family "Fira Code Retina" :height 1.0)))))
(add-hook 'org-mode-hook 'variable-pitch-mode)

(custom-theme-set-faces
 'user
 '(org-document-title ((t (:height 1.4))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.1))))
 '(line-number ((t (:inherit fixed-pitch))))
 '(line-number-current-line ((t (:inherit fixed-pitch))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(table-cell ((t (:inherit fixed-pitch :background "#f5f5f5"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(setq-default make-backup-files nil)

(setq mac-right-option-modifier 'none)  ; option + s --> ß

;; (global-display-line-numbers-mode)
;; (global-hl-line-mode t)                 ; line highlighting
;; (global-visual-line-mode t)             ; line wrap
(setq display-line-numbers-width 4)
(dolist (mode '(text-mode-hook
                  prog-mode-hook
                  conf-mode-hook))
    (add-hook mode (lambda ()
                     (display-line-numbers-mode 1)
                     (visual-line-mode t)
                     (hl-line-mode t))))

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (setq ns-use-proxy-icon  nil)
  (setq ns-pop-up-frames nil)
  (setq frame-title-format nil))

(setq
  x-select-enable-clipboard t
  x-select-enable-primary nil
  x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
  x-stretch-cursor nil)

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

(defun my/org-enable-write-mode ()
  (interactive)
  (display-line-numbers-mode -1)
  ;; (setq line-prefix "\t")
  ;; (setq line-prefix nil)
  (org-num-mode t)
  (setq-local visual-fill-column-width 80)
  (visual-fill-column-mode)
  (hl-line-mode -1)
  (setq-local org-bullets-bullet-list '(" "))
  (org-bullets-mode)
  )

(defun individual-visibility-source-blocks ()
  "Fold some blocks in the current buffer."
  (interactive)
  (org-show-block-all)
  (org-block-map
   (lambda ()
     (let ((case-fold-search t))
       (when (and
              (save-excursion
                (beginning-of-line 1)
                (looking-at org-block-regexp))
              (cl-assoc
               ':hidden                 ;  mark blocks with :hidden
               (cl-third
                (org-babel-get-src-block-info))))
         (org-hide-block-toggle))))))

(add-hook
 'org-mode-hook
 (function individual-visibility-source-blocks))

(defun my/org-backward-narrow ()
  (interactive)
  (progn
    (widen)
    (org-backward-heading-same-level 1 t)
    (org-narrow-to-subtree)))

(defun my/org-forward-narrow ()
  (interactive)
  (progn
    (widen)
    (org-forward-heading-same-level 1 t)
    (org-narrow-to-subtree)))

;; (defun my/open-alacritty ()
;;   (interactive "@")
;;   (shell-command (concat "alacritty --working-directory"
;;                          (file-name-directory (or load-file-name buffer-file-name))
;;                          " > /dev/null 2>&1 & disown") nil nil))
(defun my/open-alacritty ()
  (interactive)
  (call-process "alacritty" nil 0 nil "--working-directory" default-directory))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun my/org-extract-index-from-directory (directory)
  (let* ((filenames (directory-files directory nil "org$"))
         (metadata '())
         (text "# -*- mode: org -*-\n#+title: index\n\n"))
    (dolist (filename filenames text)
      (let* ((filepath (concat directory "/" filename))
             (meta (my/org-get-metadata filepath)))
        (add-to-list 'metadata meta)))
    (setq metadata (sort metadata #'(lambda (x y) (string< (nth 2 x) (nth 2 y)))))
    (dolist (meta metadata text)
      (setq text (concat text
                         (format "* TODO [[%s][%s]]" (nth 0 meta) (nth 2 meta))
                         ;; (replace-regexp-in-string "/" ":" (format "%s" (nth 1 meta)))
                         (if (nth 3 meta) (format "    %s\n" (nth 3 meta)) "\n")
                         )))
    text))

(defun my/org-insert-index (directory outpath)
  (with-temp-buffer
    (insert (my/org-extract-index-from-directory directory))
      (write-region (point-min) (point-max) outpath nil)))

(defun my/org-generate-index ()
  (interactive)
  (my/org-insert-index "~/cloud/personal/orgroam" "~/temp/index.org")
  (find-file "~/temp/index.org"))

(defun my/org-insert-modified-time ()
  (when (eq major-mode 'org-mode)
    (org-roam-set-keyword "time_updated" (format-time-string "%Y-%m-%dT%H:%M:%S%:z"))))
(setq auto-revert-avoid-polling t)

(global-set-key (kbd "C-c m f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c m m") 'toggle-frame-maximized)
(global-set-key (kbd "C-c m 0") 'text-scale-adjust)
(global-set-key (kbd "C-c m g") 'goto-line)
(global-set-key (kbd "C-c m r") 'revert-buffer)
(global-set-key (kbd "C-c m v") 'add-file-local-variable-prop-line)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "s-，") 'customize)
(global-set-key (kbd "M-【") 'previous-buffer)
(global-set-key (kbd "M-】") 'next-buffer)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-c C-x C-i") 'org-clock-in)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-{") 'my/org-backward-narrow)
(global-set-key (kbd "C-}") 'my/org-forward-narrow)
(global-set-key (kbd "C-c m i") #'(lambda () (interactive) (find-file "~/.emacs.d/init.org")))
(global-set-key (kbd "C-c m s") #'(lambda () (interactive) (find-file "~/.ssh/config")))
(global-set-key (kbd "C-c m l") #'(lambda () (interactive) (load-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c m t") 'my/org-generate-toc)
(global-set-key (kbd "C-c m d") 'my/org-generate-index)
(global-set-key (kbd "C-c m w") 'my/org-enable-write-mode)

(use-package exec-path-from-shell
  ;; :if (memq window-system '(mac ns))
  :init
  (exec-path-from-shell-initialize)
  :config
  (setq exec-path-from-shell-arguments '("-l")))

(use-package all-the-icons-dired
  :ensure nil
  :load-path "git/all-the-icons-dired"
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

(use-package crux
  :ensure t)

(use-package deft
  :bind ("<f8>" . deft)
  :commands deft
  :init
  (setq deft-extensions '("txt" "tex" "org" "md"))
  (setq deft-directory "~/cloud")
  (setq deft-recursive t)
  ;; replace all slashes and spaces with hyphens and will convert the file name to lowercase
  (setq deft-file-naming-rules
  '((noslash . "-")
    (nospace . "-")
    (case-fn . downcase))))

(use-package diff
  :config
  (setq diff-switches "-u -r"))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map (("?" . my/dired-get-size)))
  :config
  (setq dired-listing-switches "-avlh --time-style=long-iso --group-directories-first")
  (setq dired-guess-shell-alist-user '(("\\.pdf\\'" "okular")
                                       ("\\.doc\\'" "libreoffice")
                                       ("\\.docx\\'" "libreoffice")
                                       ("\\.ppt\\'" "libreoffice")
                                       ("\\.pptx\\'" "libreoffice")
                                       ("\\.xls\\'" "libreoffice")
                                       ("\\.xlsx\\'" "libreoffice")
                                       ("\\.jpg\\'" "pinta")
                                       ("\\.png\\'" "pinta")
                                       ("\\.java\\'" "idea")))
  (add-to-list 'display-buffer-alist
               (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil))))

(use-package org
  :ensure t
  :load-path "git/org-mode/lisp"
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  :init
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook 'org-babel-tangle nil t))) ; tangle on save
  (add-hook 'org-mode-hook (lambda () (add-hook 'before-save-hook 'my/org-insert-modified-time nil 'local)))
  :config
  (require 'ox-latex)
  (setq org-adapt-indentation nil) ; prevent demoting heading also shifting text inside sections
  (setq org-tags-column 60)        ; set position of tags
  (setq org-hide-emphasis-markers nil)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-+]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (setq org-image-actual-width nil)
  (setq org-startup-shrink-all-tables t)
  (setq org-startup-with-inline-images t)
  ;; (setq org-startup-numerated t)

  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-crypt)
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-modules 'org-attach-git)
  (add-to-list 'org-modules 'org-protocol)
  (setq org-habit-graph-column 50) ; set position of habit graph
  (setq org-agenda-tags-column 80)
  ;; (setq org-agenda-files '("~/cloud/org/"))
  (setq org-agenda-file-regexp "\\`[^.].*\\.org\\.gpg\\'") ; ".org.gpg"
  (setq org-agenda-file-regexp "\\`[^.].*\\.org\\(\\.gpg\\)?\\'") ; ".org" or ".org.gpg"
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)          ; record close time for todo item
  (setq org-duration-format 'h:mm)   ; time format
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-custom-commands
        '(("r" "Review this week"
           ((agenda "" ((org-agenda-span 8)
                        (org-agenda-start-day "-7d")
                        (org-agenda-entry-types '(:timestamp))
                        ;; (org-agenda-tag-filter-preset '("+work"))
                        (org-agenda-show-log t)
                        (org-agenda-archives-mode t)
                        (org-agenda-clockreport-mode t)
                        ;; (org-agenda-log-mode-items '(clock closed))
                        ;; (org-agenda-start-with-log-mode '(closed))
                        ))
            ))
          ("w" "Agenda for work" agenda ""
           ((org-agenda-tag-filter-preset '("+work")) (org-agenda-clockreport-mode t))
           ("~/cloud/agenda.ics" "~/cloud/agenda.ps"))
          ("t" "Agenda for today" agenda ""
           ((org-agenda-span 1)))
          ;; ("Y" alltodo "" nil ("~/cloud/todo.html" "~/cloud/todo.txt"))
          ;; ("W" "Completed and/or deferred tasks from previous week"
          ;;  ((agenda "" ((org-agenda-span 7)
          ;;               (org-agenda-start-day "-7d")
          ;;               (org-agenda-entry-types '(:timestamp))
          ;;               (org-agenda-show-log t)))))
          ;; ("n" todo ""
          ;;  ((org-agenda-max-entries 5) (org-agenda-tag-filter-preset '("+work"))))
          ;; ("~/cloud/agenda.ics" "~/cloud/agenda.ps")
          ))
  (setq org-babel-python-command "python3")
  (setq org-export-babel-evaluate nil)
  (setq org-confirm-babel-evaluate nil)
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
  (setq org-default-notes-file "~/cloud/org/refile.gpg")
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
  
  ;; org-latex
  (setq org-latex-listings 'minted)
  (setq org-latex-compiler "xelatex")
  (add-to-list 'org-latex-classes
               '("ctexart" "\\documentclass[11pt]{ctexart}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-packages-alist '(("" "minted")
                                   ("" "booktabs")
                                   ("" "listings")
                                   ("" "amssymb")
                                   ("" "amsmath")
                                   ("" "amsfonts")
                                   ("" "amsthm")
                                   ("" "mathtools")
                                   ("" "ctex" t ("xelatex"))))
  (setq org-latex-pdf-process
        '("latexmk -shell-escape -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))
  
  
  ;; org-latex-preview
  (setq org-latex-preview-numbered t
        org-latex-preview-compiler-command-map '(("xelatex" . "xelatex -no-pdf -shell-escape")
                                                 ("pdflatex" . "latex")
                                                 ("lualatex" . "dvilualatex")))
  
  ;; org-html
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-head-include-default-style nil)
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (setq org-publish-project-alist
        '(
          ("notes-html-org"
           :base-directory "~/cloud/personal/orgroam"
           :base-extension "org"
           :publishing-directory "~/sync/publish/orgroam"
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
           ;; :auto-sitemap t
           ;; :sitemap-sort-files alphabetically
           ;; :sitemap-filename "sitemap.org"
           ;; :sitemap-title "Sitemap"
           )
          ("notes-html-static"
           :base-directory "~/cloud/personal/orgroam"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/sync/publish/orgroam"
           :recursive t
           :exclude "\\*proj\\*"
           :publishing-function org-publish-attachment)
          ("notes-html" :components ("notes-html-org" "notes-html-static"))))
  (setq org-attach-preferred-new-method 'dir)
  (setq org-attach-store-link-p t)
  (setq org-attach-dir-relative t)
  (setq org-attach-git-use-annex nil)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key nil)
  (setq auto-save-default nil)
  (setq org-edit-src-content-indentation 0
        org-src-window-setup 'current-window
        org-src-strip-leading-and-trailing-blank-lines t
  )
  (defface org-link-id
    '((t :foreground "#9bae4c" :weight bold :underline t))
    "Face for Org-Mode links starting with id:."
    :group 'org-faces)
  (defface org-link-file
    '((t :foreground "#ff5555" :weight bold :underline t))
    "Face for Org-Mode links starting with file:."
    :group 'org-faces)
  (defface org-link-zotero
    '((t :foreground "#8959a8" :weight bold :underline t))
    "Face for Org-Mode links starting with zotero:."
    :group 'org-faces)
  ;; (org-link-set-parameters "id" :face 'org-link-id)
  ;; (org-link-set-parameters "file" :face 'org-link-file)
  (org-link-set-parameters
   "zotero"
   :face 'org-link-zotero
   :follow (lambda (zpath)
             ;; we get the "zotero:"-less url, so we put it back.
             (browse-url (format "zotero:%s" zpath))))
  (org-link-set-parameters
   "file"
   :face (lambda (path) (if (file-exists-p path) 'org-link 'org-warning)))
  (org-link-set-parameters
   "id"
   :face (lambda (id) (if (org-roam-id-find id) 'org-link-id 'org-warning)))
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :after org
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/cloud/personal/orgroam"))
  (org-roam-db-location (file-truename "~/cloud/personal/orgroam/org-roam.db"))
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+time_created: %<%Y-%m-%dT%H:%M:%S%:z>\n")
      :unnarrowed t)
     ("p" "recipe" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-recipe_${slug}.org"
                         "#+title: (recipe) ${title}\n#+filetags: :recipe:\n#+time_created: %<%Y-%m-%dT%H:%M:%S%:z>\n")
      :unnarrowed t)
     ("r" "reference" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-ref_${slug}.org"
                         "#+title: (ref) ${title}\n#+filetags: :ref:\n#+time_created: %<%Y-%m-%dT%H:%M:%S%:z>\n")
      :unnarrowed t)
     ("e" "English dictionary" plain
      "%?\n\n* Definition\n\n* Try\n\n* Further reading\n\n"
      :target (file+head "%<%Y%m%d%H%M%S>-english_dictionary_${slug}.org"
                         "#+title: English dictionary: ${title}\n#+filetags: :english:\n#+time_created: %<%Y-%m-%dT%H:%M:%S%:z>\n")
      :unnarrowed t)
     ))
  (org-roam-capture-ref-templates
   '(("r" "ref" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-ref_${slug}.org"
                         "#+title: (ref) ${title}\n#+filetags: :ref:\n#+time_created: %<%Y-%m-%dT%H:%M:%S%:z>\n\n${body}")
      :unnarrowed t)))
  (org-roam-node-display-template
      (concat "(" (propertize "${tags:10}" 'face 'org-tag) ") " "${title}"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n e" . org-roam-ref-add)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n t" . org-roam-tag-add)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-protocol
  :after org-protocol)

(use-package org-journal
  :ensure t
  :defer t
  :config
  (setq org-journal-dir "~/cloud/personal/journal")
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

(use-package org-download
  :ensure t
  :defer t
  :init
  ;; Add handlers for drag-and-drop when Org is loaded.
  (with-eval-after-load 'org (org-download-enable))
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "assets")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y-%m-%dT%H-%M-%S%:z_")
  (org-image-actual-width 600)
  (org-download-screenshot-method
   (cond
    ((eq system-type 'gnu/linux)
     "xclip -selection clipboard -t image/png -o > '%s'")
    ((eq system-type 'darwin)
     "pngpaste %s")))
  :bind
  ("C-M-y" . org-download-screenshot))

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
           ("personal" (and (filename . "personal/")
                        (or (mode . org-mode)
                            (mode . prog-mode))))
           ("magit" (or
                     (name . "magit\*")
                     (mode . Magit)))
           ("ssh" (filename . "ssh:"))
           ("org" (mode . org-mode))
           ("dired" (mode . dired-mode))
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

(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-port 7070)
  (setq httpd-host (system-name)))

(use-package impatient-mode
  :ensure t
  :commands impatient-mode)

(defun my/markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

(defun my/markdown-preview ()
  "Preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'my/markdown-filter)
  (imp-visit-buffer))

(use-package magit
  :ensure t
  :init
  :bind
  ("C-x g" . magit-status)
  :config
  ;; (setq magit-display-buffer-function
  ;;     (lambda (buffer)
  ;;       (display-buffer buffer '(display-buffer-same-window))))
  (setq magit-display-buffer-function
    (lambda (buffer)
      (display-buffer
       buffer (if (and (derived-mode-p 'magit-mode)
                       (memq (with-current-buffer buffer major-mode)
                             '(magit-process-mode
                               magit-revision-mode
                               magit-diff-mode
                               magit-stash-mode
                               magit-status-mode)))
                  nil
                '(display-buffer-same-window)))))
  )

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-engine 'xetex)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t)
  (setq latex-run-command "pdflatex")
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (push
                                '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
                                  :help "Run latexmk on file")
                                TeX-command-list)))
  (add-hook 'LaTeX-mode-hook #'latex-extra-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook
            (defun preview-larger-previews ()
              (setq preview-scale-function
                    (lambda () (* 1.25
                                  (funcall (preview-scale-from-face))))))))

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
  ;; (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  (setq projectile-project-search-path '(("~/cloud". 4)))
  ()
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package flycheck
  :hook
  (after-init . global-flycheck-mode)
  (haskell-mode-hook . flycheck-haskell-setup)
  :config
  (setq flycheck-haskell-hlint-executable "~/.local/share/cabal/bin/hlint")
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package flyspell
  :hook (org-mode . flyspell-mode))

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

(use-package page-break-lines
:ensure t
:hook (dashboard-mode-hook . page-break-lines-mode))

(use-package beancount
  :ensure nil
  :load-path "git/beancount-mode"
  :mode (("\\.beancount\\'" . beancount-mode)))
;; (add-to-list 'load-path "~/.emacs.d/beancount-mode")
;; (require 'beancount)
;; (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

(use-package activity-watch-mode
  :config
  (global-activity-watch-mode))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc -t html5"))

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
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-part-padding 0)
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0))

(use-package js
  :config
  (setq js-indent-level 2))
