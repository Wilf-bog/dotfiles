(when (< emacs-major-version 29)
  (error "Emacs Writing Studio requires Emacs version 29 or later"))

;; Custom settings in a separate file and load the custom settings

(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(keymap-global-set "C-c w v" 'customize-variable)

;; Revert buffers when the underlying file has changed

(global-auto-revert-mode 1)

;; Revert Dired and other buffers

(setq global-auto-revert-non-file-buffers t)

;; Auto-save mode for org files

(auto-save-visited-mode +1)
(setq auto-save-visited-predicate
      (lambda () (eq major-mode 'org-mode)))

;; Set package archives

(use-package package
  :config
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

;; Package Management

(use-package use-package
  :custom
  (use-package-always-ensure t)
  (package-native-compile t)
  (warning-minimum-level :emergency))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; Load EWS functions

(load-file (concat (file-name-as-directory user-emacs-directory) "ews.el"))

;; Check for missing external software
;;
;; - soffice (LibreOffice): View and create office documents
;; - zip: Unpack ePub documents
;; - pdftotext (poppler-utils): Convert PDF to text
;; - djvu (DjVuLibre): View DjVu files
;; - curl: Reading RSS feeds
;; - divpng: Part of LaTeX
;; - dot (GraphViz): Create note network diagrams
;; - convert (ImageMagick): Convert image files 
;; - gm (GraphicsMagick): Convert image files
;; - latex (TexLive, MacTex or MikTeX): Preview LaTex and export Org to PDF
;; - hunspell: Spellcheck. Also requires a hunspell dictionary
;; - grep: Search inside files
;; - ripgrep: Faster alternative for grep
;; - gs (GhostScript): View PDF files
;; - mutool (MuPDF): View PDF files
;; - mpg321, ogg123 (vorbis-tools), mplayer, mpv, vlc: Media players
;; - git: Version control

(ews-missing-executables
 '("soffice" "zip" "pdftotext" "ddjvu"
   "curl"
   "dvipng"
   "dot"
   ("convert" "gm")
   "latex"
   "hunspell"
   ("grep" "ripgrep")
   ("gs" "mutool")
   ("mpg321" "ogg123" "mplayer" "mpv" "vlc")
   "git"))

;;(load-file (concat (file-name-as-directory user-emacs-directory) "prot-eww.el"))
(add-to-list 'load-path "~/.emacs.d/manual-packages/denote")

;; Keyboard-centric user interface removing tool, menu and scroll bars

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(column-number-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Icons

(use-package all-the-icons)

;; Short answers only please

(setq use-short-answers t)

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; Spacious padding

(use-package spacious-padding
  :custom
  (line-spacing 3)
  :init
  (spacious-padding-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Modus Themes

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-to-toggle
   '(modus-operandi-tinted modus-vivendi-tinted))
  :init
  (load-theme 'modus-operandi-tinted :no-confirm)
  :bind
  (("C-c w t t" . modus-themes-toggle)
   ("C-c w t m" . modus-themes-select)
   ("C-c w t s" . consult-theme)))

;; Cappucin

					;(use-package catppuccin
					;:ensure t
					;:defer t)

;; Doom-themes

					;(use-package doom-themes
					;:ensure t
					;:config
    ;;; Global settings (defaults)
					;(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
					;doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;;; (load-theme 'doom-one t)
					;
    ;;; Enable flashing mode-line on errors
					(doom-themes-visual-bell-config)
    ;;; Enable custom neotree theme (all-the-icons must be installed!)
					;(doom-themes-neotree-config)
    ;;; or for treemacs users
					;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
					;(doom-themes-treemacs-config)
    ;;; Corrects (and improves) org-mode's native fontification.
					;(doom-themes-org-config))

;; Mixed-pitch

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

;; Fonts 'default, 'fixed-pitch and 'variable-pitch
(set-face-attribute 'default nil
		    :family "Iosevka Comfy Motion"
		    :height 120
		    :weight 'semilight)
(when (eq system-type 'windows-nt)
  (set-face-attribute 'variable-pitch nil :family "Iosevka Comfy Motion Duo"))
(when (eq system-type 'gnu/linux)
  (set-face-attribute 'variable-pitch nil :family "Noto Serif"))
(set-face-attribute 'fixed-pitch nil :family "Iosevka Comfy")

;; Window management
;; Split windows sensibly

(setq split-width-threshold 120
      split-height-threshold nil)

;; Keep window sizes balanced

(use-package balanced-windows
  :config
  (balanced-windows-mode))

;; Switch quickly between windows

(global-set-key (kbd "M-o") 'other-window)

;; Read the pulsar manual: <https://protesilaos.com/emacs/pulsar>.
(use-package pulsar
  :ensure t
  :config
  (setopt pulsar-pulse t
	  pulsar-delay 0.055
	  pulsar-iterations 10
	  pulsar-face 'pulsar-cyan
	  pulsar-highlight-face 'pulsar-magenta)

  (pulsar-global-mode 1)
  :hook
  ;; There are convenience functions/commands which pulse the line using
  ;; a specific colour: `pulsar-pulse-line-red' is one of them.
  ((next-error . (pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))
   (minibuffer-setup . pulsar-pulse-line-red))
  :bind
  ;; pulsar does not define any key bindings.  This is just my personal
  ;; preference.  Remember to read the manual on the matter.  Evaluate:
  ;;
  ;; (info "(elisp) Key Binding Conventions")
  (("C-x l" . pulsar-pulse-line) ; override `count-lines-page'
   ("C-x L" . pulsar-highlight-dwim))) ; or use `pulsar-highlight-line'

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;(use-package general
  ;:after evil
  ;:config
  ;(general-create-definer efs/leader-keys
    ;:keymaps '(normal insert visual emacs)
    ;:prefix "SPC"
    ;:global-prefix "C-SPC")
  ;(efs/leader-keys
   ;"t" '(:ignore t :which-key "toggles")
   ;"tt" '(consult-theme :which-key "choose theme")
   ;"fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org")))))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Enable vertico

(use-package vertico
  :init
  (vertico-mode)
  :bind (("C-s" . consult-line)
	 :map vertico-map
	 ("C-j" . vertico-next)
	 ("C-k" . vertico-previous)
	 ("C-f" . vertico-exit)
	 :map minibuffer-local-map
	 ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  (vertico-sort-function 'vertico-sort-history-alpha))

;; Persist history over Emacs restarts.

(use-package savehist
  :init
  (savehist-mode 1))

;; Save last place in file after closing it

(add-hook 'org-tab-first-hook 'org-end-of-line)

;; Search for partial matches in any order

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

;; Enable richer annotations using the Marginalia package

(use-package marginalia
  :init
  (marginalia-mode))

;; Improve keyboard shortcut discoverability

(use-package which-key
  :config
  (which-key-mode)
  :custom
  (which-key-max-description-length 40)
  (which-key-lighter nil)
  (which-key-sort-order 'which-key-description-order))

;; Improved help buffers

(use-package helpful
  :bind
  (("C-h f" . helpful-function)
   ("C-h x" . helpful-command)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)))

(use-package text-mode
  :ensure
  nil
  :hook
  (text-mode . visual-line-mode)
  :init
  (delete-selection-mode t)
  :custom
  (sentence-end-double-space nil)
  (scroll-error-top-bottom t)
  (save-interprogram-paste-before-kill t))

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package flyspell
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary ews-hunspell-dictionaries)
  (flyspell-mark-duplications-flag nil) ;; Writegood mode does this
  (org-fold-core-style 'overlays) ;; Fix Org mode bug
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic ews-hunspell-dictionaries)
  :hook
  (text-mode . flyspell-mode)
  :bind
  (("C-c w s s" . ispell)
   ("C-;"       . flyspell-auto-correct-previous-word)))

(use-package org
  (message "Org Mode Loaded!")
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(450))
  (org-fold-catch-invisible-edits 'error)
  (org-startup-with-latex-preview t)
  (org-pretty-entities t)
  (org-use-sub-superscripts "{}")
  (org-id-link-to-org-use-id t))

;; Make navigation easier between org titles

(add-hook 'org-tab-first-hook 'org-end-of-line)

;; Org tags

(setq org-tag-alist
      '(;; Places
	("@home" . ?H)
	("@work" . ?W)

	;; Devices
	("@computer" . ?C)
	("@phone" . ?P)

	;; Activities
	("@ménage" . ?m)
	("@lecture" . ?l)
	("@planning" . ?n)
	("@writing" . ?w)
	("@creative" . ?c)
	("@écouter" . ?é)
	("@visionner" . ?v)
	("@email" . ?e)
	("@calls" . ?a)
	("@errands" . ?r)))


;; More TODO states
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "STARTED(s!)" "WAITING(w)" "|" "DONE(d@/@)" "DELEGATED(é@/@)" "CANCELED(c@/@)")))

;; Show hidden emphasis markers

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

;; LaTeX previews

(use-package org-fragtog
  :after org
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

;; Org modern: Most features disables for beginnng users

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  :custom
  (org-modern-table nil)
  (org-modern-keyword nil)
  (org-modern-timestamp nil)
  (org-modern-priority nil)
  (org-modern-checkbox nil)
  (org-modern-tag nil)
  (org-modern-block-name nil)
  (org-modern-keyword nil)
  (org-modern-footnote nil)
  (org-modern-internal-target nil)
  (org-modern-radio-target nil)
  (org-modern-statistics nil)
  (org-modern-progress nil))

(use-package consult
  :bind
  (("C-c w h" . consult-org-heading)
   ("C-M-j" . consult-buffer)                ;; orig. switch-to-buffer
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("C-c w g" . consult-grep)))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

;; Doc-View

(use-package doc-view
  :custom
  (doc-view-resolution 300)
  (large-file-warning-threshold (* 50 (expt 2 20))))

;; Read ePub files

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Reading LibreOffice files
;; Fixing a bug in Org Mode pre 9.7
;; Org mode clobbers associations with office documents

(use-package ox-odt
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:OD[CFIGPST]\\|od[cfigpst]\\)\\'"
                 . doc-view-mode-maybe)))

(use-package bibtex
  :custom
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file"     "Relative or absolute path to attachments" "" )))
  (bibtex-align-at-equal-sign t)
  (bibtex-set-dialect 'biblatex)
  :config
  (ews-bibtex-register)
  :bind
  (("C-c w b r" . ews-bibtex-register)))

;; Biblio package for adding BibTeX records

(use-package biblio
  :bind
  (("C-c w b b" . ews-bibtex-biblio-lookup)))

;; Citar to access bibliographies

(use-package citar
  :custom
  (citar-bibliography ews-bibtex-files)
  :bind
  (("C-c w b o" . citar-open)))

(use-package citar-embark
:after citar embark
:no-require
:config (citar-embark-mode)
:bind (("C-M-." . embark-act)
       :map citar-embark-citation-map
       ("c" . citar-denote-find-citation)))

;; Read RSS feeds with Elfeed

(use-package elfeed
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-c w e" . elfeed))

;; Configure Elfeed with org mode

(use-package elfeed-org
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files
   (list (concat (file-name-as-directory (getenv "HOME")) ".emacs.d/elfeed/elfeed.org"))))

;; Allow better synchronization
;; See http://babbagefiles.blogspot.com/2017/03/take-elfeed-everywhere-mobile-rss.html

;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force)
  (elfeed-update))

;;write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

;; Easy insertion of weblinks

(use-package org-web-tools
  :bind
  (("C-c w w" . org-web-tools-insert-link-for-url)))

;; `browse-url'

(use-package browse-url
  :ensure nil
  :defer t
  :config
  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-secondary-browser-function 'browse-url-default-browser))

;;;; `eww' (Emacs Web Wowser)
(use-package eww
  :ensure nil
  :commands (eww)
  :bind
  ( :map eww-link-keymap
    ("v" . nil) ; stop overriding `eww-view-source'
    :map eww-mode-map
    ("L" . eww-list-bookmarks)
    :map dired-mode-map
    ("E" . eww-open-file) ; to render local HTML files
    :map eww-buffers-mode-map
    ("d" . eww-bookmark-kill)   ; it actually deletes
    :map eww-bookmark-mode-map
    ("d" . eww-bookmark-kill)) ; same
  :config
  (setq eww-restore-desktop t)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format nil)
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory (expand-file-name "~/Documents/eww-downloads"))
  (setq eww-suggest-uris
        '(eww-links-at-point
          thing-at-point-url-at-point))
  (setq eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/"))
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)") ; On GNU/Linux check your mimeapps.list
  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")
  ;; NOTE `eww-retrieve-command' is for Emacs28.  I tried the following
  ;; two values.  The first would not render properly some plain text
  ;; pages, such as by messing up the spacing between paragraphs.  The
  ;; second is more reliable but feels slower.  So I just use the
  ;; default (nil), though I find wget to be a bit faster.  In that case
  ;; one could live with the occasional errors by using `eww-download'
  ;; on the offending page, but I prefer consistency.
  ;;
  ;; '("wget" "--quiet" "--output-document=-")
  ;; '("chromium" "--headless" "--dump-dom")
  (setq eww-retrieve-command nil))

;;;; `prot-eww' extras
  ;; (use-package prot-eww
    ;; :ensure nil
    ;; :after eww
    ;; :config
    ;; (setq prot-eww-save-history-file
	  ;; (locate-user-emacs-file "prot-eww-visited-history"))
    ;; (setq prot-eww-save-visited-history t)
    ;; (setq prot-eww-bookmark-link nil)
;; 
    ;; (add-hook 'prot-eww-history-mode-hook #'hl-line-mode)
;; 
    ;; (define-prefix-command 'prot-eww-map)
    ;; (define-key global-map (kbd "C-c w") 'prot-eww-map)
;; 
    ;; (prot-emacs-keybind prot-eww-map
			;; "b" #'prot-eww-visit-bookmark
			;; "e" #'prot-eww-browse-dwim
			;; "s" #'prot-eww-search-engine)
    ;; (prot-emacs-keybind eww-mode-map
			;; "B" #'prot-eww-bookmark-page
			;; "D" #'prot-eww-download-html
			;; "F" #'prot-eww-find-feed
			;; "H" #'prot-eww-list-history
			;; "b" #'prot-eww-visit-bookmark
			;; "e" #'prot-eww-browse-dwim
			;; "o" #'prot-eww-open-in-other-window
			;; "E" #'prot-eww-visit-url-on-page
			;; "J" #'prot-eww-jump-to-url-on-page
			;; "R" #'prot-eww-readable
			;; "Q" #'prot-eww-quit))

;; Emacs Multimedia System

(use-package emms
  :init
  (require 'emms-setup)
  (require 'emms-mpris)
  (emms-all)
  (emms-default-players)
  (emms-mpris-enable)
  :custom
  (emms-browser-covers #'emms-browser-cache-thumbnail-async)
  :bind
  (("C-c w m b" . emms-browser)
   ("C-c w m e" . emms)
   ("C-c w m p" . emms-play-playlist )
   ("<XF86AudioPrev>" . emms-previous)
   ("<XF86AudioNext>" . emms-next)
   ("<XF86AudioPlay>" . emms-pause)))

(use-package openwith
  :disabled t
  :config
  (openwith-mode nil)
  :custom
  (openwith-association nil))

(use-package somafm
  :ensure t)

(use-package telega)

(setq telega-server-libs-prefix "/home/frdrcv/bin/td/build/tdtl")

(use-package mastodon
  :ensure t
  :config
  (setq mastodon-instance-url "https://eldritch.cafe"
	mastodon-active-user "bogdanoviste"))

;; Fleeting notes

(use-package org
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)))

;; Capture templates

(setq org-capture-templates
 '(("f" "Fleeting note"
    item
    (file+headline org-default-notes-file "Notes")
    "- %?")
   ("p" "Permanent note" plain
    (file denote-last-path)
    #'denote-org-capture
    :no-save t
    :immediate-finish nil
    :kill-buffer t
    :jump-to-captured t)
   ("t" "New task" entry
    (file+headline "~/gtd/inbox.org" "Tasks")
    "* TODO %i%? \n %U")
   ("r" "Read article" entry
    (file+headline "~/gtd/inbox.org" "Tasks")
    "* %i%? \n %U")
   ("T" "Tickler" entry
    (file+headline "~/gtd/tickler.org" "Tickler")
    "* TODO %i%? \n %U")))

;; Start writing immediately after triggering org-capture

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(with-eval-after-load 'org
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(setq org-agenda-files '("~/gtd/inbox.org"
                           "~/gtd/gtd.org"
                           "~/gtd/projets.org"
                           "~/gtd/tickler.org"))

  (setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
                             ("~/gtd/someday.org" :level . 1)
                             ("~/gtd/projets.org" :maxlevel . 5)
                             ("~/gtd/tickler.org" :maxlevel . 2)))

;; Inbox location

;; (setq org-default-notes-file (concat org-directory "/notes.org"))

;; Denote

(use-package denote
  :custom
  (denote-sort-keywords t)
  (denote-rename-buffer-mode 1)
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :init
  (require 'denote-org-extras)
  :bind
  (("C-c w d b" . denote-find-backlink)
   ("C-c w d d" . denote-date)
   ("C-c w d f" . denote-find-link)
   ("C-c w d h" . denote-org-extras-link-to-heading)
   ("C-c w d i" . denote-link-or-create)
   ("C-c w d I" . denote-org-extras-dblock-insert-links)
   ("C-c w d k" . denote-rename-file-keywords)
   ("C-c w d l" . denote-link-find-file)
   ("C-c w d n" . denote)
   ("C-c w d r" . denote-rename-file)
   ("C-c w d R" . denote-rename-file-using-front-matter)))

;; Consult-Notes for easy access to notes

(use-package consult-notes
  :bind
  (("C-c w f"   . consult-notes)
   ("C-c w d g" . consult-notes-search-in-all-notes))
  :init
  (consult-notes-denote-mode))

;; Citar-Denote to manage literature notes

(use-package citar-denote
  :custom
  (citar-open-always-create-notes t)
  :init
  (citar-denote-mode)
  :bind
  (("C-c w b c" . citar-create-note)
   ("C-c w b n" . citar-denote-open-note)
   ("C-c w b x" . citar-denote-nocite)
   :map org-mode-map
   ("C-c w b k" . citar-denote-add-citekey)
   ("C-c w b K" . citar-denote-remove-citekey)
   ("C-c w b d" . citar-denote-dwim)
   ("C-c w b e" . citar-denote-open-reference-entry)))

;; Explore and manage your Denote collection

(use-package denote-explore
  :bind
  (;; Statistics
   ("C-c w x c" . denote-explore-count-notes)
   ("C-c w x C" . denote-explore-count-keywords)
   ("C-c w x b" . denote-explore-keywords-barchart)
   ("C-c w x x" . denote-explore-extensions-barchart)
   ;; Random walks
   ("C-c w x r" . denote-explore-random-note)
   ("C-c w x l" . denote-explore-random-link)
   ("C-c w x k" . denote-explore-random-keyword)
   ;; Denote Janitor
   ("C-c w x d" . denote-explore-identify-duplicate-notes)
   ("C-c w x z" . denote-explore-zero-keywords)
   ("C-c w x s" . denote-explore-single-keywords)
   ("C-c w x o" . denote-explore-sort-keywords)
   ("C-c w x w" . denote-explore-rename-keyword)
   ;; Visualise denote
   ("C-c w x n" . denote-explore-network)
   ("C-c w x v" . denote-explore-network-regenerate)
   ("C-c w x D" . denote-explore-degree-barchart)))

(use-package org
  :bind
  (:map org-mode-map
        ("C-c w n" . ews-org-insert-notes-drawer)
        ("C-c w p" . ews-org-insert-screenshot)
        ("C-c w c" . ews-org-count-words)))

(use-package olivetti
  :demand t
  :bind
  (("C-c w o" . ews-olivetti)))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  :bind
  (("C-c w u" . undo-tree-visualize)))

(require 'oc-natbib)
(require 'oc-csl)

(setq org-cite-global-bibliography ews-bibtex-files
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar)

(use-package dictionary
  :custom
  (dictionary-server "dict.org")
  :bind
  (("C-c w s d" . dictionary-lookup-definition)))

(use-package powerthesaurus
:bind
(("C-c w s p" . powerthesaurus-transient)))

(use-package writegood-mode
  :bind
  (("C-c w s r" . writegood-reading-ease))
  :hook
  (text-mode . writegood-mode))

(add-hook 'text-mode-hook 'abbrev-mode)

(use-package lorem-ipsum
  :custom
  (lorem-ipsum-list-bullet "- ") ;; Org mode bullets
  :init
  (setq lorem-ipsum-sentence-separator (if sentence-end-double-space "  " " "))
  :bind
  (("C-c w i s" . lorem-ipsum-insert-sentences)
   ("C-c w i p" . lorem-ipsum-insert-paragraphs)
   ("C-c w i l" . lorem-ipsum-insert-list)))

(use-package ediff
  :ensure nil
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package fountain-mode)

(use-package markdown-mode)

(use-package org
  :custom
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-broken-links t)
  (org-export-with-toc nil)
  (org-export-with-smart-quotes t)
  (org-export-date-timestamp-format "%e %B %Y"))

(when (eq system-type 'gnu/linux)	;For now, pdf-tools can't be installed on Windows
  (use-package pdf-tools
    :config
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-page)
    :bind (:map pdf-view-mode-map
		("\\" . hydra-pdftools/body)
		("<s-spc>" .  pdf-view-scroll-down-or-next-page)
		("g"  . pdf-view-first-page)
		("G"  . pdf-view-last-page)
		("l"  . image-forward-hscroll)
		("h"  . image-backward-hscroll)
		("j"  . pdf-view-next-page)
		("k"  . pdf-view-previous-page)
		("e"  . pdf-view-goto-page)
		("u"  . pdf-view-revert-buffer)
		("al" . pdf-annot-list-annotations)
		("ad" . pdf-annot-delete)
		("aa" . pdf-annot-attachment-dired)
		("am" . pdf-annot-add-markup-annotation)
		("at" . pdf-annot-add-text-annotation)
		("y"  . pdf-view-kill-ring-save)
		("i"  . pdf-misc-display-metadata)
		("s"  . pdf-occur)
		("b"  . pdf-view-set-slice-from-bounding-box)
		("r"  . pdf-view-reset-slice)))

  (pdf-tools-install))

;; LaTeX PDF Export settings

(use-package ox-latex
  :ensure nil
  :demand t
  :custom
  ;; Multiple LaTeX passes for bibliographies
  (org-latex-pdf-process
   '("pdflatex -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; Clean temporary files after export
  (org-latex-logfiles-extensions
   (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
           "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
           "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
           "tex" "bcf"))))

;; LaTeX templates

(with-eval-after-load 'ox-latex
  (add-to-list
   'org-latex-classes
   '("crc"
     "\\documentclass[krantz2]{krantz}
        \\usepackage{lmodern}
        \\usepackage[authoryear]{natbib}
        \\usepackage{nicefrac}
        \\usepackage[bf,singlelinecheck=off]{caption}
        \\captionsetup[table]{labelsep=space}
        \\captionsetup[figure]{labelsep=space}
        \\usepackage{Alegreya}
        \\usepackage[scale=.8]{sourcecodepro}
        \\usepackage[breaklines=true]{minted}
        \\usepackage{rotating}
        \\usepackage[notbib, nottoc,notlot,notlof]{tocbibind}
        \\usepackage{amsfonts, tikz, tikz-layers}
        \\usetikzlibrary{fadings, quotes, shapes, calc, decorations.markings}
        \\usetikzlibrary{patterns, shadows.blur}
        \\usetikzlibrary{shapes,shapes.geometric,positioning}
        \\usetikzlibrary{arrows, arrows.meta, backgrounds}
        \\usepackage{imakeidx} \\makeindex[intoc]
        \\renewcommand{\\textfraction}{0.05}
        \\renewcommand{\\topfraction}{0.8}
        \\renewcommand{\\bottomfraction}{0.8}
        \\renewcommand{\\floatpagefraction}{0.75}
        \\renewcommand{\\eqref}[1]{(Equation \\ref{#1})}
        \\renewcommand{\\LaTeX}{LaTeX}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\paragraph*{%s}"))))

(use-package ox-epub
  :demand t)

;; Use GraphViz for flow diagrams
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)))) ; this line activates dot

;; Bind org agenda command

(use-package org
  :custom
  (org-log-into-drawer t)
  :bind
  (("C-c a" . org-agenda)))

(use-package dired
  :ensure
  nil
  :commands
  (dired dired-jump)
  :custom
  (dired-listing-switches
   "-goah --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)
  :init
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(autoload 'dired-omit-mode "dired-x")

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Hide hidden files

(use-package dired-hide-dotfiles
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map "H" 'dired-hide-dotfiles-mode))

;; Backup files

(setq-default backup-directory-alist
              `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
              version-control t
              delete-old-versions t
              create-lockfiles nil)

;; Recent files

(use-package recentf
  :config
  (recentf-mode t)
  (run-at-time nil (* 5 60)
               (lambda () (let ((save-silently t))
                            (recentf-save-list))))
  :custom
  (recentf-max-saved-items 50)
  :bind
  (("C-c w r" . recentf-open)))

;; Bookmarks

(use-package bookmark
  :custom
  (bookmark-save-flag 1)
  :bind
  ("C-x r D" . bookmark-delete))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
