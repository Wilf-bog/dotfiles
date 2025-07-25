(when (< emacs-major-version 29)
  (error "Emacs Writing Studio requires Emacs version 29 or later"))

;; Custom settings in a separate file and load the custom settings

(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(keymap-global-set "C-c w v" 'customize-variable)

;; Revert buffers when the underlying file has changed

(global-auto-revert-mode 1)

;; Use ibuffer by default instead of the stock buffer list

(global-set-key [remap list-buffers] 'ibuffer)

;; Easier access to imenu

(global-set-key (kbd "M-i") 'imenu)

;; Revert Dired and other buffers

(setq global-auto-revert-non-file-buffers t)

;; Auto-save mode for org files

(auto-save-visited-mode +1)
(setq auto-save-visited-predicate
	(lambda () (eq major-mode 'org-mode)))

(use-package casual
  :ensure t
  :defer t)

(require 'casual-dired) ; optional if using autoloaded menu
(keymap-set dired-mode-map "M-c" #'casual-dired-tmenu)
(keymap-set dired-mode-map "s" #'casual-dired-sort-by-tmenu) ; optional
(keymap-set dired-mode-map "/" #'casual-dired-search-replace-tmenu) ; optional

(require 'casual-agenda) ; optional if using autoloaded menu
(keymap-set org-agenda-mode-map "M-c" #'casual-agenda-tmenu)

; Bindings to make jumping consistent between Org Agenda and Casual Agenda
(keymap-set org-agenda-mode-map "M-j" #'org-agenda-clock-goto) ; optional
(keymap-set org-agenda-mode-map "J" #'bookmark-jump) ; optional

(require 'casual-info) ; optional if using autoloaded menu
(keymap-set Info-mode-map "C-o" #'casual-info-tmenu)

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

(load-file (concat (file-name-as-directory user-emacs-directory) "prot-common.el"))
(load-file (concat (file-name-as-directory user-emacs-directory) "prot-eww.el"))
(add-to-list 'load-path "~/.emacs.d/manual-packages/denote")

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

;; Keyboard-centric user interface removing tool, menu and scroll bars

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell 1)
(column-number-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq use-dialog-box nil)

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

;; Adding the ability to hide the modeline

(use-package hide-mode-line
  :ensure t)

;; Better modeline

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
  (load-theme 'modus-vivendi-tinted :no-confirm)
  :bind
  (("C-c w t t" . modus-themes-toggle)
   ("C-c w t m" . modus-themes-select)
   ("C-c w t s" . consult-theme)))

;; Auto-dark-theme

(use-package auto-dark
:ensure t
:custom
(auto-dark-themes '((modus-vivendi-tinted) (modus-operandi-tinted)))
(auto-dark-polling-interval-seconds 5)
(auto-dark-allow-powershell nil)
;; (auto-dark-detection-method nil) ;; dangerous to be set manually
:hook
(auto-dark-dark-mode
 . (lambda ()
      ;; something to execute when dark mode is detected
      ))
(auto-dark-light-mode
 . (lambda ()
      ;; something to execute when light mode is detected
      ))
:init (auto-dark-mode))

;; Mixed-pitch

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

;; Fonts 'default, 'fixed-pitch and 'variable-pitch
(if (eq system-name 'effondrement)
    (set-face-attribute 'default nil
			:family "Aporetic Sans Mono"
			:height 140
			:weight 'Regular)
  (set-face-attribute 'default nil
                      :family "Aporetic Sans Mono"
                      :height 120
                      :weight 'Regular))
(when (eq system-type 'windows-nt)
  (set-face-attribute 'variable-pitch nil :family "Iosevka Comfy Duo"))
(when (eq system-type 'gnu/linux)
  (set-face-attribute 'variable-pitch nil :family "Aporetic Serif"))
(set-face-attribute 'fixed-pitch nil :family "Aporetic Sans Mono")

;; Split windows sensibly

(setq split-width-threshold 120
      split-height-threshold nil)

;; Keep window sizes balanced

(use-package balanced-windows
  :config
  (balanced-windows-mode))

;; Switching window quickly
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

;; Enable vertico

(use-package vertico
  :init
  (vertico-mode)
  :bind (("C-c w l" . consult-line)
         :map vertico-map
         ("C-n" . vertico-next)
         ("C-b" . vertico-previous)
         ("C-h" . vertico-exit)
         :map minibuffer-local-map
         ("M-DEL" . backward-kill-word))
  :custom
  (vertico-cycle t)
  (vertico-sort-function 'vertico-sort-history-alpha))

;; Persist history over Emacs restarts.

;; By default, the built-in `savehist-mode' only keeps a record of
;; minibuffer histories.  This is helpful as it surfaces the most
;; recently selected items to the top, allowing you to access them again
;; very quickly.  With the variable `savehist-additional-variables' we
;; can make `savehist-mode' keep a record of any variable we want, so
;; that it persists between Emacs sessions.  I do this to store the
;; `kill-ring' and the `register-alist'.

(use-package savehist
  :init
  (savehist-mode 1))
(setq savehist-additional-variables '(register-alist kill-ring))


;; Save last place in file after closing it

(add-hook 'org-cycle-tab-first-hook 'org-end-of-line)

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
 :ensure t
 :bind (:map corfu-map ("<tab>" . corfu-complete))
 ;; Optional customizations
 :custom
 (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
 (corfu-auto t)                 ;; Enable auto completion
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
 (global-corfu-mode)
 (with-eval-after-load 'savehist
   (corfu-history-mode 1)
   (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package emacs
  :config
  (setq-default scroll-preserve-screen-position t)
  (setq-default scroll-conservatively 1) ; affects `scroll-step'
  (setq-default scroll-margin 0)

  (define-minor-mode prot/scroll-centre-cursor-mode
    "Toggle centred cursor scrolling behaviour."
    :init-value nil
    :lighter " S="
    :global nil
    (if prot/scroll-centre-cursor-mode
        (setq-local scroll-margin (* (frame-height) 2)
                    scroll-conservatively 0
                    maximum-scroll-margin 0.5)
      (dolist (local '(scroll-preserve-screen-position
                       scroll-conservatively
                       maximum-scroll-margin
                       scroll-margin))
        (kill-local-variable `,local))))

  ;; C-c l is used for `org-store-link'.  The mnemonic for this is to
  ;; focus the Line and also works as a variant of C-l.
  :bind ("C-c L" . prot/scroll-centre-cursor-mode))

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

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "fr_CA en_CA"))

(use-package plz
  :ensure t)
(use-package go-translate
  :ensure t
  :defer t
  :custom
  (setq gt-langs '(en fr))
  (setq gt-default-translator (gt-translator :engines (gt-google-engine))))
  ;; (setq gt-default-translator
  ;;       (gt-translator
  ;;        :taker   (gt-taker :text 'buffer :pick 'paragraph)  ; config the Taker
  ;;        :engines (list (gt-bing-engine) (gt-google-engine)) ; specify the Engines
  ;;        :render  (gt-buffer-render))))                       ; config the Render

;; (setq gt-preset-translators
;;   `((ts-1 . ,(gt-translator
;;               :taker (gt-taker :langs '(es fr) :text 'word)
;;               :engines (gt-bing-engine)
;;               :render (gt-overlay-render)))
;;     (ts-2 . ,(gt-translator
;;               :taker (gt-taker :langs '(es fr) :text 'sentence)
;;               :engines (gt-google-engine)
;;               :render (gt-insert-render))))))

(use-package org
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
;; (add-hook 'org-tab-first-hook 'org-end-of-line)

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
	'((sequence "TODO(t)" "NEXT(n)" "STARTED(s!)" "WAITING(w!)" "|" "DONE(d!)" "DELEGATED(é!)" "CANCELED(c!)")))

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
  (org-mode . org-modern-mode))
  ;; :custom
  ;; (org-modern-table nil)
  ;; (org-modern-keyword nil)
  ;; (org-modern-timestamp nil)
  ;; (org-modern-priority nil)
  ;; (org-modern-checkbox nil)
  ;; (org-modern-tag nil)
  ;; (org-modern-block-name nil)
  ;; (org-modern-keyword nil)
  ;; (org-modern-footnote nil)
  ;; (org-modern-internal-target nil)
  ;; (org-modern-radio-target nil)
  ;; (org-modern-statistics nil)
  ;; (org-modern-progress nil))

(use-package consult
  :bind
  (("C-c w h" . consult-org-heading)
   ("C-M-j" . consult-buffer)                ;; orig. switch-to-buffer
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("C-c w g" . consult-grep)))

(defun wilf-show-todos ()
  (interactive)
  (occur "* TODO\\|* NEXT\\|* STARTED\\|* WAITING"))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(defun my-org-ql-shuffle-todo ()
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and
	(todo "TODO" "STARTED")
	(not (done))
	(not (scheduled))
	(not (deadline))
	(not (ts-active))
	(not (tags "cooking")))
    :sort 'random))

(defun my-org-ql-shuffle-someday ()
  (interactive)
  (org-ql-search (~/Documents/gtd/someday.org)
    '(and
	(todo "SOMEDAY")
	(not (done))
	(not (scheduled))
	(not (deadline))
	(not (ts-active))
	(not (tags "cooking")))
    :sort 'random))

(use-package yaml-mode
  :ensure nil
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

; ;; Projectile

   ; (use-package projectile
   ;   :diminish projectile-mode
   ;   :config (projectile-mode)
   ;   :custom ((projectile-completion-system 'ivy))
   ;   :bind-keymap
   ;   ("C-c p" . projectile-command-map)
   ;   :init
   ;   ;; NOTE: Set this to the folder where you keep your Git repos!
   ;   (when (file-directory-p "~/Projects/Code")
   ;     (setq projectile-project-search-path '("~/Projects/Code")))
   ;   (setq projectile-switch-project-action #'projectile-dired))

   ; (use-package counsel-projectile
   ;   :after projectile
   ;   :config (counsel-projectile-mode))

   ;; Magit

   (use-package magit
	:ensure t)

(add-hook 'magit-process-find-password-functions
	       'magit-process-password-auth-source)

   ;   :commands magit-status
   ;   :custom
   ;   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

   ; (setq auth-sources '("~/.authinfo")

   ; ;; NOTE: Make sure to configure a GitHub token before using this package!
   ; ;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
   ; ;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
   ; (use-package forge
   ;   :after magit)

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

(use-package ebib
  :ensure t
  :defer t
  :config
  (setq ebib-bibtex-dialect "biblatex"
	ebib-preload-bib-files "~/Documents/library/library.bib"
	ebib-file-associations `(("ps" . "gv")))
  :bind
  (("C-c b" . ebib)))

(use-package calibredb
  :defer t
  :config
  (setq calibredb-root-dir "~/Documents/Calibre")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  :bind
  (("C-c w b l" . calibredb)))
  ;; (setq calibredb-library-alist '(("~/OneDrive/Org/Doc/Calibre")
  ;;                                 ("~/Documents/Books Library")
  ;;                                 ("~/Documents/LIB1")
  ;;                                 ("/Volumes/ShareDrive/Documents/Library/"))))

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
   (list (concat (file-name-as-directory (getenv "HOME")) ".emacs.d/elfeed/elfeed.gpg"))))

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

;; 
;;   (load-file (concat (file-name-as-directory user-emacs-directory) "prot-elfeed.el"))
;; 
;;   (use-package prot-elfeed
;;   :ensure nil
;;   :after elfeed
;;   :bind
;;   ( :map elfeed-search-mode-map
;;     ("s" . prot-elfeed-search-tag-filter)
;;     ("+" . prot-elfeed-toggle-tag)
;;     :map elfeed-show-mode-map
;;     ("+" . prot-elfeed-toggle-tag))
;;   :hook
;;   (elfeed-search-mode . prot-elfeed-load-feeds)
;;   :config
;;   (setq prot-elfeed-tag-faces t)
;;   (prot-elfeed-fontify-tags))
;;

(use-package elfeed-tube
  :ensure t ;; or :straight t
  :after elfeed
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

(use-package elfeed-tube-mpv
  :ensure t ;; or :straight t
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))

;; Easy insertion of weblinks

(use-package org-web-tools
  :bind
  (("C-c w w" . org-web-tools-insert-link-for-url)))

(use-package browse-url
  :ensure nil
  :defer t
  :config
  (setq browse-url-browser-function 'eww-browse-url
        browse-url-secondary-browser-function 'browse-url-firefox))

(use-package shr
  :ensure nil
  :defer t
  :config
  (setq shr-use-colors nil             ; pour un meilleur contraste
        shr-use-fonts t
        shr-max-image-proportion 0.9	; 0.9 par défaut
        ;shr-width fill-column          ; check `prot-eww-readable'
        shr-max-width 120		; 120 par défaut
        shr-discard-aria-hidden t	; nil par défaut
        ;shr-fill-text nil              ; Emacs 31
        shr-cookie-policy nil))

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
  (setq eww-restore-desktop t
        eww-auto-rename-buffer "title"
        eww-desktop-remove-duplicates t
        eww-header-line-format nil	; défaut: %t: %u
        eww-search-prefix "https://duckduckgo.com/html/?q="
        eww-download-directory (expand-file-name "~/Documents/eww-downloads")
        eww-suggest-uris '(eww-links-at-point thing-at-point-url-at-point)
        eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/")
        eww-history-limit 150
        eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)" ; On GNU/Linux check your mimeapps.list
        eww-browse-url-new-window-is-tab nil
        eww-form-checkbox-selected-symbol "[X]"
        eww-form-checkbox-symbol "[ ]"
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
        eww-retrieve-command nil))

(use-package prot-eww
  :ensure nil
  :after eww
  :config
  (setq prot-eww-save-history-file
        (locate-user-emacs-file "prot-eww-visited-history")
        prot-eww-save-visited-history t
        prot-eww-bookmark-link nil)

  (add-hook 'prot-eww-history-mode-hook #'hl-line-mode)
  :bind (:map eww-mode-map
              ("B" . prot-eww-bookmark-page)
              ("D" . prot-eww-download-html)
              ("F" . prot-eww-find-feed)
              ("H" . prot-eww-list-history)
              ("b" . prot-eww-visit-bookmark)
              ("e" . prot-eww-browse-dwim)
              ("o" . prot-eww-open-in-other-window)
              ("E" . prot-eww-visit-url-on-page)
              ("J" . prot-eww-jump-to-url-on-page)
              ("R" . prot-eww-readable)
              ("Q" . prot-eww-quit)))

(defvar-keymap prot-eww-map
  :doc "Prefix map to call prot-eww functions"
  "b" #'prot-eww-visit-bookmark
  "e" #'prot-eww-browse-dwim
  "s" #'prot-eww-search-engine)

(keymap-set global-map "C-c e" prot-eww-map)

(use-package elpher
  :ensure t)

;; Image viewer
(use-package emacs
  :bind
  ((:map image-mode-map
		("K" . image-kill-buffer)
		("<right>" . image-next-file)
		("<left>"  . image-previous-file))
   (:map dired-mode-map
    ("C-<return>" . image-dired-dired-display-external))))

(use-package image-dired
  :custom
  (image-dired-external-viewer "gimp")
  (image-dired-thumb-margin 10)
  :bind
  (("C-c w I" . image-dired))
   (:map image-dired-thumbnail-mode-map
    ("C-<right>" . image-dired-display-next)
    ("C-<left>" . image-dired-display-previous)))

(use-package ready-player
  :ensure t
  :config
  (ready-player-mode +1))
(setq ready-player-my-media-collection-location "/mnt/data/Music/")

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

(setq erc-server "irc.libera.chat"
      erc-nick "wilf"
      erc-user-full-name "Frédéric Vachon"
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))
      erc-kill-buffer-on-part t
      erc-auto-query 'bury
      erc-prompt-for-password nil)

;; (add-to-list 'erc-modules 'notifications)

(setq erc-fill-column 120
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20)

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
    (file+headline "~/Documents/gtd/inbox.org" "Tasks")
    "* TODO %i%? \n %U")
   ("r" "Read article" entry
    (file+headline "~/Documents/gtd/inbox.org" "Tasks")
    "* %i%? \n %U")
   ("T" "Tickler" entry
    (file+headline "~/Documents/gtd/tickler.org" "Tickler")
    "* TODO %i%? \n %U")))

;; Start writing immediately after triggering org-capture

;; (add-hook 'org-capture-mode-hook 'evil-insert-state)

;; (with-eval-after-load 'org
;;   (require 'org-tempo)

;;   (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
;;   (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
;;   (add-to-list 'org-structure-template-alist '("py" . "src python")))

(use-package org-tempo
  :ensure nil
  :after org
  :config
  (dolist (item '(("sh" . "src shell")
		    ("el" . "src emacs-lisp")
		    ("cel" . "src emacs-lisp :tangle .emacs.d/init.el")
		    ("cco" . "src conf :tangle DIR")
		    ("py" . "src python")))
    (add-to-list 'org-structure-template-alist item)))

(setq org-agenda-files '("~/Documents/gtd/inbox.org"
                           "~/Documents/gtd/gtd.org"
                           "~/Documents/gtd/projets.org"
                           "~/Documents/gtd/tickler.org"))

  (setq org-refile-targets '(("~/Documents/gtd/gtd.org" :maxlevel . 3)
                             ("~/Documents/gtd/someday.org" :level . 1)
                             ("~/Documents/gtd/projets.org" :maxlevel . 5)
                             ("~/Documents/gtd/tickler.org" :maxlevel . 2)))

;; Inbox location

;; (setq org-default-notes-file (concat org-directory "/notes.org"))

; (use-package hledger-mode
                                        ; :pin manual
                                        ; :after htmlize
                                        ; :load-path "packages/rest/hledger-mode/"
                                        ; :mode ("\\.journal\\'" "\\.hledger\\'")
                                        ; :commands hledger-enable-reporting
                                        ; :preface
                                        ; (defun hledger/next-entry ()
                                        ; "Move to next entry and pulse."
                                        ; (interactive)
                                        ; (hledger-next-or-new-entry)
                                        ; (hledger-pulse-momentary-current-entry))
                                        ; 
                                        ; (defface hledger-warning-face
                                        ; '((((background dark))
                                        ; :background "Red" :foreground "White")
                                        ; (((background light))
                                        ; :background "Red" :foreground "White")
                                        ; (t :inverse-video t))
                                        ; "Face for warning"
                                        ; :group 'hledger)
                                        ; 
                                        ; (defun hledger/prev-entry ()
                                        ; "Move to last entry and pulse."
                                        ; (interactive)
                                        ; (hledger-backward-entry)
                                        ; (hledger-pulse-momentary-current-entry))
                                        ; 
                                        ; :bind (("C-c j" . hledger-run-command)
                                        ; :map hledger-mode-map
                                        ; ("C-c e" . hledger-jentry)
                                        ; ("M-p" . hledger/prev-entry)
                                        ; ("M-n" . hledger/next-entry))
                                        ; :init
                                        ; (setq hledger-jfile
                                        ; (expand-file-name "~/miscellany/personal/finance/accounting.journal")
                                        ; hledger-email-secrets-file (expand-file-name "secrets.el"
                                        ; emacs-assets-directory))
                                        ; ;; Expanded account balances in the overall monthly report are
                                        ; ;; mostly noise for me and do not convey any meaningful information.
                                        ; (setq hledger-show-expanded-report nil)
                                        ; 
                                        ; (when (boundp 'my-hledger-service-fetch-url)
                                        ; (setq hledger-service-fetch-url
                                        ; my-hledger-service-fetch-url))
                                        ; 
                                        ; :config
                                        ; (add-hook 'hledger-view-mode-hook #'hl-line-mode)
                                        ; (add-hook 'hledger-view-mode-hook #'center-text-for-reading)
                                        ; 
                                        ; (add-hook 'hledger-view-mode-hook
                                        ; (lambda ()
                                        ; (run-with-timer 1
                                        ; nil
                                        ; (lambda ()
                                        ; (when (equal hledger-last-run-command
                                        ; "balancesheet")
                                        ; ;; highlight frequently changing accounts
                                        ; (highlight-regexp "^.*\\(savings\\|cash\\).*$")
                                        ; (highlight-regexp "^.*credit-card.*$"
                                        ; 'hledger-warning-face))))))
                                        ; 
                                        ; (add-hook 'hledger-mode-hook
                                        ; (lambda ()
                                        ; (make-local-variable 'company-backends)
                                        ; (add-to-list 'company-backends 'hledger-company))))
                                        ; 
                                        ; (use-package hledger-input
                                        ; :pin manual
                                        ; :load-path "packages/rest/hledger-mode/"
                                        ; :bind (("C-c e" . hledger-capture)
                                        ; :map hledger-input-mode-map
                                        ; ("C-c C-b" . popup-balance-at-point))
                                        ; :preface
                                        ; (defun popup-balance-at-point ()
                                        ; "Show balance for account at point in a popup."
                                        ; (interactive)
                                        ; (if-let ((account (thing-at-point 'hledger-account)))
                                        ; (message (hledger-shell-command-to-string (format " balance -N %s "
                                        ; account)))
                                        ; (message "No account at point")))
                                        ; 
                                        ; :config
                                        ; (setq hledger-input-buffer-height 20)
                                        ; (add-hook 'hledger-input-post-commit-hook #'hledger-show-new-balances)
                                        ; (add-hook 'hledger-input-mode-hook #'auto-fill-mode)
                                        ; (add-hook 'hledger-input-mode-hook
                                        ; (lambda ()
                                        ; (make-local-variable 'company-idle-delay)
                                        ; (setq-local company-idle-delay 0.1)))) 

(use-package ledger-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.\\(h?ledger\\|journal\\|j\\)$" . ledger-mode))
  (setq ledger-binary-path "~/.emacs.d/ledger.sh"
        ledger-mode-should-check-version nil
        ledger-report-links-in-register nil
        ledger-report-auto-width nil
        ledger-report-native-highlighting-arguments '("--color=always")
        ledger-highlight-xact-under-point nil
        ledger-use-iso-dates t))
        ;; ledger-default-date-format ledger-iso-date-format))

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

(setq xref-search-program #'ripgrep)

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
  (("C-c w o" . ews-olivetti))
  :custom
  (olivetti-style 'fancy))

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

(defun hide-dired-details-include-all-subdir-paths ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward dired-subdir-regexp nil t)
	(let* ((match-bounds (cons (match-beginning 1) (match-end 1)))
	       (path (file-name-directory (buffer-substring (car match-bounds)
							    (cdr match-bounds))))
	       (path-start (car match-bounds))
	       (path-end (+ (car match-bounds) (length path)))
	       (inhibit-read-only t))
	  (put-text-property path-start path-end
			     'invisible 'dired-hide-details-information)))))

(use-package dired
  :hook ((dired-mode . dired-hide-details-mode)
	   (dired-after-readin . hide-dired-details-include-all-subdir-paths))
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
  (put 'dired-find-alternate-file 'disabled nil))
  ;; Additional configuration usefuL with evil
  ;; :config
  ;; (evil-collection-define-key 'normal 'dired-mode-map
  ;;   "h" 'dired-up-directory
  ;;   "l" 'dired-find-file))

(autoload 'dired-omit-mode "dired-x")

;; Adding icons
(use-package all-the-icons-dired
  :hook (dired-mode))

;; Adding colors (retiré car en conflit avec Denote-dired)
;; (use-package diredfl
;;   :hook (dired-mode))
;;   ;;
;; :hook (dired-mode . diredfl-global-mode))

;; Adding git infos
(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map
		(")" . dired-git-info-mode)))

;; Adding Dirvish-mode
;; (use-package dirvish
;;   :hook (dired-mode)
;;   :config (dirvish-override-dired-mode))

;; Hide hidden files
;; (use-package dired-hide-dotfiles
;;   :hook
;;   (dired-mode)
;;   :config
;;   (evil-collection-define-key 'normal 'dired-mode-map "H" 'dired-hide-dotfiles-mode))

(use-package dired-preview
  :hook (dired . dired-preview)
  :config
  (setq dired-preview-delay 0.7
	  dired-preview-max-size (expt 6 20)
	  dired-preview-ignored-extensions-regexp (concat "\\."
							  "\\(gz\\|"
							  "zst\\|"
							  "tar\\|"
							  "xz\\|"
							  "rar\\|"
							  "zip\\|"
							  "iso\\|"
							  "epub"
							  "\\)"))

  ;; Enable `dired-preview-mode' in a given Dired buffer or do it ;; globally:
  (dired-preview-global-mode 1))

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

(use-package bookmark
  :custom
  (bookmark-save-flag 1)
  :bind
  ("C-x r D" . bookmark-delete))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq register-preview-delay 0.8
      register-preview-function #'consult-register-format)

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package w32-browser
  :after (dired))

(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq search-whitespace-regexp ".*?")

(use-package mu4e
  :ensure nil
  ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :load-path "/usr/share/emacs/site-lisp/elpa-src/mu4e-1.8.14/"
  :defer 10 ; Wait until 10 seconds after startup
  :bind (("C-c u" . mu4e))
  :config

  (setq mu4e-change-filenames-when-moving t ; avoid sync conflicts
        mu4e-update-interval (* 10 60) ; check mail 10 minutes
        mu4e-compose-format-flowed t ; re-flow mail so it's not hard wrapped
        mu4e-get-mail-command "mbsync -a"
        mu4e-maildir "~/Documents/Mail"
        mu4e-attachment-dir "~/Downloads")

  (setq mu4e-contexts
        (list

         ;; Compte principal
         (make-mu4e-context
          :name "Fred"
          :match-func
          (lambda (msg)
            (when msg
              (string-match-p "/Mailbox" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "fvachon@tamiaso.com")
                  (user-full-name    . "Frédéric Vachon")
                  (smtpmail-smtp-server  . "smtp.mailbox.org")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)
                  (mu4e-refile-folder  . "/Mailbox/Archive")
                  (mu4e-sent-folder  . "/Mailbox/Sent")
                  (mu4e-drafts-folder  . "/Mailbox/Drafts")
                  (mu4e-trash-folder  . "/Mailbox/Trash")
                  (mu4e-compose-signature . "Frédéric Vachon")
                  (mu4e-maildir-shortcuts .
                                          (("/Mailbox/Inbox"     . ?i)
                                           ("/Mailbox/Archive"     . ?a)
                                           ("/Mailbox/Sent"       . ?s)
                                           ("/Mailbox/Drafts"       . ?d)
                                           ("/Mailbox/Trash"       . ?t)))
         ))

         ;; Compte alternatif
         (make-mu4e-context
          :name "Wilf"
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg :to "wilf@tamiaso.com")))
          :vars '((user-mail-address . "wilf@tamiaso.com")
                  (user-full-name    . "Wilf")
                  (smtpmail-smtp-server  . "smtp.mailbox.org")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)
                  (mu4e-refile-folder  . "/Mailbox/Archive")
                  (mu4e-sent-folder  . "/Mailbox/Sent")
                  (mu4e-drafts-folder  . "/Mailbox/Drafts")
                  (mu4e-trash-folder  . "/Mailbox/Trash")
                  (mu4e-compose-signature . "Wilf")
                  (mu4e-maildir-shortcuts .
                                          (("/Mailbox/Inbox"     . ?i)
                                           ("/Mailbox/Archive"     . ?a)
                                           ("/Mailbox/Sent"       . ?s)
                                           ("/Mailbox/Drafts"       . ?d)
                                           ("/Mailbox/Trash"       . ?t)))
         ))

         ;; Compte principal mailbox
         (make-mu4e-context
          :name "Mailbox"
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg :to "tamiaso@mailbox.org")))
          :vars '((user-mail-address . "tamiaso@mailbox.org")
                  (user-full-name    . "Frédéric Vachon")
                  (smtpmail-smtp-server  . "smtp.mailbox.org")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)
                  (mu4e-refile-folder  . "/Mailbox/Archive")
                  (mu4e-sent-folder  . "/Mailbox/Sent")
                  (mu4e-drafts-folder  . "/Mailbox/Drafts")
                  (mu4e-trash-folder  . "/Mailbox/Trash")
                  (mu4e-compose-signature . "Frédéric Vachon")
                  (mu4e-maildir-shortcuts .
                                          (("/Mailbox/Inbox"     . ?i)
                                           ("/Mailbox/Archive"     . ?a)
                                           ("/Mailbox/Sent"       . ?s)
                                           ("/Mailbox/Drafts"       . ?d)
                                           ("/Mailbox/Trash"       . ?t)))
         ))

         ;; Compte Proton
         (make-mu4e-context
          :name "Proton"
          :match-func
          (lambda (msg)
            (when msg
              ;; (mu4e-message-contact-field-matches msg :to "vachonfrederic@proton.me")))
              (string-match-p "/Mailbox" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "vachonfrederic@proton.me")
                  (user-full-name    . "Frédéric Vachon")
                  (auth-source  .  ("~/.authinfo.gpg"))
                  (smtpmail-smtp-server  . "127.0.0.1")
                  (smtpmail-smtp-service . 1025)
                  (smtpmail-stream-type  . starttls)
                  (mu4e-refile-folder  . "/Proton/Archive")
                  (mu4e-sent-folder  . "/Proton/Sent")
                  (mu4e-drafts-folder  . "/Proton/Drafts")
                  (mu4e-trash-folder  . "/Proton/Trash")
                  (mu4e-compose-signature . "Frédéric Vachon")
                  (mu4e-maildir-shortcuts .
                                          (("/Proton/INBOX"     . ?i)
                                           ("/Proton/All Mail"     . ?a)
                                           ("/Proton/Archive"     . ?A)
                                           ("/Proton/Sent"       . ?s)
                                           ("/Proton/Spam"       . ?S)
                                           ("/Proton/Drafts"       . ?d)
                                           ("/Proton/Trash"       . ?t)))
                  ))))


  ;; Run mu4e in the background to sync mail periodically
  (mu4e t))

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(use-package org-mime
  :ensure t)

(setq org-mime-export-options '(:section-numbers nil
                                :with-author nil
                                :with-toc nil))

(add-hook 'message-send-hook 'org-mime-htmlize)

;; (use-package zoxide
;;   :ensure t)
;; ;  (define-key evil-normal-state-map "gz" 'zoxide-find-file)

(setq org-latex-listings 'minted
	org-latex-packages-alist '(("" "minted"))
	org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-latex-minted-options '(("breaklines" "true")
				   ("breakanywhere" "true")))

(use-package anki-editor
  :after org
  :bind (:map org-mode-map
              ("<f12>" . anki-editor-cloze-region-auto-incr)
              ("<f11>" . anki-editor-cloze-region-dont-incr)
              ("<f10>" . anki-editor-reset-cloze-number)
              ("<f9>"  . anki-editor-push-tree))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags t)

  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))
  ;; Initialize
  (anki-editor-reset-cloze-number)
  )

;; Org-capture templates
(setq org-my-anki-file "~/Git/my-anki-decks/anki.org")
(add-to-list 'org-capture-templates
             '("a" "Anki"))
(add-to-list 'org-capture-templates
             '("ae" "Anki basic español"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Español::Espagnol personnel\n:END:\n** Front\n%?\n** Back\n%x\n"))
(add-to-list 'org-capture-templates
             '("aE" "Anki cloze español"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Español::Espagnol personnel\n:END:\n** Text\n%x\n** Extra\n"))
(add-to-list 'org-capture-templates
             '("as" "Anki basic esperanto"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Esperanto::Esperanto personnel\n:END:\n** Front\n%?\n** Back\n%x\n"))
(add-to-list 'org-capture-templates
             '("aS" "Anki cloze esperanto"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Esperanto::Esperanto personnel\n:END:\n** Text\n%x\n** Extra\n"))
(add-to-list 'org-capture-templates
             '("ag" "Anki basic general"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Général\n:END:\n** Front\n%?\n** Back\n%x\n"))
(add-to-list 'org-capture-templates
             '("aG" "Anki cloze general"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Général\n:END:\n** Text\n%x\n** Extra\n"))

;; Allow Emacs to access content from clipboard.
(setq x-select-enable-clipboard t
      x-select-enable-primary t)
