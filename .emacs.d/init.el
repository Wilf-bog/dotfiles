;; Built-in since Emacs 29
(require 'use-package)

(which-key-mode)

(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :bind
  (;; Keymap for buffers (Emacs28)
   :map ctl-x-x-map
   ("f" . follow-mode)  ; override `font-lock-update'
   ("r" . rename-uniquely)
   ("l" . visual-line-mode)))

(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Auto-save mode for org files
(auto-save-visited-mode +1)
(setq auto-save-visited-predicate
	(lambda () (eq major-mode 'org-mode)))

(customize-set-variable 'bookmark-save-flag 1)

(global-set-key [remap list-buffers] 'ibuffer)

(setq custom-file (make-temp-file "emacs-custom-"))

(use-package tab-bar
  :ensure nil
  :config
  (setq tab-bar-new-button-show nil)
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-show 1))

(use-package use-package
  :custom
  (use-package-always-ensure nil)	; older config had it true.
  (package-native-compile t)
  (warning-minimum-level :emergency))



(setq custom-safe-themes t)

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-to-toggle
   '(modus-operandi modus-vivendi))
  (modus-themes-variable-pitch-ui t)
  (modus-themes-completions '((t . (bold))))
  (modus-themes-prompts '(bold))
  (modus-themes-headings
      '((agenda-structure . (variable-pitch light 2.2))
        (agenda-date . (variable-pitch regular 1.3))
        (t . (regular 1.15))))
  :init
  (load-theme 'modus-vivendi :no-confirm)
  :bind
  (("C-c t t" . modus-themes-toggle)
   ("C-c t m" . modus-themes-select)
   ("C-c t s" . consult-theme)))

(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-themes '((modus-vivendi) (modus-operandi)))
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-powershell nil)
  :init (auto-dark-mode))

;; Recent files

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 100)
  (setq recentf-save-file-modes nil)
  (setq recentf-keep nil)
  (setq recentf-auto-cleanup nil)
  (setq recentf-initialize-file-name-history nil)
  (setq recentf-filename-handlers nil)
  (setq recentf-show-file-shortcuts-flag nil)) ; I don't use the recentf tool.

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
  :ensure nil
  :commands
  (dired dired-jump)
  :custom
  (dired-listing-switches
   "-goah --group-directories-first --time-style=long-iso") ; Customizing ls
  (dired-dwim-target t)			; Allow to move stuff from a
					; window to another.
  (delete-by-moving-to-trash t)
  :init
  (put 'dired-find-alternate-file 'disabled nil))
(autoload 'dired-omit-mode "dired-x")

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

;; Org tags
(setq org-tag-alist
	'(;; Places
	  ("@home" . ?H)
	  ("@work" . ?W)

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

(use-package org
  :custom
  (org-log-into-drawer t)
  :bind
  (("C-c a" . org-agenda)))

(setq org-agenda-files '("~/Documentos/gtd/inbox.org"
                         "~/Documentos/gtd/gtd.org"
                         "~/Documentos/gtd/projets.org"
                         "~/Documentos/gtd/tickler.org"))

(setq org-refile-targets '(("~/Documentos/gtd/gtd.org" :maxlevel . 2)
                           ("~/Documentos/gtd/someday.org" :level . 1)
                           ("~/Documentos/gtd/projets.org" :maxlevel . 5)
                           ("~/Documentos/gtd/tickler.org" :maxlevel . 2)))

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
    (file+headline "~/Documentos/gtd/inbox.org" "Tasks")
    "* TODO %i%? \n %U")
   ("r" "Read article" entry
    (file+headline "~/Documentos/gtd/inbox.org" "Tasks")
    "* %i%? \n %U")
   ("T" "Tickler" entry
    (file+headline "~/Documentos/gtd/tickler.org" "Tickler")
    "* TODO %i%? \n %U")))

(use-package bibtex
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file" "Link to a document file." "" )))
  (bibtex-align-at-equal-sign t))

(use-package magit
  :ensure t)

(add-hook 'magit-process-find-password-functions
	  'magit-process-password-auth-source)

(use-package denote
  :ensure t
  :hook
  ( ;; If you use Markdown or plain text files, then you want to make
   ;; the Denote links clickable (Org renders links as buttons right
   ;; away)
   (text-mode . denote-fontify-links-mode-maybe)
   ;; Apply colours to Denote names in Dired.  This applies to all
   ;; directories.  Check `denote-dired-directories' for the specific
   ;; directories you may prefer instead.  Then, instead of
   ;; `denote-dired-mode', use `denote-dired-mode-in-directories'.
   (dired-mode . denote-dired-mode))
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  ( :map global-map
    ("C-c n n" . denote)
    ("C-c n d" . denote-dired)
    ("C-c n g" . denote-grep)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    ("C-c n i" . denote-link-or-create)
    ("C-c n l" . denote-link)
    ("C-c n L" . denote-add-links)
    ("C-c n b" . denote-backlinks)
    ("C-c n q c" . denote-query-contents-link) ; create link that triggers a grep
    ("C-c n q f" . denote-query-filenames-link) ; create link that triggers a dired
    ;; Note that `denote-rename-file' can work from any context, not just
    ;; Dired bufffers.  That is why we bind it here to the `global-map'.
    ("C-c n r" . denote-rename-file)
    ("C-c n R" . denote-rename-file-using-front-matter)

    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))

  :config
  ;; Remember to check the doc string of each of those variables.
  (setq denote-directory (expand-file-name "~/Documentos/notes/"))
  (setq denote-save-buffers t)
  (setq denote-known-keywords '("emacs" "philosophy" "politics" "economics"))
  (setq denote-infer-keywords t)
  (setq denote-file-type "org")
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :ensure t
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

(use-package denote-silo
  :ensure t
  ;; Bind these commands to key bindings of your choice.
  ;; :commands ( denote-silo-create-note
  ;;             denote-silo-open-or-create
  ;;             denote-silo-select-silo-then-command
  ;;             denote-silo-dired
  ;;             denote-silo-cd )
  :bind
  (("C-c n s n" . denote-silo-create-note)
   ("C-c n s o" . denote-silo-open-or-create)
   ("C-c n s s" . denote-silo-select-silo-then-command)
   ("C-c n s d" . denote-silo-dired)
   ("C-c n s c" . denote-silo-cd))
  :config
  ;; Add your silos to this list.  By default, it only includes the
  ;; value of the variable `denote-directory'.
  (setq denote-silo-directories
        (list denote-directory
              "~/Documentos/notes/"
              "~/Documentos/notes-exaequo/"
	      "~/Imágenes/")))

(use-package denote-org
  :ensure t
  :commands
  ;; I list the commands here so that you can discover them more
  ;; easily.  You might want to bind the most frequently used ones to
  ;; the `org-mode-map'.
  ( denote-org-link-to-heading
    denote-org-backlinks-for-heading

    denote-org-extract-org-subtree

    denote-org-convert-links-to-file-type
    denote-org-convert-links-to-denote-type

    denote-org-dblock-insert-files
    denote-org-dblock-insert-links
    denote-org-dblock-insert-backlinks
    denote-org-dblock-insert-missing-links
    denote-org-dblock-insert-files-as-headings))

(use-package denote-journal
  :ensure t
  ;; Bind those to some key for your convenience.
  :commands ( denote-journal-new-entry
              denote-journal-new-or-existing-entry
              denote-journal-link-or-create-entry )
  :bind
  (("C-c n j n" . denote-journal-new-entry)
   ("C-c n j o" . denote-journal-new-or-existing-entry)
   ("C-c n j l" . denote-journal-link-or-create-entry))
  :hook (calendar-mode . denote-journal-calendar-mode)
  :config
  ;; Use the "journal" subdirectory of the `denote-directory'.  Set this
  ;; to nil to use the `denote-directory' instead.
  (setq denote-journal-directory
        (expand-file-name "journal" denote-directory))
  ;; Default keyword for new journal entries. It can also be a list of
  ;; strings.
  (setq denote-journal-keyword "journal")
  ;; Read the doc string of `denote-journal-title-format'.
  (setq denote-journal-title-format 'day-date-month-year))

(use-package citar
  :ensure t
  :custom
  (citar-bibliography '("~/Documentos/library/library.bib"))
  :bind
  (("C-c b o" . citar-open)))

(use-package citar-denote
  :ensure t
  :custom
  (citar-open-always-create-notes t)
  :init
  (citar-denote-mode)
  :bind
  (("C-c b c" . citar-create-note)
   ("C-c b n" . citar-denote-open-note)
   ("C-c b x" . citar-denote-nocite)
   :map org-mode-map
   ("C-c b k" . citar-denote-add-citekey)
   ("C-c b K" . citar-denote-remove-citekey)
   ("C-c b d" . citar-denote-dwim)
   ("C-c b e" . citar-denote-open-reference-entry)))

(setq xref-search-program #'ripgrep)

(use-package citar-embark
  :ensure t
  :after (citar embark)
  :no-require
  :config (citar-embark-mode))

(use-package biblio
  :ensure t)

(use-package ef-themes
  :ensure t
  :demand t
  :bind
  (("<f5>" . ef-themes-rotate)
   ("C-<f5>" . ef-themes-select))
  :config
  (setq ef-themes-variable-pitch-ui t
        ef-themes-mixed-fonts t
        ef-themes-to-rotate ef-themes-items
        ef-themes-headings ; read the manual's entry of the doc string
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch regular 1.7))
          (3 . (variable-pitch regular 1.6))
          (4 . (variable-pitch regular 1.5))
          (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (agenda-date . (semilight 1.5))
          (agenda-structure . (variable-pitch light 1.9))
          (t . (variable-pitch 1.1)))))

(use-package doric-themes
  :ensure t
  :demand t
  :config
  ;; These are the default values.
  (setq doric-themes-to-toggle '(doric-light doric-dark))
  (setq doric-themes-to-rotate doric-themes-collection)

  (doric-themes-select 'doric-light)

  ;; ;; To load a random theme instead, use something like one of these:
  ;;
  ;; (doric-themes-load-random)
  ;; (doric-themes-load-random 'light)
  ;; (doric-themes-load-random 'dark)

  ;; ;; For optimal results, also define your preferred font family (or use my `fontaine' package):
  ;;
  ;; (set-face-attribute 'default nil :family "Aporetic Sans Mono" :height 160)
  ;; (set-face-attribute 'variable-pitch nil :family "Aporetic Sans" :height 1.0)
  ;; (set-face-attribute 'fixed-pitch nil :family "Aporetic Sans Mono" :height 1.0)

  :bind
  (("<f5>" . doric-themes-toggle)
   ("C-<f5>" . doric-themes-select)
   ("M-<f5>" . doric-themes-rotate)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; Fonts settings

(dolist (face '(default fixed-pitch))
  (set-face-attribute `,face nil
		      :font "Aporetic Sans Mono"
		      :weight 'regular
		      :height 120))
(set-face-attribute 'variable-pitch nil
		    :font "Aporetic Sans"
		    :weight 'regular
		    :height 1.0) ; :height 1.0 fix an issue with zooming on EWW

;;;;; `variable-pitch-mode' setup
(use-package face-remap
  :ensure nil
  :bind ( :map ctl-x-x-map
          ("v" . variable-pitch-mode))
  :hook ((text-mode notmuch-show-mode elfeed-show-mode) . wilf/enable-variable-pitch)
  :config
  (defun wilf/enable-variable-pitch ()	;originally, named after Prot.
    (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
      (variable-pitch-mode 1))))

(use-package dired-preview
  :ensure t
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

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode))

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

;; Enable Vertico.
(use-package vertico
  :ensure t
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

(use-package org-tempo
  :after org
  :config
  (dolist (item '(("sh" . "src shell")
		    ("el" . "src emacs-lisp")
		    ("cel" . "src emacs-lisp :tangle init.el")
		    ("cco" . "src conf :tangle DIR")
		    ("py" . "src python")))
    (add-to-list 'org-structure-template-alist item)))

(use-package org-modern
  :ensure t
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
  ;; (org-modern-progress nil)
  :hook
  (org-mode . org-modern-mode))

;; Show hidden emphasis markers

(use-package org-appear
  :ensure t
  :hook
  (org-mode . org-appear-mode))

;; Easy insertion of weblinks

(use-package org-web-tools
  :ensure t
  :bind
  (("C-c w" . org-web-tools-insert-link-for-url)))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package spacious-padding
  :ensure t
  :custom
  (line-spacing 3)
  (setq spacious-padding-widths
        `( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 15
           :scroll-bar-width ,(if x-toolkit-scroll-bars 8 6)
           :left-fringe-width 20
           :right-fringe-width 20))
  (setq spacious-padding-subtle-frame-lines
        '( :mode-line-active spacious-padding-line-active
           :mode-line-inactive spacious-padding-line-inactive
           :header-line-active spacious-padding-line-active
           :header-line-inactive spacious-padding-line-inactive))
  :init
  (spacious-padding-mode 1))

(use-package olivetti
  :ensure t
  :bind
  ("C-c o" . olivetti-mode)
  :custom
  (olivetti-style 'fancy))

(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "fr_CA es_CO en_CA"))

;; Read RSS feeds with Elfeed

(use-package elfeed
  :ensure t
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-c e" . elfeed))

;; Configure Elfeed with org mode

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files
   (list (concat (file-name-as-directory (getenv "HOME")) "/.emacs.d/elfeed/elfeed.org"))))

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

(when (eq system-type 'gnu/linux)	;For now, pdf-tools can't be installed on Windows
  (use-package pdf-tools
    :ensure t
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
		("r"  . pdf-view-reset-slice))))

(use-package doc-view
  :ensure nil
  :custom
  (doc-view-resolution 300)
  (large-file-warning-threshold (* 50 (expt 2 20))))

(use-package nov
  :ensure t
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
