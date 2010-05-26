;; DESCRIPTION: jstr settings (heavily based upon topfunky settings)

;; Manually set PATH for use by eshell, rspec-mode, etc.
(let ((path))
  (setq path (concat "/opt/ruby-enterprise/bin:"
                     "~/bin:"
                     "~/src/homebrew/bin:"
                     "/usr/local/bin:"
                     "/usr/bin:"
                     "/bin"))
  (setenv "PATH" path))

(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

;; Save backups in one place
;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir
  (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq default-tab-width 4)
(setq-default tab-stop-list (list 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108))
(setq tab-width 4)

;; Custom frame width
(add-to-list 'default-frame-alist '(width . 180))

;; Open current file in TextMate.
(defun textmate-open-buffer ()
  (interactive)
  (shell-command-to-string (concat "mate " buffer-file-name)))


;; Plain Text
;;; Stefan Monnier <foo at acm.org>. It is the opposite of
;;; fill-paragraph. Takes a multi-line paragraph and makes
;;; it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun refresh-file ()
  (interactive)
  (revert-buffer t t t))
(global-set-key [f5] 'refresh-file)

;; Snippets
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yasnippet.el"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "/vendor/yasnippet.el/snippets"))

;; Commands
(require 'unbound)

;; Minor Modes
(add-to-list 'load-path (concat dotfiles-dir "/vendor/textmate.el"))
(require 'textmate)
(textmate-mode)
(require 'whitespace)

;; Centered cursor mode support
(load (concat dotfiles-dir "vendor/centered-cursor-mode.el"))
(require 'centered-cursor-mode)
(global-centered-cursor-mode +1)

;; Major Modes

;; Enable javascript-mode
(autoload 'javascript-mode "javascript-mode.el"
  "Major mode for editing JavaScript files" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

;; Enable nav mode
(add-to-list 'load-path (concat dotfiles-dir "vendor/nav"))
(require 'nav)

(add-to-list 'load-path (concat dotfiles-dir "/vendor/rspec-mode"))
(require 'rspec-mode)

(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))

(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(add-to-list 'auto-mode-alist '("\\.sake\\'" . ruby-mode))

(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; gist
(require 'gist)

;; Color Themes
(add-to-list 'load-path (concat dotfiles-dir "/vendor/color-theme"))
(require 'color-theme)
(color-theme-initialize)

;; Functions

(require 'line-num)

;; Full screen toggle
;; Doesn't appear to work in Cocoa Emacs.
;; (defun toggle-fullscreen ()
;;   (interactive)
;;   (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
;;                                            nil
;;                                          'fullboth)))
;; (global-set-key (kbd "M-n") 'toggle-fullscreen)

;; Outdent
;; TODO: Fix issue: does not function with tab width in document
;; (e.g. ruby files) does not ==
;; default-tab-width/tab-width. Investigate further.
(defun outdent ()
  (interactive)
  (move-beginning-of-line nil)
  (push-mark)
  (move-end-of-line nil)
  (indent-rigidly (region-beginning) (region-end) (* -1 (symbol-value 'tab-width)))
  (pop-mark))
(global-set-key (kbd "S-<tab>") 'outdent)

;; Keyboard

;; Some Mac-friendly key counterparts
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-z") 'undo)

;; Custom tab formatting stuff..
(global-set-key (kbd "C-c n") 'nav)

;; Custom key bindings for other-window
(global-set-key (kbd "C-`") 'other-window)
(global-set-key (kbd "s-`") 'other-window)

;; Custom key binding for ido-imenu
(global-set-key (kbd "C-.") 'ido-imenu)

;; Custom key binding for comment or uncomment
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

;; Custom key binding for goto-line
(global-set-key (kbd "C-c g") 'goto-line)

;; Kill current buffer without prompting
(global-set-key (kbd "C-c k") 'kill-buffer-now)
(defun kill-buffer-now ()
  "Kill current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

;; Refresh ctags for current project.
(defun reload-tags ()
  "Rebuild gtags and visit tags table"
  (interactive)
  (shell-command "/usr/local/bin/ctags-create")
  (visit-tags-table "TAGS"))

(global-set-key [(tab)] 'smart-tab)
(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      ;; Perform the default behaviour. Do not do dabbrev-expand
      (minibuffer-complete)
    (if (eq (symbol-value 'major-mode) 'shell-mode)
        (unless (shell-dynamic-complete-command)
          (shell-dynamic-complete-filename))
      (if mark-active
          (indent-region (region-beginning)
                         (region-end))
        (if (looking-at "\\_>")
            (dabbrev-expand nil)
          (indent-for-tab-command))))))

;; Duplicate line
(defun duplicate-line ()
  "EASY"
  (interactive)
  (save-excursion
    (let ((line-text (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
      (move-end-of-line 1)
      (newline)
      (insert line-text))))
(global-set-key "\C-cd" 'duplicate-line)

;; Select current line and move cursor to end of line
(defun select-line ()
  "Selects the current line."
  (interactive)
  (end-of-line)
  (push-mark nil t t)
  (back-to-indentation))
(global-set-key (kbd "C-c s") 'select-line)

;; Other

(prefer-coding-system 'utf-8)

(server-start)

;; Activate theme
(load (concat dotfiles-dir "color-theme-github.el"))
(color-theme-github)

;; Enable org-mode drawers
(setq org-mode-drawers t)
