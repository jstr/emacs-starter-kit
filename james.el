;; Include Textmate minor node from Defunkt
(add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
(require 'textmate)
(textmate-mode)

;; Custom tab formatting stuff..
(global-set-key (kbd "C-M-g") 'magit-status)

;; Smart tab behaviour. Completes or tabs depending on context.
;; From a comment by Marius Andersen at http://emacsblog.org/2007/03/12/tab-completion-everywhere/#comment-14058.
(global-set-key [(tab)] 'smart-tab)
(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      ;; Do nothing in the minibuff.
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (dabbrev-expand nil)
        (indent-for-tab-command)))))

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

(setq-default tab-width 2)
(setq-default tab-stop-list (list 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108))

;; Set a custom frame width
(add-to-list 'default-frame-alist '(width . 150))

;; Set our theme..
;; (color-theme-zenburn)
(load (concat dotfiles-dir "topfunky-theme.el"))
(color-theme-topfunky)

;; A large typeface if we want one...
;; (set-face-attribute 'default nil :height 140)

;; Start emacsclient server for access from the command line.
(server-start)

