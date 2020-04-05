(setq-default
    ad-redefinition-action 'accept                   ; Silence warnings for redefinition
    auto-window-vscroll nil                          ; Lighten vertical scroll
    confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
    cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
    delete-by-moving-to-trash t                      ; Delete files to trash
    display-time-default-load-average nil            ; Don't display load average
    display-time-format "%H:%M"                      ; Format the time string
    fill-column 80                                   ; Set width for automatic line breaks
    help-window-select t                             ; Focus new help windows when opened
    indent-tabs-mode nil                             ; Stop using tabs to indent
    inhibit-startup-screen t                         ; Disable start-up screen
    initial-scratch-message "The triforce is coming for us in bash." ; Empty the initial *scratch* buffer
    initial-buffer-choice (lambda () (find-file "~/.emacs.d/personal/mfc-init.el"))
    mouse-yank-at-point t                            ; Yank at point rather than pointer
    ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
    recenter-positions '(5 top bottom)               ; Set re-centering positions
    scroll-conservatively most-positive-fixnum       ; Always scroll by one line
    scroll-margin 10                                 ; Add a margin when scrolling vertically
    select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
    sentence-end-double-space nil                    ; End a sentence after a dot and a space
    show-help-function nil                           ; Disable help messages
    show-trailing-whitespace nil                     ; Display trailing whitespaces
    split-height-threshold nil                       ; Disable vertical window splitting
    split-width-threshold nil                        ; Disable horizontal window splitting
    tab-width 4                                      ; Set width for tabs
    uniquify-buffer-name-style 'forward              ; Uniquify buffer names
    window-combination-resize t                      ; Resize windows proportionally
    x-stretch-cursor t)                              ; Stretch cursor to the glyph width
(add-to-list 'default-frame-alist '(fullheight))
(cd "~/")                                         ; Move to the user directory
(delete-selection-mode 1)                         ; Replace region when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(fringe-mode 0)                                   ; Disable fringes
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-subword-mode 1)                           ; Iterate through CamelCase words
(menu-bar-mode 0)                                 ; Disable the menu bar
(mouse-avoidance-mode 'banish)                    ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(add-hook 'focus-out-hook #'garbage-collect)
(setq css-fontify-colors t)

;; Set custom theme path
(use-package zenburn-theme
  :config (load-theme 'zenburn t))
(use-package ag)

(defun ora-move-beginning-of-line ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(define-prefix-command 'mfc-editing-map)
(global-set-key (kbd "C-o") mfc-editing-map)
(global-set-key (kbd "C-a") 'ora-move-beginning-of-line)
(global-set-key (kbd "C-p") counsel-help-map)
(global-set-key (kbd "C-h") 'previous-line)
(global-set-key (kbd "C-u") 'undo)
(global-set-key (kbd "C-/") 'universal-argument)
(global-unset-key (kbd "<up>"))
(global-set-key (kbd "<up>") 'previous-line)
                                        ;(global-set-key (kbd "<f1>") 'universal-argument)
(global-set-key (kbd "M-j") 'open-line)

(use-package yasnippet-snippets)

(use-package yasnippet
  :config (yas-global-mode)
  :bind (:map mfc-editing-map

              ("S" . 'yas-insert-snippet)
              ("s" . 'yas-expand)))

(use-package auto-yasnippet
  :bind (:map mfc-editing-map
              ("C-o" . #'aya-open-line)
              ("w" . #'aya-create)
              ("y" . #'aya-expand)))


(message "loaded mfc init")

(defmacro mfc-kill-server ()
  (interactive)
  (defalias 'yes-or-no-p '(lambda (a &rest b) t))
  (server-mode -1))

(global-set-key (kbd "C-x M-c") #'mfc-kill-server)
(require 'avy)
(global-set-key (kbd "C-'") 'avy-goto-char)

(add-hook 'text-mode-hook 'auto-fill-mode)

(setq-default fill-column 80)

(global-unset-key (kbd "C-c t"))

(require 'treemacs)
(define-key mfc-editing-map (kbd "t") 'treemacs)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(setq create-lockfiles nil)

(provide 'mfc-init)
