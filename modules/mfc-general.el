(global-whitespace-mode 0)
(smartparens-global-strict-mode)
(whitespace-mode 0)
(add-to-list 'exec-path "C:/msys64/usr/bin")
(add-to-list 'exec-path "C:/Users/matthew.conway/Code/bin")

(global-set-key (kbd "C-o") 'ace-jump-mode)

(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)

(remove-hook 'comint-output-filter-functions
             'comint-postoutput-scroll-to-bottom)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(global-unset-key (kbd "C-c r"))

(defun kill-backwards ()
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(prelude-require-packages '(auto-complete bind-key ace-jump-mode))
(require 'auto-complete)
(require 'prelude-helm-everywhere)
(global-auto-complete-mode)

(require 'bind-key)
(bind-keys*
 ("M-k" . kill-backwards)
 ("C-M-SPC" . er/expand-region)
 ("<f10>" . gud-cont)
 ("<f9>" . gud-step)
 ("<f8>" .  gud-next)
 ("<f7>"  . gud-finish)
 ("C-x f" . helm-recentf)
 ("C-c r d" . duplicate-current-line-or-region))


(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'cider-mode-hook (lambda () (lispy-mode 1)))

(setq cider-default-repl-command "lein")
(setq cider-jack-in-default "lein")

(setq prelude-guru nil)



(setenv "PATH"
        (concat
         ;; Change this with your path to MSYS bin directory
         "c:\\msys64\\usr\\bin;"
         (getenv "PATH")))
(projectile-global-mode) ; Quickly navigate projects using Projectile (C-c p C-h for available commands)
(global-set-key (kbd "C-c p s g") 'helm-grep-do-git-grep)
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths) ; Projectile shows full relative paths

(setenv "USER" "MFC")

(setq temporary-file-directory "~/.emacs-tmp/")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'yasnippet)
(yas-global-mode 1)

(global-set-key (kbd "C-<tab>") 'yas-expand)

;; (load-library "auctex")
;; (setq latex-run-command "pdflatex")

(setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "VERIFY" "BLOCKED" "|" "DONE" "DELEGATED")))

(setq org-agenda-files (list "~/Documents/agenda.org"))

(if  (eq system-type 'gnu/linux)  (load-theme 'zenburn t) (load-theme 'ample-zen t))

(add-to-list 'default-frame-alist
             '(font . "Fira Code"))


;; (require 'ein)
;; (require 'ein-loaddefs)
;; (require 'ein-notebook)
;; (require 'ein-subpackages)
(require 'ob-ipython)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)))
;; (yas-global-mode 1)

;; (global-set-key (kbd "<tab>") 'yas-expand)

(provide 'mfc-general)
;;; mfc-general.el ends here
