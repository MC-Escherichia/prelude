;;; prelude-org.el --- Emacs Prelude: org-mode configuration.
;;
;; Copyright © 2011-2020 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for org-mode.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'mfc-packages)

(setq org-log-done t)

(defun prelude-org-mode-defaults ()
  (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-c +") nil)
    (define-key newmap (kbd "C-c -") nil)
    (define-key newmap (kbd "C-a") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist))
)

(setq prelude-org-mode-hook 'prelude-org-mode-defaults)

(add-hook 'org-mode-hook (lambda () (run-hooks 'prelude-org-mode-hook)))

(use-package org
     :mode ("\\.org\\'" . org-mode)
     :config (require 'org-agenda)
     :bind (("C-c l" . 'org-store-link)
            ("C--" . 'org-capture)
            ("C-c a" . 'org-agenda)
            ("C-x M-b" . 'org-switchb)
            :map org-mode-map
            ("C-c t" . 'org-todo)
            ("}" . 'mfc-org-insert-checkbox)))

   (use-package org-agenda
     :ensure nil
     :bind (:map org-agenda-mode-map
                 ("}" . 'org-agenda-todo)))
   (use-package org-bullets
     :hook (org-mode . org-bullets-mode))
   ;; basic startup
   (setq
    org-directory "~/Code/org/"
    org-agenda-files (--filter (s-match ".*org$" it) (f-files "~/Code/org/tasks"))
    org-startup-folded t
    org-archive-location "~/Code/org/archive/gtd.org_archive")
   ;; todo keywords
   (setq-default org-todo-keywords
                 '((sequence "IDEA(i)" "TODO(t)" "NEXT(n)"
                             "STARTED(s)" "WAITING(w)" "BLOCKED(b)"
                             "|"
                             "DONE(d)" "CANCELLED(c)")))
   (setq org-todo-keyword-faces
         '(("IDEA" . (:foreground "yellow" :weight bold))
           ("TODO" . (:foreground "red" :weight bold))
           ("NEXT" . (:foreground "blue" :weight bold))
           ("STARTED" . (:foreground "teal" :weight bold))
           ("BLOCKED" . (:foreground "magenta" :weight bold))
           ("WAITING" . (:foreground "orange" :weight bold))
           ("DONE" . (:foreground "forest green" :weight bold))
           ("CANCELLED" . (:foreground "light green" :weight bold))))

   (setq org-treat-S-cursor-todo-selection-as-state-change t)
   (setq org-enforce-todo-dependencies t)
   (setq org-log-done nil)
   (setq org-deadline-warning-days 14)
   (defun mfc-org-insert-checkbox (go-inside)
   (interactive "p")
   (insert "[ ]")
   (if (equal go-inside 4) (backward-char 2) (insert " ")))
   ;; clock
   (setq org-clock-out-remove-zero-time-clocks t)
   (setq org-clock-string-limit 30)
   ;; call outs
   (setq org-file-apps
         '((auto-mode . emacs)
           (directory . emacs)
           ("\\.*" . "cmd.exe /c start %s")))
   ;; priorites

   ;; tags
   ;; agenda
   (setq org-agenda-span 'day)
   (setq org-agenda-remove-tags t)
   ;; capture templates


   ;; dodads

   (use-package org-bullets
     :config
     (setcdr org-bullets-bullet-map nil))

(provide 'prelude-org)

;;; prelude-org.el ends here
