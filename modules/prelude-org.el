;;; prelude-org.el --- Emacs Prelude: org-mode configuration.
;;
;; Copyright © 2011-2020 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;;This file is not part of GNU Emacs.

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
(require 'worf)

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
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
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
    org-archive-location "~/Code/org/archive/gtd.org::")
   ;; todo keywords
   (setq-default org-todo-keywords
                 '((sequence "TODO(t)" "NEXT(n)"
                             "STARTED(s)" "WAITING(w)" "BLOCKED(b)"
                             "|"
                             "DONE(d)" "CANCELLED(c)" "IDEA(i)" "BACKLOG(b)")))
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

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (cond ((= n-not-done 0) "DONE")
                    ((= n-done 0) "TODO")
                    ('t "STARTED")))))


(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(defun ndk/checkbox-list-complete ()
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
          (if (match-end 1)
              (org-todo (cond ((equal (match-string 1) "100%") "DONE")
                          ((equal (match-string 1) "0%") "TODO")
                          ('t "IN PROGRESS")))
            (if (and (> (match-end 2) (match-beginning 2))
                     (equal (match-string 2) (match-string 3)))
                (org-todo 'done)
              (org-todo 'todo)))))))

(eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))

(setq org-capture-templates
      (quote (("t" "Todo" entry (file+headline "~/Code/org/refile.org" "Todo")
               "* TODO %?\n %U\n" :clock-in t :clock-resume t)
              ("i" "Investigate" entry (file+headline "~/Code/org/refile.org" "Investigate")
               "* TODO %? :investigate:\n\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file+headline "~/Code/org/tasks/respond.org" "Respond")
               "* NEXT Urgent %? \nSCHEDULED: %t\n%U\n%a\n")
              ("n" "Note" entry (file+headline "~/Code/org/refile.org" "Notes")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file "~/Code/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "Tweak" entry (file+headline "~/Code/org/refile.org" "Tweaks")
               "* IDEA %? :tweak: \n")
              ("i" "Idea" entry (file+headline "~/Code/org/refile.org" "Ideas")
               "* IDEA %? \n%U\n%a\n")
              ("m" "Meeting" entry (file "~/Code/org/refile.org")
               "* MEETING with %? :MEETING:\n%U\n" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/Code/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))
(setq org-completion-use-ido t)
(require 'org-agenda)

(let ((map org-agenda-mode-map))
  ;; unbind
  (define-key map "a" 'worf-reserved)
  (define-key map "b" 'worf-reserved)
  (define-key map "c" 'worf-reserved)
  (define-key map "d" 'worf-reserved)
  (define-key map "e" 'worf-reserved)
  (define-key map "f" 'worf-reserved)
  (define-key map "n" 'worf-reserved)
  (define-key map "o" 'org-agenda-show)
  (define-key map "u" 'worf-reserved)
  (define-key map "w" 'worf-reserved)
  (define-key map "y" 'worf-reserved)
  (define-key map "z" 'worf-reserved)
  ;; arrows
  (define-key map "j" 'org-agenda-next-item)
  (define-key map "k" 'org-agenda-previous-item)
  (define-key map "h" 'org-agenda-earlier)
  (define-key map "l" 'org-agenda-later)
  ;; worf
  (define-key map "s" 'worf-schedule)
  (define-key map "N" 'worf-agenda-narrow)
  (define-key map "W" 'worf-agenda-widen)
  (define-key map "t" 'worf-todo)
  ;; misc
  (define-key map (kbd "C-j") 'org-open-at-point)
  (define-key map "i" 'org-agenda-clock-in)
  (define-key map "O" 'org-agenda-clock-out)
  (define-key map "0" 'digit-argument)
  (define-key map "1" 'digit-argument)
  (define-key map "v" 'hydra-org-agenda-view/body)
  (define-key map "x" 'hydra-org-agenda-ex/body)
  (define-key map "S" 'org-save-all-org-buffers)
  (define-key map "T" 'worf-clock-in-and-out)
  ;; disable
  (define-key map "f" nil))

(define-key org-mode-map (kbd "C-,") nil)
(define-key org-mode-map (kbd "C-'") nil)
(define-key org-mode-map (kbd "C-TAB") nil)
(define-key org-mode-map (kbd "C-M-i") 'ora-org-complete-symbol)
(define-key org-mode-map (kbd "C-m") 'newline)
(define-key org-mode-map (kbd "C-c C-r") nil)
(define-key org-mode-map [C-tab] nil)
(define-key org-mode-map (kbd "<f2> a") 'org-archive)
(define-key org-mode-map (kbd "χ") 'worf-back-to-heading)
(define-key org-mode-map (kbd "C-σ") 'org-edit-special)
(define-key org-mode-map (kbd "C-a") 'ora-move-beginning-of-line)
(define-key org-mode-map (kbd "M-r") 'org-ctrl-c-ctrl-c)
(define-key org-src-mode-map (kbd "C-c C-c") nil)
(define-key org-src-mode-map (kbd "C-σ") 'org-edit-src-exit)
(ora-advice-add 'org-edit-src-exit :after (lambda (&rest _) (save-buffer)))
(define-key org-mode-map (kbd "C-c C-v") nil)
(define-key org-mode-map (kbd "C-c C-q") 'counsel-org-tag)
(define-key org-mode-map (kbd "C-c t") 'worf-todo)
(define-key org-agenda-mode-map (kbd "C-c t") 'worf-todo)
(define-key org-agenda-mode-map (kbd "<backspace>") 'ora-org-agenda-unmark-backward)

(require 'hydra)

(defun org-agenda-cts ()
  (and (eq major-mode 'org-agenda-mode)
       (let ((args (get-text-property
                    (min (1- (point-max)) (point))
                    'org-last-args)))
         (nth 2 args))))

(defhydra hydra-org-agenda-view (:hint none)
  "
_d_: ?d? day        _g_: time grid=?g?  _a_: arch-trees
_w_: ?w? week       _[_: inactive       _A_: arch-files
_t_: ?t? fortnight  _f_: follow=?f?     _r_: clock report=?r?
_m_: ?m? month      _e_: entry text=?e? _D_: include diary=?D?
_y_: ?y? year       _q_: quit           _L__l__c_: log = ?l?"
  ("SPC" org-agenda-reset-view)
  ("D" org-agenda-day-view (if (eq 'day (org-agenda-cts)) "[x]" "[ ]") :exit t)
  ("d" (org-agenda nil "d") (if (eq 'day (org-agenda-cts)) "[x]" "[ ]") :exit t)
  ("w" (org-agenda nil "w") (if (eq 'week (org-agenda-cts)) "[x]" "[ ]") :exit t)
  ("t" org-agenda-fortnight-view (if (eq 'fortnight (org-agenda-cts)) "[x]" "[ ]") :exit t)
  ("m" org-agenda-month-view (if (eq 'month (org-agenda-cts)) "[x]" "[ ]") :exit t)
  ("y" org-agenda-year-view (if (eq 'year (org-agenda-cts)) "[x]" "[ ]") :exit t)
  ("l" org-agenda-log-mode (format "% -3S" org-agenda-show-log))
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("f" org-agenda-follow-mode (format "% -3S" org-agenda-follow-mode))
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode (format "% -3S" org-agenda-clockreport-mode))
  ("e" org-agenda-entry-text-mode (format "% -3S" org-agenda-entry-text-mode))
  ("g" org-agenda-toggle-time-grid (format "% -3S" org-agenda-use-time-grid))
  ("D" org-agenda-toggle-diary (format "% -3S" org-agenda-include-diary))
  ("!" org-agenda-toggle-deadlines)
  ("[" (let ((org-agenda-include-inactive-timestamps t))
         (org-agenda-check-type t 'timeline 'agenda)
         (org-agenda-redo)
         (message "Display now includes inactive timestamps as well")))
  ("q" (message "Abort") :exit t)
  ("x" org-agenda-exit :exit t)
  ("v" nil))

(setq org-archive-location "~/Code/org/archive.org::* From %s")

(provide 'prelude-org)

;;; prelude-org.el ends here
