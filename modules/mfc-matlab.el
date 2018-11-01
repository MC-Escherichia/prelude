;;; mfc-matlab.el --- Emacs Prelude: Nice config for Elisp programming.
;;
;; Copyright © 2011-2017 Matthew Conway
;;
;; Author: Matthew Conway matthew.conway@regeneron.com
;; Version: 1.0.0
;; Keywords: convenience
;; Package-Requires: ((prelude-lisp "1.0.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; MFC Config for MATLAB on windows

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

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/modules/lib/matlab-emacs-src"))

;; (load-library "matlab-load")

;; (require 'matlab)


;; (setq matlab-shell-command "c:/Users/matthew.conway/matlabShell.exe")

;; (matlab-cedet-setup)


;; (setq matlab-indent-function t)
;; Change path


;; (setq matlab-shell-command "c:/Programs/matlabshell/matlabshell.exe")

;; (setq matlab-shell-command-switches '())
;; (setq matlab-shell-echoes nil)
;; (setq matlab-shell-command-switches '("10000" "20000"))
;; the following is for non-session based evaluations
;; (setq org-babel-matlab-shell-command
;;       "c:/Users/matthew.conway/Code/matlabShell/matlabshell.cmd")

;; Have libeng.dll on your PATH or use the following
;; (setenv "PATH" (concat "C:/PROGRA~1/MATLAB/R2016a/bin/win64;" (getenv "PATH")))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((matlab . t) (octave . t)
;;    ))


;; (setq org-babel-default-header-args:matlab
;;   '((:results . "output") (:session . "*MATLAB*")))


;; (defun matlab-prev-cell ()
;;   (interactive)
;;   (search-backward "%%")
;;   (search-backward "%%")
;;   (forward-line)
;;   )

;; (defun matlab-next-cell ()
;;   (interactive)
;;   (search-forward "%%")
;;   (forward-line)
;;   )

;; (defun matlab-eval-cell ()
;;   (interactive)
;;   (let ((start (search-backward "%%"))
;;         (end   (search-forward "%%" nil nil 2)))
;;     (matlab-shell-run-region start end)))



;; (define-key matlab-mode-map (kbd "C-c M-e") 'matlab-eval-cell)
;; (define-key matlab-mode-map (kbd "C-c C-n") 'matlab-next-cell)
;; (define-key matlab-mode-map (kbd "C-c C-p") 'matlab-prev-cell)
;; (define-key matlab-mode-map (kbd "C-c <return>") (lambda ()
;;                                                    (interactive)
;;                                                    (matlab-eval-cell)
;;                                                    (matlab-next-cell)))

;; (provide 'mfc-matlab)
;;; mfc-matlab.el ends here
