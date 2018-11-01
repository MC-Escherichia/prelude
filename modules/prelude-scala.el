;;; prelude-scala.el --- Emacs Prelude: scala-mode configuration.
;;
;; Copyright © 2011-2017 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic support for the Scala programming language

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

(require 'prelude-programming)
(prelude-require-packages '(scala-mode
                            ;; ensime
                            use-package))

(use-package ensime
  :ensure t
  :pin melpa)
(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa)

(defun prelude-scala-mode-defaults ()
  (subword-mode +1)
  ;; (ensime-mode +1)
  )

(setq prelude-scala-mode-hook 'prelude-scala-mode-defaults)

;; (setq ensime-startup-notification nil)
(setq sbt:prefer-nested-projects t)

(add-hook 'scala-mode-hook (lambda ()
                             (run-hooks 'prelude-scala-mode-hook)))

(require 'sbt-mode)

(setenv "PATH" (concat (getenv "PATH") ":C:\\Program Files (x86)\\sbt\\bin"))
(setq sbt:program-name "sbt.bat")
(setq ensime-sbt-command "C:\\Program Files (x86)\\sbt\\bin\\sbt.bat")

(setenv "JDK_HOME" "C:\\Program Files\\Java\\jdk1.8.0_131\\")


(setq sbt:program-options '())
(setenv "SBT_OPTS" "-Djline.terminal=jline.UnsupportedTerminal")
(provide 'prelude-scala)

;;; prelude-scala.el ends here
