;;; prelude-org.el --- Emacs Prelude: org-mode configuration.
;;
;; Copyright © 2011-2023 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

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

(require 'org)
(require 'org-habit)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; a few useful global keybindings for org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)

(setq org-log-done t)
(setq org-log-into-drawer t)

(defun prelude-org-mode-defaults ()
  (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-c +") nil)
    (define-key newmap (kbd "C-c -") nil)
    (define-key newmap (kbd "C-a") 'org-beginning-of-line)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist))
  )

(setq prelude-org-mode-hook 'prelude-org-mode-defaults)

(add-hook 'org-mode-hook (lambda () (run-hooks 'prelude-org-mode-hook)))

;;; the rest are from
;;; https://wiki.softwareheritage.org/wiki/Presentation_with_org_beamer

(require 'ox-beamer)

(defun inria-presentation/template-headers-at-pt ()
  "Inject default template inside the current buffer.
   No intelligence whatsoever."
  (interactive)
  (with-current-buffer (buffer-name)
    (insert
     "# org export options
   #+LANGUAGE:  en
   #+OPTIONS:   H:2 num:t toc:t \\n:nil @:t ::t |:t ^:t -:t f:t *:t <:t
   #+OPTIONS:   TeX:t LaTeX:t skip:nil d:nil todo:t pri:nil tags:not-in-toc
   #+EXPORT_SELECT_TAGS: export
   #+EXPORT_EXCLUDE_TAGS: noexport
   #+LINK_UP:
   #+LINK_HOME:

   # activate org-beamer-mode minor mode automatically
   #+STARTUP: beamer
   #+LaTeX_CLASS: beamer
   #+LaTeX_CLASS_OPTIONS: [presentation,xcolor=table]

   #+COLUMNS: %40ITEM %10BEAMER_env(Env) %9BEAMER_envargs(Env Args) %4BEAMER_col(Col) %10BEAMER_extra(Extra)

   # have the theme desired
   #+latex_header: \\mode<presentation>{\\usetheme{irill} \\beamertemplatenavigationsymbolsempty \\setbeamertemplate{navigation symbols}{} \\setbeamertemplate{headline}{} }

   # some color
   #+latex_header: \\rowcolors[]{1}{blue!10}{blue!05}

   # to have a toc for each section
   #+latex_header: \\AtBeginSection[] {\\begin{frame}<beamer> \\frametitle{Outline} \\tableofcontents[currentsection]\\end{frame} }

   # set the paths for images
   #+latex_header: \\graphicspath{{pics/}{../images/}{../../images/}{../pics/}{../../pics/}{../figures/}{../../figures/}{../logos/}{../../logos/}}

   # some default information I did not find how to set this in org-mode
   #+latex_header: \\institute[Irill/INRIA/UPD]{Présentation en ComDir\\\\ \\url{roberto@diiacosmo.org}\\\\ \\includegraphics[height=2cm]{SWH_logo_4-02b}}

   # to add the picblock macro
   #+latex_header: \\usepackage{extblocks}
   #+latex_header: \\usepackage{pgfpages}\n")))

(provide 'prelude-org)

;;; prelude-org.el ends here
