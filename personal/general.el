;;; general.el
;;; grammarly

;; (use-package lsp-grammarly
;;   :ensure t
;;   :hook (text-mode . (lambda ()
;;                        (require 'lsp-grammarly)
;;                        (lsp))))  ; or lsp-deferred


(add-hook 'after-init-hook 'projectile-mode)

;; Shorter modeline
(setq-default projectile-mode-line-prefix " Proj")

(when (executable-find "rg")
  (setq-default projectile-generic-command "rg --files --hidden -0"))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ibuffer-projectile
  :ensure t)

(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))


;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(setq prelude-clean-whitespace-on-save 'nil)

;; MATLAB
(use-package matlab
  :ensure matlab-mode
  :config
  (add-to-list
   'auto-mode-alist
   '("\\.m\\'" . matlab-mode))
  (setq matlab-indent-function nil)
  (setq matlab-shell-command "/Applications/MATLAB_R2023a.app/bin/matlab")
  (setq matlab-shell-command-switches (list "-nodesktop"))
  (setq-default matlab-indent-function-body nil)
  )

(prelude-require-packages '(s dash editorconfig company))

(use-package copilot
  :load-path (lambda () (expand-file-name "copilot.el" prelude-vendor-dir))
  ;; don't show in mode line
  :diminish
  :bind (:map copilot-completion-map
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion))
  :config
  (setq copilot-indent-offset-warning-disable t)
  )

(defun rk/no-copilot-mode ()
  "Helper for `rk/no-copilot-modes'."
  (copilot-mode -1))

(defvar rk/no-copilot-modes '(shell-mode
                              inferior-python-mode
                              eshell-mode
                              term-mode
                              vterm-mode
                              comint-mode
                              compilation-mode
                              debugger-mode
                              dired-mode-hook
                              compilation-mode-hook
                              flutter-mode-hook
                              minibuffer-mode-hook)
  "Modes in which copilot is inconvenient.")

(defun rk/copilot-disable-predicate ()
  "When copilot should not automatically show completions."
  (or rk/copilot-manual-mode
      (member major-mode rk/no-copilot-modes)
      (company--active-p)))

(add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)

(defvar rk/copilot-manual-mode nil
  "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

(defun rk/copilot-change-activation ()
  "Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
  (interactive)
  (if (and copilot-mode rk/copilot-manual-mode)
      (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq rk/copilot-manual-mode nil))
    (if copilot-mode
        (progn
          (message "activating copilot manual mode")
          (setq rk/copilot-manual-mode t))
      (message "activating copilot mode")
      (global-copilot-mode))))

(define-key global-map (kbd "C-s-<escape>") #'rk/copilot-change-activation)

(defun rk/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (copilot-complete)))

(define-key copilot-mode-map (kbd "C-s-f") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "C-s-b") #'copilot-previous-completion)
(define-key copilot-mode-map (kbd "C-s-<right>") #'copilot-accept-completion-by-word)
(define-key copilot-mode-map (kbd "C-s-<down>") #'copilot-accept-completion-by-line)
(define-key global-map (kbd "C-s-<return>") #'rk/copilot-complete-or-accept)

(defun rk/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
                     (setq copilot-disable-predicates (list (lambda () t)))
                     (copilot-clear-overlay)
                     (run-with-idle-timer
                      1.0
                      nil
                      (lambda ()
                        (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

(advice-add 'keyboard-quit :before #'rk/copilot-quit)

(prelude-require-package 'dracula-theme)

(require 'yasnippet)

(prelude-require-package 'yasnippet-snippets)
(yas-global-mode 1)

(setq hl-todo-keyword-faces
      '(("TODO"   . "#FFFFFF")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")))

(use-package beancount
  :ensure t)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Workspace/zettelkasten"))
  :bind (("C-c m l" . org-roam-buffer-toggle)
         ("C-c m f" . org-roam-node-find)
         ("C-c m g" . org-roam-graph)
         ("C-c m i" . org-roam-node-insert)
         ("C-c m c" . org-roam-capture)
         ;; Dailies
         ("C-c m j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  ;; (org-roam-db-autosync-mode) ; enabled at md-roam
  ;; If using org-roam-protocol
  ;;(require 'org-roam-protocol)
  )

(use-package org-roam-ui
  :ensure t
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package md-roam
  :load-path (lambda () (expand-file-name "md-roam" prelude-vendor-dir))
  :after org-roam
  :config
  (setq org-roam-file-extensions '("org" "md")) ; enable Org-roam for a markdown extension
  (md-roam-mode 1) ; md-roam-mode must be active before org-roam-db-sync
  (setq md-roam-file-extension "md") ; default "md". Specify an extension such as "markdown"
  (org-roam-db-autosync-mode) ; autosync-mode triggers db-sync. md-roam-mode must be already active
  (add-to-list 'org-roam-capture-templates
               '("m" "Markdown" plain "" :target
                 (file+head "%<%Y-%m-%dT%H%M%S>-${slug}.md"
                            "---\ntitle: ${title}\nid: %<%Y-%m-%dT%H%M%S>\ncategory: \n---\n")
                 :unnarrowed t))
  (add-to-list 'org-roam-capture-templates
               '("e" "env716" plain "%?" :target
                 (file+head "env716f2024/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
")
                 :unnarrowed t))
  (add-to-list 'org-roam-capture-templates
               '("x" "dnx" plain "%?" :target
                 (file+head "dnx/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
")
                 :unnarrowed t))
  (add-to-list 'org-roam-capture-templates
               '("b" "bass-conn" plain "%?" :target
                 (file+head "bass2425/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
")
                 :unnarrowed t))
  )

(use-package deft
  :ensure t
  :bind
  ("C-c m d" . deft)
  :config
  (setq deft-recursive t)
  (setq deft-use-filter-string-for-filename t)
  ;; (setq deft-strip-summary-regexp "\\`\\(.+\n\\)+\n") ;; "\\([\n ]\\|^#\\+[[:upper:][:lower:]_]+:.*$\\)")
  ;; (setq deft-use-filename-as-title t)

  (defun cm/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
          (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
        (deft-base-filename file))))

  (advice-add 'deft-parse-title :override #'cm/deft-parse-title)

  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
                "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
                "\\)"))

  (setq deft-extensions '("txt" "tex" "org" "md"))
  (setq deft-directory  "~/Workspace/zettelkasten")
  (setq deft-recursive-ignore-dir-regexp "\\(?:^\\|/\\)\\(?:\\.\\.?\\|content[^/]*\\)\\(?:/\\|$\\)")
  )

(use-package org-ref
  :ensure t
  :after org
  :config
  (setq bibtex-completion-bibliography '("~/Workspace/GRACE/bibs/ref.bib"))
  )

(setq org-agenda-files '("~/Workspace/zettelkasten" "~/Workspace/zettelkasten/daily") )

(use-package org-pomodoro
  :ensure t
  )

(use-package alert
  :config
  (if (eq system-type 'darwin)
      (setq
       ;; alert-default-style 'notifier
       alert-default-style 'osx-notifier
       ))
  ;; (alert "This is an alert" :severity 'high)
  ;; (alert "This is an alert" :title "My Alert" :category 'debug)
  )

(setq aw-scope 'frame) ;; was 'global

(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 5 :fileskip0 t))

;; https://www.gnu.org/software/emacs/manual/html_node/org/Breaking-Down-Tasks.html
;;
;; If you would like a TO-DO entry to automatically change to DONE when
;; all children are done, you can use the following setup:
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-todo-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

(setq org-columns-default-format-for-agenda "%25ITEM %TODO %17Effort(Estimated Effort){:} %CLOCKSUM %3PRIORITY %TAGS")

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo . " %l %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

(setq org-pomodoro-start-sound-args "-v 0.15")
(setq org-pomodoro-start-sound-args "-v 0.15")
(setq org-pomodoro-finished-sound-args "-v 0.15")
(setq org-pomodoro-overtime-sound-args "-v 0.15")
(setq org-pomodoro-killed-sound-args "-v 0.15")
(setq org-pomodoro-short-break-sound-args "-v 0.15")
(setq org-pomodoro-long-break-sound-args "-v 0.15")
(setq org-pomodoro-ticking-sound-args "-v 0.15")

(defun org-pomodoro-overtime-osxfix ()
  "Is invoked when the time for a pomodoro runs out.
Notify the user that the pomodoro should be finished by calling `org-pomodoro'"
  (org-pomodoro-maybe-play-sound :overtime)
  (org-pomodoro-notify "Pomodoro completed. Now on overtime!" "Start break by calling `org-pomodoro'")
  (org-pomodoro-start :overtime)
  (org-pomodoro-update-mode-line)
  (run-hooks 'org-pomodoro-overtime-hook))

(advice-add 'org-pomodoro-overtime :override #'org-pomodoro-overtime-osxfix)

(setq org-pomodoro-manual-break t)


(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

(add-to-list 'org-latex-classes
             '("tau"
               "\\documentclass{tau-class/tau}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("extarticle"
               "\\documentclass{extarticle}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("memo"
               "\\documentclass{memo-class/memo}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
             '("draftarticle"
               "\\documentclass{draft-class/draftarticle}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("cbeamer"
               "\\documentclass[presentation]{custom-beamer/cbeamer}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(setq org-latex-src-block-backend 'listings)

(defun count-characters-in-section ()
  "Count characters and words in each level 1 section of the Org-mode buffer, excluding the headline."
  (interactive)
  (org-map-entries
   (lambda ()
     (let* ((start (progn (forward-line 6) (point))) ; Move past the headline
            (end (save-excursion (org-end-of-subtree t)))
            (char-count (how-many "\\S-" start end)) ; Count non-whitespace characters
            (char-count-wsp (- end start)) ; Count total characters including whitespace
            (word-count (count-words start end))) ; Count words
       (org-set-property "charcount" (number-to-string char-count))
       (org-set-property "charcountwsp" (number-to-string char-count-wsp))
       (org-set-property "wordcount" (number-to-string word-count))))
   "LEVEL=1")) ; Restrict to level 1 headings

(use-package ox-ipynb
  :load-path (lambda () (expand-file-name "ox-ipynb" prelude-vendor-dir))
  :config
  (require 'ox-ipynb)
  )

(use-package ob-ipython
  :ensure t
  :config
  (require 'ob-ipython))

(setq org-export-allow-bind-keywords t)
(setenv "PYDEVD_DISABLE_FILE_VALIDATION" "1")
;; (setq debug-on-error t)

(use-package org-download
  :ensure t
  :config
  (require 'org-download))
