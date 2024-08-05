;;; general.el
;;; grammarly

(use-package lsp-grammarly
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-grammarly)
                       (lsp))))  ; or lsp-deferred


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

(setq prelude-clean-whitespace-on-save nil)

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

(prelude-require-package 'dracula-theme)
