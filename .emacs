(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


(setq package-selected-packages '(simplenote2
				  use-package
				  lsp-mode
				  yasnippet
				  dracula-theme
				  ledger-mode
				  exec-path-from-shell
				  expand-region
				  linum
				  treemacs
				  lsp-treemacs
				  helm-lsp
				  projectile
				  hydra
				  flycheck
				  company
				  avy
				  drag-stuff
				  which-key
				  helm-xref
				  json-mode))


(when (cl-find-if-not #'package-installed-p package-selected-packages)
   (package-refresh-contents)
   (mapc #'package-install package-selected-packages))


(setq mac-command-modifier 'super)

(setq display-time-format "%d %b %H:%M")
(setq display-time-default-load-average nil)
(display-time-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq make-backup-files nil)

(cua-mode)

;;line numbers
(require 'linum)
(line-number-mode 1)
(column-number-mode 1)  ;; Line numbers on left most column
(global-linum-mode 1)

;;simplenote
(require 'simplenote2)
(simplenote2-setup)
(global-set-key (kbd "<f12>") 'simplenote2-list)
(load-theme 'dracula t)

;;helm
(defun double-flash-mode-line ()
 "Flash the modeline"
 (let ((flash-sec (/ 1.0 20)))
   (invert-face 'mode-line)
   (run-with-timer flash-sec nil #'invert-face 'mode-line)
   (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
   (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)))
(defun helm-execute-if-single-persistent-action (&optional attr split-onewindow)
 "Execute persistent action if the candidate list is less than 2"
 (interactive)
 (with-helm-alive-p
   (if (> (helm-get-candidate-number) 2)
       (double-flash-mode-line)
     (helm-execute-persistent-action)
     )))
(use-package helm
   :demand t
   :bind* (:map helm-map 
                ([tab] . helm-execute-if-single-persistent-action)
                ("C-i" . helm-execute-persistent-action)
                ))

(use-package helm-files
 :bind (:map helm-find-files-map
             ("<C-backspace>" . helm-find-files-up-one-level)
             )
 )
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "s-V") 'helm-show-kill-ring)
(global-set-key (kbd "s-e") 'helm-mini)

;;ledger
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t) (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))


(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-set-key (kbd "s-d") 'duplicate-line)

(setq save-interprogram-paste-before-kill t)

(add-hook 'after-init-hook 'global-company-mode)

;; expand region
(require 'expand-region)
(global-set-key (kbd "s-w") 'er/expand-region)

(setq tab-always-indent 'complete)

;; drag stuff
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))




;; ;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
;; (defun semnav-up (arg)
;;   (interactive "p")
;;   (when (nth 3 (syntax-ppss))
;;     (if (> arg 0)
;;         (progn
;;           (skip-syntax-forward "^\"")
;;           (goto-char (1+ (point)))
;;           (decf arg))
;;       (skip-syntax-backward "^\"")
;;       (goto-char (1- (point)))
;;       (incf arg)))
;;   (up-list arg))

;; (setq ns-right-alternate-modifier (quote none))

;; (defun move-line (n)
;;   "Move the current line up or down by N lines."
;;   (interactive "p")
;;   (setq col (current-column))
;;   (beginning-of-line) (setq start (point))
;;   (end-of-line) (forward-char) (setq end (point))
;;   (let ((line-text (delete-and-extract-region start end)))
;;     (forward-line n)
;;     (insert line-text)
;;     ;; restore point to original column in moved line
;;     (forward-line -1)
;;     (forward-char col)))

;; (defun move-line-up (n)
;;   "Move the current line up by N lines."
;;   (interactive "p")
;;   (move-line (if (null n) -1 (- n))))

;; (defun move-line-down (n)
;;   "Move the current line down by N lines."
;;   (interactive "p")
;;   (move-line (if (null n) 1 n)))

;; (global-set-key (kbd "M-S-<up>") 'move-line-up)
;; (global-set-key (kbd "M-S-<down>") 'move-line-down)
