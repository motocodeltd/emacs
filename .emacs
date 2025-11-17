;; Ensure MELPA is loaded
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Load environment variables on macOS
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq package-selected-packages '(simplenote2
                                  use-package
                                  lsp-mode
                                  yasnippet
                                  ledger-mode
                                  exec-path-from-shell
                                  expand-region
                                  treemacs
                                  lsp-treemacs
                                  projectile
                                  dumb-jump
                                  magit
                                  hydra
                                  flycheck
                                  company
                                  avy
                                  diff-hl
                                  drag-stuff
                                  which-key
                                  json-mode
                                  multiple-cursors
                                  flyspell
                                  vertico
                                  orderless
                                  marginalia
                                  consult))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
   (package-refresh-contents)
   (mapc #'package-install package-selected-packages))

(delete-selection-mode 1)
(setq mac-command-modifier 'super)
(setq display-time-format "%d %b %H:%M")
(setq display-time-default-load-average nil)
(display-time-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq make-backup-files nil)
(cua-mode)

;; Line numbers
(global-display-line-numbers-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(global-hl-line-mode 1)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(save-place-mode 1)

;; Save history
(setq history-length 1024)
(savehist-mode 1)
(recentf-mode 1)

(setq use-dialog-box nil)

;; Simplenote
(require 'simplenote2)
(simplenote2-setup)
(global-set-key (kbd "<f12>") 'simplenote2-list)
(load-theme 'modus-vivendi-deuteranopia t)

;; Ledger
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

;; Duplicate line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(global-set-key (kbd "s-d") 'duplicate-line)

(setq save-interprogram-paste-before-kill t)
(add-hook 'after-init-hook 'global-company-mode)

(defun company-mode-indent ()
  (define-key company-mode-map [remap indent-for-tab-command]
    #'company-indent-or-complete-common))
(eval-after-load "company-mode" 'company-mode-indent)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "s-w") 'er/expand-region)

(setq tab-always-indent 'complete)

;; Drag Stuff
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; macOS # insertion
(if (eq system-type 'darwin)
    (global-set-key (kbd "M-3") #'(lambda () (interactive) (insert "#"))))

;; Multiple cursors
(defun my-mc-mark-next-like-this ()
  (interactive)
  (mc/mark-next-like-this 1))
(global-set-key (kbd "M-j") 'my-mc-mark-next-like-this)
(global-set-key (kbd "s-j") 'my-mc-mark-next-like-this)

;; Kill line
(defun my/kill-line (&optional arg)
  (interactive "P")
  (delete-region
   (point)
   (progn
     (if arg
         (forward-visible-line (prefix-numeric-value arg))
       (if (eobp) (signal 'end-of-buffer nil))
       (let ((end (save-excursion (end-of-visible-line) (point))))
         (if (or (save-excursion (skip-chars-forward " \t" end)
                                 (= (point) end))
                 (and kill-whole-line (bolp)))
             (forward-visible-line 1)
           (goto-char end))))
     (point))))
(global-set-key (kbd "C-k") 'my/kill-line)

;; Spellchecking
(with-eval-after-load "ispell"
  (setenv "LANG" "en_GB.UTF-8")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_GB,el_GR")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_GB,el_GR")
  (setq ispell-personal-dictionary "~/.hunspell_personal")
  (unless (file-exists-p ispell-personal-dictionary)
    (with-temp-file ispell-personal-dictionary)))
(dolist (hook '(text-mode-hook markdown-mode-hook org-mode-hook))
  (add-hook hook 'flyspell-mode))
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(global-diff-hl-mode)
(setq vc-follow-symlinks t)

;; ===========================
;; IntelliJ-like Configuration
;; ===========================

;; Cmd-TAB like buffer switcher
(global-set-key (kbd "C-<tab>") 'consult-buffer)

;; Projectile
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; Use Emacs completion now (not helm)
  (setq projectile-completion-system 'default))

;; IntelliJ Ctrl+Shift+N → find file in project
(global-set-key (kbd "C-S-n") 'projectile-find-file)

;; IntelliJ Ctrl+Shift+F → project search
(global-set-key (kbd "C-S-f") 'consult-ripgrep)

;; IntelliJ navigate to symbol
(global-set-key (kbd "C-S-o") 'consult-imenu)

;; Command-e (⌘-e) → IntelliJ “Navigate / Recent Files”
(global-set-key (kbd "s-e") 'consult-buffer)

;; Better buffer list than default
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Comment/uncomment line
(global-set-key (kbd "C-/") 'comment-line)

;; Dumb-jump for definitions
(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'default)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-pop-marker-stack)

;; Company completion tweaks
(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection))

;; LSP
(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-snippet t)
  (setq lsp-prefer-flymake nil))

;; Treemacs
(use-package treemacs
  :config
  (global-set-key (kbd "C-c t") 'treemacs)
  (treemacs-project-follow-mode t))

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)))

;; Refactor hydra (LSP)
(defhydra hydra-refactor (:color blue :hint nil)
  "
^Refactor^
^^^^^^^^^-----------------------------------------
_r_: rename      _e_: extract function    _i_: inline
_l_: extract local var    _o_: organize imports
"
  ("r" lsp-rename)
  ("e" lsp-extract-function)
  ("l" lsp-extract-variable)
  ("i" lsp-execute-code-action)
  ("o" lsp-organize-imports)
  ("q" nil))
(global-set-key (kbd "C-c r") 'hydra-refactor/body)

;; Run/debug
(global-set-key (kbd "C-S-x") 'lsp-execute-code-action)
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "<f5>") 'recompile)

;; Which-key
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

;; Remote sudo editing
(defun open-remote-sudo-file ()
  (interactive)
  (let* ((host (read-string "SSH host: "))
         (ssh-user (read-string "SSH user: " user-login-name))
         (sudo-user (read-string "Sudo as user: " "root"))
         (tramp-dir (format "/ssh:%s@%s|sudo:%s@%s:~/" ssh-user host sudo-user host))
         (file (read-file-name "Remote file: " tramp-dir)))
    (find-file file)))

(define-prefix-command 'remote-sudo-map)
(define-key global-map (kbd "C-x r s") 'remote-sudo-map)
(define-key remote-sudo-map (kbd "f") #'open-remote-sudo-file)

;; UI tweaks
(pixel-scroll-precision-mode 1)
(scroll-bar-mode -1)
(setq-default cursor-type 'box)

(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier nil)

;; diff-hl real-time updates
(global-diff-hl-mode 1)
(diff-hl-flydiff-mode 1)
(setq auto-revert-check-vc-info t)
(with-eval-after-load 'magit
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
(add-hook 'after-save-hook 'diff-hl-update)
(add-hook 'focus-in-hook 'diff-hl-update)
(run-with-idle-timer 2 t 'diff-hl-update)

;;;; ======================
;;;; Phase 2 – Modern Completion Stack
;;;; ======================

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides
        '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t)

;; IntelliJ Paste History
(global-set-key (kbd "s-S-v") #'consult-yank-from-kill-ring)
(global-set-key (kbd "M-y")   #'consult-yank-from-kill-ring)
(global-set-key (kbd "s-V")   #'consult-yank-from-kill-ring)

(global-set-key (kbd "C-x C-r") 'consult-recent-file)
