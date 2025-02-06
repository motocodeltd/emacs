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
                                  dracula-theme
                                  ledger-mode
                                  exec-path-from-shell
                                  expand-region
                                  treemacs
                                  lsp-treemacs
                                  helm-lsp
                                  projectile
                                  hydra
                                  flycheck
                                  company
                                  avy
                                  diff-hl
                                  drag-stuff
                                  which-key
                                  helm-xref
                                  json-mode
                                  multiple-cursors
                                  flyspell))  ;; Live spell checking

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

;; Simplenote
(require 'simplenote2)
(simplenote2-setup)
(global-set-key (kbd "<f12>") 'simplenote2-list)
(load-theme 'dracula t)

;; Helm
(defun double-flash-mode-line ()
  "Flash the modeline."
  (let ((flash-sec (/ 1.0 20)))
    (invert-face 'mode-line)
    (run-with-timer flash-sec nil #'invert-face 'mode-line)
    (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
    (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)))

(defun helm-execute-if-single-persistent-action (&optional attr split-onewindow)
  "Execute persistent action if the candidate list is less than 2."
  (interactive)
  (with-helm-alive-p
    (if (> (helm-get-candidate-number) 2)
        (double-flash-mode-line)
      (helm-execute-persistent-action))))

(use-package helm
  :demand t
  :bind* (:map helm-map 
               ([tab] . helm-execute-if-single-persistent-action)
               ("C-i" . helm-execute-persistent-action)))

(use-package helm-files
  :bind (:map helm-find-files-map
              ("<C-backspace>" . helm-find-files-up-one-level)))

(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "s-V") 'helm-show-kill-ring)
(global-set-key (kbd "s-e") 'helm-mini)

;; Ledger
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t) 
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

;; Duplicate line function
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
  (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common))
(eval-after-load "company-mode" 'company-mode-indent)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "s-w") 'er/expand-region)

(setq tab-always-indent 'complete)

;; Drag Stuff
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

(if (eq system-type 'darwin)
    (global-set-key (kbd "M-3") #'(lambda () (interactive) (insert "#"))))

;; Multiple cursors
(defun my-mc-mark-next-like-this ()
  "Select the next occurrence using multiple cursors."
  (interactive)
  (mc/mark-next-like-this 1))

(global-set-key (kbd "M-j") 'my-mc-mark-next-like-this)

(when (eq system-type 'darwin)
  (global-set-key (kbd "s-j") 'my-mc-mark-next-like-this))

;; Kill line function
(defun my/kill-line (&optional arg)
  "Delete the rest of the current line."
  (interactive "P")
  (delete-region
   (point)
   (progn
     (if arg
         (forward-visible-line (prefix-numeric-value arg))
       (if (eobp)
           (signal 'end-of-buffer nil))
       (let ((end (save-excursion (end-of-visible-line) (point))))
         (if (or (save-excursion (skip-chars-forward " \t" end) (= (point) end))
                 (and kill-whole-line (bolp)))
             (forward-visible-line 1)
           (goto-char end))))
     (point))))

(global-set-key (kbd "C-k") 'my/kill-line)

(setq mac-right-option-modifier nil)

(with-eval-after-load "ispell"
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_GB.UTF-8")
  (setq ispell-program-name "hunspell")
  ;; Configure German, Swiss German, and two variants of English.
  (setq ispell-dictionary "en_GB,el_GR")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_GB,el_GR")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
 ;; Ensure `ispell-personal-dictionary` is properly set before use
  (setq ispell-personal-dictionary "~/.hunspell_personal")

  ;; Ensure the personal dictionary file exists before using it
  (unless (and ispell-personal-dictionary (file-exists-p ispell-personal-dictionary))
    (with-temp-file ispell-personal-dictionary)))
;; Enable `flyspell-mode` for text
(dolist (hook '(text-mode-hook markdown-mode-hook org-mode-hook))
  (add-hook hook 'flyspell-mode))

;; Enable `flyspell-prog-mode` to check comments & strings in code
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


(global-diff-hl-mode)
(setq vc-follow-symlinks t)
