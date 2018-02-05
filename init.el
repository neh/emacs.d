;;; init.el --- Initialize emacs
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq exec-path (append exec-path '("~/bin")))

;; Set font/size based on display DPI
(let ((mydpi (/ (display-pixel-width) (/ (display-mm-width) 25.4)))
      (myfont "mononoki"))
  ;;(myfont "Iosevka Light"))
  (if (> mydpi 160)
      (set-frame-font (format "%s-10" myfont))
    (set-frame-font (format "%s-10" myfont))))

;; set comments to always be italic
(set-face-italic 'font-lock-comment-face t)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 101) ;; Don't jump around when scrolling

(setq delete-old-versions t)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq-default indent-tabs-mode nil)
;; (setq-default show-trailing-whitespace t)
(setq make-backup-files nil)
(setq sentence-end-double-space nil)
(setq ring-bell-function 'ignore)
(setq enable-recursive-minibuffers t)
(save-place-mode 1)
(global-hl-line-mode t)

(setq frame-title-format "%b") ;; focused window title format
(setq icon-title-format "%b") ;; unfocused window title format

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'no-error)

;; from http://www.jethrokuan.com/init.html
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

(global-subword-mode 1)

;; Bootstrap straight.el
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)


;; (use-package color-theme-sanityinc-tomorrow)
;; (load-theme sanityinc-tomorrow-night)
;; (use-package gruvbox-theme)
;; (load-theme 'gruvbox t)
;; (use-package plan9-theme)
;; (load-theme 'plan9 t)
;; (use-package material-theme)
;;(load-theme 'material t)
;; (load-theme 'material-light t)
(use-package darktooth-theme)
(load-theme 'darktooth t)

;; (straight-use-package 'ample-theme)
;; (load-theme 'ample t t)
;; (load-theme 'ample-flat t t)
;; (load-theme 'ample-light t t)
;; (enable-theme 'ample-flat)

;; (straight-use-package 'alect-themes)
;; (load-theme 'alect-dark-alt t)

;; (straight-use-package 'solarized-theme)
;; (load-theme 'solarized-light)
;; (setq solarized-distinct-fringe-background t)
;; (setq solarized-use-more-italic t)

(use-package undo-tree
  :config
  (global-undo-tree-mode t))

(use-package paren
  :init
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis)
  :config
  (show-paren-mode 1))

;; A fun to call from programming mode hooks to deal with auto-fill if
;; the below config doesn't work well.
(defun comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode nil))

(setq-default fill-column 80)
(setq comment-auto-fill-only-comments t)
;; (setq-default auto-fill-function 'do-auto-fill)

;; uses the fill-column setting for visual-line-mode
(use-package visual-fill-column)

(use-package whitespace
  :init
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face trailing tabs lines-tail))
  :config
  (whitespace-mode))

(setq uniquify-buffer-name-style 'forward)

;; Will probably just not bother with line numbers until emacs 26, which has
;; them built in.
;;
;; nlinum is supposed to be faster, but elpa is maybe broken? so can't install
;; (use-package nlinum-relative
;;   :config
;;   (nlinum-relative-setup-evil)
;;   (add-hook 'prog-mode-hook 'nlinum-relative-mode)
;;   (setq nlinum-relative-current-symbol ""))

;; (use-package linum-relative
;;   :init
;;   (setq linum-relative-current-symbol "")
;;   :config
;;   (linum-relative-global-mode))

(use-package evil
  :init
  (setq evil-want-integration nil)
  (setq evil-move-cursor-back t)
  (setq evil-vsplit-window-right t)
  :config
  (evil-mode 1)

  (evil-set-initial-state 'ivy-occur-mode 'emacs)

  ;; (unbind-key "s" evil-normal-state-map)
  (unbind-key "C-t" evil-normal-state-map)
  (unbind-key "C-n" evil-normal-state-map)
  (unbind-key "C-e" evil-motion-state-map)

  ;; (setq evil-normal-state-tag   (propertize "N" 'face '((:background "green" :foreground "black")))
  ;;       evil-emacs-state-tag    (propertize "E" 'face '((:background "orange" :foreground "black")))
  ;;       evil-insert-state-tag   (propertize "I" 'face '((:background "red")))
  ;;       evil-motion-state-tag   (propertize "M" 'face '((:background "blue")))
  ;;       evil-visual-state-tag   (propertize "V" 'face '((:background "grey80" :foreground "black")))
  ;;       evil-operator-state-tag (propertize "O" 'face '((:background "purple"))))
  (setq evil-normal-state-tag   (propertize "N")
        evil-emacs-state-tag    (propertize "E")
        evil-insert-state-tag   (propertize "I")
        evil-replace-state-tag  (propertize "R")
        evil-motion-state-tag   (propertize "M")
        evil-visual-state-tag   (propertize "V")
        evil-operator-state-tag (propertize "O"))

  (defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  (use-package evil-surround
    :config
    (global-evil-surround-mode)))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :after evil-collection
  :config
  (general-create-definer
    neh/leader-keys
    :keymaps 'override
    :states '(emacs normal visual motion insert)
    :non-normal-prefix "C-SPC"
    :prefix "SPC")

  (general-override-mode)

  (general-define-key
   :states '(normal visual)
    "h" 'evil-backward-char
    "t" 'evil-next-visual-line
    "n" 'evil-previous-visual-line
    "s" 'evil-forward-char

    "l" 'evil-search-next
    "L" 'evil-search-previous
    "S" 'evil-window-bottom)

  (neh/leader-keys
    "<SPC>" '(save-buffer :which-key "save")

    "b" '(:ignore t :which-key "buffer")
    "bd" '(evil-delete-buffer :which-key "delete buffer")

    "cc" '(comment-or-uncomment-region-or-line :which-key "toggle comment")

    "f" '(:ignore t :which-key "formatting")
    "fa" '(auto-fill-mode :which-key "auto fill")
    "fi" '(indent-region :which-key "indent region")
    "fp" '(fill-paragraph :which-key "paragraph")
    "fr" '(fill-region :which-key "fill region")
    "ft" '(toggle-truncate-lines :which-key "wrap lines")
    "fw" '(whitespace-mode :which-key "show whitespace")

    "g" '(:ignore t :which-key "git")
    "gc" '(magit-commit :which-key "commit")
    "gd" '(magit-diff-popup :which-key "diff")
    "gf" '(magit-stage-file :which-key "stage file")
    "gh" '(git-gutter+-stage-hunks :which-key "stage hunk")
    "gl" '(magit-log-popup :which-key "log")
    "gm" '(magit-dispatch-popup :which-key "menu")
    "gP" '(magit-push-popup :which-key "push")
    "gs" '(magit-status :which-key "status")

    "h" '(help-command :which-key "help")
    "ha" 'helm-apropos
    "hf" '(counsel-describe-function :which-key "describe function")
    "hv" '(counsel-describe-variable :which-key "describe variable")

    "i" '(:ignore t :which-key "insert")
    "ip" '(clipboard-yank :which-key "paste from clipboard")

    "o" '(:ignore t :which-key "open")
    "oa" '(counsel-linux-app :which-key "app")
    "oe" '(mode-line-other-buffer :which-key "previous buffer")
    "of" '(counsel-find-file :which-key "open file")
    "og" '(counsel-git :which-key "open git file")
    "oh" '(counsel-projectile :which-key "open file in project")
    "ol" '(org-open-at-point :which-key "follow link")
    ;; "oo" '(ivy-switch-buffer :which-key "switch buffer")
    "oo" '(persp-switch-to-buffer :which-key "switch buffer")
    "op" '(counsel-projectile-switch-project :which-key "switch project")
    "ov" '(persp-switch :which-key "switch perspective")

    "pr" '(package-refresh-contents :which-key "refresh package info")

    "s" '(:ignore t :which-key "search")
    "sa" '(swiper-all :which-key "search all buffers")
    "sf" '(counsel-ag :which-key "search files")
    "sg" '(counsel-git-grep :which-key "search files in git")
    "sh" '(counsel-grep-or-swiper :which-key "search buffer")

    "v" '(:ignore t :which-key "view")

    "x" '(:ignore t :which-key "execute")
    "xa" '(ivy-resume :which-key "ivy resume")
    "xb" '(eval-buffer :which-key "eval buffer")
    "xe" '(eval-expression :which-key "eval expression")
    "xr" '(eval-region :which-key "eval region")
    "xs" '(eval-last-sexp :which-key "eval sexp")
    "xx" '(counsel-M-x :which-key "M-x")))

(use-package which-key
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-idle-secondary-delay 0.25)
  (which-key-mode))


(use-package dired
  :straight nil
  :general
  (general-define-key
   :states 'normal
    "U" '(dired-jump :which-key "dired"))
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
    "U" '(dired-up-directory :which-key "go to parent directory")))

(use-package dired-sidebar
  :general
  (neh/leader-keys
    "ot" 'dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-theme 'nerd)
  (add-hook 'dired-load-hook
            (function (lambda () (load "dired-x")))))

(use-package dired-k
  :after dired
  :config
  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert))


(use-package flx)

(use-package avy
  :general
  (general-define-key
   :states '(normal visual)
   :prefix "j"
    "c" '(avy-goto-char-timer :which-key "char")
    "h" '(avy-org-goto-heading-timer :which-key "org heading")
    "l" '(avy-goto-line :which-key "line"))
  :config
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

(use-package amx
  :after ivy
  :config
  (amx-mode))

(use-package ivy
  :init
  (defun reloading (cmd)
    (lambda (x)
      (funcall cmd x)
      (ivy--reset-state ivy-last)))
  (defun given-file (cmd prompt) ; needs lexical-binding
    (lambda (source)
      (let ((target
             (let ((enable-recursive-minibuffers t))
               (read-file-name
                (format "%s %s to:" prompt source)))))
        (funcall cmd source target 1))))
  (defun confirm-delete-file (x)
    (dired-delete-file x 'confirm-each-subdirectory))
  (defun neh-open-file-in-vsplit (f)
    (evil-window-vsplit 80 f)
    (balance-windows))

  :general
  (general-define-key
   :keymaps 'ivy-minibuffer-map
    "<escape>" 'keyboard-escape-quit
    "C-t" 'ivy-next-line
    "C-n" 'ivy-previous-line
    "C-M-t" 'ivy-next-line-and-call
    "C-M-n" 'ivy-previous-line-and-call
    "C-b" 'ivy-scroll-down-command
    "C-f" 'ivy-scroll-up-command
    "C-d" 'ivy-call)

  (general-define-key
   :keymaps 'counsel-find-file-map
    "TAB" 'ivy-alt-done
    "C-s" 'neh-open-file-in-vsplit)

  (general-define-key
   :keymaps 'ivy-occur-mode-map
    "t" 'ivy-occur-next-line
    "n" 'ivy-occur-previous-line
    "RET" 'ivy-occur-press
    "a" 'ivy-occur-read-action
    "c" 'ivy-occur-toggle-calling
    "C-f" 'evil-scroll-page-down
    "C-b" 'evil-scroll-page-up)

  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-extra-directories nil)
  (setq ivy-height 10)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :after ivy
  :config
  (setq counsel-ag-base-command "ag --nocolor --nogroup --ignore-case %s")
  (setq counsel-grep-base-command "grep -inE '%s' %s")
  (counsel-mode 1)
  ;; These don't work on a fresh load, but seem to start working at some
  ;; point. Strange.
  (ivy-add-actions
   'counsel-find-file
   `(("c" ,(given-file #'copy-file "Copy") "copy")
     ("d" ,(reloading #'confirm-delete-file) "delete")
     ("s" neh-open-file-in-vsplit "vsplit")
     ("m" ,(reloading (given-file #'rename-file "Move")) "move")))
  (ivy-add-actions
   'counsel-projectile-find-file
   `(("c" ,(given-file #'copy-file "Copy") "copy")
     ("d" ,(reloading #'confirm-delete-file) "delete")
     ("m" ,(reloading (given-file #'rename-file "Move")) "move")
     ("b" counsel-find-file-cd-bookmark-action "cd bookmark"))))

(use-package counsel-projectile)

(use-package historian
  :config
  (historian-mode))

(use-package ivy-historian
  :after ivy
  :config
  (setq ivy-historian-freq-boost-factor 300)
  (setq ivy-historian-recent-boost 300)
  (setq ivy-historian-recent-decrement 50)

  (ivy-historian-mode))

(use-package hydra)
(use-package ivy-hydra
  :config
  (defhydra hydra-ivy (:hint nil
                       :color pink)
    "
    ^ ^ ^ ^ ^ ^ | ^Call^  | ^Cancel^ | ^Options^ | Action _r_/_c_/_a_: %-14s(ivy-action-name)
    ^-^-^-^-^-^-+----^-^--+-^-^------+-^-^-------+-^^^^^^^^^^^^^^^^^^^^^^^^^^^^^---------------------------
    ^ ^ _n_ ^ ^ | occ_u_r | _i_nsert | _C_: calling %-5s(if ivy-calling \"on\" \"off\") Case-_F_old: %-10`ivy-case-fold-search
    _h_ ^+^ _s_ | _d_one  | ^ ^      | _m_: matcher %-5s(ivy--matcher-desc)^^^^^^^^^^^^ _T_runcate: %-11`truncate-lines
    ^ ^ _t_ ^ ^ | _g_o    | ^ ^      | _<_/_>_: shrink/grow^^^^^^^^^^^^^^^^^^^^^^^^^^^^ _D_efinition of this menu
    "
    ;; arrows
    ("h" ivy-beginning-of-buffer)
    ("t" ivy-next-line)
    ("n" ivy-previous-line)
    ("s" ivy-end-of-buffer)
    ;; actions
    ("<ESC>" keyboard-escape-quit :exit t)
    ("C-g" keyboard-escape-quit :exit t)
    ("q" keyboard-escape-quit :exit t)
    ("i" nil)
    ("C-o" nil)
    ;; ("f" ivy-alt-done :exit nil)
    ("C-j" ivy-alt-done :exit nil)
    ("d" ivy-done :exit t)
    ("g" ivy-call)
    ("S" (ivy-exit-with-action
          (lambda (f) (evil-window-vsplit 80 f)
            (balance-windows)))
     :exit t)
    ("C-m" ivy-done :exit t)
    ("C" ivy-toggle-calling)
    ("m" ivy-toggle-fuzzy)
    (">" ivy-minibuffer-grow)
    ("<" ivy-minibuffer-shrink)
    ("r" ivy-prev-action)
    ("c" ivy-next-action)
    ("a" ivy-read-action)
    ("T" (setq truncate-lines (not truncate-lines)))
    ("F" ivy-toggle-case-fold)
    ("u" ivy-occur :exit t)
    ("D" (ivy-exit-with-action
          (lambda (_) (find-function 'hydra-ivy/body)))
     :exit t)))

(use-package ace-window
  ;; :general
  ;; (general-define-key
  ;;  :states '(normal motion emacs)
  ;;  :prefix "SPC"
  ;;  "w" 'ace-window
  ;;  )
  :config
  ;; need some window moving keys here
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)
        aw-dispatch-always t
        aw-dispatch-alist
        '((?d aw-delete-window " Ace - Delete Window")
          (?m aw-swap-window " Ace - Swap Window")
          (?M aw-move-window " Ace - Move Window")
          (?w aw-flip-window)
          (?r balance-windows)
          (?c aw-split-window-fair " Ace - Split Fair Window")
          (?v aw-split-window-vert " Ace - Split Vert Window")
          (?b aw-split-window-horz " Ace - Split Horz Window")
          (?x delete-other-windows " Ace - Maximize Window"))
        ))


(defhydra hydra-windows (:hint nil)
  "
   Go: _h_ _t_ _n_ _s_
 Move: _H_ _T_ _N_ _S_

 Only: _o_
Close: _c_

 Exit: _q_
"
  ("h" evil-window-left)
  ("n" evil-window-up)
  ("t" evil-window-down)
  ("s" evil-window-right)

  ("H" evil-window-move-far-left)
  ("T" evil-window-move-very-bottom)
  ("N" evil-window-move-very-top)
  ("S" evil-window-move-far-right)

  ("o" delete-other-windows)
  ("c" evil-window-delete)

  ("q" nil))


(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode))

(use-package persp-mode
  :config
  (setq persp-auto-resume-time 1.0)
  (setq persp-add-buffer-on-after-change-major-mode t)
  (add-hook 'persp-common-buffer-filter-functions
            #'(lambda (b) (string-prefix-p "*" (buffer-name b))))

  ;; Ivy customization
  (with-eval-after-load "persp-mode"
    (with-eval-after-load "ivy"
      (add-hook 'ivy-ignore-buffers
                #'(lambda (b)
                    (when persp-mode
                      (let ((persp (get-current-persp)))
                        (if persp
                            (not (persp-contain-buffer-p b persp))
                          nil)))))

      (setq ivy-sort-functions-alist
            (append ivy-sort-functions-alist
                    '((persp-kill-buffer   . nil)
                      (persp-remove-buffer . nil)
                      (persp-add-buffer    . nil)
                      (persp-switch        . nil)
                      (persp-window-switch . nil)
                      (persp-frame-switch . nil))))))

  ;; Persp MRU
  (with-eval-after-load "persp-mode"
    (add-hook 'persp-before-switch-functions
              #'(lambda (new-persp-name w-or-f)
                  (let ((cur-persp-name (safe-persp-name (get-current-persp))))
                    (when (member cur-persp-name persp-names-cache)
                      (setq persp-names-cache
                            (cons cur-persp-name
                                  (delete cur-persp-name persp-names-cache)))))))

    (add-hook 'persp-renamed-functions
              #'(lambda (persp old-name new-name)
                  (setq persp-names-cache
                        (cons new-name (delete old-name persp-names-cache)))))

    (add-hook 'persp-before-kill-functions
              #'(lambda (persp)
                  (setq persp-names-cache
                        (delete (safe-persp-name persp) persp-names-cache))))

    (add-hook 'persp-created-functions
              #'(lambda (persp phash)
                  (when (and (eq phash *persp-hash*)
                             (not (member (safe-persp-name persp)
                                          persp-names-cache)))
                    (setq persp-names-cache
                          (cons (safe-persp-name persp) persp-names-cache))))))
  
  (persp-mode 1))

(use-package persp-mode-projectile-bridge
  :config
  (with-eval-after-load "persp-mode-projectile-bridge-autoloads"
    (add-hook 'persp-mode-projectile-bridge-mode-hook
              #'(lambda ()
                  (if persp-mode-projectile-bridge-mode
                      (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                    (persp-mode-projectile-bridge-kill-perspectives))))
    (add-hook 'after-init-hook
              #'(lambda ()
                  (persp-mode-projectile-bridge-mode 1))
              t)))

(use-package flycheck
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package company
  :config
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-t") #'company-select-next)
    (define-key company-active-map (kbd "C-n") #'company-select-previous))

  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-statistics
  :config
  (company-statistics-mode))

(use-package company-terraform
  :config
  (company-terraform-init))

(fringe-mode 8)

;; (use-package diff-hl
;;   :config
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;;   (diff-hl-flydiff-mode)
;;   (global-diff-hl-mode))

(use-package git-gutter+
  :general
  (general-define-key
   :states '(normal visual)
    "gp" 'git-gutter+-previous-hunk
    "gn" 'git-gutter+-next-hunk
    "gs" 'git-gutter+-show-hunk
    "gS" 'git-gutter+-stage-hunks
    "gU" 'git-gutter+-revert-hunks)

  :init
  (global-git-gutter+-mode)

  :config
  (setq git-gutter+-added-sign " ")
  (setq git-gutter+-deleted-sign " ")
  (setq git-gutter+-modified-sign " ")
  (set-face-background 'git-gutter+-modified "gold")
  (set-face-foreground 'git-gutter+-added "forestgreen")
  (set-face-foreground 'git-gutter+-deleted "red4"))

;; disable annoying multi-line docs in echo area
;; (global-eldoc-mode -1)
(setq eldoc-echo-area-use-multiline-p nil)

(straight-use-package 'magit)
(use-package magit
  :straight nil
  :general
  (general-define-key
   :keymaps 'magit-mode-map
    "C-b" 'evil-scroll-page-up
    "C-f" 'evil-scroll-page-down
    "C-t" 'magit-section-forward
    "C-n" 'magit-section-backward
    "M-t" 'magit-section-forward-sibling
    "M-n" 'magit-section-backward-sibling
    "r" 'magit-refresh
    "R" 'magit-rebase-popup
    "g" 'magit-tag-popup
    "t" 'evil-next-visual-line
    "n" 'evil-previous-visual-line)

  (general-define-key
   :keymaps 'magit-diff-mode-map
    "/" 'evil-search-forward
    "l" 'evil-search-next
    "L" 'evil-search-previous)

  :config
  ;; There doesn't seem to be a "nice" way to adjust magit popups, so I stole
  ;; this method from evil-magit
  ;; refresh
  (magit-change-popup-key 'magit-dispatch-popup
                          :action (string-to-char "g") (string-to-char "r"))
  ;; rebase popup
  (magit-change-popup-key 'magit-dispatch-popup
                          :action (string-to-char "r") (string-to-char "R"))
  ;; tag popup
  (magit-change-popup-key 'magit-dispatch-popup
                          :action (string-to-char "t") (string-to-char "g"))

  (setq magit-completing-read-function 'ivy-completing-read)
  (add-hook 'git-commit-mode-hook 'evil-insert-state))


;; Installing org-plus-contrib as a lazy workaround for the built-in older org-mode
;; https://github.com/jwiegley/use-package/issues/319
(straight-use-package 'org-plus-contrib)
(use-package org
  :straight nil
  :general
  (:keymaps 'org-mode-map
   :states '(normal emacs)
   :prefix  "g"
   "x" 'org-open-at-point
   )
  ;; :general
  ;; (:keymaps 'org-mode-map
  ;;  "H" 'org-shiftleft
  ;;  "T" 'org-shiftdown
  ;;  "N" 'org-shiftup
  ;;  "S" 'org-shiftright
  ;;  "C-h" 'org-shiftleft
  ;;  "C-t" 'org-shiftdown
  ;;  "C-n" 'org-shiftup
  ;;  "C-s" 'org-shiftright
  ;;  )
  :init
  (setq org-ellipsis " …")
  (setq org-hide-emphasis-markers t)
  (setq org-src-fontify-natively t)
  (setq org-todo-keywords
        '((sequence "TODO" "INPROGRESS" "WAITING" "|" "DONE" "CANCELED")))

  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'visual-fill-column-mode))

(use-package evil-org
  :after evil
  :config
  (setq evil-org-movement-bindings '((up . "n")
                                     (down . "t")
                                     (left . "h")
                                     (right . "s")))

  (add-to-list 'evil-org-key-theme 'shift)

  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))


(defun neh-org-jira-hook ()
  "Personal settings for org-jira."
  (setq org-todo-keywords
        '((sequence "TO-DO" "IN-PROGRESS" "QA" "CODE-REVIEW" "|" "DONE" "WONTFIX"))))

(use-package org-jira
  :init
  (add-hook 'org-jira-mode-hook 'neh-org-jira-hook)
  (setq jiralib-host "fresh-grade.atlassian.net")
  (setq jiralib-user-login-name "nathan.howell")
  (setq org-jira-use-status-as-todo t)
  (setq jiralib-url "https://fresh-grade.atlassian.net"))

(use-package deft
  :init
  (setq deft-directory "~/notes")
  (setq deft-extensions '("txt" "org" ""))
  (setq deft-recursive t))

;; (use-package focus)
;; (use-package darkroom
;;   :init
;;   (setq darkroom-text-scale-increase 0)
;;   :general
;;   (general-define-key
;;    :states '(normal visual insert emacs)
;;    :prefix "SPC"
;;    :non-normal-prefix "M-SPC"
;;    "vd" '(darkroom-tentative-mode :which-key "darkroom")
;;    )
;;   )

;;(use-package adaptive-wrap)

(use-package frames-only-mode)

(use-package aggressive-indent
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package direnv
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '(".*\\(host\\|group\\)_vars.*" . yaml-mode)))

(use-package ansible
  :init
  (setq ansible::vault-password-file "~/freshgrade/vaultpass"))

(use-package ansible-doc
  :general
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
    "hA" '(:ignore t :which-key "ansible")
    "hAA" '(ansible-doc :which-key "ansible docs")
    "hAR" '((lambda ()
              (interactive)
              (setq ansible-doc--modules nil)
              (ansible-doc))
            :which-key "ansible docs refresh")
    ))

(use-package spaceline
  :config
  (setq-default powerline-default-separator 'slant)
  (setq spaceline-separator-dir-left '(left . left))
  (setq spaceline-separator-dir-right '(right . right)))
(use-package spaceline-config
  :straight nil
  :config
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

  (spaceline-define-segment neh/buffer-status
    (cond (buffer-read-only (propertize ""))
          ((buffer-modified-p) (propertize ""))
          (t "")))

  (spaceline-define-segment neh/hud
    "A HUD that shows which part of the buffer is currently visible."
    (powerline-hud highlight-face default-face)
    :tight t)
  
  ;; fancy git icon
  (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((gitlogo (replace-regexp-in-string "^ Git." "  " vc-mode)))
        (setq vc-mode gitlogo))))

  (spaceline-install
    'main
    '((evil-state :face highlight-face)
      (persp-name)
      (buffer-id)
      (neh/buffer-status :face highlight-face))
    '((version-control :when active)
      (major-mode)
      ((line column buffer-position) :priority 0)
      (neh/hud :priority 0)))
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

;; ((main
;;   ((((((persp-name :fallback workspace-number)
;;        window-number)
;;       :separator "|")
;;      buffer-modified buffer-size)
;;     :face highlight-face :priority 0)
;;    (anzu :priority 4)
;;    auto-compile
;;    ((buffer-id remote-host)
;;     :priority 5)
;;    major-mode
;;    (process :when active)
;;    ((flycheck-error flycheck-warning flycheck-info)
;;     :when active :priority 3)
;;    (minor-modes :when active)
;;    (mu4e-alert-segment :when active)
;;    (erc-track :when active)
;;    (version-control :when active :priority 7)
;;    (org-pomodoro :when active)
;;    (org-clock :when active)
;;    nyan-cat)
;;   which-function
;;   (python-pyvenv :fallback python-pyenv)
;;   purpose
;;   (battery :when active)
;;   (selection-info :priority 2)
;;   input-method
;;   ((buffer-encoding-abbrev point-position line-column)
;;    :separator " | " :priority 3)
;;   (global :when active)
;;   (buffer-position :priority 0)
;;   (hud :priority 0)))

;; (defvar mode-line-directory
;;   '(:propertize
;;     (:eval (if (buffer-file-name) (concat " " (shorten-directory default-directory 20)) " "))
;;                 face my-pl-segment1-active)
;;   "Formats the current directory.")
;; (put 'mode-line-directory 'risky-local-variable t)

(use-package expand-region
  :general
  (general-define-key
   :states 'visual
    "v" 'er/expand-region))

(use-package markdown-mode)

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))

(use-package hcl-mode)

(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package groovy-mode
  :config
  (add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . groovy-mode)))

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package docker
  :config
  (setenv "DOCKER_HOST" "tcp://127.0.0.1:2375"))


(setq keymaps-with-jk-keybindings '(dired-mode-map))
(dolist (keymap keymaps-with-jk-keybindings)
  (general-translate-key 'normal keymap
    "t" "j"
    "n" "k"))


(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))


;; from https://www.reddit.com/r/emacs/comments/5rnpsm/nice_hydra_to_set_frame_transparency/
(defun my--set-transparency (inc)
  "Increase or decrease the selected frame transparency"
  (let* ((alpha (frame-parameter (selected-frame) 'alpha))
         (next-alpha (cond ((not alpha) 100)
                           ((> (- alpha inc) 100) 100)
                           ((< (- alpha inc) 0) 0)
                           (t (- alpha inc)))))
    (set-frame-parameter (selected-frame) 'alpha next-alpha)))

(defhydra hydra-transparency (:columns 2)
  "
ALPHA : [ %(frame-parameter nil 'alpha) ]
"
  ("n" (lambda () (interactive) (my--set-transparency +1)) "+ more")
  ("t" (lambda () (interactive) (my--set-transparency -1)) "- less")
  ("N" (lambda () (interactive) (my--set-transparency +10)) "++ more")
  ("T" (lambda () (interactive) (my--set-transparency -10)) "-- less")
  ("=" (lambda (value) (interactive "nTransparency Value 0 - 100 opaque:")
         (set-frame-parameter (selected-frame) 'alpha value)) "Set to ?" :color blue))

;; these two font functions make emacs crashy
(defun font-is-mono-p (font-family)
  ;; with-selected-window
  (let ((wind (selected-window))
        m-width l-width)
    (with-current-buffer "*Monospace Fonts*"
      (set-window-buffer (selected-window) (current-buffer))
      (text-scale-set 4)
      (insert (propertize "l l l l l" 'face `((:family ,font-family))))
      (goto-char (line-end-position))
      (setq l-width (car (posn-x-y (posn-at-point))))
      (newline)
      (forward-line)
      (insert (propertize "m m m m m" 'face `((:family ,font-family) italic)))
      (goto-char (line-end-position))
      (setq m-width (car (posn-x-y (posn-at-point))))
      (eq l-width m-width))))

(defun compare-monospace-fonts ()
  "Display a list of all monospace font faces."
  (interactive)
  (pop-to-buffer "*Monospace Fonts*")

  (erase-buffer)
  (dolist (font-family (font-family-list))
    (when (font-is-mono-p font-family)
      (let ((str font-family))
        (newline)
        (insert
         (propertize (concat "The quick brown fox jumps over the lazy dog 1 l; 0 O o ("
                             font-family ")\n") 'face `((:family ,font-family)))
         (propertize (concat "The quick brown fox jumps over the lazy dog 1 l; 0 O o ("
                             font-family ")\n") 'face `((:family ,font-family) italic)))))))

;; https://github.com/noctuid/general.el#use-package-keyword
;; https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
(defun Fuco1/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)))

(provide 'init)
;;; init.el ends here
