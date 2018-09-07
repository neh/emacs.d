;;; init.el --- Initialize emacs
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold 800000)))

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
      (myfont "Iosevka"))
  (if (> mydpi 160)
      (set-frame-font (format "%s-10" myfont))
    (set-frame-font (format "%s-10" myfont))))

(setq confirm-kill-emacs 'y-or-n-p)
(fset 'yes-or-no-p #'y-or-n-p)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 1 ;; keyboard scroll one line at a time
      scroll-margin 5
      hscroll-step 5
      hscroll-margin 5
      scroll-preserve-screen-position t
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-conservatively 101) ;; Don't jump around when scrolling

(setq-default auto-hscroll-mode 'current-line)

(setq coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(setq-default indent-tabs-mode nil)
;; (setq-default show-trailing-whitespace t)
(setq sentence-end-double-space nil)
(setq ring-bell-function 'ignore)
(setq enable-recursive-minibuffers t)
(save-place-mode 1)

(setq frame-title-format "%b" ;; focused window title format
      icon-title-format "%b") ;; unfocused window title format

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'no-error)

;; Allow the confusing narrowings
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; from http://www.jethrokuan.com/init.html
(setq make-backup-files nil
      delete-old-versions t
      backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
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

;; set man page width, also put in olivetti-mode set to same width?
(setenv "MANWIDTH" "100")

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; (use-package gruvbox-theme)
;; (load-theme 'gruvbox t)
;; (use-package plan9-theme)
;; (load-theme 'plan9 t)
;; (use-package darktooth-theme)
;; (load-theme 'darktooth t)
;; (use-package ample-theme)
;; (load-theme 'ample-light t)
;; (use-package autumn-light-theme)
;; (load-theme 'autumn-light t)
;; (use-package tao-theme)
;; (load-theme 'tao-yang t)
(use-package brutalist-theme)
(load-theme 'brutalist t)

;; (use-package poet-theme)
;; (load-theme 'poet t)

(use-package olivetti)

;; (use-package eink-theme
;;   :config
;;   (load-theme 'eink t)
;;   (custom-theme-set-faces
;;    'eink
;;    `(mode-line ((t (:height 1.0))))
;;    `(mode-line-inactive ((t (:height 1.0))))
;;    `(modeline ((t (:height 1.0))))))

;; set comments to always be italic (must be after theming)
(set-face-italic 'font-lock-comment-face t)
(set-face-attribute 'mode-line nil :height 1.0)
(set-face-attribute 'mode-line-inactive nil :height 1.0)

(use-package undo-tree
  :config
  (global-undo-tree-mode t))

(use-package paren
  :init
  (setq show-paren-delay 0
        show-paren-style 'parenthesis)
  :config
  (show-paren-mode 1))

;; A fun to call from programming mode hooks to deal with auto-fill if
;; the below config doesn't work well.
(defun comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode nil))

(setq-default fill-column 110)
(setq comment-auto-fill-only-comments t)
;; (setq-default auto-fill-function 'do-auto-fill)

;; uses the fill-column setting for visual-line-mode
(use-package visual-fill-column)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(use-package whitespace
  :init
  (setq whitespace-line-column 80
        whitespace-style '(face trailing tabs lines-tail))
  :config
  (whitespace-mode))

(setq uniquify-buffer-name-style 'forward)


(defun my-prog-mode-hook ()
  (setq display-line-numbers 'relative))
(add-hook 'prog-mode-hook #'my-prog-mode-hook)
(add-hook 'yaml-mode-hook #'my-prog-mode-hook)


(use-package evil
  :init
  (setq evil-want-integration nil
        evil-move-cursor-back t
        evil-vsplit-window-right t)
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

  (evil-define-operator evil-narrow-indirect (beg end type)
    "Indirectly narrow the region from BEG to END."
    (interactive "<R>")
    (evil-normal-state)
    (narrow-to-region-indirect beg end))

  (use-package evil-surround
    :config
    (global-evil-surround-mode)))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-indent-plus
  :config
  (evil-indent-plus-default-bindings))

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

  (general-add-advice (list #'evil-search-previous
                            #'evil-search-next)
                      :after #'evil-scroll-line-to-center)

  (general-define-key
   "C-M-t" 'scroll-other-window
   "C-M-n" 'scroll-other-window-down
   )

  (general-define-key
   :states '(normal visual)
   "h" 'evil-backward-char
   "t" 'evil-next-visual-line
   "n" 'evil-previous-visual-line
   "s" 'evil-forward-char

   "l" 'evil-search-next
   "L" 'evil-search-previous
   "S" 'evil-window-bottom

   "N" 'evil-narrow-indirect)

  (neh/leader-keys
    "<SPC>" '(save-buffer :which-key "save")

    "b" '(:ignore t :which-key "buffer")
    "bd" '(evil-delete-buffer :which-key "delete buffer")
    "bg" '(hydra-git-gutter/body :which-key "browse git hunks")

    "cc" '(comment-or-uncomment-region-or-line :which-key "toggle comment")

    "f" '(:ignore t :which-key "formatting")
    "fa" '(auto-fill-mode :which-key "auto fill")
    "fc" '(visual-fill-column-mode :which-key "visual fill column")
    "fi" '(indent-region :which-key "indent region")
    "fp" '(fill-paragraph :which-key "paragraph")
    "fr" '(fill-region :which-key "fill region")
    "ft" '(toggle-truncate-lines :which-key "truncate lines")
    "fw" '(whitespace-mode :which-key "show whitespace")
    "fv" '(visual-line-mode :which-key "visual line mode")

    "g" '(:ignore t :which-key "git")
    "gc" '(magit-commit :which-key "commit")
    "gd" '(magit-diff-popup :which-key "diff")
    "gf" '(magit-stage-file :which-key "stage file")
    "gh" '(git-gutter:stage-hunk :which-key "stage hunk")
    "gl" '(magit-log-popup :which-key "log")
    "gm" '(magit-dispatch-popup :which-key "menu")
    "gP" '(magit-push-popup :which-key "push")
    "gs" '(magit-status :which-key "status")

    "h" '(help-command :which-key "help")
    "ha" 'helm-apropos
    "hf" '(counsel-describe-function :which-key "describe function")
    "hv" '(counsel-describe-variable :which-key "describe variable")

    "hl" '(highlight-lines-matching-regexp :which-key "highlight line")
    "hr" '(highlight-regexp :which-key "highlight regexp")
    "hu" '(unhighlight-regexp :which-key "unhighlight regexp")

    "i" '(:ignore t :which-key "insert")
    "ip" '(clipboard-yank :which-key "paste from clipboard")

    "nb" '(org-narrow-to-block :which-key "narrow to block")
    "nd" '(narrow-to-defun :which-key "narrow to defun")
    "ne" '(org-narrow-to-element :which-key "narrow to element")
    "ns" '(org-narrow-to-subtree :which-key "narrow to subtree")
    "np" '(narrow-to-page :which-key "narrow to page")
    "nr" '(narrow-to-region :which-key "narrow to region")
    "nw" '(widen :which-key "widen")

    "o" '(:ignore t :which-key "open")
    "oa" '(counsel-linux-app :which-key "app")
    "oe" '(mode-line-other-buffer :which-key "previous buffer")
    "of" '(counsel-find-file :which-key "open file")
    "og" '(counsel-git :which-key "open git file")
    "oh" '(counsel-projectile-find-file :which-key "open file in project")
    "ol" '(org-open-at-point :which-key "follow link")
    "om" '(hydra-org/body :which-key "org tour")
    "oo" '(ivy-switch-buffer :which-key "switch buffer")
    ;; "oo" '(persp-switch-to-buffer :which-key "switch buffer")
    "op" '(counsel-projectile-switch-project :which-key "switch project")
    "ov" '(persp-switch :which-key "switch perspective")
    ;; "ov" '(persp-switch :which-key "switch perspective")

    "pr" '(package-refresh-contents :which-key "refresh package info")

    "Q" #'bury-buffer

    "s" '(:ignore t :which-key "search")
    "sa" '(swiper-all :which-key "search all buffers")
    "sf" '(counsel-ag :which-key "search files")
    "sg" '(counsel-git-grep :which-key "search files in git")
    "sh" '(counsel-grep-or-swiper :which-key "search buffer")
    "sp" '(counsel-projectile-rg :which-key "search project")

    "v" '(:ignore t :which-key "view")

    "x" '(:ignore t :which-key "execute")
    "xa" '(ivy-resume :which-key "ivy resume")
    "xb" '(eval-buffer :which-key "eval buffer")
    "xe" '(eval-expression :which-key "eval expression")
    "xr" '(eval-region :which-key "eval region")
    "xs" '(eval-last-sexp :which-key "eval sexp")
    "xx" '(counsel-M-x :which-key "M-x")

    "zt" '(hydra-zoom/body :which-key "zoom text")))

(use-package which-key
  :config
  (which-key-setup-side-window-bottom)
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
   "U" '(dired-jump :which-key "go to parent directory")))

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
   "j" '(avy-goto-char-2 :which-key "char(2)")
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
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-format-function #'ivy-format-function-arrow
        ivy-extra-directories nil
        ivy-height 15
        ivy-use-selectable-prompt t
        ivy-re-builders-alist
        '((t . ivy--regex-fuzzy))
        ivy-initial-inputs-alist nil))

;; (use-package ivy-posframe
;;   :after ivy
;;   :config
;;   ;; (setq ivy-display-function #'ivy-posframe-display)
;;   (setq ivy-display-function #'ivy-posframe-display-at-window-center
;;         ivy-posframe-parameters
;;         '((left-fringe . 10)
;;           (right-fringe . 10)))
;;   (ivy-posframe-enable))

;; (use-package smex)
(use-package prescient
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :after (prescient ivy)
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :after (prescient company)
  :config
  (company-prescient-mode))

(use-package counsel
  :after ivy
  :config
  (setq counsel-ag-base-command "ag --nocolor --nogroup --ignore-case %s"
        counsel-grep-base-command "grep -inE '%s' %s")
  (counsel-mode 1)

  ;; These don't work on a fresh load, but seem to start working at some
  ;; point. Strange.
  (ivy-add-actions
   'counsel-find-file
   `(("c" ,(given-file #'copy-file "Copy") "copy")
     ("d" ,(reloading #'confirm-delete-file) "delete")
     ("s" neh-open-file-in-vsplit "vsplit")
     ("m" ,(reloading (given-file #'rename-file "Move")) "move"))))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  ;; Set the default switch project action to find files so that paths are included in the search list
  (counsel-projectile-modify-action 'counsel-projectile-switch-project-action
                                    '((default counsel-projectile-switch-project-action-find-file)))

  (ivy-add-actions
   'counsel-projectile-find-file
   `(("c" ,(given-file #'copy-file "Copy") "copy")
     ("d" ,(reloading #'confirm-delete-file) "delete")
     ("m" ,(reloading (given-file #'rename-file "Move")) "move")
     ("b" counsel-find-file-cd-bookmark-action "cd bookmark")))
  )

(use-package historian
  :config
  (historian-mode))

(use-package ivy-historian
  :after ivy
  :config
  (setq ivy-historian-freq-boost-factor 100
        ivy-historian-recent-boost 100
        ivy-historian-recent-decrement 5)

  (ivy-historian-mode 1))

(use-package hydra
  :config
  (defhydra hydra-zoom ()
    "zoom"
    ("+" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("0" (text-scale-adjust 0) "reset")
    ("q" nil "quit" :color blue))

  (defhydra hydra-git-gutter ()
    "Browse/stage/revert git hunks"
    ("p" (progn (git-gutter:previous-hunk 1)
                (evil-scroll-line-to-center (line-number-at-pos))) "previous hunk")
    ("n" (progn (git-gutter:next-hunk 1)
                (evil-scroll-line-to-center (line-number-at-pos))) "next hunk")
    ("s" git-gutter:stage-hunk "stage hunk")
    ("r" git-gutter:revert-hunk "revert hunk")
    ("q" nil "quit" :color blue))

  (defhydra hydra-org (:color red :columns 3)
    "Org Mode Movements"
    ("t" outline-next-visible-heading "next heading")
    ("n" outline-previous-visible-heading "prev heading")
    ("T" org-forward-heading-same-level "next heading at same level")
    ("N" org-backward-heading-same-level "prev heading at same level")
    ("H" outline-up-heading "up heading")
    ("g" org-goto "goto" :exit t)))

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

;; (use-package persp-mode
;;   :config
;;   (setq persp-auto-resume-time 1.0)
;;   (setq persp-add-buffer-on-after-change-major-mode t)
;;   (add-hook 'persp-common-buffer-filter-functions
;;             #'(lambda (b) (string-prefix-p "*" (buffer-name b))))

;;   ;; Ivy customization
;;   (with-eval-after-load "persp-mode"
;;     (with-eval-after-load "ivy"
;;       (add-hook 'ivy-ignore-buffers
;;                 #'(lambda (b)
;;                     (when persp-mode
;;                       (let ((persp (get-current-persp)))
;;                         (if persp
;;                             (not (persp-contain-buffer-p b persp))
;;                           nil)))))

;;       (setq ivy-sort-functions-alist
;;             (append ivy-sort-functions-alist
;;                     '((persp-kill-buffer   . nil)
;;                       (persp-remove-buffer . nil)
;;                       (persp-add-buffer    . nil)
;;                       (persp-switch        . nil)
;;                       (persp-window-switch . nil)
;;                       (persp-frame-switch . nil))))))

;;   ;; Persp MRU
;;   (with-eval-after-load "persp-mode"
;;     (add-hook 'persp-before-switch-functions
;;               #'(lambda (new-persp-name w-or-f)
;;                   (let ((cur-persp-name (safe-persp-name (get-current-persp))))
;;                     (when (member cur-persp-name persp-names-cache)
;;                       (setq persp-names-cache
;;                             (cons cur-persp-name
;;                                   (delete cur-persp-name persp-names-cache)))))))

;;     (add-hook 'persp-renamed-functions
;;               #'(lambda (persp old-name new-name)
;;                   (setq persp-names-cache
;;                         (cons new-name (delete old-name persp-names-cache)))))

;;     (add-hook 'persp-before-kill-functions
;;               #'(lambda (persp)
;;                   (setq persp-names-cache
;;                         (delete (safe-persp-name persp) persp-names-cache))))

;;     (add-hook 'persp-created-functions
;;               #'(lambda (persp phash)
;;                   (when (and (eq phash *persp-hash*)
;;                              (not (member (safe-persp-name persp)
;;                                           persp-names-cache)))
;;                     (setq persp-names-cache
;;                           (cons (safe-persp-name persp) persp-names-cache))))))

;;   (persp-mode 1))

;; (use-package persp-mode-projectile-bridge
;;   :config
;;   (with-eval-after-load "persp-mode-projectile-bridge-autoloads"
;;     (add-hook 'persp-mode-projectile-bridge-mode-hook
;;               #'(lambda ()
;;                   (if persp-mode-projectile-bridge-mode
;;                       (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
;;                     (persp-mode-projectile-bridge-kill-perspectives))))
;;     (add-hook 'after-init-hook
;;               #'(lambda ()
;;                   (persp-mode-projectile-bridge-mode 1))
;;               t)))

(use-package wgrep)
(use-package multiple-cursors)

(use-package elec-pair
  :ensure nil
  :commands electric-pair-mode
  :hook (prog-mode . electric-pair-mode))

(use-package executable
  :ensure nil
  :commands executable-make-buffer-file-executable-if-script-p
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package flycheck
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-posframe
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode))

(use-package company
  :bind (:map company-active-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("C-t" . #'company-select-next)
         ("C-n" . #'company-select-previous)
         ("TAB" . #'company-complete)
         ("<tab>" . #'company-complete)
         ("RET" . #'company-complete-selection))
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-statistics
  :init
  (add-hook 'company-mode-hook #'company-statistics-mode))

(use-package company-terraform
  :config
  (company-terraform-init))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(fringe-mode 8)

;; (use-package diff-hl
;;   :config
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;;   (diff-hl-flydiff-mode)
;;   (global-diff-hl-mode))

(use-package git-gutter
  :general
  (general-define-key
   :states '(normal visual)
   "gp" 'git-gutter:previous-hunk
   "gn" 'git-gutter:next-hunk
   "gs" 'git-gutter:popup-hunk
   "gS" 'git-gutter:stage-hunk
   "gU" 'git-gutter:revert-hunk)

  :init
  (setq git-gutter:disabled-modes '(org-mode))
  (global-git-gutter-mode +1)

  :config
  (setq git-gutter:added-sign "▊"
        git-gutter:deleted-sign "▊"
        git-gutter:modified-sign "▊"
        git-gutter:ask-p nil)
  (set-face-foreground 'git-gutter:modified "SteelBlue1")
  (set-face-foreground 'git-gutter:added "forestgreen")
  (set-face-foreground 'git-gutter:deleted "red4")
  )


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

(setq auto-revert-check-vc-info t
      vc-follow-symlinks t)


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
  (:keymaps 'org-mode-map
   :states '(normal emacs)
   "<RET>" 'org-tree-to-indirect-buffer
   "ze" 'outline-show-branches
   "C-M-t" 'scroll-other-window
   "C-M-n" 'scroll-other-window-down
   )
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
  (let* ((variable-tuple (cond ((x-list-fonts "DejaVu Serif") '(:font "DejaVu Serif"))
                               ((x-list-fonts "Noto Sans") '(:font "Noto Sans"))
                               ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
                               (nil (warn "Cannot find a Sans Serif Font. Install Source Sans Pro."))))
         (fixed-tuple (cond ((x-list-fonts "Iosevka") '(:font "Iosevka"))
                            (nil (warn "Cannot find a fixed width font."))))
         (base-font-color     (face-foreground 'default nil 'default))
         ;; (headline           `(:inherit default :weight bold :foreground ,base-font-color))
         ;; (done               `(:inherit default :weight normal :height 0.9 :foreground "#bdae93"))
         ;; (variable           `(:inherit default :foreground ,base-font-color)))
         (headline           `(:weight bold :foreground ,base-font-color))
         (done               `(:weight normal :foreground "#7c6f64"))
         (variable           `(:foreground ,base-font-color)))

    (custom-theme-set-faces 'user
                            `(fixed-pitch ((t (,@fixed-tuple))))
                            `(variable-pitch ((t (,@variable-tuple :font "DejaVu Sans"))))
                            `(org-ellipsis ((t (:underline nil :weight normal))))
                            `(org-indent ((t (:inherit org-hide :inherit (org-hide fixed-pitch)))))
                            `(org-code ((t (:inherit fixed-pitch))))
                            `(org-table ((t (:inherit fixed-pitch))))
                            ;; `(org-link ((t (:inherit default :foreground "#f4e8ba"))))
                            `(org-link ((t (:inherit default))))
                            ;; `(org-level-8 ((t (,@headline ,@variable-tuple :slant italic))))
                            ;; `(org-level-7 ((t (,@headline ,@variable-tuple :slant italic))))
                            ;; `(org-level-6 ((t (,@headline ,@variable-tuple :slant italic))))
                            ;; `(org-level-5 ((t (,@headline ,@variable-tuple :slant italic))))
                            ;; `(org-level-4 ((t (,@headline ,@variable-tuple :slant italic))))
                            ;; `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1 :slant italic))))
                            ;; `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.1 :slant italic))))
                            ;; `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.2 :slant italic))))
                            `(org-todo ((t (,@headline ,@variable-tuple))))
                            `(org-done ((t (,@done ,@variable-tuple :strike-through t))))
                            `(org-archived ((t (,@done ,@variable-tuple :strike-through t))))
                            `(org-headline-done ((t (,@done ,@variable-tuple))))
                            `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

  (defmacro my-org-in-calendar (command)
    (let ((name (intern (format "my-org-in-calendar-%s" command))))
      `(progn
         (defun ,name ()
           (interactive)
           (org-eval-in-calendar '(call-interactively #',command)))
         #',name)))

  (general-def org-read-date-minibuffer-local-map
    "M-h" (my-org-in-calendar calendar-backward-day)
    "M-s" (my-org-in-calendar calendar-forward-day)
    "M-n" (my-org-in-calendar calendar-backward-week)
    "M-t" (my-org-in-calendar calendar-forward-week)
    "M-H" (my-org-in-calendar calendar-backward-month)
    "M-S" (my-org-in-calendar calendar-forward-month)
    "M-N" (my-org-in-calendar calendar-backward-year)
    "M-T" (my-org-in-calendar calendar-forward-year))

  :config
  (setq org-todo-keywords
        '((sequence "TODO" "INPROGRESS" "WAITING" "|" "DONE" "CANCELED")))

  (setq org-startup-indented t
        org-ellipsis "  "
        org-src-fontify-natively t
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-cycle-separator-lines 2
        org-M-RET-may-split-line '((default . nil)))

  (set-face-attribute 'org-ellipsis '(:underline nil :weight normal))

  (add-hook 'org-mode-hook 'org-indent-mode)
  ;; (add-hook 'org-mode-hook 'visual-fill-column-mode)
  (add-hook 'org-mode-hook 'set-buffer-variable-pitch)
  ;; (add-hook 'org-mode-hook (lambda ()
  ;;                            (setq-local global-hl-line-mode
  ;;                                        nil)))

  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t))))

(use-package rainbow-mode)

(use-package hl-line+
  :config
  (setq hl-line-inhibit-highlighting-for-modes '(org-mode))
  (toggle-hl-line-when-idle -1))

(use-package centered-cursor-mode)

(defun set-buffer-variable-pitch ()
  (interactive)
  (variable-pitch-mode t)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch))

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


(use-package org-bullets
  :after org
  :config
  ;; (setq org-bullets-bullet-list '("•"))
  (setq org-bullets-bullet-list '(" "))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-jira
  :init
  (add-hook 'org-jira-mode-hook 'neh-org-jira-hook)
  (setq jiralib-host "fresh-grade.atlassian.net"
        jiralib-user-login-name "nathan.howell"
        org-jira-use-status-as-todo t
        jiralib-url "https://fresh-grade.atlassian.net"))

(use-package deft
  :init
  (setq deft-directory "~/notes"
        deft-extensions '("txt" "org" "")
        deft-recursive t))

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

(use-package adaptive-wrap)

(use-package frames-only-mode)

(use-package aggressive-indent
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package dtrt-indent
  :commands dtrt-indent-mode
  :config
  (dtrt-indent-mode 1))

(use-package direnv
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '(".*\\(host\\|group\\)_vars.*" . yaml-mode)))

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)))

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
  (setq-default powerline-default-separator 'wave)
  (setq spaceline-separator-dir-left '(left . left)
        spaceline-separator-dir-right '(right . right)))

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
   "v" 'er/expand-region
   "V" 'er/contract-region))

(use-package markdown-mode)

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode))

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

(use-package go-mode
  :mode "\\.go\\'")

(use-package go-eldoc
  :commands go-eldoc-setup
  :hook (go-mode . go-eldoc-setup))

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package elf-mode
  :commands elf-mode
  :magic ("ELF" . elf-mode))


(setq keymaps-with-jk-keybindings '(dired-mode-map))
(dolist (keymap keymaps-with-jk-keybindings)
  (general-translate-key 'normal keymap
    "t" "j"
    "n" "k"))


(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

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


(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))


(provide 'init)
;;; init.el ends here
