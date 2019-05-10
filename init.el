;; -*- lexical-binding: t -*-

(let ((file-name-handler-alist nil))
  (setq gc-cons-threshold most-positive-fixnum)
  (setq straight-check-for-modifications '(check-on-save))
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
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
  (setq use-package-verbose nil
        ;; use-package-always-defer t
        )
  (use-package org
    :demand t)
  (let ((orgfile (concat user-emacs-directory "config.org"))
        (elfile (concat user-emacs-directory "config.el")))
    (when (or (not (file-exists-p elfile))
              (file-newer-than-file-p orgfile elfile))
      (org-babel-tangle-file orgfile elfile))
    (load-file elfile))
  (setq gc-cons-threshold (* 250 1000 1000)
        gc-cons-percentage 0.5)
  
  (defvar neh/gc-idle-timer nil)
  (unless (timerp neh/gc-idle-timer)
    (setq neh/gc-idle-timer (run-with-idle-timer 5 t #'garbage-collect))))
