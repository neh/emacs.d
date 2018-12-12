;; -*- lexical-binding: t -*-

;; This file is pretty much entirely taken from http://www.holgerschurig.de/en/emacs-efficiently-untangling-elisp/

;; This is GPLv2. If you still don't know the details, read
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html

(defun my-tangle-section-canceled ()
  "Return t if the current section header was CANCELED, else nil."
  (save-excursion
    (if (re-search-backward "^\\*+\\s-+\\(.*?\\)?\\s-*$" nil t)
        (string-prefix-p "CANCELED" (match-string 1))
      nil)))


;; This uses partially derived code from ob-core.el. So this snippet
;; is GPLv3 or later. If you still don't know the details, read
;; http://www.gnu.org/licenses/

(defun my-tangle-config-org (orgfile elfile)
  "This function will write all source blocks from =config.org= into =config.el= that are ...

    - not marked as :tangle no
    - have a source-code of =emacs-lisp=
    - doesn't have the todo-marker CANCELED"

  (let* ((body-list ())
         (gc-cons-threshold most-positive-fixnum)
         (org-babel-src-block-regexp (concat
                                      ;; (1) indentation                 (2) lang
                                      "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
                                      ;; (3) switches
                                      "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
                                      ;; (4) header arguments
                                      "\\([^\n]*\\)\n"
                                      ;; (5) body
                                      "\\([^\000]*?\n\\)??[ \t]*#\\+end_src")))
    (with-temp-buffer
      (insert-file-contents orgfile)
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (let ((lang (match-string 2))
              (args (match-string 4))
              (body (match-string 5))
              (canc (my-tangle-section-canceled)))
          (when (and (string= lang "emacs-lisp")
                     (not (string-match-p ":tangle\\s-+no" args))
                     (not canc))
            (add-to-list 'body-list body)))))
    (with-temp-file elfile
      (apply 'insert (reverse body-list)))
    (message "Wrote %s ..." elfile)))


;; This is GPLv2. If you still don't know the details, read
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html

(let ((orgfile (concat user-emacs-directory "config.org"))
      (elfile (concat user-emacs-directory "config.el")))
  (when (or (not (file-exists-p elfile))
            (file-newer-than-file-p orgfile elfile))
    (my-tangle-config-org orgfile elfile))
  (load-file elfile))
