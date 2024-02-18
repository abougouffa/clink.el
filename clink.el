;;; clink.el --- Clink interface to Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Abdelhak BOUGOUFFA

;; Author: Abdelhak BOUGOUFFA (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Maintainer: Abdelhak BOUGOUFFA (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024
;; Version: 0.2
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/abougouffa/clink.el
;; Keywords: matching, files, completion, languages, c, tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'sqlite)
(require 'grep)
(require 'cl-extra)
(eval-when-compile (require 'savehist))

(defgroup clink nil
  "Clink integration."
  :link '(url-link "https://github.com/abougouffa/clink.el")
  :group 'clink)

;;; Customization variables

(defcustom clink-command "clink"
  "The Clink command."
  :type 'string
  :group 'clink)

(defcustom clink-number-of-jobs nil
  "Number of jobs (option: -j NUM), leave it nil to use the defaults."
  :type '(choice (const nil) natnum)
  :group 'clink)

(defcustom clink-parse-modes-alist '((asm . generic)
                                     (c . auto)
                                     (cxx . auto)
                                     (def . generic)
                                     (lex . auto)
                                     (python . generic)
                                     (tablegen . generic)
                                     (yacc . auto))
  "--parse-X=MODE."
  :type 'alist
  :group 'clink)

(defcustom clink-root-project-detection-files '(".tags/" ".repo/" ".clink.db")
  "A list of files/directories that indicates the root of a workspace."
  :type '(repeat string)
  :group 'clink)

(defcustom global-clink-modes '(c-mode c-ts-mode c++-mode c++-ts-mode)
  "A predicate list of major modes to use with `global-clink-mode'.

See the `:predicate' section of `define-globalized-minor-mode'."
  :group 'clink
  :type '(repeat symbol))

(defvar-local clink-project-root nil
  "Path to the directory containing Clink database (the `clink-database-filename').")

(defvar clink-database-filename ".clink.db"
  "The Clink database filename (\".clink.db\" by default).")

;;; Utils and internals

(defvar clink-prompt-symbol-history nil
  "The history of terms we searched for.

This is one common history for ALL search types.")

(defvar clink--databases-map (make-hash-table :test 'equal))

(defun clink--get-sqlite-database (dir)
  "Get the SQLite database associated with the project under DIR.

If the database has been created and opened, return the object,
if it is the first call, open it and return the object."
  (let* ((dir (expand-file-name dir))
         (db-file (expand-file-name clink-database-filename dir)))
    (if (file-exists-p db-file)
        (if-let ((db (gethash dir clink--databases-map)))
            db
          (let ((db (sqlite-open db-file)))
            (puthash dir db clink--databases-map)
            db))
      (error "Database not found %S" db-file))))

(defun clink--show-find-symbol-results (buff-name results template &optional root-dir)
  (with-current-buffer (get-buffer-create buff-name)
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (dolist (result results)
      (let* ((result-alist (cl-mapcar #'cons template result))
             (path (alist-get 'path result-alist))
             (line (alist-get 'line result-alist))
             (parent (alist-get 'parent result-alist))
             (name (alist-get 'name result-alist))
             (body (alist-get 'body result-alist)))
        (insert (format
                 "%s%s: %s%s%s\n"
                 (if root-dir (expand-file-name path root-dir) path)
                 (if line (format ":%d" line) "")
                 (if (and parent (not (string-empty-p parent))) (format " P: (%s) " parent) "")
                 (if (and name (not (string-empty-p name))) (format " N: (%s) " name) "")
                 (if (and body (not (string-empty-p body))) (format " B: %s" body) "")))))
    (grep-mode)
    (switch-to-buffer-other-window (current-buffer))))

(defun clink--prompt-for-symbol ()
  "Prompt for a symbol."
  (let ((sym (or (thing-at-point 'region)
                 (thing-at-point 'symbol)
                 (read-string "Symbol: " (car clink-prompt-symbol-history) clink-prompt-symbol-history))))
    (add-to-history 'clink-prompt-symbol-history sym)
    sym))

(defvar clink--queries-plist
  `(:symbol
    (:template (path line parent body)
     :query ,(concat
              "SELECT records.path, symbols.line, symbols.parent, content.body "
              "FROM symbols INNER JOIN records ON symbols.path = records.id "
              "LEFT JOIN content ON records.id = content.path AND "
              "symbols.line = content.line WHERE symbols.name = ? ORDER BY " ;; name
              "records.path, symbols.line, symbols.col;"))
    :definition
    (:template (path line body)
     :query ,(concat
              "SELECT records.path, symbols.line, content.body "
              "FROM symbols INNER JOIN records ON symbols.path = records.id "
              "LEFT JOIN content ON records.id = content.path AND "
              "symbols.line = content.line WHERE symbols.name = ? AND " ;; name
              "symbols.category = 0 ORDER BY "
              "records.path, symbols.line, symbols.col;"))
    :reference
    (:template (path line parent body)
     :query ,(concat
              "SELECT records.path, symbols.line, symbols.parent, content.body "
              "FROM symbols INNER JOIN records ON symbols.path = records.id "
              "LEFT JOIN content ON records.id = content.path AND "
              "symbols.line = content.line WHERE symbols.name = ? AND " ;; name
              "symbols.category = 3 ORDER BY "
              "records.path, symbols.line, symbols.col;"))
    :calls
    (:template (path line name body)
     :query ,(concat
              "SELECT records.path, symbols.line, symbols.name, content.body "
              "FROM symbols INNER JOIN records ON symbols.path = records.id "
              "LEFT JOIN content ON records.id = content.path AND "
              "symbols.line = content.line WHERE symbols.parent = ? AND " ;; caller
              "symbols.category = 1 ORDER BY "
              "records.path, symbols.line, symbols.col;"))
    :callers
    (:template (path line parent body)
     :query ,(concat
              "SELECT records.path, symbols.line, symbols.parent, content.body "
              "FROM symbols INNER JOIN records ON symbols.path = records.id "
              "LEFT JOIN content ON records.id = content.path AND "
              "symbols.line = content.line WHERE symbols.name = ? AND " ;; callee
              "symbols.category = 1 ORDER BY "
              "records.path, symbols.line, symbols.col;"))
    :file
    (:template (path)
     :query ,(concat
              "SELECT DISTINCT path FROM records WHERE path = ? OR " ;; filename
              "PATH LIKE ? ORDER BY path")))) ;; pattern

(defun clink--apply-partially-right (fun &rest args)
  "Like `apply-partially', but apply the ARGS to the right of FUN."
  (lambda (&rest args2) (apply fun (append args2 args))))

(defun clink--directory-root-containing-file (files &optional start-path)
  "Return the path containing a file from FILES starting from START-PATH."
  (let ((dir (or start-path (and buffer-file-name (file-name-directory buffer-file-name)) default-directory)))
    (catch 'root
      (while dir
        (when (cl-some #'file-exists-p (mapcar (clink--apply-partially-right #'expand-file-name dir) (ensure-list files)))
          (throw 'root dir))
        (setq dir (file-name-parent-directory dir))))))

(with-eval-after-load 'savehist
  (push 'clink-prompt-symbol-history savehist-additional-variables))

;;; Functions

(defun clink-find-project-root ()
  "Search recursively until we find one of `clink-root-project-detection-files'."
  (clink--directory-root-containing-file clink-root-project-detection-files))

;;; Commands

;;;###autoload
(defun clink-open-database-dir (&optional root-directory)
  "Find Clink database starting from ROOT-DIRECTORY."
  (interactive)
  (when-let ((dir (or (and clink-project-root (file-exists-p clink-project-root) (file-name-directory clink-project-root))
                      (clink--directory-root-containing-file clink-root-project-detection-files root-directory))))
    (dired dir)))

;;;###autoload
(defun clink-set-database-dir ()
  "Set the Clink database root directory."
  (interactive)
  (let* ((dir (or (and clink-project-root (file-exists-p (expand-file-name clink-database-filename clink-project-root)) clink-project-root)
                  (clink-find-project-root)))
         (set-dir (read-directory-name "Select database directory: " dir)))
    (setq-local clink-project-root (expand-file-name clink-database-filename set-dir))))

;;;###autoload(autoload 'clink-find-symbol "clink" "Find symbol" t)
;;;###autoload(autoload 'clink-find-file "clink" "Find file" t)
;;;###autoload(autoload 'clink-find-callers "clink" "Find callers" t)
;;;###autoload(autoload 'clink-find-calls "clink" "Find calls" t)
;;;###autoload(autoload 'clink-find-reference "clink" "Find reference" t)
;;;###autoload(autoload 'clink-find-definition "clink" "Find definition" t)

(dolist (cat '(symbol file callers calls reference definition))
  (let ((fn-name (intern (format "clink-find-%s" cat)))
        (fn-doc (format "Find %s." cat)))
    (defalias
      fn-name
      (lambda (arg)
        (interactive (list (clink--prompt-for-symbol)))
        (let ((root-dir (file-name-directory clink-project-root))
              (plist (plist-get clink--queries-plist (intern (format ":%s" cat)))))
          (clink--show-find-symbol-results
           (format "*clink-find-%s (%s)*" cat arg)
           (sqlite-select (clink--get-sqlite-database root-dir) (plist-get plist :query) (vector arg))
           (plist-get plist :template)
           root-dir)))
      fn-doc)))

(defun clink-turn-on ()
  "Setup Clink integration for the current buffer."
  (when-let* ((root (clink-find-project-root))
              (clink-db (expand-file-name clink-database-filename root))
              (db-exists (file-exists-p clink-db)))
    (setq-local clink-project-root clink-db)))

(defun clink-turn-off ()
  "Unset the current buffer integration with Clink."
  (when-let ((db (and clink-project-root (clink--get-sqlite-database clink-project-root))))
    (sqlite-close db)
    (remhash (expand-file-name clink-project-root) clink--databases-map)))

;;; Modes

;;;###autoload
(define-minor-mode clink-mode
  "Enable Clink integration for the current buffer."
  :lighter " Clink"
  :group 'clink
  (if clink-mode (clink-turn-on) (clink-turn-off)))

;;;###autoload
(define-globalized-minor-mode global-clink-mode clink-mode clink-turn-on
  :group 'clink
  :predicate)


(provide 'clink)

;;; clink.el ends here
