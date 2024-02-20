;;; clink.el --- Clink interface to Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Abdelhak BOUGOUFFA

;; Author: Abdelhak BOUGOUFFA (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Maintainer: Abdelhak BOUGOUFFA (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024
;; Version: 0.3
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

(defcustom clink-compile-commands-file nil
  "Path to the \"compile_commands.json\" file.

Set to nil to auto-detect the file or fallback and CScope-based search."
  :type '(choice (const nil) file)
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

;;;###autoload
(defcustom global-clink-modes '(c-mode c-ts-mode c++-mode c++-ts-mode)
  "A predicate list of major modes to use with `global-clink-mode'.

See the `:predicate' section of `define-globalized-minor-mode'."
  :group 'clink
  :type '(repeat symbol))

(defcustom clink-show-absolute-paths-in-results nil
  "Show absolute paths in results."
  :type 'boolean
  :group 'clink)

(defvar-local clink-project-root nil
  "Path to the directory containing Clink database (the `clink-database-filename').")

(defvar clink-database-filename ".clink.db"
  "The Clink database filename (\".clink.db\" by default).")

;;; Utils and internals

(defvar clink-prompt-symbol-history nil)

(defvar clink-prompt-file-history nil)

(defvar clink-prompt-includers-history nil)

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

(defun clink--results-show-in-buffer (buff-name results template root-dir)
  (with-current-buffer (get-buffer-create buff-name)
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (setq-local default-directory root-dir) ; for better navigation
    (dolist (result results)
      (let* ((result-alist (cl-mapcar #'cons template result))
             (path (alist-get 'path result-alist))
             (line (alist-get 'line result-alist))
             (parent (alist-get 'parent result-alist))
             (name (alist-get 'name result-alist))
             (body (alist-get 'body result-alist)))
        (insert (format
                 "%s%s: %s%s%s\n"
                 (if clink-show-absolute-paths-in-results (expand-file-name path root-dir) path)
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
                 (read-string "Symbol: " (car clink-prompt-symbol-history) 'clink-prompt-symbol-history))))
    (add-to-history 'clink-prompt-symbol-history (substring-no-properties sym))
    sym))

(defun clink--prompt-for-file ()
  "Prompt for a file."
  (let* ((init (or (thing-at-point 'region) (thing-at-point 'string)))
         (file (read-string
                "File: "
                (and init (string-trim init "[ \\t\\n\\r\"'<{(]+" "[ \\t\\n\\r\"'>})]+"))
                'clink-prompt-file-history)))
    (add-to-history 'clink-prompt-file-history file)
    file))

(defalias 'clink--prompt-for-definition 'clink--prompt-for-symbol)
(defalias 'clink--prompt-for-reference 'clink--prompt-for-symbol)
(defalias 'clink--prompt-for-calls 'clink--prompt-for-symbol)
(defalias 'clink--prompt-for-callers 'clink--prompt-for-symbol)
(defalias 'clink--prompt-for-includers 'clink--prompt-for-file)

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
              "symbols.category = 0 ORDER BY " ;; 0 -> definition
              "records.path, symbols.line, symbols.col;"))
    :reference
    (:template (path line parent body)
     :query ,(concat
              "SELECT records.path, symbols.line, symbols.parent, content.body "
              "FROM symbols INNER JOIN records ON symbols.path = records.id "
              "LEFT JOIN content ON records.id = content.path AND "
              "symbols.line = content.line WHERE symbols.name = ? AND " ;; name
              "symbols.category = 2 ORDER BY " ;; 2 -> reference
              "records.path, symbols.line, symbols.col;"))
    :includers
    (:template (path line parent body)
     :query ,(concat
              "SELECT records.path, symbols.line, symbols.parent, content.body "
              "FROM symbols INNER JOIN records ON symbols.path = records.id "
              "LEFT JOIN content ON records.id = content.path AND "
              "symbols.line = content.line WHERE symbols.name LIKE ? AND " ;; name
              "symbols.category = 3 ORDER BY "
              "records.path, symbols.line, symbols.col;"))
    :calls
    (:template (path line name body)
     :query ,(concat
              "SELECT records.path, symbols.line, symbols.name, content.body "
              "FROM symbols INNER JOIN records ON symbols.path = records.id "
              "LEFT JOIN content ON records.id = content.path AND "
              "symbols.line = content.line WHERE symbols.parent = ? AND " ;; caller
              "symbols.category = 1 ORDER BY " ;; 1 -> function call
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
  (or clink-project-root
      (clink--directory-root-containing-file clink-root-project-detection-files)))

;;; Commands

(defvar clink-root-directory-history nil)

;;;###autoload
(defun clink-build-database (&optional root-directory)
  "Build the Clink database under ROOT-DIRECTORY."
  (interactive "P")
  (when-let* ((default-directory
               (or root-directory
                   (unless current-prefix-arg (clink-find-project-root))
                   (read-directory-name "Root directory to index: " (car clink-root-directory-history))))
              (compile-commands-file (or clink-compile-commands-file (expand-file-name "compile_commands.json")))
              (proc-name (format "clink-build-database-%s" (substring (md5 (expand-file-name default-directory)) 0 8)))
              (buff-name (format "*clink-build-database (%s)*" (abbreviate-file-name default-directory)))
              (clink-cmd (concat
                          clink-command
                          (when clink-number-of-jobs
                            (format " --jobs=%d" clink-number-of-jobs))
                          " --build-only"
                          " --animation=off"
                          " --syntax-highlighting=lazy"
                          " --color=never"
                          " --database=" (shell-quote-argument (expand-file-name clink-database-filename))
                          (when (file-exists-p compile-commands-file)
                            (format " --compile-commands=%s" (shell-quote-argument (file-name-directory compile-commands-file)))))))
    (add-to-history 'clink-root-directory-history default-directory)
    (setq-local clink-project-root default-directory)
    (with-current-buffer (get-buffer-create buff-name)
      (insert (format "Starting Clink indexing with command:\n%s\n\n===============\n" clink-cmd))
      (start-process-shell-command proc-name buff-name clink-cmd))))

;;;###autoload
(defun clink-update-database (&optional root-directory)
  "Update the Clink database under ROOT-DIRECTORY."
  (interactive)
  (clink-build-database root-directory))

;;;###autoload
(defun clink-open-database-directory-in-dired (&optional root-directory)
  "Open the Clink database directory in Dired, starting from ROOT-DIRECTORY."
  (interactive)
  (when-let ((dir (or (and clink-project-root (file-exists-p (expand-file-name clink-database-filename clink-project-root)) clink-project-root)
                      (clink--directory-root-containing-file clink-root-project-detection-files root-directory))))
    (dired dir)))

;;;###autoload
(defun clink-set-database-dir ()
  "Set the Clink database root directory."
  (interactive)
  (let* ((dir (or (and clink-project-root (file-exists-p (expand-file-name clink-database-filename clink-project-root)) clink-project-root)
                  (clink-find-project-root)))
         (set-dir (read-directory-name "Select database directory: " dir)))
    (setq-local clink-project-root set-dir)))

;;;###autoload(autoload 'clink-find-symbol "clink" "Find symbol" t)
;;;###autoload(autoload 'clink-find-file "clink" "Find file" t)
;;;###autoload(autoload 'clink-find-callers "clink" "Find callers" t)
;;;###autoload(autoload 'clink-find-calls "clink" "Find calls" t)
;;;###autoload(autoload 'clink-find-reference "clink" "Find reference" t)
;;;###autoload(autoload 'clink-find-definition "clink" "Find definition" t)
;;;###autoload(autoload 'clink-find-includers "clink" "Find includers" t)

(dolist (type '(symbol file callers calls reference definition includers))
  (let ((fn-name (intern (format "clink-find-%s" type)))
        (fn-doc (format "Find %s." type))
        (fn-prompt (intern (format "clink--prompt-for-%s" type))))
    (defalias
      fn-name
      (lambda (arg)
        (interactive (list (funcall-interactively fn-prompt)))
        (clink-internal-find type arg))
      fn-doc)))

(defun clink-internal-find (type args)
  "Find TYPE with ARGS."
  (let ((root-dir clink-project-root)
        (plist (plist-get clink--queries-plist (intern (format ":%s" type))))
        (args (ensure-list args)))
    (clink--results-show-in-buffer
     (format "*clink-find-%s (%s)*" type (car args))
     (sqlite-select (clink--get-sqlite-database root-dir)
                    (plist-get plist :query)
                    (cond ((eq type 'file)
                           (vector (car args) (concat "%" (car args))))
                          ((eq type 'includers)
                           (vector (concat "%" (car args))))
                          (t (vector (car args)))))
     (plist-get plist :template)
     root-dir)))

(defun clink-turn-on ()
  "Setup Clink integration for the current buffer."
  (when-let* ((root (clink-find-project-root))
              (clink-db (expand-file-name clink-database-filename root))
              (db-exists (file-exists-p clink-db)))
    (setq-local clink-project-root root)))

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
