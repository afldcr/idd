(require 'seq)
(require 'map)
(require 'subr-x)

(defun seq-group (pred seq)
  "Run PRED over each element in SEQ.
Matches are contained in the car, and the rest is contained in the cdr."
  (let (yes no)
    (dolist (x seq (cons (reverse yes) (reverse no)))
      (if (funcall pred x) (push x yes) (push x no)))))

(defun directory-tree* (memo path)
  "Generate MEMO containing all subdirectories of PATH."
  (let ((path (file-name-as-directory path)))
    (thread-last path
      (directory-files)
      (seq-map
       (lambda (x) (concat path x)))
      (seq-remove
       (lambda (x)
         (or (eq ?. (string-to-char (file-name-base x)))
             (gethash x memo nil)
             (not (file-directory-p x)))))
      (mapc
       (lambda (x)
         (puthash x t memo)))
      (mapc
       (apply-partially #'directory-tree* memo)))))

(defun directory-tree (path)
  "List all directories of PATH (including PATH itself)."
  (let ((memo (make-hash-table))
        (path (file-name-as-directory (expand-file-name path))))
    (puthash path t memo)
    (directory-tree* memo path)
    (hash-table-keys memo)))

(let* ((make-backup-files)
       (site-lisp-dir
        (concat (file-name-as-directory (getenv "out")) "share/emacs/site-lisp"))
       (generated-autoload-file
        (concat site-lisp-dir "/package-autoloads.el"))
       (directories
        (directory-tree site-lisp-dir)))
  ;; update the autoloads file for all packages
  (apply #'update-directory-autoloads directories))
