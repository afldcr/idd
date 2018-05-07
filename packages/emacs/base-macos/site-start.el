(require 'subr-x)
(require 'seq)

(let*
    ((version
      (string-join (seq-take (split-string emacs-version "\\." t) 2)
                   "."))

     (dir-p
      (lambda (x)
        (file-directory-p (file-truename x))))

     (nix-profile-paths
      (if-let (env (getenv "NIX_PROFILES"))
          (reverse (split-string env "\s" t))
        (list (concat (getenv "HOME") "/.nix-profile")
              "/nix/var/nix/profiles/default")))

     (get-site-paths
      (lambda (profile-dir)
        (seq-filter dir-p
                    (list (concat profile-dir "/share/emacs/site-lisp")
                          (concat profile-dir "/share/emacs/" version "/site-lisp")))))

     (get-dir-and-subdirs
      (lambda (path)
        (seq-filter dir-p
                    (cons path
                          (directory-files path 'full "^[^.]")))))

     (emacs-src
      (thread-last load-file-name
        file-name-directory
        directory-file-name
        file-name-directory))

     (site-paths
      (thread-last nix-profile-paths
        (seq-mapcat get-site-paths)
        (seq-mapcat get-dir-and-subdirs))))

  ;; Add site-paths to the load path
  (dolist (path site-paths)
    (push path load-path))

  ;; Set the location of the C source
  (setq find-function-C-source-directory
        (concat emacs-src version "src/")))

(or (require 'bundle-start nil t)
    (message "Couldn't find `bundle-start'; skipping."))
