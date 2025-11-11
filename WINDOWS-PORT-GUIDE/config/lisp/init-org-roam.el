(require-package 'org-roam)
(require-package 'pdf-tools)
(require-package 'org-pdftools)
;;(require-package 'md-roam)

;; Windows-specific directory setup
(when (eq system-type 'windows-nt)
  ;; Set org-roam directory to Windows path
  (setq org-roam-directory (expand-file-name "~/org/roam"))
  ;; Ensure the directory exists
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t)))

;; Set directory early so it will be used
(setq org-roam-database-connector 'sqlite)

;; Define org-roam capture templates early so anything that uses them
;; finds them. Adjust target and template text to your needs.
(setq org-roam-capture-templates
      '(("r" "default" plain
         "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)))

;; Windows-specific pdf-tools setup
(defun advice-pdf-tools-install-force-yes (old-fn &rest args)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) t)))
    (apply old-fn args)))
(advice-add 'pdf-tools-install :around 'advice-pdf-tools-install-force-yes)

;; Try to install pdf-tools with better error handling for Windows
(condition-case err
    (progn
      (pdf-tools-install :no-query)
      (setq-default pdf-view-display-size 'fit-width)
      (setq pdf-view-open-in-new-window nil))
  (error
   (message "PDF-Tools installation failed: %s. Please install manually on Windows." (error-message-string err))))

(add-hook 'org-mode-hook 'org-pdftools-setup-link)

(with-eval-after-load 'org-roam
  ;; md-roam support
  (add-to-list 'org-roam-file-extensions "md")
  (cl-defmethod org-roam-node-file-title ((node org-roam-node))
    "Return the TITLE of a node."
    (if (string-equal "md" (file-name-extension (org-roam-node-file node)))
        (org-roam-get-keyword "title" (org-roam-node-properties node))
      (org-roam-get-keyword "TITLE" (org-roam-node-properties node))))
  (cl-defmethod org-roam-node-read-title ((node org-roam-node))
    (if (string-equal "md" (file-name-extension (org-roam-node-file node)))
        (org-roam-get-keyword "title" (org-roam-node-properties node))
      (with-temp-buffer
        (insert-file-contents (org-roam-node-file node))
        (org-roam-get-keyword "TITLE" (org-roam-collect-keywords '("TITLE"))))))

  ;; Windows-specific database setup
  (when (eq system-type 'windows-nt)
    ;; Try to find sqlite3 executable on Windows
    (let ((sqlite-paths '("C:/Program Files/Git/usr/bin/sqlite3.exe"
                         "C:/msys64/mingw64/bin/sqlite3.exe"
                         "C:/ProgramData/chocolatey/bin/sqlite3.exe"
                         "sqlite3.exe")))
      (dolist (path sqlite-paths)
        (when (file-exists-p path)
          (setq org-roam-db-sqlite-program path)
          (return)))))

  (add-hook 'after-init-hook #'org-roam-db-autosync-mode))

(provide 'init-org-roam)