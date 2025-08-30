(require-package 'org-roam)
(require-package 'pdf-tools)
(require-package 'org-pdftools)
;;(require-package 'md-roam)

;; Set directory early so it will be used
(setq org-roam-directory (file-truename "~/org/roam"))
(setq org-roam-database-connector 'sqlite)

;; Define org-roam capture templates early so anything that uses them
;; finds them. Adjust target and template text to your needs.
(setq org-roam-capture-templates
      '(("r" "default" plain
         "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)))

(defun advice-pdf-tools-install-force-yes (old-fn &rest args)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) t)))
    (apply old-fn args)))
(advice-add 'pdf-tools-install :around 'advice-pdf-tools-install-force-yes)

(pdf-tools-install :no-query)
(setq-default pdf-view-display-size 'fit-width)
(setq pdf-view-open-in-new-window nil)

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

  (add-hook 'after-init-hook #'org-roam-db-autosync-mode))

(provide 'init-org-roam)
