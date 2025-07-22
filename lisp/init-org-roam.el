(require-package 'org-roam)
(require-package 'pdf-tools)
(require-package 'org-pdftools)
(require-package 'md-roam)

(pdf-tools-install :no-query)
(setq-default pdf-view-display-size 'fit-width)
(setq pdf-view-open-in-new-window nil)

(add-hook 'org-mode-hook 'org-pdftools-setup-link)

(setq org-roam-directory "~/orgRoam")
(setq org-roam-database-connector 'sqlite)

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

(add-hook 'after-init-hook #'org-roam-db-autosync-mode)

(provide 'init-org-roam)
