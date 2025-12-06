(require-package 'gptel)
(require-package 'markdown-mode)
;; The following mcp and integrations packages are for mcp tool use. Elisp tools are supported by default but none are provided by default.
(require-package 'mcp)
;;(require-package 'gptel-integrations)

(with-eval-after-load 'gptel
  ;; This file configures gptel to work with multiple AI providers.
  ;;
  ;; API keys are expected to be in your `~/.authinfo.gpg` file.
  ;; You need to add entries for each service you want to use.
  ;;
  ;; Example entries for `~/.authinfo.gpg`:
  ;;
  ;; machine api.openai.com login gptel password <YOUR_OPENAI_API_KEY>
  ;; machine api.anthropic.com login gptel password <YOUR_ANTHROPIC_API_KEY>
  ;; machine generativelanguage.googleapis.com login gptel password <YOUR_GOOGLE_API_KEY>
  ;; machine api.x.ai login gptel password <YOUR_XAI_API_KEY>
  ;; machine openrouter.ai login gptel password <YOUR_OPENROUTER_API_KEY>
  ;;
  ;; After adding these, reload your auth source or restart Emacs.



  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key gptel-api-key
    :models '(z-ai/glm-4.5-air:free       ;https://openrouter.ai/models?max_price=0&order=top-weekly
              z-ai/glm-4.6
              deepseek/deepseek-chat-v3.1:free
              moonshotai/kimi-k2:free
              qwen/qwen3-coder:free ;; 262k context
              tngtech/deepseek-r1t2-chimera:free ;; 164k context
              meta-llama/codellama-34b-instruct
              codellama/codellama-70b-instruct
              google/gemini-2.0-flash-exp:free ;; 1M context
              google/gemini-2.5-flash
              openai/gpt-3.5-turbo
              openai/gpt-oss-20b
              x-ai/grok-code-fast-1
              ))


  ;; (gptel-make-anthropic "Claude" :stream t :key gptel-api-key)

  ;; (gptel-make-ollama "Ollama"             ;Any name of your choosing
  ;;   :host "localhost:11434"               ;Where it's running
  ;;   :stream t                             ;Stream responses
  ;;   :models '(mistral:latest))          ;List of models






  ;; Make gptel tools
  (setq gptel-use-tools t)
  (setq gptel-tools
        (list
         (gptel-make-tool
          :name "folder_read"
          :description "List all files and subfolders in a folder"
          :category "filesystem"
          :function (lambda (path) (directory-files path t))
          :args (list '(:name "path" :type 'string :description "The path to the folder.")))
         (gptel-make-tool
          :name "folder_create"
          :description "Create a new folder"
          :category "filesystem"
          :function (lambda (path) (make-directory path t) "Folder created.")
          :args (list '(:name "path" :type 'string :description "The path of the folder to create.")))
         (gptel-make-tool
          :name "file_find"
          :description "Find file(s) matching a pattern"
          :category "filesystem"
          :function (lambda (pattern &optional path) (directory-files-recursively (or path default-directory) pattern))
          :args (list '(:name "pattern" :type 'string :description "The regex pattern to search for.")
                      '(:name "path" :type 'string :description "The directory to search in.")))
         (gptel-make-tool
          :name "file_read"
          :description "Read the contents of one or more files"
          :category "filesystem"
          :function (lambda (paths) (mapcar (lambda (f) (cons f (with-temp-buffer (insert-file-contents f) (buffer-string)))) paths))
          :args (list '(:name "paths" :type 'array :items "string" :description "A list of file paths to read.")))
         (gptel-make-tool
          :name "file_write"
          :description "Write contents to a file (overwrite if exists)"
          :category "filesystem"
          :function (lambda (path content) (with-temp-file path (insert content)) "File written.")
          :args (list '(:name "path" :type 'string :description "The path of the file to write to.")
                      '(:name "content" :type 'string :description "The content to write to the file.")))
         (gptel-make-tool
          :name "file_delete"
          :description "Delete a file"
          :category "filesystem"
          :function (lambda (path) (delete-file path) "File deleted.")
          :args (list '(:name "path" :type 'string :description "The path of the file to delete.")))
         (gptel-make-tool
          :name "text_search"
          :description "Search for a text pattern in files"
          :category "filesystem"
          :function (lambda (pattern &optional path)
                      (let ((results nil))
                        (dolist (file (directory-files-recursively (or path default-directory) ".*"))
                          (with-temp-buffer
                            (insert-file-contents file)
                            (goto-char (point-min))
                            (when (re-search-forward pattern nil t)
                              (push file results))))
                        results))
          :args (list '(:name "pattern" :type 'string :description "The regex pattern to search for.")
                      '(:name "path" :type 'string :description "The directory to search in.")))
         (gptel-make-tool
          :name "text_edit"
          :description "Edit a text file by replacing a pattern"
          :category "filesystem"
          :function (lambda (path pattern replacement)
                      (with-temp-buffer
                        (insert-file-contents path)
                        (goto-char (point-min))
                        (while (re-search-forward pattern nil t)
                          (replace-match replacement))
                        (write-region (point-min) (point-max) path))
                      "Text replaced.")
          :args (list '(:name "path" :type 'string :description "The path of the file to edit.")
                      '(:name "pattern" :type 'string :description "The regex pattern to replace.")
                      '(:name "replacement" :type 'string :description "The replacement text.")))
         (gptel-make-tool
          :name "web_search"
          :description "Search the web for a query"
          :category "web"
          :function (lambda (query) (concat "Pretend web search results for: " query))
          :args (list '(:name "query" :type 'string :description "The web search query.")))
         (gptel-make-tool
          :name "web_summarise_url"
          :description "Summarize the content from a web URL"
          :category "web"
          :function (lambda (url) (concat "Pretend summary of URL: " url))
          :args (list '(:name "url" :type 'string :description "The URL to summarize.")))
         ;; Windows-compatible shell command tool
         (gptel-make-tool
          :name "shell_command"
          :description "Execute a shell command and return stdout, stderr, and exit code"
          :category "shell"
          :function (lambda (command)
                      (with-temp-buffer
                        (let ((exit-code (if (eq system-type 'windows-nt)
                                            (call-process-shell-command command nil (current-buffer) t)
                                          (call-process-shell-command command nil (current-buffer) t))))
                          (list
                           (cons "stdout/stderr" (buffer-string))
                           (cons "exit-code" exit-code)))))
          :args (list '(:name "command" :type 'string :description "The shell command to execute.")))
         (gptel-make-tool
          :name "buffer_read"
          :description "Read the entire contents of the current buffer."
          :category "emacs"
          :function (lambda () (buffer-string))
          :args nil)
         (gptel-make-tool
          :name "buffer_modify"
          :description "Modify the current buffer by replacing the active region, or inserting at point if no region is active, with the provided text."
          :category "emacs"
          :function (lambda (content)
                      (if (use-region-p)
                          (progn
                            (delete-region (region-beginning) (region-end))
                            (insert content)
                            "Replaced active region with new text.")
                        (insert content)
                        "Inserted text at point."))
          :args (list '(:name "content" :type 'string :description "The content to insert or replace with.")))
         ))

  ;; (setq gptel-backend (gptel-make-openai "OpenRouter"))

  )
(setq gptel-log-level 'debug)
(provide 'init-gptel)
