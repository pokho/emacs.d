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
  ;; machine api.moonshot.cn login gptel password <YOUR_MOONSHOT_API_KEY>
  ;; machine openrouter.ai login gptel password <YOUR_OPENROUTER_API_KEY>
  ;; machine api.novita.ai login gptel password <YOUR_NOVITA_API_KEY>
  ;;
  ;; After adding these, reload your auth source or restart Emacs.


  (gptel-make-gemini "Gemini" :stream t :key gptel-api-key)

  (gptel-make-openai "Groq"               ;Any name you want
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key gptel-api-key                   ;can be a function that returns the key
    :models '(moonshotai/kimi-k2-instruct
              llama-3.3-70b-versatile
              llama-3.1-8b-instant
              mixtral-8x7b-32768
              gemma-7b-it))

  (gptel-make-openai "OpenRouter"               ;Any name you want
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key gptel-api-key                   ;can be a function that returns the key
    :models '(z-ai/glm-4.5-air:free       ;https://openrouter.ai/models?max_price=0&order=top-weekly
              moonshotai/kimi-k2:free
              qwen/qwen3-coder:free
              deepseek/deepseek-chat-v3-0324:free
              deepseek/deepseek-r1-0528:free
              openai/gpt-3.5-turbo
              mistralai/mixtral-8x7b-instruct
              meta-llama/codellama-34b-instruct
              codellama/codellama-70b-instruct
              google/palm-2-codechat-bison-32k
              google/gemini-pro))

  (gptel-make-openai "NovitaAI"
    :host "api.novita.ai"
    :endpoint "/v3/openai"
    :key gptel-api-key
    :stream t
    :models '(zai-org/glm-4.5
              moonshotai/kimi-k2-instruct
              gryphe/mythomax-l2-13b
              meta-llama/llama-3-70b-instruct
              meta-llama/llama-3.1-70b-instruct))

  (gptel-make-openai "Moonshot"
    :host "api.moonshot.cn" ;; or "api.moonshot.ai" for the global site
    :endpoint "/v1/openai"
    :key gptel-api-key
    :stream t ;; optionally enable streaming
    :models '(kimi-latest kimi-k2-0711-preview kimi-k2-turbo-preview))


  ;; ;; AI/ML API offers an OpenAI compatible API. Enterprise-grade uptime
  ;; (gptel-make-openai "AI/ML API"          ;Any name you want
  ;;   :host "api.aimlapi.com"
  ;;   :endpoint "/v1/chat/completions"
  ;;   :stream t
  ;;   :key gptel-api-key           ;can be a function that returns the key
  ;;   :models '(deepseek-chat gemini-pro gpt-4o))

  ;; (gptel-make-anthropic "Claude" :stream t :key gptel-api-key)

  ;; (gptel-make-kagi "Kagi" :key gptel-api-key) ;; Streaming and multi-turn not supported

  ;; (gptel-make-openai "TogetherAI"         ;Any name you want
  ;;   :host "api.together.xyz"
  ;;   :key gptel-api-key                   ;can be a function that returns the key
  ;;   :stream t
  ;;   :models '(;; has many more, check together.ai
  ;;             mistralai/Mixtral-8x7B-Instruct-v0.1
  ;;             codellama/CodeLlama-13b-Instruct-hf
  ;;             codellama/CodeLlama-34b-Instruct-hf))

  ;; (gptel-make-perplexity "Perplexity"     ;Any name you want
  ;;   :key gptel-api-key          ;can be a function that returns the key
  ;;   :stream t)                            ;If you want responses to be streamed

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
         (gptel-make-tool
          :name "shell_command"
          :description "Execute a shell command and return stdout, stderr, and exit code"
          :category "shell"
          :function (lambda (command)
                      (with-temp-buffer
                        (let ((exit-code (call-process-shell-command command nil (current-buffer) t)))
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



  ;; Create @presets from SuperClaude (or https://github.com/f/awesome-chatgpt-prompts)
;;; Undefined backends have been mapped to defined ones as follows:
;;; Sequential -> Gemini
;;; Context7   -> Groq
;;; Magic      -> OpenRouter
;;; Playwright -> NovitaAI
  ;; Architect: Systems and scalability expert
  (gptel-make-preset 'architect
    :description "Systems architect: Designs for long-term maintainability, scalability, and modularity. Analyzes whole-system impacts and dependencies."
    :system "You are a systems architect. Prioritize long-term maintainability, scalability, and modularity. Analyze the impact of changes across the system, minimize coupling, and ensure future-proof designs."
    :keywords '("architecture" "design" "scalability" "modularity" "dependency")
    :quality '("maintainability" "scalability" "modularity")
    :backend "Gemini"
    :secondary-backends '("Groq")
    :avoid-backends '("OpenRouter")
    :tools '("folder_read" "file_find" "file_read" "text_search" "text_edit" "buffer_read" "buffer_modify")
    )

  ;; Frontend: UX and accessibility specialist
  (gptel-make-preset 'frontend
    :description "Frontend developer: User-centered, accessibility-focused, and performance-conscious. Delivers intuitive, performant, and compliant UIs."
    :system "You are a frontend and UX specialist. Prioritize user experience, accessibility, and real-world performance. Ensure WCAG compliance and optimize for all devices and networks."
    :keywords '("component" "responsive" "accessibility" "ui" "design system")
    :quality '("usability" "accessibility" "performance")
    :backend "OpenRouter"
    :secondary-backends '("NovitaAI")
    :tools '("file_find" "file_read" "file_write" "text_search" "text_edit" "buffer_read" "buffer_modify" "web_search" "web_summarise_url")
    )

  ;; Backend: Reliability and security engineer
  (gptel-make-preset 'backend
    :description "Backend engineer: Focuses on reliability, security, and robust APIs. Ensures fault tolerance, consistency, and secure data flows."
    :system "You are a backend specialist. Prioritize reliability, security, and data integrity. Design robust, fault-tolerant APIs and backend systems."
    :keywords '("API" "database" "service" "reliability" "backend" "security")
    :quality '("reliability" "security" "data integrity")
    :backend "Groq"
    :secondary-backends '("Gemini")
    :avoid-backends '("OpenRouter")
    :tools '("folder_read" "folder_create" "file_find" "file_read" "file_write" "file_delete" "text_search" "text_edit" "buffer_read" "buffer_modify" "shell_command")
    )

  ;; Analyzer: Evidence-based root cause specialist
  (gptel-make-preset 'analyzer
    :description "Root cause analyst: Evidence-driven and systematic. Investigates, troubleshoots, and explains with supporting data."
    :system "You are a systematic analyst. Base conclusions on verifiable evidence, follow structured investigation, and identify true root causes before recommending solutions."
    :keywords '("analyze" "investigate" "root cause" "debug" "evidence" "troubleshoot")
    :quality '("evidence-based" "systematic" "thoroughness")
    :backend "Gemini"
    :secondary-backends '("Groq")
    :tools '("file_find" "file_read" "text_search" "buffer_read" "buffer_modify" "shell_command")
    )

  ;; Security: Threat, compliance, and vulnerability specialist
  (gptel-make-preset 'security
    :description "Security/compliance expert: Defense in depth, zero trust, and secure-by-default. Assesses threats, enforces compliance, and documents measures."
    :system "You are a security and compliance specialist. Prioritize security, enforce compliance, and implement layered defenses. Document and justify all security decisions."
    :keywords '("vulnerability" "threat" "compliance" "security" "authorization" "authentication")
    :quality '("security" "compliance" "transparency")
    :backend "Gemini"
    :secondary-backends '("Groq")
    :avoid-backends '("OpenRouter")
    :tools '("folder_read" "file_read" "file_write" "file_delete" "text_search" "buffer_read" "buffer_modify" "shell_command")
    )

  ;; Mentor: Knowledge transfer and education
  (gptel-make-preset 'mentor
    :description "Mentor: Prioritizes understanding, knowledge transfer, and teaching. Explains methodology and empowers users."
    :system "You are a mentor and educator. Focus on clear explanations and knowledge transfer. Tailor advice to the learner’s goals and understanding."
    :keywords '("explain" "learn" "understand" "guide" "teach")
    :quality '("clarity" "completeness" "engagement")
    :backend "Groq"
    :secondary-backends '("Gemini")
    :tools '("file_read" "buffer_read" "web_search" "web_summarise_url")
    )

  ;; Refactorer: Code quality and debt manager
  (gptel-make-preset 'refactorer
    :description "Code refactorer: Advocates for simplicity, maintainability, and clean code. Systematically improves code quality."
    :system "You are a code quality specialist. Prioritize simplicity, maintainability, and readability. Systematically address technical debt."
    :keywords '("refactor" "cleanup" "technical debt" "maintainability" "simplicity")
    :quality '("readability" "simplicity" "consistency")
    :backend "Gemini"
    :secondary-backends '("Groq")
    :tools '("file_find" "file_read" "file_write" "text_search" "text_edit" "buffer_read" "buffer_modify")
    )

  ;; Performance: Optimization and bottleneck expert
  (gptel-make-preset 'performance
    :description "Performance optimizer: Metrics-driven, bottleneck-focused, user-experience-centric. Measures and optimizes where it matters."
    :system "You are a performance specialist. Always measure before optimizing. Focus on critical bottlenecks and user experience. Validate all optimizations with metrics."
    :keywords '("optimize" "performance" "bottleneck" "speed" "efficiency")
    :quality '("measurement-based" "user-focused" "systematic")
    :backend "NovitaAI"
    :secondary-backends '("Gemini")
    :tools '("file_find" "file_read" "file_write" "text_search" "text_edit" "buffer_read" "buffer_modify" "shell_command")
    )

  ;; QA: Testing, validation, and edge case detective
  (gptel-make-preset 'qa
    :description "QA/testing specialist: Prevents defects, ensures comprehensive coverage, and detects edge cases. Designs robust testing strategies."
    :system "You are a QA/testing expert. Focus on defect prevention, comprehensive edge case coverage, and risk-based testing strategies."
    :keywords '("test" "quality" "validation" "edge case" "qa")
    :quality '("comprehensive" "risk-based" "preventive")
    :backend "NovitaAI"
    :secondary-backends '("Gemini")
    :tools '("file_find" "file_read" "file_write" "text_search" "buffer_read" "buffer_modify" "shell_command")
    )

  ;; DevOps: Infrastructure, automation, and deployment
  (gptel-make-preset 'devops
    :description "DevOps/infra specialist: Automates, observes, and ensures reliability. Implements IaC, monitoring, and deployment best practices."
    :system "You are a DevOps and infrastructure expert. Automate everything, ensure observability, and design for reliability and scalability."
    :keywords '("deploy" "infrastructure" "automation" "monitoring" "observability")
    :quality '("automation" "observability" "reliability")
    :backend "Gemini"
    :secondary-backends '("Groq")
    :tools '("folder_read" "folder_create" "file_find" "file_read" "file_write" "file_delete" "text_search" "text_edit" "buffer_read" "buffer_modify" "shell_command")
    )

  ;; Scribe: Professional technical writer and localization
  (gptel-make-preset 'scribe
    :description "Technical writer: Audience-first, clear, and culturally sensitive documentation. Supports multilingual and professional standards."
    :system "You are a professional writer and documentation specialist. Prioritize clarity, cultural sensitivity, and audience needs. Create high-quality, localized technical documentation."
    :keywords '("document" "write" "guide" "localization" "communication" "docs")
    :quality '("clarity" "cultural sensitivity" "professional excellence")
    :backend "Groq"
    :secondary-backends '("Gemini")
    :tools '("file_find" "file_read" "file_write" "buffer_read" "buffer_modify" "web_search" "web_summarise_url")
    )

  ;; Business Leader: CEO/CFO/COO
  (gptel-make-preset 'business-lead
    :description "Executive leader (CEO/CFO/COO/CTO): Strategic, financial, high-level direction. Focuses on growth, risk, and overall business health."
    :system "You are a business leader (CEO, CFO, COO, CTO). Prioritize strategic vision, product/service innovation, financial health, risk management, and sustainable growth. Communicate clearly, analyze business data, and align recommendations with organizational goals."
    :keywords '("strategy" "finance" "growth" "innovation" "research" "risk" "investment" "roadmap" "KPI")
    :quality '("strategic thinking" "clarity" "risk management")
    :backend "Groq"
    :secondary-backends '("Gemini")
    :tools '("file_read" "file_write" "web_search" "web_summarise_url" "buffer_read" "buffer_modify")
    )

  ;; Customer-Facing: Branding, Marketing, Sales
  (gptel-make-preset 'customer-lead
    :description "Customer-facing (branding, marketing, sales): Brand voice, campaign creation, sales enablement. Focuses on audience engagement and conversion."
    :system "You are a customer-facing specialist in branding, marketing, or sales. Prioritize audience understanding, clear communication, persuasive messaging, and brand consistency. Support campaign planning and sales enablement."
    :keywords '("branding" "marketing" "campaign" "sales" "customer" "engagement" "conversion")
    :quality '("audience focus" "persuasion" "brand consistency")
    :backend "OpenRouter"
    :secondary-backends '("Groq")
    :tools '("file_read" "file_write" "web_search" "web_summarise_url" "buffer_read" "buffer_modify")
    )

  ;; Organisational: Legal, Board, Partnership/Alliance
  (gptel-make-preset 'org-lead
    :description "Organisational (legal, board, partnership): Legal compliance, governance, alliances. Ensures regulations, contracts, and policies are upheld."
    :system "You are an organisational specialist for legal, board, or partnership matters. Prioritize compliance, governance, contract clarity, and risk mitigation. Communicate formally and document all decisions."
    :keywords '("legal" "compliance" "policy" "contract" "board" "governance" "partnership" "alliance")
    :quality '("compliance" "clarity" "risk mitigation")
    :backend "Gemini"
    :secondary-backends '("Groq")
    :tools '("file_read" "file_write" "file_delete" "web_search" "web_summarise_url" "buffer_read" "buffer_modify")
    )

  ;; Set the default model

  ;; (setq
  ;;  gptel-model 'gemini-2.5-pro-exp-03-25
  ;;  gptel-backend (gptel-make-gemini "Gemini"
  ;;                  :key gptel-api-key
  ;;                  :stream t))

  (setq gptel-model   'z-ai/glm-4.5-air:free
        gptel-backend
        (gptel-make-openai "OpenRouter"   ;Any name you want
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key gptel-api-key
          :models '(z-ai/glm-4.5-air:free
                    openai/gpt-3.5-turbo
                    mistralai/mixtral-8x7b-instruct
                    meta-llama/codellama-34b-instruct
                    codellama/codellama-70b-instruct
                    google/palm-2-codechat-bison-32k
                    google/gemini-pro))))


(provide 'init-gptel)
