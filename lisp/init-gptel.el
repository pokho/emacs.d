(require-package 'gptel)
(require-package 'markdown-mode)
;; The following mcp and integrations packages are for mcp tool use. Elisp tools are supported by default but none are provided by default.
(require-package 'mcp)
(require-package 'gptel-integrations)


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
            llama-3.1-70b-versatile
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
  :endpoint "/v1"
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






;; OPTIONAL for setting the default model

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
                  google/gemini-pro)))


(provide 'init-gptel)
