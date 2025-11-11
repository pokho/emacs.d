# Emacs Configuration for Windows 11

This is a Windows-compatible version of your Emacs configuration, specifically migrated for Windows 11 with vanilla Emacs.

## Features

- **GPTel Integration**: Multiple AI providers (OpenRouter, Groq, Moonshot, Gemini) with specialized presets
- **Org-mode & Org-roam**: GTD workflow with Zettelkasten note management
- **Universal Capture System**: System-wide content capture from browsers, PDFs, and spreadsheets
- **Projectile**: Project navigation and management
- **Magit**: Git integration
- **PowerShell Scripts**: Windows-native automation for the capture system

## Installation

### 1. Prerequisites

#### Required Software
1. **Emacs 28+** (vanilla Emacs for Windows)
   - Download from: https://www.gnu.org/software/emacs/download.html#windows
   - Install to `C:\Program Files\Emacs\` (recommended)

2. **PowerShell 7+** (usually pre-installed on Windows 11)
   - Verify: `pwsh --version`
   - If not installed: `winget install Microsoft.PowerShell`

#### Optional but Recommended Tools
1. **Ripgrep (rg)** - Fast text search for Projectile
   ```powershell
   winget install BurntSushi.ripgrep.MSVC
   ```

2. **Git for Windows** - Provides many Unix tools
   ```powershell
   winget install --id Git.Git -e --source winget
   ```

3. **SQLite3** - Database backend for org-roam
   ```powershell
   winget install -e --id SQLite.SQLite
   ```

4. **Chrome Browser** - For universal capture system
   - Download from: https://www.google.com/chrome/

### 2. Configuration Setup

1. **Extract this configuration** to your desired location, e.g.:
   ```
   C:\Users\YOURNAME\.emacs.d\
   ```

2. **Update your main `init.el`** (create if it doesn't exist):
   ```elisp
   ;; Point to your Windows-compatible configuration
   (load-file "~/.emacs.d/init.el")
   ```

3. **Set up your org directories**:
   ```powershell
   mkdir -p ~/org
   mkdir -p ~/org/roam
   ```

### 3. Package Authentication Setup

Create your `~/.authinfo.gpg` file with your API keys:

```
machine api.openai.com login gptel password YOUR_OPENAI_API_KEY
machine api.anthropic.com login gptel password YOUR_ANTHROPIC_API_KEY
machine generativelanguage.googleapis.com login gptel password YOUR_GOOGLE_API_KEY
machine api.moonshot.cn login gptel password YOUR_MOONSHOT_API_KEY
machine openrouter.ai login gptel password YOUR_OPENROUTER_API_KEY
```

**To encrypt the file**: Use Emacs to save it with encryption (Emacs will prompt for a password).

### 4. Universal Capture System Setup

#### Keyboard Shortcut (Recommended)
1. **Create a Windows shortcut**:
   - Right-click Desktop → New → Shortcut
   - Location: `powershell.exe -ExecutionPolicy Bypass -File "C:\Users\YOURNAME\.emacs.d\browser-capture.ps1"`
   - Name: "Emacs Capture"

2. **Set global hotkey**:
   - Right-click the shortcut → Properties
   - Shortcut key: `Ctrl + Alt + O`
   - Click "Apply"

#### Usage
- **Browser capture**: Select text in Chrome, press `Ctrl+Alt+O`
- **PDF capture**: Open PDF, select text, run `pdf-capture.ps1`
- **Spreadsheet capture**: Select cells in Excel, run `spreadsheet-capture.ps1`

### 5. PDF-Tools Setup (Optional but Recommended)

PDF-Tools requires compilation on Windows. Two approaches:

#### Option 1: Pre-compiled Binary
1. Download pre-compiled `pdf-tools` from a trusted source
2. Extract to your Emacs `site-lisp` directory

#### Option 2: Manual Compilation
1. Install MSYS2: `winget install MSYS2.MSYS2`
2. In MSYS2, install dependencies:
   ```bash
   pacman -S mingw-w64-x86_64-gcc mingw-w64-x86_64-poppler mingw-w64-x86_64-pkg-config
   ```
3. Follow PDF-Tools installation instructions from their GitHub repository

## Directory Structure

```
~/.emacs.d/
├── init.el                    # Main configuration loader
├── init-windows.el           # Windows-specific settings
├── lisp/                     # Configuration modules
│   ├── init-gptel.el         # AI/LLM integration
│   ├── init-org.el           # Org-mode GTD setup
│   ├── init-org-roam.el      # Zettelkasten note management
│   ├── init-projectile.el    # Project navigation
│   ├── init-git.el           # Git/Magit integration
│   └── init-utils.el         # Utility functions
├── browser-capture.ps1       # Chrome capture script
├── pdf-capture.ps1          # PDF capture script
├── spreadsheet-capture.ps1  # Spreadsheet capture script
└── org-roam-capture.ps1     # Universal capture dispatcher
```

## Key Features

### GPTel AI Integration

**Available Presets** (use `M-x gptel-prompt-select`):
- `architect` - Systems architecture expert
- `frontend` - UX and accessibility specialist
- `backend` - Reliability and security engineer
- `analyzer` - Evidence-based analysis
- `security` - Threat and compliance specialist
- `mentor` - Educational guidance
- `marketeer` - Customer-facing specialist
- And more...

**Key Bindings**:
- `C-c g` - Start GPTel chat
- `C-c G` - Send prompt to GPTel

### Org-mode GTD System

**Key Bindings**:
- `C-c a` - Open agenda
- `C-c c` - Capture new item
- `C-c o j` - Jump to clocked task
- `C-c o i` - Clock in task

**Default Org Files**:
- `~/org/inbox.org` - New items and quick capture
- `~/org/roam/` - Org-roam notes directory

### Universal Capture System

**Capture Types**:
- **Web**: Browser tabs with URL and title
- **PDF**: Documents with page numbers and highlights
- **Spreadsheet**: Excel/Calc data with cell references
- **Default**: General text capture

**PowerShell Scripts**:
- `browser-capture.ps1` - Chrome integration
- `pdf-capture.ps1` - PDF document capture
- `spreadsheet-capture.ps1` - Excel/Calc integration
- `org-roam-capture.ps1` - Universal capture dispatcher

## Troubleshooting

### Common Issues

1. **"Emacs executable not found"**
   - Ensure Emacs is in your Windows PATH
   - Or update the paths in `org-roam-capture.ps1`

2. **"PDF-Tools installation failed"**
   - Install via pre-compiled binaries or compile manually
   - See PDF-Tools setup section above

3. **"PowerShell execution policy" error**
   - Run as Administrator: `Set-ExecutionPolicy RemoteSigned`

4. **"Git/rg not found"**
   - Install Git for Windows and ripgrep
   - Ensure they're in your PATH

5. **"org-roam database error"**
   - Install SQLite3
   - Verify org-roam directory exists

### Performance Optimization

1. **Increase Emacs heap size** (add to `init.el`):
   ```elisp
   (setq gc-cons-threshold 100000000)
   ```

2. **Use native compilation** (Emacs 28+):
   ```elisp
   (setq comp-deferred-compilation t)
   ```

3. **Disable unused packages** in configuration files

## Customization

### Adding New Capture Templates

Edit `lisp/init-utils.el` and modify the `org-roam-capture-templates` section:

```elisp
(add-to-list org-roam-capture-templates
             '("m" "meeting" plain
               "* %?\n%U\n%a"
               :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                  "#+title: ${title}\n#+type: meeting\n")
               :unnarrowed t))
```

### Modifying GPTel Presets

Edit `lisp/init-gptel.el` and add new presets:

```elisp
(gptel-make-preset 'my-preset
  :system "You are a specialist in..."
  :backend "OpenRouter"
  :model 'your-preferred-model
  :stream t)
```

### Windows-Specific Customization

All Windows-specific settings are in `init-windows.el`. Common customizations:

- Font settings
- Executable paths
- Shell configuration
- PATH modifications

## Migration from Linux

If migrating from a Linux setup:

1. **Copy existing org files** to `~/org/`
2. **Update `~/.authinfo.gpg`** with your API keys
3. **Test each component** individually before using together
4. **Adjust paths** in custom configurations from Unix to Windows format

## Support

For issues specific to this Windows configuration:
1. Check the troubleshooting section above
2. Verify all prerequisites are installed
3. Test individual components before reporting issues

For general Emacs/package issues:
- Refer to official documentation
- Check GitHub repositories for each package
- Search for Windows-specific solutions

---

**Note**: This configuration is specifically adapted for Windows 11 and vanilla Emacs. Some advanced features may require additional setup or compilation steps.