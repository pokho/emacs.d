# Org-Roam Universal Capture System

A system-wide capture solution for Emacs org-roam that supports web pages, PDFs, and spreadsheets.

## Features

- **Universal Capture**: Extensible framework supporting multiple content types
- **System-wide**: Works from any application via keyboard shortcuts
- **Auto-detection**: Browser tab title and URL detection
- **Rich Metadata**: Captures source information, timestamps, and context
- **KDE Compatible**: Works with Kubuntu/KDE desktop environments

## Installation & Setup

### 1. KDE Keyboard Shortcut Setup

For Kubuntu/KDE, use the system settings:

1. Open **System Settings** → **Shortcuts** → **Custom Shortcuts**
2. Click **Edit** → **New** → **Global Shortcut** → **Command**
3. Set:
   - **Name**: `Org-Roam Browser Capture`
   - **Command**: `/home/pokho/.emacs.d/browser-capture.sh`
   - **Trigger**: `Ctrl+Alt+O` (or your preferred shortcut)

Alternative: Import the provided khotkeys configuration:
```bash
# The khotkeys file is already created at:
# ~/.config/khotkeys/org-roam-capture.khotkeys
```

### 2. Application Launchers

Desktop files are installed at:
- `~/.local/share/applications/org-roam-capture.desktop`

## Usage

### Browser Capture (Default)

**Keyboard Shortcut**: `Ctrl+Alt+O`

Automatically captures:
- Browser tab title
- Current URL
- Selected text (from primary selection/middle-click)

**Manual Usage**:
```bash
# Direct capture with parameters
./browser-capture.sh
./org-roam-capture.sh web "Article Title" "https://example.com" "Selected text"
```

### PDF Capture

```bash
# Direct capture
./pdf-capture.sh "Document Title" "/path/to/file.pdf" "Highlighted text" "42"

# With org-roam-capture.sh
./org-roam-capture.sh pdf "Research Paper" "/home/user/papers/paper.pdf" "Important quote" "page=42"
```

### Spreadsheet Capture

```bash
# Direct capture
./spreadsheet-capture.sh "Budget 2024" "/home/user/docs/budget.ods" "Cell content" "Sheet1" "A1"

# With org-roam-capture.sh
./org-roam-capture.sh spreadsheet "Financial Data" "/home/user/docs/data.xlsx" "Revenue figure" "sheet=Q1,cell=B5"
```

## Capture Templates

Each content type uses specialized org-roam templates:

### Web Pages
```org
* [[URL][Title]]

Selected text

Your notes...
```

### PDFs
```org
* PDF: Title
* Source: file:///path/to/document.pdf

Highlighted text

Your notes...
```

### Spreadsheets
```org
* Spreadsheet: Title
* Source: file:///path/to/spreadsheet.ods

Cell content

Your notes...
```

## File Structure

```
~/.emacs.d/
├── lisp/init-utils.el           # Core capture functions
├── org-roam-capture.sh          # Universal capture script
├── browser-capture.sh           # Browser-specific wrapper
├── pdf-capture.sh               # PDF-specific wrapper
├── spreadsheet-capture.sh       # Spreadsheet-specific wrapper
├── .local/share/applications/
│   └── org-roam-capture.desktop # Application launcher
└── README-CAPTURE.md           # This documentation
```

## Emacs Functions

The system adds these functions to your Emacs:

- `pokho/org-roam-universal-capture` - Core universal capture
- `pokho/org-roam-browser-capture` - Browser-specific capture
- `pokho/org-roam-pdf-capture` - PDF-specific capture
- `pokho/org-roam-spreadsheet-capture` - Spreadsheet-specific capture

## Extensions

To add new capture types:

1. **Add a new type case** in `pokho/org-roam-universal-capture` (lisp/init-utils.el)
2. **Create a wrapper script** like `pdf-capture.sh`
3. **Add new desktop file** if needed

Example: Adding a capture type for code snippets:
```elisp
;; Add to the capture-key condition:
((eq type 'code) "c")

;; Add to template case:
("c" '("code" plain
       "* Code: %?title\n* Language: %?language\n\n```%?language\n%?selected\n```\n\n%?"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n#+language: %^{language}\n#+captured: %U\n\n")
       :unnarrowed t))
```

## Troubleshooting

### Browser Detection Issues

**For Brave/Chrome/Edge on Wayland:**

Due to Wayland security restrictions, automatic URL detection is limited. Use this workflow:

1. **Copy the URL first**: Press `Ctrl+L` to focus address bar, then `Ctrl+C` to copy
2. Select any text you want to include (optional)
3. Press `Ctrl+Alt+O` to trigger capture

The capture will use the URL from your clipboard and extract the domain name for the title.

**If Firefox tab info isn't detected:**
1. Ensure Firefox remote is enabled: Check `about:config` for `dom.ipc.enabled`
2. Alternative: Use a browser extension that calls the capture script directly

### KDE Shortcut Not Working

1. Restart KDE components:
   ```bash
   kquitapp5 kglobalaccel && kglobalaccel5 &
   ```
2. Check permissions on scripts: `chmod +x ~/.emacs.d/*-capture.sh`
3. Verify the command path in KDE settings

### Selected Text Not Captured

1. Ensure text is selected before triggering shortcut
2. Try copying text to clipboard instead of primary selection
3. Install `xclip` or `xsel` for clipboard support

## Dependencies

- **Emacs** with org-roam
- **KDE Plasma** (for keyboard shortcuts)
- **Optional**: `xclip` or `xsel` (clipboard support)
- **Optional**: `xdotool` (window information)