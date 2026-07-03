#!/bin/bash

# org-roam capture script - simplified version
# Usage: ./org-roam-capture.sh "TYPE" "TITLE" "URL" "SELECTED_TEXT"

TYPE="$1"
TITLE="$2"
URL="$3"
SELECTED_TEXT="$4"

# Debug output (commented out)
# echo "=== org-roam-capture Debug ===" >> /tmp/org-roam-capture-debug.log
# echo "TYPE: '$TYPE'" >> /tmp/org-roam-capture-debug.log
# echo "TITLE: '$TITLE'" >> /tmp/org-roam-capture-debug.log
# echo "URL: '$URL'" >> /tmp/org-roam-capture-debug.log
# echo "SELECTED_TEXT: '$SELECTED_TEXT'" >> /tmp/org-roam-capture-debug.log
# echo "=========================" >> /tmp/org-roam-capture-debug.log

# A title that is clearly a file path or org-roam node name, not a real page title.
# High-precision on purpose: a legit title may contain "/" (e.g. "A/B Testing").
looks_like_garbage_title() {
    case "$1" in
        *.org|*.org\ -\ *|*org-roam*|/*|'~'/*) return 0 ;;
        *) return 1 ;;
    esac
}

# If no usable title (empty or path-like), derive one from the URL domain or timestamp.
if [ -z "$TITLE" ] || looks_like_garbage_title "$TITLE"; then
    if [ -n "$URL" ] && [[ "$URL" =~ ^https?:// ]]; then
        TITLE="Web: $(echo "$URL" | sed 's|https\?://||; s|/.*||; s|^www\.||')"
    else
        TITLE="Capture from $(date)"
    fi
fi

# If no URL provided, try to get from clipboard
if [ -z "$URL" ]; then
    # Try different clipboard tools
    URL=$(xsel --clipboard --output 2>/dev/null || xclip -selection clipboard -o 2>/dev/null || wl-paste 2>/dev/null || echo "")
    # Check if the clipboard content looks like a URL
    if [[ "$URL" =~ ^https?:// ]]; then
        echo "Using URL from clipboard: $URL"
    else
        URL=""
    fi
fi

# Create a simple temp file with content
CONTENT_FILE="/tmp/org-roam-capture-$(date +%s).org"

# Build the org file content
cat > "$CONTENT_FILE" << EOF
#+title: $TITLE
#+source: $URL
#+captured: $(date)

EOF

# Add selected text if provided (trim whitespace first)
if [ -n "$SELECTED_TEXT" ] && [ "$SELECTED_TEXT" != "" ]; then
    # Remove leading/trailing whitespace and check if there's actual content
    TRIMMED_TEXT=$(echo "$SELECTED_TEXT" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
    if [ -n "$TRIMMED_TEXT" ]; then
        echo -e "* Selected Text\n\n$TRIMMED_TEXT\n" >> "$CONTENT_FILE"
    fi
fi

# Get a unique filename in org-roam directory
TIMESTAMP=$(date +%Y%m%d%H%M%S)
SLUG=$(echo "$TITLE" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]/-/g; s/--*/-/g; s/^-//; s/-$//' | cut -c1-60)
[ -z "$SLUG" ] && SLUG="untitled"
ROAM_FILE="$HOME/org/roam/${TIMESTAMP}-${SLUG}.org"

# Never overwrite an existing note: append -2, -3, ... if the name is taken
if [ -e "$ROAM_FILE" ]; then
    i=2
    while [ -e "$HOME/org/roam/${TIMESTAMP}-${SLUG}-${i}.org" ]; do i=$((i+1)); done
    ROAM_FILE="$HOME/org/roam/${TIMESTAMP}-${SLUG}-${i}.org"
fi

# Move the temp file to org-roam directory
mv "$CONTENT_FILE" "$ROAM_FILE"

# Open the file in Emacs
# Check if Emacs server is running
if emacsclient --eval "(server-running-p)" 2>/dev/null; then
    # Use emacsclient to open in existing Emacs instance
    emacsclient -n "$ROAM_FILE" 2>/dev/null &
else
    # Check if any Emacs process is running (without daemon)
    if pgrep -f "emacs" >/dev/null; then
        # Try to start server in existing Emacs and open file
        emacsclient -n "$ROAM_FILE" 2>/dev/null || emacs "$ROAM_FILE" &
    else
        # Start new emacs if no instance running
        emacs "$ROAM_FILE" &
    fi
fi

# Clean up temp file if it still exists
rm -f "$CONTENT_FILE"

exit 0