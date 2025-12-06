#!/bin/bash

# org-roam capture script - simplified version
# Usage: ./org-roam-capture.sh "TYPE" "TITLE" "URL" "SELECTED_TEXT"

TYPE="$1"
TITLE="$2"
URL="$3"
SELECTED_TEXT="$4"

if [ -z "$TITLE" ]; then
    TITLE="Capture from $(date)"
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
SLUG=$(echo "$TITLE" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]/-/g' | sed 's/--*/-/g' | sed 's/^-//;s/-$//')
ROAM_FILE="$HOME/org/roam/${TIMESTAMP}-${SLUG}.org"

# Move the temp file to org-roam directory
mv "$CONTENT_FILE" "$ROAM_FILE"

# Open the file in Emacs
if emacsclient --eval "(message 'test')" 2>/dev/null; then
    # Use emacsclient if daemon is running
    emacsclient -n "$ROAM_FILE" 2>/dev/null &
else
    # Start new emacs if no daemon
    emacs "$ROAM_FILE" &
fi

# Clean up temp file if it still exists
rm -f "$CONTENT_FILE"

exit 0