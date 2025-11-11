#!/bin/bash

# Universal org-roam capture script
# Usage: ./org-roam-capture.sh TYPE "TITLE" "SOURCE" "SELECTED_TEXT" [EXTRA_INFO]
# TYPE: web, pdf, spreadsheet, default
# EXTRA_INFO: optional metadata (e.g., "page=42" or "sheet=Sheet1,cell=A1")

TYPE="${1:-web}"
TITLE="$2"
SOURCE="$3"
SELECTED_TEXT="$4"
EXTRA_INFO="$5"

# Create temporary elisp script
ELISP_SCRIPT=$(mktemp --suffix=.el)

# Escape strings for elisp
escape_elisp_string() {
    local str="$1"
    # Replace backslashes first, then quotes
    str="${str//\\/\\\\}"
    str="${str//\"/\\\"}"
    echo "$str"
}

# Parse extra info into elisp alist
parse_extra_info() {
    local info="$1"
    if [ -z "$info" ]; then
        echo "nil"
        return
    fi

    echo "'("
    IFS=',' read -ra PAIRS <<< "$info"
    for pair in "${PAIRS[@]}"; do
        IFS='=' read -ra KEY_VAL <<< "$pair"
        if [ ${#KEY_VAL[@]} -eq 2 ]; then
            echo " :${KEY_VAL[0]} \"$(escape_elisp_string "${KEY_VAL[1]}")\""
        fi
    done
    echo ")"
}

TITLE_ESCAPED=$(escape_elisp_string "$TITLE")
SOURCE_ESCAPED=$(escape_elisp_string "$SOURCE")
SELECTED_ESCAPED=$(escape_elisp_string "$SELECTED_TEXT")
EXTRA_PARSED=$(parse_extra_info "$EXTRA_INFO")

# Generate elisp to run the capture
cat > "$ELISP_SCRIPT" << EOF
(progn
  (load-file "$HOME/.emacs.d/init.el")
  (pokho/org-roam-universal-capture '$TYPE "$TITLE_ESCAPED" "$SOURCE_ESCAPED" "$SELECTED_ESCAPED" $EXTRA_PARSED))
EOF

# Launch Emacs with the capture function
emacs --eval "(load-file \"$ELISP_SCRIPT\")"

# Clean up
rm -f "$ELISP_SCRIPT"