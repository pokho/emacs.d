#!/bin/bash

# PDF capture script for org-roam (works with Okular/Evince)
# Usage: ./pdf-capture.sh "TITLE" "FILE_PATH" "SELECTED_TEXT" [PAGE]

TITLE="$1"
FILE_PATH="$2"
SELECTED_TEXT="$3"
PAGE="$4"

# If not enough arguments, try to get current window info
if [ $# -lt 2 ]; then
    # Try to get active window info (works with most X11 window managers)
    ACTIVE_WINDOW=$(xdotool getwindowfocus getwindowname 2>/dev/null)
    if [ -n "$ACTIVE_WINDOW" ]; then
        TITLE="$ACTIVE_WINDOW"
        FILE_PATH=""  # Would need PDF-specific tools to extract
    fi
fi

# Default title if still empty
if [ -z "$TITLE" ]; then
    TITLE="PDF Capture $(date '+%Y-%m-%d %H:%M')"
fi

# Prepare extra info
EXTRA_INFO=""
if [ -n "$PAGE" ]; then
    EXTRA_INFO="page=$PAGE"
fi

# Launch Emacs with org-roam capture
/home/pokho/.emacs.d/org-roam-capture.sh "pdf" "$TITLE" "$FILE_PATH" "$SELECTED_TEXT" "$EXTRA_INFO"