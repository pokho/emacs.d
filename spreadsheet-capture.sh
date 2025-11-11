#!/bin/bash

# Spreadsheet capture script for org-roam (works with LibreOffice Calc)
# Usage: ./spreadsheet-capture.sh "TITLE" "FILE_PATH" "SELECTED_TEXT" [SHEET] [CELL]

TITLE="$1"
FILE_PATH="$2"
SELECTED_TEXT="$3"
SHEET="$4"
CELL="$5"

# If not enough arguments, try to get current window info
if [ $# -lt 2 ]; then
    # Try to get active window info
    ACTIVE_WINDOW=$(xdotool getwindowfocus getwindowname 2>/dev/null)
    if [ -n "$ACTIVE_WINDOW" ]; then
        TITLE="$ACTIVE_WINDOW"
        FILE_PATH=""
    fi
fi

# Default title if still empty
if [ -z "$TITLE" ]; then
    TITLE="Spreadsheet Capture $(date '+%Y-%m-%d %H:%M')"
fi

# Prepare extra info
EXTRA_INFO=""
if [ -n "$SHEET" ]; then
    EXTRA_INFO="sheet=$SHEET"
    if [ -n "$CELL" ]; then
        EXTRA_INFO="$EXTRA_INFO,cell=$CELL"
    fi
elif [ -n "$CELL" ]; then
    EXTRA_INFO="cell=$CELL"
fi

# Launch Emacs with org-roam capture
/home/pokho/.emacs.d/org-roam-capture.sh "spreadsheet" "$TITLE" "$FILE_PATH" "$SELECTED_TEXT" "$EXTRA_INFO"