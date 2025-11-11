#!/bin/bash

# Browser capture script for org-roam
# This script can get current browser tab information and launch org-roam capture

# Function to get browser info from Firefox
get_firefox_info() {
    # Check if Firefox is running
    if pgrep -x "firefox" > /dev/null; then
        # Get current tab title and URL using Firefox remote
        TITLE=$(firefox -remote "getWindowTitle()" 2>/dev/null || echo "Firefox Tab")
        URL=$(firefox -remote "getURL()" 2>/dev/null || echo "")
        echo "$TITLE" "$URL"
    fi
}

# Function to get browser info from Chromium/Chrome
get_chromium_info() {
    # This is more complex and might require browser extensions
    # For now, return placeholder values
    echo "Browser Tab" ""
}

# Function to get selected text from clipboard
get_selected_text() {
    # Try to get from primary selection (middle-click)
    if command -v xclip >/dev/null 2>&1; then
        xclip -o -selection primary 2>/dev/null || echo ""
    elif command -v xsel >/dev/null 2>&1; then
        xsel --primary --output 2>/dev/null || echo ""
    else
        echo ""
    fi
}

# Main execution
TITLE=""
URL=""
SELECTED_TEXT=""

# Parse command line arguments if provided
if [ $# -ge 2 ]; then
    TITLE="$1"
    URL="$2"
    SELECTED_TEXT="$3"
else
    # Try to get browser info automatically
    BROWSER_INFO=$(get_firefox_info)
    if [ -z "$BROWSER_INFO" ]; then
        BROWSER_INFO=$(get_chromium_info)
    fi

    if [ -n "$BROWSER_INFO" ]; then
        TITLE=$(echo "$BROWSER_INFO" | cut -d' ' -f1-)
        URL=$(echo "$BROWSER_INFO" | awk '{print $NF}')
    fi

    SELECTED_TEXT=$(get_selected_text)
fi

# Default values if still empty
if [ -z "$TITLE" ]; then
    TITLE="Web Capture $(date '+%Y-%m-%d %H:%M')"
fi

# Launch Emacs with org-roam capture
/home/pokho/.emacs.d/org-roam-capture.sh "web" "$TITLE" "$URL" "$SELECTED_TEXT"