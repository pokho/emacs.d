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

# Function to get browser info from Chromium-based browsers (Chrome, Brave, Edge, etc.)
get_chromium_info() {
    local browser_name=""
    local browser_exe=""

    # Check for various Chromium browsers
    if pgrep -x "brave" > /dev/null || pgrep -f "brave-browser" > /dev/null; then
        browser_name="Brave"
        browser_exe="brave"
    elif pgrep -x "chrome" > /dev/null || pgrep -f "google-chrome" > /dev/null; then
        browser_name="Chrome"
        browser_exe="google-chrome"
    elif pgrep -x "chromium" > /dev/null || pgrep -f "chromium-browser" > /dev/null; then
        browser_name="Chromium"
        browser_exe="chromium-browser"
    elif pgrep -x "microsoft-edge" > /dev/null || pgrep -f "microsoft-edge" > /dev/null; then
        browser_name="Edge"
        browser_exe="microsoft-edge"
    fi

    if [ -n "$browser_name" ]; then
        local title=""
        local url=""

        # Try multiple methods to get the page info
        # Method 1: Check if user has copied URL to clipboard
        url=$(get_url_from_clipboard)

        # Method 2: Try to get window title
        if [ -z "$title" ]; then
            if command -v wmctrl >/dev/null 2>&1; then
                # List all windows and find browser windows
                local browser_window=$(wmctrl -l | grep -i "$browser_name" | head -1)
                if [ -n "$browser_window" ]; then
                    title=$(echo "$browser_window" | sed 's/^[^ ]* *[^ ]* *[^ ]* *//')
                    # Remove browser name from title
                    title=$(echo "$title" | sed "s/ - $browser_name$//" | sed "s/ — $browser_name$//")
                    # Remove common patterns
                    title=$(echo "$title" | sed 's/ - YouTube$//' | sed 's/ - Wikipedia$//')
                fi
            fi
        fi

        # Method 3: If we still don't have a title, use a generic one
        if [ -z "$title" ]; then
            title="$browser_name Page"
        fi

        # If we have a URL from clipboard, use it
        if [ -n "$url" ]; then
            # Extract a nice title from URL if we don't have one
            if [ "$title" = "$browser_name Page" ]; then
                local domain=$(echo "$url" | sed 's|https\?://||' | sed 's|/.*||' | sed 's|^www\.||')
                title="$browser_name: $domain"
            fi
        else
            # No URL available, leave empty
            url=""
        fi

        echo "$title" "$url"
    fi
}

# Function to get selected text from clipboard
get_selected_text() {
    local selected=""
    # First try to get from primary selection (middle-click)
    if command -v xclip >/dev/null 2>&1; then
        # Try primary selection first
        selected=$(xclip -o -selection primary 2>/dev/null)
        # If primary is empty, try clipboard selection
        if [ -z "$selected" ]; then
            selected=$(xclip -o -selection clipboard 2>/dev/null)
        fi
    elif command -v xsel >/dev/null 2>&1; then
        # Try primary selection first
        selected=$(xsel --primary --output 2>/dev/null)
        # If primary is empty, try clipboard
        if [ -z "$selected" ]; then
            selected=$(xsel --clipboard --output 2>/dev/null)
        fi
    fi
    # Return the selected text or empty string
    echo "$selected"
}

# Function to get URL from clipboard (for browsers where copy link copies URL)
get_url_from_clipboard() {
    if command -v xclip >/dev/null 2>&1; then
        local clipboard=$(xclip -o -selection clipboard 2>/dev/null)
        # Check if clipboard looks like a URL
        if echo "$clipboard" | grep -qE '^https?://'; then
            echo "$clipboard"
        fi
    elif command -v xsel >/dev/null 2>&1; then
        local clipboard=$(xsel --clipboard --output 2>/dev/null)
        # Check if clipboard looks like a URL
        if echo "$clipboard" | grep -qE '^https?://'; then
            echo "$clipboard"
        fi
    fi
}

# Function to try to get the current URL from the active application
try_get_active_url() {
    # This is a placeholder for future enhancement
    # In Wayland, it's difficult to get window titles without proper permissions
    # For now, this relies on the user copying the URL
    return 0
}

# Main execution
TITLE=""
URL=""
SELECTED_TEXT=""

# Save clipboard content early in case KDE clears it
SAVED_CLIPBOARD=""
if command -v xclip >/dev/null 2>&1; then
    SAVED_CLIPBOARD=$(xclip -o -selection clipboard 2>/dev/null)
fi

# Parse command line arguments if provided
if [ $# -ge 2 ]; then
    TITLE="$1"
    URL="$2"
    SELECTED_TEXT="$3"
else
    # For Brave and other Chromium browsers, best workflow is:
    # 1. Copy the URL (Ctrl+L, Ctrl+C)
    # 2. Select text if needed
    # 3. Press hotkey

    # Check for various browsers
    BROWSER_INFO=""

    # Firefox support
    if pgrep -x "firefox" > /dev/null; then
        BROWSER_INFO=$(get_firefox_info)
    fi

    # Chromium-based browsers
    if [ -z "$BROWSER_INFO" ]; then
        BROWSER_INFO=$(get_chromium_info)
    fi

    if [ -n "$BROWSER_INFO" ]; then
        # Extract title and URL from browser info
        URL_FROM_BROWSER=$(echo "$BROWSER_INFO" | awk '{print $NF}')
        # Check if the last part is a URL
        if echo "$URL_FROM_BROWSER" | grep -qE '^https?://'; then
            URL="$URL_FROM_BROWSER"
            # Title is everything except the URL
            TITLE=$(echo "$BROWSER_INFO" | sed "s/ $URL_FROM_BROWSER$//")
        else
            # No URL found, use entire output as title
            TITLE="$BROWSER_INFO"
            # Use saved clipboard content
            if echo "$SAVED_CLIPBOARD" | grep -qE '^https?://'; then
                URL="$SAVED_CLIPBOARD"
            else
                URL=$(get_url_from_clipboard)
            fi
        fi
    fi

    # If we still don't have a title but have a URL in saved clipboard, extract it
    if [ -z "$TITLE" ] && [ -n "$SAVED_CLIPBOARD" ] && echo "$SAVED_CLIPBOARD" | grep -qE '^https?://'; then
        # Extract domain from URL for title
        DOMAIN=$(echo "$SAVED_CLIPBOARD" | sed 's|https\?://||' | sed 's|/.*||' | sed 's|^www\.||')
        TITLE="Web: $DOMAIN"
        # Make sure URL is set
        if [ -z "$URL" ]; then
            URL="$SAVED_CLIPBOARD"
        fi
    fi

    # Get selected text
    SELECTED_TEXT=$(get_selected_text)
fi

# Filter out common non-content selections
filter_selected_text() {
    local text="$1"
    # Filter out common placeholder texts
    case "$text" in
        "Tab"|"tab"|"")
            echo ""
            ;;
        *)
            echo "$text"
            ;;
    esac
}

# Filter the selected text
SELECTED_TEXT=$(filter_selected_text "$SELECTED_TEXT")

# Default values if still empty
if [ -z "$TITLE" ]; then
    TITLE="Web Capture $(date '+%Y-%m-%d %H:%M')"
fi

# Debug output (commented out)
# echo "=== Browser Capture Debug ===" >> /tmp/browser-capture-debug.log
# echo "SAVED_CLIPBOARD: '$SAVED_CLIPBOARD'" >> /tmp/browser-capture-debug.log
# echo "Title='$TITLE'" >> /tmp/browser-capture-debug.log
# echo "URL='$URL'" >> /tmp/browser-capture-debug.log
# echo "Selected='${SELECTED_TEXT:0:100}'" >> /tmp/browser-capture-debug.log
# echo "===========================" >> /tmp/browser-capture-debug.log

# Launch Emacs with org-roam capture
/home/pokho/.emacs.d/org-roam-capture.sh "web" "$TITLE" "$URL" "$SELECTED_TEXT"