# Universal org-roam capture script (Windows PowerShell version)
# Usage: .\org-roam-capture.ps1 -Type "web" -Title "TITLE" -Source "SOURCE" -SelectedText "SELECTED_TEXT" [-ExtraInfo "EXTRA_INFO"]
# TYPE: web, pdf, spreadsheet, default
# EXTRA_INFO: optional metadata (e.g., "page=42" or "sheet=Sheet1,cell=A1")

param(
    [Parameter(Mandatory=$true)]
    [string]$Type,

    [Parameter(Mandatory=$true)]
    [string]$Title,

    [Parameter(Mandatory=$true)]
    [string]$Source,

    [string]$SelectedText = "",
    [string]$ExtraInfo = ""
)

# Function to escape string for Emacs Lisp
function ConvertTo-EscapedElispString {
    param([string]$str)

    # Escape backslashes first, then quotes
    $str = $str -replace '\\', '\\\\'
    $str = $str -replace '"', '\"'
    return $str
}

# Function to parse extra info into Emacs Lisp alist
function ConvertTo-ElispAlist {
    param([string]$info)

    if ([string]::IsNullOrWhiteSpace($info)) {
        return "'()"
    }

    $alist = "'("
    $pairs = $info -split ','

    foreach ($pair in $pairs) {
        $keyValue = $pair -split '=', 2
        if ($keyValue.Count -eq 2) {
            $key = $keyValue[0].Trim()
            $value = ConvertTo-EscapedElispString -str $keyValue[1].Trim()
            $alist += " :$key `"$value`""
        }
    }

    $alist += ")"
    return $alist
}

# Get the script directory (user's emacs config directory)
$scriptDir = $PSScriptRoot
$elispScript = Join-Path $scriptDir "temp-capture.el"

# Escape strings for elisp
$escapedTitle = ConvertTo-EscapedElispString -str $Title
$escapedSource = ConvertTo-EscapedElispString -str $Source
$escapedSelectedText = ConvertTo-EscapedElispString -str $SelectedText
$extraParsed = ConvertTo-ElispAlist -info $ExtraInfo

# Find Emacs executable on Windows
$emacsPaths = @(
    "C:\Program Files\Emacs\emacs\bin\emacs.exe",
    "C:\Program Files\Emacs\emacs\bin\emacsclientw.exe",
    "C:\msys64\mingw64\bin\emacs.exe",
    "emacs.exe"
)

$emacsExe = $null
foreach ($path in $emacsPaths) {
    if (Test-Path $path) {
        $emacsExe = $path
        break
    }
}

if (-not $emacsExe) {
    Write-Error "Emacs executable not found. Please ensure Emacs is installed and in PATH."
    exit 1
}

# Generate elisp to run the capture
$elispContent = @"
(progn
  (load-file "$scriptDir\..\init.el")
  (pokho/org-roam-universal-capture '$Type "$escapedTitle" "$escapedSource" "$escapedSelectedText" $extraParsed))
"@

# Write temporary elisp file
$elispContent | Out-File -FilePath $elispScript -Encoding UTF8

try {
    # Launch Emacs with the capture function
    & $emacsExe --eval "(load-file `"$elispScript`")"
} finally {
    # Clean up
    if (Test-Path $elispScript) {
        Remove-Item $elispScript -Force
    }
}