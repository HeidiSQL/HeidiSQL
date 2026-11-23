#!/usr/bin/env bash
#
# Pulls the HeidiSQL translations from Transifex, downloads the LCL translations
# from the Lazarus repository, and compiles both to .mo files in extra/locale/.
#
# Usage: ./update-translations.sh [lazarus-branch-or-tag]   (default: fixes_4)
#
# Runs on Linux and in Git Bash on Windows. Needs curl and these tools on PATH:
# - tx: Transifex CLI, https://github.com/transifex/cli
#     Windows: winget install Transifex.CLI
#     Linux:   curl -o- https://raw.githubusercontent.com/transifex/cli/master/install.sh | bash
#   It needs an API token, e.g. in ~/.transifexrc or in the environment variable TX_TOKEN.
# - msgfmt: GNU gettext 0.15 or newer (older versions do not support msgctxt)
#     Windows: scoop install gettext   or   winget install mlocati.GetText
#     Linux:   apt install gettext

set -euo pipefail

lazarus_ref="${1:-fixes_4}"
lazarus_api="https://gitlab.com/api/v4/projects/freepascal.org%2Flazarus%2Flazarus/repository"
locale_dir="extra/locale"

# tx expects .tx/config in the current directory
cd "$(dirname "$0")"

# HeidiSQL translations
tx pull -a
for po in "$locale_dir"/*.po; do
    msgfmt -o "${po%.po}.mo" "$po"
done

# LCL translations, downloaded into a temporary folder
lcl_dir="$(mktemp -d)"
trap 'rm -rf "$lcl_dir"' EXIT
lcl_files="$(curl -fsS "$lazarus_api/tree?path=lcl/languages&ref=$lazarus_ref&per_page=100" \
    | grep -o '"name":"lclstrconsts\.[^"]*\.po"' | cut -d '"' -f 4)"
for name in $lcl_files; do
    curl -fsS -o "$lcl_dir/$name" "$lazarus_api/files/lcl%2Flanguages%2F$name/raw?ref=$lazarus_ref"
    msgfmt -o "$locale_dir/${name%.po}.mo" "$lcl_dir/$name"
done
