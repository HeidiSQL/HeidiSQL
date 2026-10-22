#!/usr/bin/env bash
# Build HeidiSQL through lazbuild, with the current commit count of the
# branch injected as BuildNr in heidisql.lpi. The committed BuildNr is 0,
# which means "derive from commit count". Major, minor and revision numbers
# are managed manually in heidisql.lpi and are not touched here.
#
# The original heidisql.lpi is restored byte-exactly afterwards, also when
# lazbuild or this script fails, so no build number is ever committed.
#
# Usage: ./lazbuild-with-buildnr.sh <lazbuild-and-args...>
# Override the injected number with BUILDNR=<n> if ever needed.
set -euo pipefail

cd "$(dirname "$0")"
LPI=heidisql.lpi
BUILDNR="${BUILDNR:-$(git rev-list --count HEAD 2>/dev/null || echo 0)}"

backup="$(mktemp)"
trap 'mv -f "$backup" "$LPI"' EXIT
cp "$LPI" "$backup"

# Avoid "sed -i": its argument handling differs between GNU and BSD sed.
# Instead, transform the backup into the lpi.
sed "s|<BuildNr Value=\"[0-9]*\"/>|<BuildNr Value=\"$BUILDNR\"/>|" "$backup" > "$LPI"
if ! grep -q "<BuildNr Value=\"$BUILDNR\"/>" "$LPI"; then
  echo "ERROR: BuildNr pattern not found in $LPI" >&2
  exit 1
fi
echo "=== Building with BuildNr=$BUILDNR"

"$@"
