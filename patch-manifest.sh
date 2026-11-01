#!/usr/bin/env bash
# Overwrite the manifest resource (ID 1) of the built Windows exe with
# res/manifest.xml, using mt.exe from the Windows SDK.
#
# Why: the Lazarus IDE can only generate a fixed manifest template
# (heidisql.lpi -> XPManifest settings, see w32manifest.pas in the Lazarus
# IDE sources). It cannot emit the <file name="DWrite.dll" .../> element
# that trunk/res/manifest.xml uses to prevent DLL hijacking. This script
# replaces the IDE-generated manifest with our own file, so the Windows
# build has full parity with the Delphi/trunk build.
#
# Must run AFTER the exe is built and BEFORE any Authenticode signing,
# because rewriting a resource invalidates an existing signature.
#
# Usage: ./patch-manifest.sh [path-to-exe]   (default: out/heidisql.exe)
set -euo pipefail

cd "$(dirname "$0")"
EXE="${1:-out/heidisql.exe}"
MANIFEST=res/manifest.xml

if [[ ! -f "$EXE" ]]; then
  echo "patch-manifest: $EXE not found, nothing to do (non-Windows build?)" >&2
  exit 0
fi
if [[ ! -f "$MANIFEST" ]]; then
  echo "patch-manifest: ERROR: $MANIFEST not found" >&2
  exit 1
fi

# Locate the newest mt.exe (x64) from the Windows Kits.
MT=""
for sdk in $(ls "/c/Program Files (x86)/Windows Kits/10/bin" 2>/dev/null | sort -V); do
  candidate="/c/Program Files (x86)/Windows Kits/10/bin/$sdk/x64/mt.exe"
  if [[ -f "$candidate" ]]; then
    MT="$candidate"
  fi
done
if [[ -z "$MT" ]]; then
  echo "patch-manifest: ERROR: no mt.exe found under 'C:\Program Files (x86)\Windows Kits\10\bin'" >&2
  echo "Install the Windows SDK (or Visual Studio) on the build machine." >&2
  exit 1
fi

echo "=== Patching manifest of $EXE with $MANIFEST using $MT"
# The ;#1 must be quoted so the shell does not treat it as a command separator.
"$MT" -nologo -manifest "$MANIFEST" "-outputresource:$EXE;#1"

echo "=== Done. Manifest now embedded in $EXE:"
py -3.12 - "$EXE" <<'PYEOF'
import sys
data = open(sys.argv[1], 'rb').read()
i = data.find(b'<assembly')
end = data.find(b'</assembly>', i) + len(b'</assembly>')
xml = data[i:end].decode('utf-8', 'replace')
for marker in ('DWrite.dll', 'PerMonitorV2', 'longPathAware', 'activeCodePage'):
    print(('  OK  ' if marker in xml else '  MISSING ') + marker)
PYEOF
