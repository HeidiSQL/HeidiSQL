#!/usr/bin/env bash
set -euo pipefail

DO_NOTARIZE=false
DO_ADHOC=false
# parse args
for arg in "$@"; do
  case "$arg" in
    --notarize) DO_NOTARIZE=true ;;
    --adhoc) DO_ADHOC=true ;;
  esac
done


### CONFIGURATION

APP_NAME="heidisql"
BUNDLE_NAME="${APP_NAME}.app"
APP_DIR="$(pwd)/${BUNDLE_NAME}"
LPI_FILE="$(pwd)/heidisql.lpi"

# Path to the already built Lazarus executable
if [[ -f "$(pwd)/out/heidisql" ]]; then
  EXECUTABLE_SRC="$(pwd)/out/heidisql"
elif [[ -f "$(pwd)/out/macos/heidisql" ]]; then
  EXECUTABLE_SRC="$(pwd)/out/macos/heidisql"
else
  echo "ERROR: No built executable found in out/ or out/macos/" >&2
  exit 1
fi
EXECUTABLE_TRG="${APP_DIR}/Contents/MacOS/${APP_NAME}"

# Homebrew prefix (auto-detected; override if needed)
BREW_PREFIX="$(brew --prefix 2>/dev/null || echo "/opt/homebrew")"

# Developer ID identity, overridable via environment (CI), as shown by: security find-identity -v -p codesigning
TEAM_ID="${MACOS_TEAM_ID:-QBD4CC6FH3}"
CODESIGN_IDENTITY="${MACOS_CODESIGN_IDENTITY:-Developer ID Application: Ansgar Becker (${TEAM_ID})}"

# Ad-hoc signing (no Developer ID available, e.g. fork CI builds)
if $DO_ADHOC; then
  CODESIGN_IDENTITY="-"
fi

# Name for notarytool keychain profile (store once with notarytool store-credentials)
NOTARY_PROFILE="${MACOS_NOTARY_PROFILE:-notarytool-profile}"

# codesign flags: hardened runtime + secure timestamp only make sense with a real identity
if $DO_ADHOC; then
  SIGN_OPTS=""
else
  SIGN_OPTS="--options runtime --timestamp"
fi

LOCALES_ZIP_URL="https://www.heidisql.com/downloads/locale/HeidiSQL-locale.zip"


### FUNCTION: collect an essential, missing bundle part

# Client libraries and auth plugins are essential for the app bundle: without
# them the affected connection types are broken, so the script collects them
# here and aborts before signing (see check below), instead of quietly
# producing a crippled bundle.
MISSING_LIBS=()
missing_lib() {
  MISSING_LIBS+=("$1")
  echo "WARNING: $1" >&2
}


### FUNCTION: get release version string from Lazarus project file

get_version_from_lpi() {
  local lpi="$1"
  [[ -f "$lpi" ]] || return 1

  local major minor release build
  major=$(xmllint --xpath 'string(//CONFIG/ProjectOptions/VersionInfo/MajorVersionNr/@Value)' "$lpi" 2>/dev/null || true)
  minor=$(xmllint --xpath 'string(//CONFIG/ProjectOptions/VersionInfo/MinorVersionNr/@Value)' "$lpi" 2>/dev/null || true)
  release=$(xmllint --xpath 'string(//CONFIG/ProjectOptions/VersionInfo/RevisionNr/@Value)' "$lpi" 2>/dev/null || true)
  build=$(xmllint --xpath 'string(//CONFIG/ProjectOptions/VersionInfo/BuildNr/@Value)' "$lpi" 2>/dev/null || true)
  # BuildNr=0 means "not committed": derive it from the commit count, so the
  # app bundle version matches what lazbuild-with-buildnr.sh injected into the
  # lpi while building.
  if [[ -z "$build" || "$build" == "0" ]]; then
    build=$(git rev-list --count HEAD 2>/dev/null || echo 0)
  fi

  if [[ -n "$major" && -n "$minor" ]]; then
    if [[ -n "$release" && -n "$build" ]]; then
      echo "${major}.${minor}.${release}.${build}"
    else
      echo "${major}.${minor}"
    fi
    return 0
  fi
  return 1
}

APP_VERSION="$(get_version_from_lpi "$LPI_FILE" || echo "1.0")"


### INSTALL REQUIRED LIBRARIES VIA HOMEBREW

# MySQL client (libmysqlclient.dylib)
brew list mysql-client >/dev/null 2>&1 || brew install mysql-client

# PostgreSQL client (libpq.dylib)
brew list libpq >/dev/null 2>&1 || brew install libpq

# SQLite (libsqlite3.dylib; comes with macOS, but install via brew for consistency)
brew list sqlite >/dev/null 2>&1 || brew install sqlite

# MariaDB Connector/C (libmariadb*.dylib)
brew list mariadb-connector-c >/dev/null 2>&1 || brew install mariadb-connector-c
# FreeTDS (libsybdb.dylib) for MS SQL Server
brew list freetds >/dev/null 2>&1 || brew install freetds


# OpenSSL 1.1 for TFPHTTPClient (deprecated in Homebrew, may be unavailable on CI runners)
if ! brew list openssl@1.1 >/dev/null 2>&1; then
  brew install openssl@1.1 2>/dev/null || echo "WARNING: openssl@1.1 not installable, skipping" >&2
fi

MYSQL_LIB_DIR="${BREW_PREFIX}/opt/mysql-client/lib"
PG_LIB_DIR="${BREW_PREFIX}/opt/libpq/lib"
SQLITE_LIB_DIR="${BREW_PREFIX}/opt/sqlite/lib"
MARIADB_LIB_DIR="${BREW_PREFIX}/opt/mariadb-connector-c/lib"
MYSQL_PLUGIN_DIR="${MYSQL_LIB_DIR}/plugin"
MARIADB_PLUGIN_DIR="${MARIADB_LIB_DIR}/mariadb/plugin"
OPENSSL11_LIB_DIR="${BREW_PREFIX}/opt/openssl@1.1/lib"
FREETDS_LIB_DIR="${BREW_PREFIX}/opt/freetds/lib"

### PREPARE APP BUNDLE STRUCTURE

rm -rf "${APP_DIR}"
mkdir -p "${APP_DIR}/Contents/MacOS"
mkdir -p "${APP_DIR}/Contents/Resources"
mkdir -p "${APP_DIR}/Contents/Frameworks"   # where we will put .dylib and .so files

# Copy main executable
cp "${EXECUTABLE_SRC}" "${EXECUTABLE_TRG}"
chmod +x "${EXECUTABLE_TRG}"
codesign --force ${SIGN_OPTS} --sign "${CODESIGN_IDENTITY}" "${EXECUTABLE_TRG}"

# Minimal Info.plist (adjust identifiers/versions as needed)
cat > "${APP_DIR}/Contents/Info.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key>
  <string>${APP_NAME}</string>
  <key>CFBundleDisplayName</key>
  <string>${APP_NAME}</string>
  <key>CFBundleIdentifier</key>
  <string>com.${APP_NAME}.${APP_NAME}</string>
  <key>CFBundleVersion</key>
  <string>${APP_VERSION}</string>
  <key>CFBundleShortVersionString</key>
  <string>${APP_VERSION}</string>
  <key>CFBundleExecutable</key>
  <string>${APP_NAME}</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
  <key>LSMinimumSystemVersion</key>
  <string>15.0</string>
  <key>NSHighResolutionCapable</key>
  <true/>
  <key>CFBundleIconFile</key>
  <string>heidisql</string>
</dict>
</plist>
EOF


### COPY INI FILES INTO RESOURCES
cp extra/ini/*.ini "${APP_DIR}/Contents/Resources/"
cp res/heidisql.icns "${APP_DIR}/Contents/Resources/" 


### FUNCTION: COPY A DYLIB AND ITS DEPENDENCIES

copy_and_rewrite_dylib() {
  local src_dylib="$1"
  local dest_dir="${APP_DIR}/Contents/Frameworks"
  mkdir -p "${dest_dir}"

  local base
  base="$(basename "${src_dylib}")"
  local dest_dylib="${dest_dir}/${base}"

  # If already copied, skip
  if [[ -f "${dest_dylib}" ]]; then
    return
  fi

  echo "Copying ${src_dylib} -> ${dest_dylib}"
  cp "${src_dylib}" "${dest_dylib}"

  # Make it writable for install_name_tool
  chmod u+w "${dest_dylib}"

  # Change its own install name to @rpath/@loader_path-style inside the app
  install_name_tool -id "@rpath/${base}" "${dest_dylib}"

  # Find direct dependencies
  local deps
  # otool -L output: first line is the file itself, subsequent lines are dependencies
  deps=$(otool -L "${dest_dylib}" | tail -n +2 | awk '{print $1}')

  for dep in ${deps}; do
    case "${dep}" in
      /usr/lib/*|/System/*)
        # System libraries: keep them as-is, do not copy
        continue
        ;;
    esac

    # If dependency already points into @rpath/@loader_path, just rewrite to Frameworks
    local dep_base
    dep_base="$(basename "${dep}")"
    local new_dep_path="@loader_path/../Frameworks/${dep_base}"

    echo "  Rewriting dep ${dep} -> ${new_dep_path}"
    install_name_tool -change "${dep}" "${new_dep_path}" "${dest_dylib}"

    # If dep is an absolute path, copy that dylib too
    if [[ -f "${dep}" ]]; then
      copy_and_rewrite_dylib "${dep}"
    fi
  done
}


### COPY CLIENT LIBRARIES AND DEPENDENCIES

# libmysqlclient.dylib
if ls "${MYSQL_LIB_DIR}"/libmysqlclient*.dylib >/dev/null 2>&1; then
  for f in "${MYSQL_LIB_DIR}"/libmysqlclient*.dylib; do
    copy_and_rewrite_dylib "${f}"
  done
else
  missing_lib "No libmysqlclient*.dylib found in ${MYSQL_LIB_DIR} - MariaDB/MySQL connections would be broken"
fi

# libpq.dylib
if [[ -f "${PG_LIB_DIR}/libpq.dylib" ]]; then
  copy_and_rewrite_dylib "${PG_LIB_DIR}/libpq.dylib"
else
  # libpq often has versioned names
  if ls "${PG_LIB_DIR}"/libpq*.dylib >/dev/null 2>&1; then
    for f in "${PG_LIB_DIR}"/libpq*.dylib; do
      copy_and_rewrite_dylib "${f}"
    done
  else
    missing_lib "No libpq*.dylib found in ${PG_LIB_DIR} - PostgreSQL connections would be broken"
  fi
fi

# libsqlite3.dylib
if [[ -f "${SQLITE_LIB_DIR}/libsqlite3.dylib" ]]; then
  copy_and_rewrite_dylib "${SQLITE_LIB_DIR}/libsqlite3.dylib"
elif ls "${SQLITE_LIB_DIR}"/libsqlite3*.dylib >/dev/null 2>&1; then
  for f in "${SQLITE_LIB_DIR}"/libsqlite3*.dylib; do
    copy_and_rewrite_dylib "${f}"
  done
else
  missing_lib "No libsqlite3*.dylib found in ${SQLITE_LIB_DIR} - SQLite connections would be broken"
fi

# libmariadb*.dylib (MariaDB Connector/C)
if ls "${MARIADB_LIB_DIR}"/libmariadb*.dylib >/dev/null 2>&1; then
  for f in "${MARIADB_LIB_DIR}"/libmariadb*.dylib; do
    copy_and_rewrite_dylib "${f}"
  done
else
  missing_lib "No libmariadb*.dylib found in ${MARIADB_LIB_DIR} - MariaDB/MySQL connections would be broken"
fi

# libsybdb.dylib (FreeTDS, for MS SQL Server)
if ls "${FREETDS_LIB_DIR}"/libsybdb*.dylib >/dev/null 2>&1; then
  for f in "${FREETDS_LIB_DIR}"/libsybdb*.dylib; do
    copy_and_rewrite_dylib "${f}"
  done
else
  missing_lib "No libsybdb*.dylib found in ${FREETDS_LIB_DIR} - MS SQL connections would be broken"
fi

# libfbclient.dylib (Firebird/Interbase client; there is no homebrew formula, so use
# the official Firebird framework from https://firebirdsql.org, either installed on
# this machine, or extracted from its macOS pkg and passed via FBCLIENT_ROOT by the
# CI workflow)
FBCLIENT_ROOT="${FBCLIENT_ROOT:-}"
FBCLIENT_CANDIDATES=(
  "${FBCLIENT_ROOT:+${FBCLIENT_ROOT}/lib/libfbclient.dylib}"
  "/Library/Frameworks/Firebird.framework/Versions/A/Resources/lib/libfbclient.dylib"
  "/Library/Frameworks/Firebird.framework/Versions/A/Libraries/libfbclient.dylib"
  "${BREW_PREFIX}/opt/firebird/lib/libfbclient.dylib"
)
FBCLIENT_SRC=""
for fbclient_candidate in "${FBCLIENT_CANDIDATES[@]}"; do
  [[ -n "${fbclient_candidate}" ]] || continue
  if [[ -f "${fbclient_candidate}" ]]; then
    FBCLIENT_SRC="${fbclient_candidate}"
    break
  fi
done
if [[ -n "${FBCLIENT_SRC}" ]]; then
  # The Firebird client locates its companions relative to itself, detected via
  # dladdr() in Firebird's config_root.cpp: the root directory is the parent of
  # the directory containing libfbclient.dylib, and the client loads its
  # auth/wire-encryption plugins from <root>/plugins, its error message file
  # from <root>/firebird.msg. With the client placed directly in
  # Contents/Frameworks/ (so the session manager's library detection, which
  # scans exactly that directory, lists it, and so the executable's existing
  # rpath @loader_path/../Frameworks makes a bare dlopen("libfbclient.dylib")
  # load it) that root is the app's Contents/ directory: plugins go to
  # Contents/plugins/, firebird.msg to Contents/. The client's companion
  # libraries go to Contents/Frameworks/lib/. The Firebird libraries refer to
  # each other through "@rpath/lib/...", which the app does not use, so those
  # references are rewritten to @loader_path below. firebird.msg and the
  # plugins directory are kept out of Contents/Frameworks, where codesign
  # expects code only and fails on data files with "code object is not signed
  # at all".
  FB_ROOT="$(cd -P "$(dirname "${FBCLIENT_SRC}")/.." && pwd)"
  FB_FW_DIR="${APP_DIR}/Contents/Frameworks"
  echo "Copying ${FBCLIENT_SRC} -> ${FB_FW_DIR}/"
  cp "${FBCLIENT_SRC}" "${FB_FW_DIR}/libfbclient.dylib"
  chmod u+w "${FB_FW_DIR}/libfbclient.dylib"
  for fb_dep in $(otool -L "${FB_FW_DIR}/libfbclient.dylib" | tail -n +2 | awk '/@rpath\/lib\// {print $1}'); do
    install_name_tool -change "${fb_dep}" "@loader_path/lib/$(basename "${fb_dep}")" "${FB_FW_DIR}/libfbclient.dylib"
  done
  mkdir -p "${FB_FW_DIR}/lib"
  for fb_lib in libtommath.dylib libtomcrypt.dylib; do
    if [[ -f "${FB_ROOT}/lib/${fb_lib}" ]]; then
      echo "Copying ${FB_ROOT}/lib/${fb_lib} -> ${FB_FW_DIR}/lib/"
      cp "${FB_ROOT}/lib/${fb_lib}" "${FB_FW_DIR}/lib/"
      chmod u+w "${FB_FW_DIR}/lib/${fb_lib}"
    fi
  done
  # Client-side plugins: authentication (Srp, Legacy_Auth) and wire encryption
  # (ChaCha), loaded from <root>/plugins at runtime, i.e. Contents/plugins/
  if [[ -d "${FB_ROOT}/plugins" ]]; then
    FB_PLUGINS_DEST="${APP_DIR}/Contents/plugins"
    mkdir -p "${FB_PLUGINS_DEST}"
    for fb_plugin in libSrp.dylib libLegacy_Auth.dylib libChaCha.dylib; do
      if [[ -f "${FB_ROOT}/plugins/${fb_plugin}" ]]; then
        echo "Copying ${FB_ROOT}/plugins/${fb_plugin} -> ${FB_PLUGINS_DEST}/"
        cp "${FB_ROOT}/plugins/${fb_plugin}" "${FB_PLUGINS_DEST}/"
        chmod u+w "${FB_PLUGINS_DEST}/${fb_plugin}"
        for fb_dep in $(otool -L "${FB_PLUGINS_DEST}/${fb_plugin}" | tail -n +2 | awk '/@rpath\/lib\// {print $1}'); do
          fb_dep_base="$(basename "${fb_dep}")"
          if [[ "${fb_dep_base}" == "libfbclient.dylib" ]]; then
            # The client sits directly in Contents/Frameworks, not in its lib/
            install_name_tool -change "${fb_dep}" "@loader_path/../Frameworks/${fb_dep_base}" "${FB_PLUGINS_DEST}/${fb_plugin}"
          else
            install_name_tool -change "${fb_dep}" "@loader_path/../Frameworks/lib/${fb_dep_base}" "${FB_PLUGINS_DEST}/${fb_plugin}"
          fi
        done
      fi
    done
  fi
  # Error message file, expected at <root>/firebird.msg. Its data must live in
  # Contents/Resources: a regular data file anywhere else in the bundle makes
  # codesign fail with "code object is not signed at all", while symlinks are
  # not treated as code objects, so it is linked from <root> (the zip step
  # preserves symlinks). Firebird follows it like a regular file.
  if [[ -f "${FB_ROOT}/firebird.msg" ]]; then
    echo "Copying ${FB_ROOT}/firebird.msg -> ${APP_DIR}/Contents/Resources/firebird.msg"
    cp "${FB_ROOT}/firebird.msg" "${APP_DIR}/Contents/Resources/firebird.msg"
    echo "Linking Resources/firebird.msg -> ${APP_DIR}/Contents/firebird.msg"
    ln -s "Resources/firebird.msg" "${APP_DIR}/Contents/firebird.msg"
  fi
else
  missing_lib "No libfbclient.dylib found - Firebird/Interbase connections would be broken. Set FBCLIENT_ROOT, or install /Library/Frameworks/Firebird.framework"
fi

# MARIADB PLUGIN .so FILES
if ls "${MARIADB_PLUGIN_DIR}"/*.so >/dev/null 2>&1; then
  for f in "${MARIADB_PLUGIN_DIR}"/*.so; do
    [[ -f "${f}" ]] || continue
    copy_and_rewrite_dylib "${f}"
  done
else
  missing_lib "MariaDB plugin directory not found: ${MARIADB_PLUGIN_DIR} - MariaDB/MySQL authentication would fail"
fi

# MYSQL PLUGIN .so FILES
if ls "${MYSQL_PLUGIN_DIR}"/*.so >/dev/null 2>&1; then
  for f in "${MYSQL_PLUGIN_DIR}"/*.so; do
    [[ -f "${f}" ]] || continue
    copy_and_rewrite_dylib "${f}"
  done
else
  missing_lib "MySQL plugin directory not found: ${MYSQL_PLUGIN_DIR} - MySQL/MariaDB authentication would fail"
fi

# libssl.1.1.dylib and libcrypto.1.1.dylib (names may vary slightly)
if ls "${OPENSSL11_LIB_DIR}"/libssl*.1.1*.dylib >/dev/null 2>&1; then
  for f in "${OPENSSL11_LIB_DIR}"/libssl*.1.1*.dylib; do
    copy_and_rewrite_dylib "${f}"
  done
else
  echo "WARNING: No libssl*.1.1*.dylib found in ${OPENSSL11_LIB_DIR}" >&2
fi

if ls "${OPENSSL11_LIB_DIR}"/libcrypto*.1.1*.dylib >/dev/null 2>&1; then
  for f in "${OPENSSL11_LIB_DIR}"/libcrypto*.1.1*.dylib; do
    copy_and_rewrite_dylib "${f}"
  done
else
  echo "WARNING: No libcrypto*.1.1*.dylib found in ${OPENSSL11_LIB_DIR}" >&2
fi


### ABORT ON ESSENTIAL MISSING BUNDLE PARTS

# Fail loudly and completely instead of producing a bundle in which some
# connection types break at runtime. OpenSSL 1.1 stays a warning above, as it
# is only needed for the update check and is deprecated in Homebrew anyway.
if [[ ${#MISSING_LIBS[@]} -gt 0 ]]; then
  echo "ERROR: ${#MISSING_LIBS[@]} essential bundle part(s) missing, aborting instead of building a crippled app:" >&2
  for m in "${MISSING_LIBS[@]}"; do
    echo "  - ${m}" >&2
  done
  echo "Hints: brew install mysql-client libpq sqlite mariadb-connector-c freetds; for Firebird, extract the official macOS pkg and pass FBCLIENT_ROOT, or install /Library/Frameworks/Firebird.framework (see .github/workflows/lazarus.yaml)" >&2
  rm -rf "${APP_DIR}"
  exit 1
fi


### FIX MAIN EXECUTABLE’S REFERENCES TO CLIENT LIBS

# Helper: rewrite dependency of the main executable to bundled Frameworks
rewrite_exe_dep () {
  local pattern="$1"   # e.g. libmysqlclient
  local dep
  dep=$(otool -L "${EXECUTABLE_TRG}" | awk "/${pattern}.*dylib/ {print \$1}" | head -n1 || true)
  if [[ -n "${dep}" ]]; then
    local base
    base="$(basename "${dep}")"
    local new="@loader_path/../Frameworks/${base}"
    echo "Rewriting ${EXECUTABLE_TRG} dep ${dep} -> ${new}"
    chmod u+w "${EXECUTABLE_TRG}"
    install_name_tool -change "${dep}" "${new}" "${EXECUTABLE_TRG}"
  fi
}

rewrite_exe_dep "libmysqlclient"
rewrite_exe_dep "libpq"
rewrite_exe_dep "libsqlite3"
rewrite_exe_dep "libmariadb"
rewrite_exe_dep "libsybdb"
rewrite_exe_dep "libssl.3"
rewrite_exe_dep "libssl" # OpenSSL 1.1
rewrite_exe_dep "libcrypto"

### DOWNLOAD AND EXTRACT LOCALE FILES

if [[ -n "${LOCALES_ZIP_URL}" ]]; then
  LOCALE_DEST="${APP_DIR}/Contents/Resources/locale"
  mkdir -p "${LOCALE_DEST}"

  TMP_ZIP="$(mktemp -t heidisql_locales.XXXXXX).zip"
  echo "Downloading locales from ${LOCALES_ZIP_URL}..."
  curl -L --fail --show-error "${LOCALES_ZIP_URL}" -o "${TMP_ZIP}"

  echo "Extracting locales to ${LOCALE_DEST}..."
  /usr/bin/unzip -o "${TMP_ZIP}" -d "${LOCALE_DEST}" >/dev/null

  rm -f "${TMP_ZIP}"
fi


echo "Done. Bundled app is at: ${APP_DIR}"


### SIGN ALL DYLIBS AND THE APP BUNDLE

echo "Signing embedded libraries..."
find "${APP_DIR}/Contents" -type f \( -name "*.dylib" -o -name "*.so" \) | while read -r f; do
  echo "  Signing ${f}"
  codesign --force ${SIGN_OPTS} --sign "${CODESIGN_IDENTITY}" "${f}"
done

echo "Signing main app bundle..."
codesign --force ${SIGN_OPTS} --sign --deep --sign "${CODESIGN_IDENTITY}" "${APP_DIR}"

echo "Verifying code signature..."
codesign --verify --deep --strict --verbose=2 "${APP_DIR}"


### ZIP, NOTARIZE, AND STAPLE

if $DO_ADHOC; then
  echo "Skip notarization (ad-hoc signed bundle cannot be notarized)"
  exit 0
fi

if ! $DO_NOTARIZE; then
  echo "Skip notarization (--notarize not given)"
  exit 0
fi

ZIP_PATH="${APP_DIR}.zip"
rm -f "${ZIP_PATH}"
echo "Zipping app for notarization..."
/usr/bin/zip -r -y "${ZIP_PATH}" "${BUNDLE_NAME}"

echo "Submitting to Apple notary service..."
xcrun notarytool submit "${ZIP_PATH}" \
  --keychain-profile "${NOTARY_PROFILE}" \
  --team-id "${TEAM_ID}" \
  --wait

echo "Stapling notarization ticket..."
xcrun stapler staple "${APP_DIR}"

echo "Notarization complete. Distribute: ${APP_DIR}"
