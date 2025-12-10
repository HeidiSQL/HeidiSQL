#!/usr/bin/env bash
set -euo pipefail

### CONFIGURATION #########################################################

APP_NAME="heidisql"
BUNDLE_NAME="${APP_NAME}.app"
APP_DIR="$(pwd)/${BUNDLE_NAME}"

# Path to the already built Lazarus executable
EXECUTABLE_PATH="$(pwd)/out/heidisql"   # change if needed

# Directory that contains your ini files
INI_SOURCE_DIR="$(pwd)/extra/ini"             # change if needed

# List your ini files here (5 files as requested)
INI_FILES=(
  "functions.mariadb.ini"
  "functions.mysql.ini"
  "functions.mysql8.ini"
  "functions.postgresql.ini"
  "functions.redshift.ini"
  "functions.sqlite.ini"
)

# Homebrew prefix (auto-detected; override if needed)
BREW_PREFIX="$(brew --prefix 2>/dev/null || echo "/opt/homebrew")"

### INSTALL REQUIRED LIBRARIES VIA HOMEBREW ##############################

# MySQL client (libmysqlclient.dylib)
brew list mysql-client >/dev/null 2>&1 || brew install mysql-client      # [web:26]

# PostgreSQL client (libpq.dylib)
brew list libpq >/dev/null 2>&1 || brew install libpq                    # [web:28]

# SQLite (libsqlite3.dylib; comes with macOS, but install via brew for consistency)
brew list sqlite >/dev/null 2>&1 || brew install sqlite                  # [web:28]

MYSQL_LIB_DIR="${BREW_PREFIX}/opt/mysql-client/lib"
PG_LIB_DIR="${BREW_PREFIX}/opt/libpq/lib"
SQLITE_LIB_DIR="${BREW_PREFIX}/opt/sqlite/lib"

### PREPARE APP BUNDLE STRUCTURE #########################################

rm -rf "${APP_DIR}"
mkdir -p "${APP_DIR}/Contents/MacOS"
mkdir -p "${APP_DIR}/Contents/Resources"
mkdir -p "${APP_DIR}/Contents/Frameworks"   # where we will put .dylib files [web:12][web:19]

# Copy main executable
cp "${EXECUTABLE_PATH}" "${APP_DIR}/Contents/MacOS/${APP_NAME}"
chmod +x "${APP_DIR}/Contents/MacOS/${APP_NAME}"

# Minimal Info.plist (adjust identifiers/versions as needed) [web:12][web:16]
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
  <string>com.example.${APP_NAME}</string>
  <key>CFBundleVersion</key>
  <string>1.0</string>
  <key>CFBundleShortVersionString</key>
  <string>1.0</string>
  <key>CFBundleExecutable</key>
  <string>${APP_NAME}</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
  <key>LSMinimumSystemVersion</key>
  <string>15.0</string>
</dict>
</plist>
EOF

### COPY INI FILES INTO RESOURCES ########################################

for ini in "${INI_FILES[@]}"; do
  if [[ -f "${INI_SOURCE_DIR}/${ini}" ]]; then
    cp "${INI_SOURCE_DIR}/${ini}" "${APP_DIR}/Contents/Resources/${ini}"
  else
    echo "WARNING: INI file not found: ${INI_SOURCE_DIR}/${ini}" >&2
  fi
done

### FUNCTION: COPY A DYLIB AND ITS DEPENDENCIES ##########################

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

  # Change its own install name to @rpath/@loader_path-style inside the app [web:18]
  install_name_tool -id "@rpath/${base}" "${dest_dylib}"

  # Find direct dependencies
  local deps
  # otool -L output: first line is the file itself, subsequent lines are dependencies [web:18][web:31]
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

### COPY CLIENT LIBRARIES AND DEPENDENCIES ################################

# libmysqlclient.dylib [web:20][web:26]
if ls "${MYSQL_LIB_DIR}"/libmysqlclient*.dylib >/dev/null 2>&1; then
  for f in "${MYSQL_LIB_DIR}"/libmysqlclient*.dylib; do
    copy_and_rewrite_dylib "${f}"
  done
else
  echo "WARNING: No libmysqlclient*.dylib found in ${MYSQL_LIB_DIR}" >&2
fi

# libpq.dylib [web:20]
if [[ -f "${PG_LIB_DIR}/libpq.dylib" ]]; then
  copy_and_rewrite_dylib "${PG_LIB_DIR}/libpq.dylib"
else
  # libpq often has versioned names
  if ls "${PG_LIB_DIR}"/libpq*.dylib >/dev/null 2>&1; then
    for f in "${PG_LIB_DIR}"/libpq*.dylib; do
      copy_and_rewrite_dylib "${f}"
    done
  else
    echo "WARNING: No libpq*.dylib found in ${PG_LIB_DIR}" >&2
  fi
fi

# libsqlite3.dylib [web:20]
if [[ -f "${SQLITE_LIB_DIR}/libsqlite3.dylib" ]]; then
  copy_and_rewrite_dylib "${SQLITE_LIB_DIR}/libsqlite3.dylib"
elif ls "${SQLITE_LIB_DIR}"/libsqlite3*.dylib >/dev/null 2>&1; then
  for f in "${SQLITE_LIB_DIR}"/libsqlite3*.dylib; do
    copy_and_rewrite_dylib "${f}"
  done
else
  echo "WARNING: No libsqlite3*.dylib found in ${SQLITE_LIB_DIR}" >&2
fi

### FIX MAIN EXECUTABLE’S REFERENCES TO CLIENT LIBS #######################

EXE="${APP_DIR}/Contents/MacOS/${APP_NAME}"

# Helper: rewrite dependency of the main executable to bundled Frameworks [web:18]
rewrite_exe_dep () {
  local pattern="$1"   # e.g. libmysqlclient
  local dep
  dep=$(otool -L "${EXE}" | awk "/${pattern}.*dylib/ {print \$1}" | head -n1 || true)
  if [[ -n "${dep}" ]]; then
    local base
    base="$(basename "${dep}")"
    local new="@loader_path/../Frameworks/${base}"
    echo "Rewriting ${EXE} dep ${dep} -> ${new}"
    chmod u+w "${EXE}"
    install_name_tool -change "${dep}" "${new}" "${EXE}"
  fi
}

rewrite_exe_dep "libmysqlclient"
rewrite_exe_dep "libpq"
rewrite_exe_dep "libsqlite3"

echo "Done. Bundled app is at: ${APP_DIR}"
