# HeidiSQL Lazarus/FreePascal port
[![Build Status](https://github.com/HeidiSQL/HeidiSQL/actions/workflows/lazarus.yaml/badge.svg?branch=lazarus)](https://github.com/HeidiSQL/HeidiSQL/actions)
[![Supports Windows](https://img.shields.io/badge/support-Windows-blue?logo=Windows)](https://github.com/HeidiSQL/HeidiSQL/releases/latest)
[![Supports Linux](https://img.shields.io/badge/support-Linux-yellow?logo=Linux)](https://github.com/HeidiSQL/HeidiSQL/releases/latest)
[![Supports macOS](https://img.shields.io/badge/support-macOS-black?logo=macOS)](https://github.com/HeidiSQL/HeidiSQL/releases/latest)
[![License](https://img.shields.io/github/license/HeidiSQL/HeidiSQL?logo=github)](https://github.com/HeidiSQL/HeidiSQL/blob/main/LICENSE)
[![Latest Release](https://img.shields.io/github/v/release/HeidiSQL/HeidiSQL?label=latest%20release&logo=github)](https://github.com/HeidiSQL/HeidiSQL/releases/latest)
[![Downloads](https://img.shields.io/github/downloads/HeidiSQL/HeidiSQL/total?logo=github)](https://github.com/HeidiSQL/HeidiSQL/releases)


This is the code base for compiling HeidiSQL on non-Windows platforms, such as Linux. MacOS is probably an option in the future but not yet tried out.

I converted the sources from the master branch, using Lazarus 3.8 and FreePascal 3.2.2. I left away some Windows-only stuff which won't ever work on other platforms, such as some Windows message handlings, and ADO driver usage.

I started the conversion in February 2025, after a short conversation with Alessandro who explained me the advantages of a native Linux version over a Wine app.

Ansgar

![HeidiSQL 12.10.1.92 running on Ubuntu Linux 22.04](https://www.heidisql.com/images/screenshots/linux_version_datagrid.png)

### Building
Install Lazarus and FreePascal. Then load the `.lpi` file in the root directory in the Lazarus IDE. Alternatively, use `/usr/bin/lazbuild heidisql.lpi` on the command line.

### Icons8 copyright
Icons added in January 2019 into a `TImageCollection` component are copyright by [Icons8](https://icons8.com). Used with a special permission from Icons8 given to Ansgar for this project only. Do not copy them for anything else other than building HeidiSQL.

[![Lazarus logo.](https://www.heidisql.com/images/powered-by-lazarus.png)](https://www.lazarus-ide.org/)

