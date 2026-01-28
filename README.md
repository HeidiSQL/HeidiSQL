# HeidiSQL Lazarus/FreePascal port
[![Build Status](https://github.com/HeidiSQL/HeidiSQL/actions/workflows/lazarus.yaml/badge.svg?branch=lazarus)](https://github.com/HeidiSQL/HeidiSQL/actions)
[![Supports Windows](https://img.shields.io/badge/support-Windows-blue?logo=Windows)](https://github.com/HeidiSQL/HeidiSQL/releases/latest)
[![Supports Linux](https://img.shields.io/badge/support-Linux-yellow?logo=Linux)](https://github.com/HeidiSQL/HeidiSQL/releases/latest)
[![Supports macOS](https://img.shields.io/badge/support-macOS-black?logo=macOS)](https://github.com/HeidiSQL/HeidiSQL/releases/latest)
[![License](https://img.shields.io/github/license/HeidiSQL/HeidiSQL?logo=github)](https://github.com/HeidiSQL/HeidiSQL/blob/main/LICENSE)
[![Latest Release](https://img.shields.io/github/v/release/HeidiSQL/HeidiSQL?label=latest%20release&logo=github)](https://github.com/HeidiSQL/HeidiSQL/releases/latest)
[![Downloads](https://img.shields.io/github/downloads/HeidiSQL/HeidiSQL/total?logo=github)](https://github.com/HeidiSQL/HeidiSQL/releases)


This is the code base for compiling HeidiSQL on Linux and macOS. From v13 onwards, the Windows version
will be compiled from here.

Since February 2025 I am migrating the sources from the master branch, using Lazarus and FreePascal. I
left away some Windows-only stuff which won't ever work on other platforms, such as some Windows message
handlings, and ADO driver usage. Therefore, support for MS SQL is being redeveloped via FreeTDS
(formerly ADO), but is not yet fully mature.  

Ansgar

![HeidiSQL GTK2 running on Ubuntu Linux 22.04](https://www.heidisql.com/images/screenshots/linux_version_datagrid.png)

### Building
Install Lazarus 4.4 and FreePascal. Then load the `.lpi` file in the root directory in the Lazarus IDE.
Alternatively, use `/usr/bin/lazbuild heidisql.lpi` on the command line.

### Icons8 copyright
Icons added in January 2019 are copyright by [Icons8](https://icons8.com). Used with a special permission
from Icons8 given to Ansgar for this project only. Do not copy them for anything else other than building
HeidiSQL.

[![Lazarus logo.](https://www.heidisql.com/images/powered-by-lazarus.png)](https://www.lazarus-ide.org/)

