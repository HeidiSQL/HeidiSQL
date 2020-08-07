![](https://img.shields.io/github/license/HeidiSQL/HeidiSQL.svg?style=flat)
![](https://img.shields.io/github/release/HeidiSQL/HeidiSQL.svg?style=flat)
![](https://img.shields.io/github/languages/top/HeidiSQL/HeidiSQL.svg?style=flat)
![](https://img.shields.io/github/languages/code-size/HeidiSQL/HeidiSQL.svg?style=flat)

# HeidiSQL
HeidiSQL is a graphical interface for managing [MariaDB](http://www.mariadb.org/) or [MySQL](http://www.mysql.com/) servers, [Microsoft SQL databases](http://www.microsoft.com/sql/), [PostgreSQL](http://www.postgresql.org/) or [SQLite](https://www.sqlite.org/). "Heidi" lets you browse and edit data, create and edit tables, views, procedures, triggers and scheduled events. Also, you can export structure and data, either to SQL file, clipboard or to other servers. Read about [features](https://www.heidisql.com/#featurelist) or see some [screenshots](https://www.heidisql.com/screenshots.php). 

# Need help?
Look at [the online help page](https://www.heidisql.com/help.php) to learn how to use HeidiSQL. The [forum](https://www.heidisql.com/forum.php) is meant to ask questions. The [issue tracker](https://github.com/HeidiSQL/HeidiSQL/issues) is the place to report bugs or request new features.

# Building
Delphi 10.4 is required for building HeidiSQL. Older Delphi versions will most likely fail; newer Delphi versions may work or fail. Unfortunately, Lazarus or one 
of the other free compilers cannot currently compile HeidiSQL.

Once Delphi is installed, you need to load the SynEdit project from the components folder. Build both run-time and design-time packages. Install the 
design-time package. Do the same for the VirtualTree component project, and install madExcept.

Afterwards, load the HeidiSQL project from the packages folder.

# Translation
If you'd like to contribute by translating HeidiSQL into your mother tongue, you need to register at
[Transifex](https://www.transifex.com/heidisql/heidisql/), and join an existing language or request a
new one.

# Contributing to HeidiSQL
* Pull requests will only be accepted for bugfixes. No new features please.
* Please mention a ticket id in your pull request. If there is no ticket for that particular bug yet, go and create an issue request first, and fill out all fields of the issue template.
* To become a developer member, ask Ansgar via email (see https://www.heidisql.com/imprint.php for email address)

# Icons8 copyright
Icons added in January 2019 into a TImageCollection component are copyright by [Icons8](https://icons8.com). Used with a special permission from Icons8 given to Ansgar for this project only. Do not copy them for anything else other than building HeidiSQL.
