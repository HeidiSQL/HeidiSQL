# HeidiSQL
HeidiSQL is a useful and reliable tool designed for web developers using the popular [MariaDB](http://www.mariadb.org/) or [MySQL](http://www.mysql.com/) server, [Microsoft SQL databases](http://www.microsoft.com/sql/) or [PostgreSQL](http://www.postgresql.org/). It enables you to browse and edit data, create and edit tables, views, procedures, triggers and scheduled events. Also, you can export structure and data, either to SQL file, clipboard or to other servers. Read about [features](https://www.heidisql.com/#featurelist) or see some [screenshots](https://www.heidisql.com/screenshots.php). 

Look at [the online help page](http://www.heidisql.com/help.php) for learning how to use HeidiSQL.

# Building
Delphi 10.3 is required for building HeidiSQL. Older Delphi versions will most likely fail; newer Delphi versions may work or fail. Unfortunately, Lazarus or one 
of the other free compilers cannot currently compile HeidiSQL.

Once Delphi is installed, you need to load the SynEdit project from the components folder. Build both run-time and design-time packages. Install the 
design-time package. Do the same for the VirtualTree component project, and install madExcept.

Afterwards, load the HeidiSQL project from the packages folder.

# Icons8 copyright
Icons added in January into a TImageCollection component are copyright by [Icons8](https://icons8.com). Used with a special permission from Icons8 given to Ansgar for this project only. Do not copy these for anything else then building HeidiSQL.