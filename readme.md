# HeidiSQL
HeidiSQL is a useful and reliable tool designed for web developers using the popular [MySQL](http://www.mysql.com/) server, [Microsoft SQL databases](http://www.microsoft.com/sql/), [PostgreSQL](http://www.postgresql.org/) and [MariaDB](http://www.mariadb.org/). It enables you to browse and edit data, create and edit tables, views, procedures, triggers and scheduled events. Also, you can export structure and data, either to SQL file, clipboard or to other servers. Read about [features](https://www.heidisql.com/#featurelist) or see some [screenshots](https://www.heidisql.com/screenshots.php). 

Look at [the online help page](http://www.heidisql.com/help.php) for learning how to use HeidiSQL.

# Building
Delphi XE5 is required for building HeidiSQL. Older Delphi versions will most likely fail; newer Delphi versions may work or fail. Unfortunately, Larazus or one 
of the other free compilers cannot currently compile HeidiSQL.

Once Delphi XE5 is installed, you need to load the SynEdit project from the components folder. Build both run-time and design-time packages. Install the 
design-time package. Do the same for the VirtualTree component project, and install madExcept4.

Afterwards, load the HeidiSQL project from the packages folder.
