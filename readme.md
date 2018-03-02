# HeidiSQL
HeidiSQL is a useful and reliable tool designed for web developers using the popular [MySQL](http://www.mysql.com/) server, [Microsoft SQL databases](http://www.microsoft.com/sql/) and [PostgreSQL](http://www.postgresql.org/). It enables you to browse and edit data, create and edit tables, views, procedures, triggers and scheduled events. Also, you can export structure and data, either to SQL file, clipboard or to other servers. Read about [features](https://www.heidisql.com/#featurelist) or see some [screenshots](https://www.heidisql.com/screenshots.php). 

Look at [the online help page](http://www.heidisql.com/help.php) for learning how to use HeidiSQL.

# New user manager window
In this branch we are working on a new user manager. The goal is to make managing user privileges as easy and clear as possible. 

Privileges are inherited in database systems. This means that if you remove the write privileges for a database schema from a user, this applies to all tables in this schema. However, you can give this user write privileges for a single table, which then apply to all columns in this table. Unless you revoke write privileges for a single column.

This inheritance is hierarchically organized and always works from top to bottom. For this reason, in the new user manager we visualize all privileges in a tree structure as you already know them from the main window: All database schemas, tables, columns, procedures and functions are displayed as a tree. The individual privileges appear as checkable columns to the right of them.

Translated with www.DeepL.com/Translator

# Building
Delphi XE5 is required for building HeidiSQL. Older Delphi versions will most likely fail; newer Delphi versions may work or fail. Unfortunately, Larazus or one 
of the other free compilers cannot currently compile HeidiSQL.

Once Delphi XE5 is installed, you need to load the SynEdit project from the components folder. Build both run-time and design-time packages. Install the 
design-time package. Do the same for the VirtualTree component project, and install madExcept4.

Afterwards, load the HeidiSQL project from the packages folder.
