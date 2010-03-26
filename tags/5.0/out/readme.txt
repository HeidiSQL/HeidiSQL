Some infos around HeidiSQL

Project website: http://www.heidisql.com/
Google Code: http://code.google.com/p/heidisql/


*** What is HeidiSQL?

HeidiSQL is an easy-to-use interface and a "working-horse" for web-developers
using the popular MySQL-Database. It allows you to manage and browse your
databases and tables from an intuitive Windows® interface.

With HeidiSQL you will also be able to 
* Generate nice SQL-exports
* Synchronize tables between two databases
* Manage user-privileges
* Import text-files
* Export table-data as CSV, HTML, XML and SQL
* Browse and edit table-data using a comfortable grid
* Create and edit tables, views, stored routines and triggers
* Bulk edit tables (move to db, change engine, collation etc.)
* Batch-insert ascii or binary files into tables
* Write queries with customizable syntax-highlighting and code-completion
* Pretty reformat disordered SQL
* Monitor and kill client-processes
* Connect to servers via commandline
* Find specific text in all tables of all databases of one server
* Optimize and repair tables in a batch manner
* And much more 


*** Requirements:

HeidiSQL runs fine on Windows 2000, XP, Vista and 7. Running HeidiSQL on Wine/Linux
also works fine, apart from some minor graphical glitches, e.g. the transparent
areas of all icons are displayed with black color. 

HeidiSQL does not run on Windows 95/98 or ME, as the Unicode extensions are not
available on these systems.


*** Command line switches

Although HeidiSQL is a pure GUI application, it can be automated for connecting
and opening files via command line parameters. Parameter names are case sensitive
and are based on those used by the MySQL command line applications, e.g. mysqldump.

-d, -description    Session name
-h, -host           Host name
-u, -user           User name
-p, -password       Password
-P, -port           Port (defaults to 3306 if not given)
-S, -socket         Socket name (for connecting via named pipe)

Examples:

* Start over using stored settings from session "xyz":
    c:\path\to\heidisql.exe -d=xyz
    c:\path\to\heidisql.exe -description=xyz

* Connect with different username or port:
    c:\path\to\heidisql.exe -d=xyz -u=OtherUser
    c:\path\to\heidisql.exe -d=xyz -P=3307

* Connect to a non-stored session:
    c:\path\to\heidisql.exe -h=localhost -u=root -p=Mypass -P=3307

* Open multiple .sql files in query tabs:
    c:\path\to\heidisql.exe fileA.sql path\to\fileB.sql fileC.sql ...


*** License:

HeidiSQL is OpenSource and released under GPL (GNU GENERAL PUBLIC LICENSE).

Maybe it saved you a lot of time and therefore you like it. In this case
you may make a donation. For this purpose point your web-browser
to the following URL:
http://www.heidisql.com/donate.php


*** Credits:

Ansgar Becker (project lead and development), David Dindorp (development),
Francisco Ernesto Teixeira (development), Sven Lorenz (graphics), Mikkel M.
Gerhardt-Pedersen (additional bugfixes), Mike Lischke (VirtualTreeView, GLPL),
SynEdit (LGPL), Mark James (Silk icon set), Inno Setup 

1000 thanks to MySQL for great database software
