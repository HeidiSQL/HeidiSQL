
  Synopse mORMot framework

An Open Source Client-Server ORM/SOA framework
 (c) 2010-2012 Synopse Informatique
  http://synopse.info
  http://mormot.net


Synopse mORMot is a Client-Server ORM and Service Oriented Architecture framework for Delphi 6 up to XE2.

It provides an Open Source self-sufficient set of units (even Delphi stater is enough) for creating any application, up to the most complex Domain-Driven design:

- Presentation layer featuring MVC UI generation with i18n and reporting for rich Delphi clients, or rich AJAX clients;
- Application layer implementing Service Oriented Architecture via interface-based services (like WCF) and Client-Server ORM - following a RESTful model using JSON over several communication protocols (including HTTP/1.1);
- Domain Model layer handling all the needed business logic in plain Delphi objects, including high-level managed types like dynamic arrays or records for Value Objects, or dedicated classes for entities or aggregates;
- Data persistence infrastructure layer with ORM persistence over Oracle, MS SQL, OleDB, with a powerful SQLite3 kernel;
- Cross-Cutting infrastructure layers for handling data filtering and validation, security, caching, logging and testing.

With mORMot, ORM is not used only for data persistence of objects (like in other implementations), but as part of a global n-Tier, Service Oriented Architecture (SOA), ready to implement Domain-Driven solutions. 
This really makes the difference.

The framework Core is non visual (only a set of classes), but you have also some User Interface units available (including reporting and ribbon GUI), and you can use it in any RAD or AJAX clients.

Licensed under a disjunctive tri-license giving you the choice of one of the three following sets of free software/open source licensing terms:
- Mozilla Public License, version 1.1 or later;
- GNU General Public License, version 2.0 or later;
- GNU Lesser General Public License, version 2.1 or later.
This allows the use of our code in as wide a variety of software projects as possible, while still maintaining copyleft on code we wrote.

Main project page:
http://synopse.info/fossil/wiki?name=SQLite3+Framework

How to get the source:
http://synopse.info/fossil/wiki?name=Get+the+source

Download links and documentation:
http://synopse.info/fossil/wiki?name=Downloads

A forum is dedicated to this framework:
http://synopse.info

And a blog is available:
http://blog.synopse.info


Don't forget to download the documentation (available in pdf files, created by our SynProject tool).
In particular, you should take a look at all general introduction chapters of the supplied SAD document. It will cover all key-concepts and code modeling used by the framework.
A developer guide is included in this SAD document, in its first part. You'll get good practice guidance, presentation of the ORM/SOA approach and other underlying concepts, and the multi-tier architecture.

Enjoy!


Quick steps to install mORMot:

0) Read the licenses at http://synopse.info/forum/viewtopic.php?id=27
   Using any part of this project implies that you accept the terms of one of those licenses.

1) Download:
- Latest stable version (less features and bug fixes) from http://synopse.info/fossil/wiki?name=Downloads
- Latest trunk version (preferred) from http://synopse.info/fossil/wiki?name=Get+the+source (do not forget to get also the *.obj files)

2) There is no install program, so unzip the files (including sub folders) to your Delphi component directory.
   Example: D:\Dev\Synopse\

3) Modify the Delphi library path to include:
   D:\Dev\Synopse;D:\Dev\Synopse\SQLite3

4) Compile the project in D:\Dev\Synopse\Sqlite3\TestSQL3.dpr and run it to make sure it passes all tests.
   On some computers, named pipes communication tests may fail - see http://synopse.info/forum/viewtopic.php?id=678
   If you want to run the tests with the fast http.sys kernel-based HTTP server, you'll need to compile and run (as administrator) TestSQL3Register.dpr before launching TestSQL3.dpr

5) Sample programs are found in: D:\Dev\Synopse\Sqlite3\Samples

6) Download the mORMot documentation from http://synopse.info/fossil/wiki?name=Downloads
   In particular, the SAD document is worth reading.

7) After having consulted both the documentation and the existing posts in the forum, feel free to ask your questions in the forum at http://synopse.info


Some units (like SynPdf, SynGdiPlus, SynBigTable, SynCommons, SynDB, SynSQLite3, SQLite3Pages) are used by mORMot, but do not require the whole framework to be used.
That is, you can use e.g. only the PDF generation features of SynPdf, the fast database access classes of SynDB, the static-linked SQLite3 engine of SynSQLite3, the code-generated reports of SQLite3Pages, or the TDynArray / TSynLog classes of SynCommons, without using the main mORMot classes and features (ORM, Client-Server, services, UI, reporting).