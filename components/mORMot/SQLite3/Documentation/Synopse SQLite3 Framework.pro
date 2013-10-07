[People]
;Name=Function,Function details
Arnaud Bouchez=Project Manager,Develop software and manage associated projects

[Project]
Name=Synopse mORMot Framework
Company=Synopse
ReleaseVersion=
ReleaseDate=
Manager=Arnaud Bouchez
MainSection=DI
; so we first check for [DI] and [DILayout]
NoRiskDisplay=Not implemented
DestinationDir=D:\Documents\SynProject
; path to store all created .doc (not to be inside versioning tree)
OldWordOpen=No
; if OldWordOpen=Yes, Conversion is made visible on the screen (compatible with some Word 2000 installations)
DefLang=1033
Logo=logo.png
; this picture will be displayed top of every document front page
HeaderColWidth=22,37,22,19
; page header columns width, according to Manager=The Manager's name
NoConfidential=Yes
; so that no "Confidential" text will appear in page footer - seems convenient for a GPL document ;)
HeaderWithLogo=Yes
; custom page header with the synopse logo

{\b Document License}
THE ATTACHED DOCUMENTS DESCRIBE INFORMATION RELEASED BY SYNOPSE INFORMATIQUE UNDER A GPL 3.0 LICENSE.
{\i Synopse SQLite3/mORMot Framework Documentation}.\line Copyright (C) 2008-2012 Arnaud Bouchez.\line Synopse Informatique - @http://synopse.info
This document is free document; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.
The {\i Synopse mORMot Framework Documentation} is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
You should have received a copy of the GNU General Public License along with this documentation. If not, see @http://www.gnu.org/licenses
{\b Trademark Notice}
Rather than indicating every occurrence of a trademarked name as such, this document uses the names only in an editorial fashion and to the benefit of the trademark owner with no intention of infringement of the trademark.

[Pictures]
synfilevcl.png=1015x666 95%,User Interface generated using VCL components
synfiletms.png=955x610 95%,User Interface generated using TMS components
logo.png=200x47 25%,Synopse Logo
; here are stored the Pictures properties, as PictureFileName=WIDTHxHEIGHT PERCENT%,Caption

[DI]
Owner=DI
Order=DI
Name=Design Input Product Specifications
ItemName=DI
DisplayName=Design Input
Purpose=Create high level description of software specifications
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
; Revision* multiple revision Table: ignored values are taken from current, older below
RevisionDescription=Initial Version
RevisionDate=
Revision=1.17
; [DILayout] list the global DI outline (lines beginning with : are titles)
; [DI-*] details all items
DefaultPreparedBy=Arnaud Bouchez
; all [DI-*] PreparedBy= default name
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose
;SubDocFrontPage=Warning,PeopleDetails

:System Specifications
; special lines begin with
;   :  for titles (no Name=Value pairs after a :title)
;   :# Title   for a later reference as @#@ (":1 Title" then @1@)
;   -  for a list item
;   !  for pascal source
;   !! for modified pascal source line
;   &  for c c++ source
;   &! for modified c c++ source line
;   #  for c# source
;   #! for modified c# source line
;   $  for text file (fixed-width font)
;   $! for modified text file line (fixed-width font)
;   %filename.jpg [640x480 85%] for images jpg/bmp/png/emf - see [Pictures]
;   %%FirmwareBoot for diagram images, i.e. not listed in [Pictures] but created with \graph FirmwareBoot ...
;   |%30%40%30   then |Col 1|Col 2|Col 3  for every row, ending with |%  for columns
;   |%=-30%40%30  -> =:no indent -:no border
;   =[SectionName] to inline the [SectionName] content at this place
; text can be formated as rtf (with \b \i { } e.g.) - each new text paragraph will be ended with \par
; {} can be used for a \par alone (void lines are just ignored)
; you can link to another item with @SectionName@ (@DI-4.1@ e.g) or @DocName@ (@SRS@) or @PeopleName@ (@A.Bouchez@) or either @%Picture.png@
; you can embedd a picture within a table cell e.g., by using @=%picture.png@ - in this case, this is not a "button"
; internet links will be handled as hyperlink, with @http://synopse.info
; in the [SDD-*] sections, specify @Module\filename.pas@ for each file name, @!Module\filename.pas@ for filename modified or @!procedurename!Module\filename.pas@ in order to specify the procedure name. The corresponding units (and procedures) will be highlited in the [SAD] document. Just click on the button to use the Object Browser window.
; some special lines commands can be entered:
;  \page        to force a new page
;  \landscape   to change the page orientation to landscape
;  \portrait    to change the page orientation to portrait
;  \footer blabla  to change the footer text
;  \Layout      to add a list with all DILayout titles
;  \LayoutPage  idem + associated pages in the document
;  \risk        to add the Risk Assessment Scale table
;  \Source      (for [SAD] section) to add the list of the Source=.. modules
;  \SourcePage  idem + associated pages in the document
;  \include filename.ext    ext will be used to append !&#$ left
;  \graph UniqueImageName [Title] then following lines either .dot normal text, either "\From Text\To Text[\Label between both]" - use F12 to have the dialog
;  \TableSoftwareChanges or \TableTraceabilityMatrix for SCRS
;  \TableNewFeatures or \TableBugFixes or \TableTests[=October 16, 2008] for Release Notes
;  \TableDI=6.3.1.2,6.3.1.3 for a table with all the supplied Design Inputs
;  \TableDocuments[=DI,SRS,SDD,SAD] for a table with the supplied document details
;  \Implements TableImplementsName #.# [Description][\DocumentName] (\Implements ISO 4.3 Software safety classification) in the text - points to the current document, or the specified DocumentName
;  \TableImplements=TableImplementsName (\TableImplements=ISO) to create the list, sorted by ascending #.# numbers, with description if any and corresponding document
;  =[SectionName] to include this section content at the current place
; in the [Test-*] sections, special auto-defined columns can be used with |Actions[|Expected Results] - manual tables can be used as usual (with |%..)
This document is intended to describe the Design Input Product Specifications.
: Definitions
{\b Added Value} - This level of achievement should be the target of the design team, because achieving this level of performance adds value to the product. However failure to achieve this level does not evoke additional management review.
{\b Must Have} - This level of achievement must be reached in the final design output. Because of possible negative financial impacts, if this level of performance is not achieved, management review will be triggered.
: Project Concept
:  Purpose and Scope
This document focuses on the {\i Synopse mORMot Framework} library.
The purpose of this @DI@ is to detail the marketing requirements/product specifications for the 1.17 release of the {\i Synopse mORMot Framework library}. The requirements and specifications found in this document are derived from customer market research, regulatory input and industry common practice.
:  Concept Statement
It was observed that a true JSON and RESTful oriented Client-Server framework was missing in the Delphi programing environment.
Latest versions of Delphi (i.e. Delphi 2010/XE/XE2) provide a JSON and RESTful mechanism named DataSnap (in the {\i Architect} or {\i Enterprise} editions), but such a feature could be implemented with previous versions of the Delphi compiler as well, with a more open architecture.
This framework shall use a innovative ORM (Object-relational mapping) approach, based on the RTTI (Runtime Type Information) provided by the Delphi language. It shall expose Server data access and business services to Clients, using JSON over several communication protocols.
After evaluation of most used database engines, the {\i SQLite3} engine was found out to be secure, fast, and perfectly adapted as a stand-alone database engine for this framework, able to access other remote database engines using its unique {\i Virtual Tables} mechanism.
Together with this Client-Server data and business architecture, a set of User Interface components (especially Database Grid and Reporting system) are provided within the framework.
The main approach of this framework is to avoid RAD in the development of projects. RAD has been proved to be a good candidate about prototyping, but is not the best approach for creating a robust and maintainable application. Best practices (as MVC, n-Tier or SOA) shall be used instead.
: Expected Use
Any application which need moderate database usage (up to some GB of data) with easy setup and administration, together with a secure @*ACID@ behavior in a Client-Server environment should consider using the {\i Synopse mORMot Framework}.
: Requirement Exceptions
This framework was developed in order to run under any Delphi compiler, from version Delphi 6 to version Delphi XE2.
It was conceived so that it could be compatible also with the Free Pascal Compiler, which is more advanced than the Embarcadero Delphi compiler for cross-platform support. This support is not tested, but was taken in account during coding.
=[License]
\page
:Software Design Input
The Software @DI@ items follow these main divisions:
\LayoutPage

[DILayout]
; lines beginning with : will be titles for general DI layout - the 'DI-' chars are added before numbers listed below
:Client Server JSON framework
2.1.1
2.1.1.1
2.1.1.2
2.1.2
2.1.3
:SQlite3 engine
2.2.1
2.2.2
2.2.3
:User interface
2.3
2.3.1
2.3.2

[DI-2.1.1]
Risk=1,1,3,Arnaud Bouchez,Initial release
Request=Initial release
Ident=The framework shall be Client-Server oriented

[DI-2.1.1.1]
Risk=1,1,3,Arnaud Bouchez,Initial release
Request=Initial release
Ident=A RESTful mechanism shall be implemented

[DI-2.1.1.2]
Risk=1,1,3,Arnaud Bouchez,Initial release
Request=Initial release
Ident=Commmunication should be available directly in the same process memory, or remotly using Named Pipes, Windows messages or HTTP/1.1 protocols

[DI-2.1.2]
Risk=1,1,3,Arnaud Bouchez,Initial release
Request=Initial release
Ident=UTF-8 JSON format shall be used to communicate

[DI-2.1.3]
Risk=1,1,3,Arnaud Bouchez,Initial release
Request=Initial release
Ident=The framework shall use an innovative ORM (Object-relational mapping) approach, based on classes RTTI (Runtime Type Information)

[DI-2.2.1]
Risk=1,1,3,Arnaud Bouchez,Initial release
Request=Initial release
Ident=The {\i SQLite3} engine shall be embedded to the framework

[DI-2.2.2]
Risk=1,1,3,Arnaud Bouchez,Initial release
Request=Initial release
Ident=The framework libraries, including all its {\i SQLite3} related features, shall be tested using Unitary testing

[DI-2.2.3]
Risk=1,1,3,Arnaud Bouchez,Initial release
Request=Initial release
Ident=The framework shall be able to access any external database, via OleDB, ODBC or direct access for Oracle (OCI) or SQLite3 (for external database files)

[DI-2.3]
Risk=1,1,3,Arnaud Bouchez,Initial release
Request=Initial release
Ident=User Interface and Report generation should be integrated

[DI-2.3.1]
Risk=1,1,3,Arnaud Bouchez,Initial release
Request=Initial release
Ident=An User Interface, with buttons and toolbars shall be easily being created from the code, with no RAD needed, using RTTI and data auto-description

[DI-2.3.2]
Risk=1,1,3,Arnaud Bouchez,Initial release
Request=Initial release
Ident=A reporting feature, with full preview and export as PDF or TXT files, shall be integrated

[RK]
; self-owned Risk Asssessment document
Owner=RK
Order=RK
ItemName=FMEA
DisplayName=Design FMEA
Name=Design FMEA File
DocName=Design FMEA File
; this DocName will be used for generating .DOC filename (instead of ItemName)
Purpose=List {\i Failure Modes and Effects Analysis} (FMEA)
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=1.17
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose
WriteRisk=Yes
; Write Risk assessment table summary after every RK item
WriteTableOfContent=Yes
; Write global Table Of Contents
TableOfContentsAtTheBeginning=Yes
; if the Table of Contents must be at the beginning (default=No=at the end of the file)
DocumentIndex=Pictures,Implements ISO=ISO 123456 requirements
; "Pictures" will create a table with all picture appearing in this document
; "Implements ISO=..." will create a table with all appearing "\Implements ISO 3.4" pages, with the specified item name

:Introduction
The @RK@ is a reference document used to list the {\i Failure Modes and Effects Analysis} (FMEA) identified for the {\i Synopse mORMot Framework} library.
The "{\i Failure modes and effects analysis}" (FMEA) is a procedure in operations management for analysis of potential failure modes within a system for classification by severity or determination of the effect of failures on the system. {\i Failure modes} are any errors or defects in a process, design, or item, especially those that affect the customer, and can be potential or actual. {\i Effects analysis} refers to studying the consequences of those failures.
In practice, a Risk Assessment team starts with a block diagram of a system. The team then considers what happens if each block of the diagram fails, and fills in a table in which failures are paired with their effects and an evaluation of the effects. The design of the system is then corrected, and the table adjusted until the system is known not to have unacceptable problems.
This @RK@ lists most FMEA items identified as possible Software Failure for the {\i Synopse mORMot Framework}.
: Risk Assessment
In the following @RK@, a numerical Risk Assessment is given for every FMEA item, according to the {\i Risk Assessment Scale} table below.
A summary explanation is indicated, together with the names of those who made each evaluation.
\risk
: Responsibilities
- Synopse will try to correct any identified issue;
- The Open Source community will create tickets in a public Tracker web site located at @http://synopse.info/fossil ;
- Synopse work on the framework is distributed without any warranty, according to the chosen license terms;
- This documentation is released under the GPL (GNU General Public License) terms, without any warranty of any kind.
\page
:FMEA
: Fault Tree
Here is the Fault Tree of the framework, displayed in a graphical way:
\graph FTA mORMot Framework Fault Tree
\mORMot Framework\Framework Architecture
\Framework Architecture\Invalid Concurent Access
\Invalid Concurent Access\Database corruption
\Invalid Concurent Access\Wrong Client-Server synchro
\Wrong Client-Server synchro\Enduser problems
\Framework Architecture\Main Server Crashed
\Framework Architecture\Security issue
\Security issue\Enduser problems
\Security issue\Database corruption
\Main Server Crashed\Database corruption
\Database corruption\Enduser problems
\mORMot Framework\User Interface
\User Interface\Security issue
\User Interface\Inconsistent Layout
\Inconsistent Layout\Timeout problems
\Inconsistent Layout\Incorrect User action
\Incorrect User action\Enduser problems
\User Interface\Function not working
\Function not working\Timeout problems
\

[SRS]
Owner=DI
Order=SRS
; Owner: [SRS-*] -> * reference; Order=SRS -> [SRS-*] have sub items
;Refers=RK
; Refers will add all [SRS-RK-*] items to the list, after the DI
Name=Software Requirements Specifications
ItemName=SWRS
Purpose=Interpret design inputs and specify software design features
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=1.17
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
;PreparedBy=..   ignored values are taken from current
; [SRS-*] sections describe each item ([DI] items + other items)
; [SRS-*] are displayed as they appear in the file
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose
WriteRisk=Yes
; Write Risk assessment table summary after every DI
WriteTableOfContent=Yes
; Write global Table Of Contents at the end of the file
TableOfContentsAtTheBeginning=Yes
; if the Table of Contents must be at the beginning (default=No=at the end of the file)
DocumentIndex=Pictures,Implements ISO=ISO 123456 requirements

:Introduction
: Documentation overview
The whole Software documentation process follows the typical steps of this diagram:
\graph FMEADI Design Inputs, FMEA and Risk Specifications
\User¤Requirements\Design Inputs¤(DI)\define
\Regulatory¤Requirements\Design Inputs¤(DI)
\Design Inputs¤(DI)\Specifications¤(SWRS)\are specified by
\System-wide¤Risk Assessment\SW FMEA¤(RK)\defines
\SW FMEA¤(RK)\Specifications¤(SWRS)
\Specifications¤(SWRS)\Architecture + Design¤(SAD+SDD)\is implemented by
\Architecture + Design¤(SAD+SDD)\Test + Documentation\is associated to
\Test + Documentation\Specifications¤(SWRS)\refers to
\
: Purpose
This @SRS@ applies to the first public release of the {\i Synopse mORMot Framework}.
It describes the software implementation of each design input as specified by the @DI@.
The sections of this document follow the @DI@ divisions:
\LayoutPage
;Then all items created from the @RK@ are listed:
;\referspage
For each Design Input item, the corresponding justification is specified, between parenthesis (SCR #65, e.g.).
Every @SRS@ item is named about the corresponding @DI@ item, or, in case the initial {\i Design Input} is too large and must be divided into some {\i SWRS} more precise items, an unique name is proposed.
: Risk Assessment
The Risk assessment indicated below was evaluated as a team work, based on the software solution proposed.
In the following @SRS@, a numerical Risk Assessment is given for every Design Input item, according to the {\i Risk Assessment Scale} table below.
A summary explanation is indicated, together with the names of those who made each evaluation.
\risk
: Responsibilities
- Synopse will try to correct any identified issue;
- The Open Source community will create tickets in a public Tracker web site located at @http://synopse.info/fossil ;
- Synopse work on the framework is distributed without any warranty, according to the chosen license terms;
- This documentation is released under the GPL (GNU General Public License) terms, without any warranty of any kind.

[SRS-DI-2.1.1]
; DI-2.1.1 - The framework shall be Client-Server oriented
ShortName=Client-Server framework

Client–Server model of computing is a distributed application structure that partitions tasks or workloads between service providers, called servers, and service requesters, called clients.
Often clients and servers communicate over a computer network on separate hardware, but both client and server may reside in the same system. A server machine is a host that is running one or more server programs which share its resources with clients. A client does not share any of its resources, but requests a server's content or service function. Clients therefore initiate communication sessions with servers which await (listen for) incoming requests.
The {\i Synopse mORMot Framework} shall implement such a Client-Server model by a set of dedicated classes, over various communication protocols, but in an unified way. Application shall easily change the protocol used, just by adjusting the class type used in the client code. By design, the only requirement is that protocols and associated parameters are expected to match between the Client and the Server.

[SRS-DI-2.1.1.1]
; DI-2.1.1.1 - A RESTful mechanism shall be implemented
ShortName=RESTful framework

REST-style architectures consist of clients and servers, as was stated in @SRS-DI-2.1.1@. Clients initiate requests to servers; servers process requests and return appropriate responses. Requests and responses are built around the transfer of "representations" of "resources". A resource can be essentially any coherent and meaningful concept that may be addressed. A representation of a resource is typically a document that captures the current or intended state of a resource.
In the {\i Synopse mORMot Framework}, so called "resources" are individual records of the underlying database, or list of individual fields values extracted from these databases, by a SQL-like query statement.

[SRS-DI-2.1.1.2]
; DI-2.1.1.2 - Commmunication should be available directly in the same process memory, or remotly using Named Pipes, Windows messages or HTTP/1.1 protocols
ShortName=Communication via diverse protocols

In computing and telecommunications, a protocol or communications protocol is a formal description of message formats and the rules for exchanging those messages.
The {\i Synopse mORMot Framework} shall support the following protocols for remote access, according to the Client-Server architecture defined in @SRS-DI-2.1.1@:
- Direct in-process communication;
- Using GDI messages;
- Using Named pipe;
- Using HTTP/1.1 over TCP/IP.

[SRS-DI-2.1.1.2.1]
Parent=DI-2.1.1.2
Ident=Client-Server Direct communication shall be available inside the same process
ShortName=In-Process communication

[SRS-DI-2.1.1.2.2]
Parent=DI-2.1.1.2
Ident=Client-Server Named Pipe communication shall be made available by some dedicated classes
ShortName=Named Pipe protocol

[SRS-DI-2.1.1.2.3]
Parent=DI-2.1.1.2
Ident=Client-Server Windows GDI Messages communication shall be made available by some dedicated classes
ShortName=Windows Messages protocol

[SRS-DI-2.1.1.2.4]
Parent=DI-2.1.1.2
Ident=Client-Server HTTP/1.1 over TCP/IP protocol communication shall be made available by some dedicated classes, and ready to be accessed from outside any Delphi Client (e.g. the implement should be AJAX ready)
ShortName=HTTP/1.1 protocol

[SRS-DI-2.1.2]
; DI-2.1.2 - UTF-8 JSON format shall be used to communicate

JSON, as defined in the @SAD@, is used in the {\i Synopse mORMot Framework} for all Client-Server communication. JSON (an acronym for JavaScript Object Notation) is a lightweight text-based open standard designed for human-readable data interchange. Despite its relationship to JavaScript, it is language-independent, with parsers available for virtually every programming language.
JSON shall be used in the framework for returning individual database record content, in a disposition which could make it compatible with direct JavaScript interpretation (i.e. easily creating JavaScript object from JSON content, in order to facilitate AJAX application development). From the Client to the Server, record content is also JSON-encoded, in order to be easily interpreted by the Server, which will convert the supplied field values into proper SQL content, ready to be inserted to the underlying database.
JSON should be used also within the transmission of request rows of data. It therefore provide an easy way of data formating between the Client and the Server.
The {\i Synopse mORMot Framework} shall use UTF-8 encoding for the character transmission inside its JSON content. UTF-8 (8-bit Unicode Transformation Format) is a variable-length character encoding for Unicode. UTF-8 encodes each character (code point) in 1 to 4 octets (8-bit bytes). The first 128 characters of the Unicode character set (which correspond directly to the ASCII) use a single octet with the same binary value as in ASCII. Therefore, UTF-8 can encode any Unicode character, avoiding the need to figure out and set a "code page" or otherwise indicate what character set is in use, and allowing output in multiple languages at the same time. For many languages there has been more than one single-byte encoding in usage, so even knowing the language was insufficient information to display it correctly.

[SRS-DI-2.1.3]
; DI-2.1.3 - The framework shall use an innovative ORM (Object-relational mapping) approach, based on classes RTTI (Runtime Type Information)

ORM, as defined in the @SAD@, is used in the {\i Synopse mORMot Framework} for accessing data record fields directly from Delphi Code.
Object-relational mapping (ORM, O/RM, and O/R mapping) is a programming technique for converting data between incompatible type systems in relational databases and object-oriented programming languages. This creates, in effect, a "virtual object database" that can be used from within the Delphi programming language.
The {\f1\fs20 published} properties of classes inheriting from a new generic type named {\f1\fs20 TSQLRecord} are used to define the field properties of the data. Accessing database records (for reading or update) shall be made by using these classes properties, and some dedicated Client-side methods.

[SRS-DI-2.2.1]
; DI-2.2.1 - The {\i SQLite3} engine shall be embedded to the framework

The {\i SQLite3} database engine is used in the {\i Synopse mORMot Framework} as its kernel database engine. {\i SQLite3} is an ACID-compliant embedded relational database management system contained in a C programming library.
This library shall be linked statically to the {\i Synopse mORMot Framework}, and interact directly from the Delphi application process.
The {\i Synopse mORMot Framework} shall enhance the standard {\i SQLite3} database engine by introducing some new features stated in the @SAD@, related to the Client-Server purpose or the framework - see @SRS-DI-2.1.1@.

[SRS-DI-2.2.2]
; DI-2.2.2 - The framework libraries, including all its {\i SQLite3} related features, shall be tested using Unitary testing

The {\i Synopse mORMot Framework} shall use all integrated Unitary testing features provided by a common testing framework integrated to all Synopse products. This testing shall be defined by classes, in which individual published methods define the actual testing of most framework features.
All testing shall be run at once, for example before any software release, or after any modification to the framework code, in order to avoid most regression bug.

[SRS-DI-2.2.3]
; DI-2.2.3 - The framework shall be able to access any external database, via OleDB or direct access for Oracle (OCI) or SQLite3 (for external database files)

The following external database providers shall be made available to the framework ORM:
- Any {\i OleDB} provider;
- Any {\i ODBC} provider;
- {\i Oracle} database, via direct OCI client access;
- {\i SQlite3} database engine.
A dedicated set of classes shall implement access, together with some advanced syntaxic sugar, like fast late-binding, or advanced ORM mechanism, like Virtual Tables.

[SRS-DI-2.3]
; DI-2.3 - User Interface and Report generation should be integrated

The {\i Synopse mORMot Framework} shall provide User Interface and Report generation from code.
Such a ribbon-oriented interface shall be made available, in a per-table approach, and associated reports.
Here is a sample of screen content, using proprietary TMS components:
%synfiletms.png
And here is the same application compiled using only VCL components, available from Delphi 6 up to XE2:
%synfilevcl.png

[SRS-DI-2.3.1]
; DI-2.3.1 - An User Interface, with buttons and toolbars shall be easily being created from the code, with no RAD needed, using RTTI and data auto-description

The {\i Synopse mORMot Framework} shall provide some User-Interface dedicated units, allowing database grid display, on screen tool-bar creation (by an internal system of actions using Delphi RTTI - see the corresponding paragraph in the @SAD@), and integrated reporting - see @SRS-DI-2.3.2@.
No RAD approach is to be provided: the Client application User Interface will be designed not by putting components on the IDE screen, but directly from code.

[SRS-DI-2.3.1.1]
Parent=DI-2.3.1
Ident=A Database Grid shall be made available to provide data browsing in the Client Application - it shall handle easy browsing, by column resizing and sorting, on the fly customization of the cell content
ShortName=Database Grid Display

[SRS-DI-2.3.1.2]
Parent=DI-2.3.1
Ident=Toolbars shall be able to be created from code, using RTTI and enumerations types for defining the action
ShortName=RTTI generated Toolbars

[SRS-DI-2.3.1.3]
Parent=DI-2.3.1
Ident=Internationalization (i18n) of the whole User Interface shall be made available by defined some external text files: Delphi resourcestring shall be translatable on the fly, custom window dialogs automaticaly translated before their display, and User Interface generated from RTTI should be included in this i18n mechanism
ShortName=Automated i18n

[SRS-DI-2.3.2]
; DI-2.3.2 - A reporting feature, with full preview and export as PDF or TXT files, shall be integrated

The {\i Synopse mORMot Framework} shall provide a reporting feature, which could be used stand-alone, or linked to its database mechanism. Reports shall not be created using a RAD approach (e.g. defining bands and fields with the mouse on the IDE), but shall be defined from code, by using some dedicated methods, adding text, tables or pictures to the report. Therefore, any kind of report shall be generated.
This reports shall be previewed on screen, and exported as PDF or TXT on request.

[Risk]
Owner=SRS
Order=SRS
; Owner: [Risk-*] -> * reference; Order=SRS -> [Test-*] have no sub items
Name=Software Implementation Risk Assessment
DocName=Risk Assessment
; this DocName will be used for generating .DOC filename (instead of ItemName)
Purpose=Perform a risk assessment of the SWRS implementation
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=1.17
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
; rev.1 summary values are in [DI] Risk=Severity,Probability,Occurence,Comment
; [Risk-*] sections contain rev.2 details for each SRS, [Risk-SER-03] or [Risk-4.5.1] e.g.
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose,RiskTable

[SAD]
Owner=SRS
Order=SRS
; Owner: [SAD-*] -> * reference; Order=SRS -> [SAD-*] have no sub items
Name=Software Architecture Design
Purpose=Describe the implications of each software requirement specification on all the affected software modules
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=1.17
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
; [SAD-*] sections contain details for each SRS, [SAD-SER-03] or [SAD-DI-4.10.6] e.g.
; [SAD-*] are displayed as they appear in the [SRS-*] sections
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose
Source=Main,SynFile
; presence of Source=.. adds global description for each SourceFile=.. and for @Module\filename.pas@ for the project (TProject.Parse: array of TSection - [SAD-Module] will get files from @Module\name.pas@, e.g.)
SourceSDD=SDD
; name of the [SDD] section to be parsed for modified files as @!Module\bidule.pas@
DefaultPath=D:\Dev
; global default directory to trim in the documentation, and used by default in [SAD-module] SourcePath=..
WriteTableOfContent=Yes
; Write global Table Of Contents of the file
TableOfContentsAtTheBeginning=Yes
; if the Table of Contents must be at the beginning (default=No=at the end of the file)
DocumentIndex=Pictures,Source,Index
; "Source" will create a table with all @source.code@ files within this document
; "Index" an index of all @*keyword@, Picture of all %pictures
WithAllfields=Yes
; write all field properties of all object and classes: document is huge, but contains all available architecture intel

:Introduction
The whole Software documentation process follows the typical steps of this diagram:
%%FMEADI
: Purpose
This @SAD@ applies to the 1.17 release of the {\i Synopse mORMot Framework} library.
It summarizes the implications of every software features detailed in the @SDD@.
:  Document layout
In a first part, this document presents the global Software architecture involved in this implementation, i.e.:
\Sourcepage
A second part then describes every @SRS@ item, according to the @DI@ main sections:
\LayoutPage
:  First part: global Software architecture
All the source code architecture of the library is deeply analyzed. After a global introduction, each source code unit is detailed, with clear diagrams and tables showing the dependencies between the units, and the class hierarchy of the objects implemented within.
The main sections of this architecture description are the following:
- Architecture principles - see @40@;
- General design - see @41@;
- ORM and MVC - see @3@;
- Database layer - see @42@;
- Client-Server - see @6@;
- Security and testing - see @43@;
- Source code - see @44@;
- The {\f1\fs20 SynCommons} unit - see @45@;
- Followed by the per-unit description of every defined class or type;
- {\i SynFile} main demo - see @50@.
:  Second part: SWRS implications
For each SWRS item, links are made to the units sections of the first part, referring directly to the unit, class or function involved with the @SDD@.
: Responsibilities
- Synopse will try to correct any identified issue;
- The Open Source community will create tickets in a public Tracker web site located at @http://synopse.info/fossil ;
- Synopse work on the framework is distributed without any warranty, according to the chosen license terms - see @34@;
- This documentation is released under the GPL ({\i GNU @*General Public License@}) terms, without any warranty of any kind.
=[GPL]

[SAD-Source]
; this Section body is added as the introduction of the document first part
DisplayName=mORMot Framework Overview

:Synopse mORMot Framework Overview
{\i Synopse mORMot} is a @*Client-Server@ @*ORM@ and Service Oriented Architecture framework (@*SOA@) for Delphi 6 up to XE2.
It provides an Open Source {\i self-sufficient set of units} (even Delphi starter edition is enough) for creating any {\i Multi-@*tier@} application, up to the most complex {\i @*Domain-Driven@} design - see @54@:
- {\i Presentation layer} featuring @*MVC@ UI generation with @*i18n@ and reporting for rich Delphi clients, or rich @*AJAX@ clients;
- {\i Application layer} implementing Service Oriented Architecture via {\f1\fs20 interface}-based services (like @*WCF@) and Client-Server ORM - following a @*REST@ful model using @*JSON@ over several communication protocols (including @*HTTP@/1.1);
- {\i Domain Model layer} handling all the needed business logic in plain Delphi objects, including high-level managed types like dynamic arrays or records for Value Objects, or dedicated classes for entities or aggregates;
- {\i Data persistence infrastructure layer} with ORM persistence over @*Oracle@, @*MS SQL@, @*OleDB@, @*ODBC@ with a powerful @*SQLite3@ kernel, and direct @*SQL@ access if needed;
- {\i Cross-Cutting infrastructure layers} for handling data filtering and validation, @*security@, @*session@, @*cache@, logging and @*test@ing (framework uses test-driven approach).
If you do not know some of those concepts, don't worry: this document will detail them - see @40@.
The main two features of {\i mORMot} shine at the application layer:
- Client-Server {\i ORM}: objects persistence and remote access;
- Client-Server {\i Services}: remote call of high-level data process.
With {\i mORMot}, {\i ORM} is not used only for data persistence of objects in databases (like in other implementations), but as part of a global n-@*Tier@, Service Oriented Architecture, ready to implement {\i Domain-Driven} solutions.\line This really makes the difference.
The business logic of your applications will be easily exposed as {\i Services}, and will be accessible from light clients (written in Delphi or any other mean, including AJAX).
The framework Core is non-visual: it provides only a set of classes to be used from code. But you have also some UI units available (including screen auto-creation, reporting and ribbon GUI), and you can use it from any RAD or AJAX clients.
No dependency is needed at the client level (no DB driver, nor third-party runtime): it is able to connect via standard HTTP, even through a corporate proxy or a VPN. Rich Delphi clients can be deployed just by copying and running a stand-alone small executable, with no installation process.\line Speed and scalability has been implemented from the ground up - see @59@: a single server is able to handle a lot of clients, and our rich SOA architecture is able to implement both vertical and horizontal scalable @*hosting@.
In short, with {\i mORMot}, your ROI is maximized.
: Highlights
At first, some points can be highlighted, which make this framework distinct to other available solutions:
- @*Client-Server@ orientation, with optimized request caching and intelligent update over a @*REST@ful architecture - but can be used in stand-alone applications;
- No RAD components, but true @*ORM@ and @*SOA@ approach;
- Multi-@*Tier@ architecture, with integrated @*Business rules@ as fast ORM-based classes (not via external scripting or such) and @*Domain-Driven@ design;
- {\i Service-Oriented-Architecture} model, using custom RESTful JSON services - you can send as JSON any {\f1\fs20 TStrings, TCollection, TPersistent} or {\f1\fs20 TObject} (via registration of a custom serializer) instance, or even a {\i dynamic array}, or any record content, with integrated JSON @*serialization@, via an @*interface@-based contract shared on both client and server sides;
- Truly RESTful authentication with a dual @*security@ model (session + per-query);
- Very fast @*JSON@ producer and parser, with caching at SQL level;
- Fastest available @*HTTP@ server using {\i http.sys} kernel-mode server - but may communicate via named pipes, GDI messages or in-process as lighter alternatives;
- Using {\i SQLite3} as its kernel, but able to connect to any other database (via @*OleDB@ / @*ODBC@ or direct client library access e.g. for @*Oracle@) - the {\f1\fs20 SynDB} classes are self-sufficient, and do not depend on the Delphi DB unit nor any third-party (so even the Delphi Starter edition is enough);
- Ability to use @*SQL@ and RESTful requests over multiple databases at once (thanks to {\i SQLite3} unique @*Virtual Table@s mechanism);
- Full @*Text Search@ engine included, with enhanced Google-like ranking algorithm;
- Direct User Interface generation: grids are created on the fly, together with a modern Ribbon ('Office 2007'-like) screen layout - the code just has to define actions, and assign them to the tables, in order to construct the whole interface from a few lines of code, without any IDE usage;
- Integrated @*Report@ing system, which could serve complex @*PDF@ reports from your application;
- Designed to be as fast as possible (asm used when needed, buffered reading and writing avoid most memory consumption, multi-thread ready architecture...) so benchmarks sound impressive when compared to other solutions - see @59@;
- More than 1000 pages of documentation;
- Delphi and @*AJAX@ clients can share the same server;
- Full source code provided - so you can enhance it to fulfill any need;
- Works from Delphi 6 up to XE2, truly Unicode (uses UTF-8 encoding in its kernel, just like JSON), with any version of Delphi (no need to upgrade your IDE).
: mORMot
Why is this framework named {\i mORMot}?
- Because its initial identifier was "{\i Synopse SQLite3 database framework}", which may induce a {\i SQLite3}-only library, whereas the framework is now able to connect to any database engine;
- Because we like mountains, and those large ground rodents;
- Because marmots do hibernate, just like our precious objects;
- Because even if they eat greens, they use to fight at Spring;
- Because it may be an acronym for "Manage Object Relational Mapping Over Tables", or whatever you may think of...
\page
:40Architecture principles
This framework tries to implement some "best-practice" pattern, among them:
- {\i Model-View Controller} - see @10@;
- {\i Multi-@*tier@ architecture} - see @7@;
- {\i Unit @*test@ing} - see @12@;
- {\i Object-relational mapping} - see @13@;
- {\i @*Service@-oriented architecture} - see @17@;
- {\i @*Stateless@} @*CRUD@/@*REST@ - see @9@.
All those points render possible any project implementation, up to complex @*Domain-Driven@ design - see @54@.
:10 Model-View-Controller
The {\i Model-View-Controller} (@**MVC@) is a software architecture, currently considered an architectural pattern used in software engineering. The pattern isolates "domain logic" (the application logic for the user) from the user interface (input and presentation), permitting independent development, testing and maintenance of each (separation of concerns).
The {\b model} manages the behavior and data of the application domain, responds to requests for information about its state (usually from the view), and responds to instructions to change state (usually from the controller). In event-driven systems, the model notifies observers (usually views) when the information changes so that they can react - but since our ORM is @*stateless@, it does not need to handle those events - see @15@.
The {\b view} renders the model into a form suitable for interaction, typically a user interface element. Multiple views can exist for a single model for different purposes. A {\i viewport} typically has a one to one correspondence with a display surface and knows how to render to it.
The {\b controller} receives user input and initiates a response by making calls on model objects. A controller accepts input from the user and instructs the model and viewport to perform actions based on that input.
\graph MVCModel Model View Controller concept
rankdir=LR;
\Model\View\indirect¤association
\Controller\Model\direct¤association
\View\Controller\indirect¤association
\Controller\View\direct¤association
\
In the framework, the {\i model} is not necessarily merely a database; the {\i model} in MVC is both the data and the business/domain logic needed to manipulate the data in the application. In our ORM, a model is implemented via a {\f1\fs20 @*TSQLModel@} class, which centralizes all {\f1\fs20 @*TSQLRecord@} inherited classes used by an application, both database-related and business-logic related.
The {\i view} is currently only all the User-Interface part of the framework, which is mostly auto-generated from code. It will use the {\i model} as reference for rendering the data. @*AJAX@ clients can also be used - @*REST@ful and @*JSON@ will make it easy.
The {\i controller} is mainly already implemented in our framework, within the RESTful commands, and will interact with both the associated {\i view} (e.g. for refreshing the User Interface) and {\i model} (for data handling). Some custom actions, related to the business logic, can be implemented via some custom {\f1\fs20 TSQLRecord} classes or via custom RESTful @*Service@s - see @11@.
:7 Multi-tier architecture
In software engineering, multi-@**tier@ architecture (often referred to as {\i n-tier} architecture) is a client–server architecture in which the presentation, the application processing, and the data management are logically separate processes. For example, an application that uses middle-ware to service data requests between a user and a database employs multi-tier architecture. The most widespread use of multi-tier architecture is the three-tier architecture.
Both @*ORM@ and @*SOA@ aspects of our @*REST@ful framework make it easy to develop using such a three-tier architecture.
The {\i Synopse mORMot Framework} follows this development pattern:
- {\b Data Tier} is either {\i @*SQLite3@} and/or an internal very fast in-memory database; most @*SQL@ queries are created on the fly, and database table layout are defined from Delphi classes; you can also use external databases (as {\i @*MS SQL@ Server} or {\i @*Oracle@}) - see @27@;
- {\b Logic Tier} is performed by pure ORM aspect and SOA implementation: you write Delphi classes which are mapped by the {\i Data Tier} into the database, and you can write your business logic as Services called as Delphi {\f1\fs20 interface}, up to a @*Domain-Driven@ design - see @54@ - if your project reaches some level of complexity;
- {\b Presentation Tier} is either a Delphi Client, either an @*AJAX@ application, because the framework can communicate using @*REST@ful @*JSON@ over @*HTTP@/1.1 (the Delphi Client User Interface is generated from Code, by using @*RTTI@ and structures, not as a RAD - and the Ajax applications need to be written by using your own tools and @*JavaScript@ framework, there is no "official" Ajax framework included yet).
:17 Service-oriented architecture
@*Service@-oriented architecture (@**SOA@) is a flexible set of design principles used during the phases of systems development and integration in computing. A system based on a SOA will package functionality as a suite of inter-operable services that can be used within multiple, separate systems from several business domains.
The SOA implementations rely on a mesh of software services. Services comprise unassociated, {\i loosely coupled} units of functionality that have no calls to each other embedded in them. Each service implements {\i one action}, such as filling out an online application for an account, or viewing an online bank statement, or placing an online booking or airline ticket order. Rather than services embedding calls to each other in their source code, they use defined protocols that describe how services pass and parse messages using description meta-data.
For more details about SOA, see @http://en.wikipedia.org/wiki/Service-oriented_architecture
SOA and ORM - see @13@ - do not exclude themselves. In fact, even if some software architects tend to use only one of the two features, both can coexist and furthermore complete each other, in any @*Client-Server@ application:
- ORM access could be used to access to the data with objects, that is with the native presentation of the Server or Client side (Delphi, @*JavaScript@...) - so ORM can be used to provide efficient access to the data or the business logic;
- SOA will provide a more advanced way of handling the business logic: with custom parameters and data types, it's possible to provide some high-level Services to the clients, hiding most of the business logic, and reducing the needed bandwidth.
In particular, SOA will help leaving the business logic on the Server side, therefore will help increasing the @7@. By reducing the back-and-forth between the Client and the Server, it will also reduce the network bandwidth, and the Server resources (it will always cost less to run the service on the Server than run the service on the Client, adding all remote connection and @*serialization@ to the needed database access). Our @*interface@-based SOA model allows the same code to run on both the client and the server side, with a much better performance on the server side, but a full interoperability of both sides.
:13 Object-relational mapping
Before defining what is an @**ORM@, let's face one drawback of using a database via an object-oriented language like {\i Delphi} is that you must have your objects interact with the database. Some implementation schemes are possible, in which are some pros and cons. The table below is a very suggestive (but it doesn't mean wrong) {\i Resumé} of some common schemes, in the {\i Delphi} world. @*ORM@ is just one nice possibility among others.
|%17%40%44
|\b\qc Scheme|Pros|Cons\b0
|Use DB views and tables, with GUI components|- @*SQL@ is a powerful language\line - Can use high-level DB tools (UML) and RAD approach|- Business logic can't be elaborated without stored procedures\line - SQL code and stored procedures will bind you to a DB engine\line - Poor Client interaction\line - Reporting must call the DB directly\line - No Multi-tier architecture
|Map DB tables or views with Delphi classes|- Can use elaborated business logic, in Delphi\line - Separation from UI and data|- SQL code must be coded by hand and synchronized with the classes\line - Code tends to be duplicated\line - SQL code could bind you to a DB engine\line - Reports can be made from code or via DB related tools\line - Difficult to implement true Multi-tier architecture
|Use a Database ORM|- Can use very elaborated business logic, in Delphi\line - SQL code is generated (in most cases) by the ORM\line - ORM will adapt the generated SQL to the DB engine|- More abstraction needed at design time (no RAD approach)\line - In some cases, could lead to retrieve more data from DB than needed\line - Not yet a true Multi-tier architecture, because ORM is for DB access only and business logic will need to create separated classes
|Use a @*Client-Server@ ORM|- Can use very elaborated business logic, in Delphi\line - SQL code is generated (in most cases) by the ORM\line - ORM will adapt the generated SQL to the DB engine\line - Services will allow to retrieve or process only needed data\line - Server can create objects viewed by the Client as if they were DB objects, even if they are only available in memory or the result of some business logic defined in Delphi\line - Complete Multi-tier architecture|- More abstraction needed at design time (no RAD approach)
|%
Of course, you'll find out that this framework implements a Client-Server ORM.
:54 Domain-Driven design
The "official" definition of @**Domain-driven@ design is supplied at  @http://domaindrivendesign.org :
{\i Over the last decade or two, a philosophy has developed as an undercurrent in the object community. The premise of domain-driven design is two-fold:}
- {\i For most software projects, the primary focus should be on the domain and domain logic;}
- {\i Complex domain designs should be based on a model.}
{\i Domain-driven design is not a technology or a methodology. It is a way of thinking and a set of priorities, aimed at accelerating software projects that have to deal with complicated domains.}
It can be implemented in a kind of @7@. In this case, we are talking about {\i N-Layered Domain-Oriented Architecture}. It involves a common representation splitting the {\i Logic Tier} into two layers, i.e. {\i Application layer} and {\i Domain Model layer}. Of course, this particular layered architecture is customizable according to the needs of each project. We simply propose following an architecture that serves as a baseline to be modified or adapted by architects according to their needs and requirements.
It could therefore be presented as in the following model:
|%30%50
|\b Layer|Description\b0
|Presentation|MVC UI generation and reporting
|Application|Services and high-level adapters
|Domain Model|Where business logic remains
|Data persistence|ORM and external services
|Cross-Cutting|Horizontal aspects shared by other layers
|%
In fact, this design matches perfectly the RESTful SOA approach of the {\i Synopse mORMot framework}.
See the following diagram:
\graph mORMotDesign0 N-Layered Domain-Oriented Architecture of mORMot
subgraph cluster1 {
\AJAX\REST Client
\UI\REST Client
\UI\i18n
\Reporting\i18n
\Reporting\REST Client
label="Presentation";
}
subgraph cluster2 {
"REST Server";
label="Application";
}
\REST Client\REST Server\HTTP 1.1
subgraph cluster3 {
\ORM\SQLite3
\ORM\External DB
\SQLite3\External DB
label="Data persistence";
}
subgraph cluster4 {
\REST Server\Services
\REST Server\ORM
\Services\ORM
label="Domain Model";
}
subgraph cluster5 {
label="Cross-Cutting";
"Logging";
\UI\Filtering¤Validation
\Services\Filtering¤Validation
\REST Server\Security¤Sessions
\Services\Security¤Sessions
\REST Client\Cache
\ORM\Cache
}
\
In fact, since a @17@ tends to ensure that services comprise unassociated, loosely coupled units of functionality that have no calls to each other embedded in them, we may define two levels of services, implemented by two {\f1\fs20 interface} factories, using their own @*hosting@ and communication:
- One set of services at {\i Application layer}, to define the uncoupled contracts available from Client applications;
- One set of services at {\i Domain Model layer}, which will allow all involved domains to communicate with each other, without exposing it to the remote clients.
Therefore, those layers could be also implemented as such:
\graph mORMotDesign0b Alternate Domain-Oriented Architecture of mORMot
rankdir=LR;
subgraph cluster2 {
\Application¤REST Server\Application¤Services
\Application¤Services\Domain¤REST Client
label="Application";
}
\Application¤REST Client\Application¤REST Server\HTTP 1.1
subgraph cluster4 {
\Domain¤REST Client\Domain¤REST Server\Fast¤RPC
\Domain¤REST Server\Domain¤Services
label="Domain Model";
}
\Domain¤REST Server\ORM
\Domain¤Services\ORM
\
Due to the @*SOLID@ design of mORMot - see @47@ - you can use as many Client-Server services layers as needed in the same architecture (i.e. a Server can be a Client of other processes), in order to fit your project needs, and let it evolve from the simpliest architecture to a full scalable Domain-Driven design.
In order to provide the better scaling of the server side, @*cache@ can be easily implemented at every level, and hosting can be tuned in order to provide the best response time possible: one central server, several dedicated servers for application, domain and persistence layers...
With {\i mORMot}, your software solution will never be stucked in a dead-end. You'll be able to always adapt to your customers need, and maximize your ROI.
\page
:41General design
: SQLite3-powered, not SQLite3-limited
The core database of this framework uses the {\i @*SQLite3@} library, which is a Free, Secure, Zero-Configuration, Server-less, Single Stable Cross-Platform Database File database engine.
As stated below, you can use any other database access layer, if you wish. A fast in-memory engine is included, and can be used instead or together with the {\i SQLite3} engine. Since revision 1.15 of the framework you may be able to access any remote database, and use one or more {\i @*OleDB@}, {\i @*ODBC@} or {\i @*Oracle@} connections to store your precious ORM objects. For instance, @*MS SQL@ server can be used via {\i OleDB}. {\i SQlite3} will be used as the main SQL engine, able to JOIN all those tables, thanks to its @*Virtual Table@ unique feature.
: Client-Server ORM/SOA architecture
The {\i Synopse mORMot Framework} implements a Client-Server ORM-based architecture, trying to follow all best-practice patterns just introduced (MVC, n-Tier, SOA).
Several clients, can access to the same remote or local server, using diverse communication protocols:
\graph mORMotDesign1 General mORMot architecture - Client / Server
\Internet (VPN)\Local Network
node [shape=box];
\Local Network\Server
subgraph cluster_0 {
"Client 1\n(Delphi)";
label="PC 1";
}
subgraph cluster_1 {
"Client 2\n(AJAX)";
label="PC 2";
}
subgraph cluster_2 {
\Client 4¤(Delphi)\Server
\Server\Client 4¤(Delphi)\JSON + REST¤named pipe¤connection
label="PC Server";
}
subgraph cluster_3 {
"Client n\n(Delphi)";
label="PC n";
}
subgraph cluster_4 {
"Client 3\n(Delphi)";
label="PC 3";
}
\Client 1¤(Delphi)\Local Network\JSON + REST¤over HTTP/1.1
\Client 2¤(AJAX)\Internet (VPN)\JSON + REST¤over HTTP/1.1
\Client 3¤(Delphi)\Local Network
\Client n¤(Delphi)\Internet (VPN)
\
Or the applicaton can be stand-alone:
\graph mORMotDesign2 General mORMot architecture - Stand-alone application
rankdir=LR;
node [shape=box];
subgraph cluster {
\Client\Server\direct ¤access
\Server\Client
label="Stand-Alone application";
}
\
A more detailed design may be summarized as in the following diagram:
\graph mORMotDesign3 General mORMot architecture - Client Server implementation
\RESTful Client\RESTful Server\REST¤JSON
subgraph cluster_0 {
label="Client (Delphi)";
\Services\RESTful Client
\RESTful Client\Business rules\uses
\User Interface\Business rules\uses
\Reporting\ORM methods¤over TSQLRecord\asks
\ORM methods¤over TSQLRecord\RESTful Client\requests
\User Interface\ORM methods¤over TSQLRecord\asks
\User Interface\Services
\User Interface\Reporting\runs
}
subgraph cluster_1 {
label="              Server";
\RESTful Server\Business¤rules\uses
\Business¤rules\ORM
\Services¤implementation\ORM
\Authentication¤(users, sessions)\ORM
\RESTful Server\Authentication¤(users, sessions)\requires
\RESTful Server\external tables\makes¤CRUD
\RESTful Server\in-memory tables¤JSON or binary files\makes¤CRUD
\RESTful Server\SQlite3¤engine\REST to SQL
\RESTful Server\Services¤implementation\runs
\SQlite3¤engine\in-memory tables¤JSON or binary files
\SQlite3¤engine\external tables
\SQlite3¤engine\SQLite3 data file\makes¤CRUD
\ORM\SQlite3¤engine
\ORM\in-memory tables¤JSON or binary files
\ORM\external tables
}
\external tables\External¤Database 1\SQL
\external tables\External¤Database 2
subgraph cluster_2 {
"External\nDatabase 1";
}
subgraph cluster_3 {
"External\nDatabase 2";
}
\
The following pages will detail and explain how this framework implements this architecture.
;:Global Architecture layout
;In the following sections, the {\i Synopse mORMot Framework} library architecture is detailed as:
;\SourcePage

[SAD-Main]
SourcePath=Lib\SQLite3
IncludePath=Lib;Lib\SQLite3
;Lib\SQLite3\Samples\MainDemo
SourceFile=TestSQL3.dpr;SQLite3i18n.pas;SQLite3ToolBar.pas;SQLite3UI.pas;SQLite3UILogin.pas;SQLite3Pages.pas;SQLite3UIOptions.pas;SQLite3UIQuery.pas;SQLite3Service.pas;SQLite3.pas;SQLite3HttpClient.pas;SQLite3HttpServer.pas;SynSQLite3.pas;SynDB.pas;SynOleDB.pas;SynDBOracle.pas;SynDBSQLite3.pas;SynDBODBC.pas
;Samples\MainDemo\SynFile.dpr
Version=1.17
DisplayName=mORMot Framework

:3Object-Relational Mapping
In order to implement @13@ in our framework, generic access to the data is implemented by defining high-level objects as {\i Delphi} classes, descendant from the {\f1\fs20 @**TSQLRecord@} class.
In our @*Client-Server@ @**ORM@, those classes can be used for at least three main purposes:
- Map the Database tables;
- Define business logic in {\i Delphi} code;
- Auto-generate the User Interface (grids, edition screens and menus).
This allows to truly implement a Multi-tier architecture - see @7@.
All @*published properties@ of the {\f1\fs20 TSQLRecord} descendant classes are then accessed via @*RTTI@ in a Client-Server @*REST@ful architecture. Following the three previous purposes, these properties will be used:
- To store and retrieve data from the {\i @*SQLite3@} database engine, by the framework - for most common usage, the coder doesn't have to write @*SQL@ queries: they are all created on the fly by the {\i Object-relational mapping} (ORM) framework;
- To have business logic objects accessible for both the Client and Server side, in a RESTful approach;
- To fill a grid content with the proper field type (e.g. grid column names are retrieved from property names after translation, enumerations are displayed as plain text, or {\f1\fs20 boolean} as a checkbox); to create menus and reports directly from the field definition; to have edition window generated in an automated way.
:26 TSQLRecord fields definition
For example, a database {\f1\fs20 Baby} Table is defined in Delphi code as:
!/// some enumeration
!// - will be written as 'Female' or 'Male' in our UI Grid
!// - will be stored as its ordinal value, i.e. 0 for sFemale, 1 for sMale
!// - as you can see, ladies come first, here
!TSex = (sFemale, sMale);
!
!/// table used for the Babies queries
!TSQLBaby = class(TSQLRecord)
!  private
!    fName: RawUTF8;
!    fAddress: RawUTF8;
!    fBirthDate: TDateTime;
!    fSex: TSex;
!  published
!    property Name: RawUTF8 read fName write fName;
!    property Address: RawUTF8 read fAddress write fAddress;
!    property BirthDate: TDateTime read fBirthDate write fBirthDate;
!    property Sex: TSex read fSex write fSex;
!end;
By adding this {\f1\fs20 TSQLBaby} class to a {\f1\fs20 @*TSQLModel@} instance, common for both Client and Server, the corresponding {\i Baby} table is created by the Framework in the {\i @*SQLite3@} database engine. All @*SQL@ work ('{\f1\fs20 CREATE TABLE ...}') is done by the framework. Just code in Pascal, and all is done for you. Even the needed indexes will be created by the ORM. And you won't miss any ' or ; in your SQL query any more.
The following {\f1\fs20 @**published properties@} types are handled by the @*ORM@, and will be converted as specified to database content (in {\i SQLite3}, an INTEGER is an {\f1\fs20 Int64}, FLOAT is a double, TEXT is an @*UTF-8@ encoded text):
|%24%14%64
|\b Delphi|SQLite3|Remarks\b0
|{\f1\fs20 byte}|INTEGER|
|{\f1\fs20 word}|INTEGER|
|{\f1\fs20 integer}|INTEGER|
|{\f1\fs20 cardinal}|N/A|You should use {\f1\fs20 Int64} instead
|{\f1\fs20 Int64}|INTEGER|
|{\f1\fs20 boolean}|INTEGER|0 is {\f1\fs20 false}, anything else is {\f1\fs20 true}
|enumeration|INTEGER|store the ordinal value of the @*enumerated@ item(i.e. starting at 0 for the first element)
|set|INTEGER|each bit corresponding to an enumerated item (therefore a set of up to 64 elements can be stored in such a field)
|{\f1\fs20 single}|FLOAT|
|{\f1\fs20 double}|FLOAT|
|{\f1\fs20 extended}|FLOAT|stored as {\f1\fs20 double} (precision lost)
|{\f1\fs20 @*currency@}|FLOAT|safely converted to/from {\f1\fs20 currency} type with fixed decimals, without rounding error
|{\f1\fs20 @*RawUTF8@}|TEXT|this is the {\b preferred} field type for storing some textual content in the ORM
|{\f1\fs20 WinAnsiString}|TEXT|{\i WinAnsi} char-set (code page 1252) in Delphi
|{\f1\fs20 RawUnicode}|TEXT|{\i UCS2} char-set in Delphi, as {\f1\fs20 AnsiString}
|{\f1\fs20 @*TDateTime@}|TEXT|@*ISO 8601@ encoded date time
|{\f1\fs20 TTimeLog}|INTEGER|as proprietary fast {\f1\fs20 Int64} date time
|{\f1\fs20 TModTime}|INTEGER|the server date time will be stored when a record is modified (as proprietary fast {\f1\fs20 Int64})
|{\f1\fs20 TCreateTime}|INTEGER|the server date time will be stored when a record is created (as proprietary fast {\f1\fs20 Int64})
|{\f1\fs20 @*TSQLRecord@}|INTEGER|{\f1\fs20 RowID} pointing to another record (warning: the field value contains {\f1\fs20 pointer(RowID)}, not a valid object instance - the record content must be retrieved via its {\f1\fs20 ID} using a {\f1\fs20 PtrInt(Field)} typecast or the {\f1\fs20 Field.ID} method)
|{\f1\fs20 @*TSQLRecordMany@}|nothing|data is stored in a separate {\i pivot} table; this is a particular case of {\f1\fs20 TSQLRecord}: it won't contain {\f1\fs20 pointer(RowID)}, but an instance)
|{\f1\fs20 TRecordReference}|INTEGER|store both {\f1\fs20 ID} and {\f1\fs20 TSQLRecord} type in a {\f1\fs20 RecordRef}-like value (use e.g. {\f1\fs20 @*TSQLRest@. Retrieve(Reference)} to get a record content)
|{\f1\fs20 @*TPersistent@}|TEXT|@*JSON@ object ({\f1\fs20 ObjectToJSON})
|{\f1\fs20 @*TCollection@}|TEXT|JSON array of objects ({\f1\fs20 ObjectToJSON})
|{\f1\fs20 @*TStrings@}|TEXT|JSON array of string ({\f1\fs20 ObjectToJSON})
|{\f1\fs20 TRawUTF8List}|TEXT|JSON array of string ({\f1\fs20 ObjectToJSON})
|any {\f1\fs20 @*TObject@}|TEXT|See {\f1\fs20 TJSONSerializer.@*RegisterCustomSerializer@}
|{\f1\fs20 TSQLRawBlob}|@*BLOB@|
|{\i @*dynamic array@s}|BLOB|in the {\f1\fs20 TDynArray.SaveTo} binary format
|%
Some additional attributes may be added to the {\f1\fs20 published} field definitions:
- If the property is marked as {\f1\fs20 stored false}, it will be created as UNIQUE in the database (i.e. an index will be created and unicity of the value will be checked at insert/update);
- For a dynamic array field, the {\f1\fs20 index} number can be used for the {\f1\fs20 TSQLRecord. DynArray(DynArrayFieldIndex)} method to create a {\f1\fs20 TDynArray} wrapper mapping the dynamic array data;
- For a {\f1\fs20 WinAnsiString / RawUTF8} field of an "external" class - i.e. a TEXT field stored in a remote {\f1\fs20 @*SynDB@}-based database - see @27@, the {\f1\fs20 index} number will be used to define the maximum character size of this field, when creating the corresponding column in the database (@*SQLite3@ does not have any such size limit).
:  Text fields
Note that {\f1\fs20 WideString, shortstring, UnicodeString} (i.e. Delphi 2009/2010/XE/XE2 generic string), and indexed properties are not handled yet (use faster {\f1\fs20 RawUnicodeString} instead of {\f1\fs20 WideString} or {\f1\fs20 UnicodeString}). In fact, the generic {\f1\fs20 string} type is handled (as {\f1\fs20 UnicodeString} under Delphi 2009/2010/XE/XE2), but you may loose some content if you're working with pre-Unicode version of Delphi (in which {\f1\fs20 string = AnsiString} with the current system code page). So we won't recommend its usage.
The natural Delphi type to be used for TEXT storage in our framework is {\f1\fs20 @**RawUTF8@}. All business process should be made using {\f1\fs20 RawUTF8} variables and methods (you have all necessary functions in {\f1\fs20 SynCommons.pas}), then you should explicitly convert the {\f1\fs20 RawUTF8} content into a string using {\f1\fs20 U2S / S2U} from {\f1\fs20 SQlite3i18n.pas} or {\f1\fs20 StringToUTF8 / UTF8ToString} which will handle proper char-set conversion according to the current @*i18n@ settings.
For additional information about @*UTF-8@ handling in the framework, see @32@.
:  Date and time fields
For {\f1\fs20 @**TTimeLog@ / @**TModTime@ / @**TCreateTime@}, the proprietary fast {\f1\fs20 Int64} date time format will map the {\f1\fs20 Iso8601} record type, as defined in {\f1\fs20 SynCommons}:
- 0..5 bits will map seconds,
- 6..11 bits will map minutes,
- 12..16 bits will map hours,
- 17..21 bits will map days (minus one),
- 22..25 bits will map months (minus one),
- 26..38 bits will map years.
This format will be very fast for comparing dates or convert into/from text, and will be stored more efficiently than plain @*ISO 8601@ TEXT as used for {\f1\fs20 @**TDateTime@} fields.
Note that since {\f1\fs20 TTimeLog} type is bit-oriented, you can't just use {\i add} or {\i substract} two {\f1\fs20 TTimeLog} values when doing such date/time computation: use a {\f1\fs20 TDateTime} temporary conversion in such case. See for instance how the {\f1\fs20 TSQLRest.ServerTimeStamp} property is computed:
!function TSQLRest.GetServerTimeStamp: TTimeLog;
!begin
!  PIso8601(@result)^.From(Now+fServerTimeStampOffset);
!end;
!
!procedure TSQLRest.SetServerTimeStamp(const Value: TTimeLog);
!begin
!  fServerTimeStampOffset := PIso8601(@Value)^.ToDateTime-Now;
!end;
But if you simply want to {\i compare} {\f1\fs20 TTimeLog} kind of date/time, it is safe to directly compare their {\f1\fs20 Int64} underlying value.
Due to compiler limitation in older versions of Delphi, direct typecast of a {\f1\fs20 TTimeLog} or {\f1\fs20 Int64} variable into a {\f1\fs20 Iso8601} record (as with {\f1\fs20 Iso8601(aTimeLog).ToDateTime}) could create an internal compiler error. In order to circumvent this bug, you would have to use a {\f1\fs20 pointer} typecast, e.g. as in {\f1\fs20 PIso8601(@Value)^.ToDateTime} above.
:  Enumeration fields
{\i Enumerations} should be mapped as INTEGER, i.e. via {\f1\fs20 ord(aEnumValue)} or {\f1\fs20 TEnum(aIntegerValue)}.
{\i Enumeration sets} should be mapped as INTEGER, with {\f1\fs20 byte/word/integer} type, according to the number of elements in the set: for instance, {\f1\fs20 byte(aSetValue)} for up to 8 elements, {\f1\fs20 word(aSetValue)} for up to 16 elements, and {\f1\fs20 integer(aSetValue)} for up to 32 elements in the set.
:  Floating point and Currency fields
For standard floating-point values, the framework only handle the {\f1\fs20 double} and {\f1\fs20 @**currency@} kind of variables.
In fact, {\f1\fs20 double} is the native type handled by  most database providers (when it comes to money, a dedicated type is worth the cost in a "rich man's world") - it is also native to the SSE set of opcodes of newer CPUs (as handled by Delphi XE 2 in 64 bit mode). Lack of {\f1\fs20 extended} should not be problematic (if it is mandatory, a dedicated set of mathematical classes should be prefered to a database), and could be implemented with the expected precision via a TEXT field (or a BLOB mapped by a @*dynamic array@).
The {\f1\fs20 currency} type is the standard Delphi type to be used when storing and handling monetary values. It will avoid any rounding problems, assuming exact 4 decimals precision. It is able to safely store numbers in the range -922337203685477.5808 .. 922337203685477.5807. Should be enough for your pocket change.
As stated by the official Delphi documentation:
{\i {\f1\fs20 Currency} is a fixed-point data type that minimizes rounding errors in monetary calculations. On the Win32 platform, it is stored as a scaled 64-bit integer with the four least significant digits implicitly representing decimal places. When mixed with other real types in assignments and expressions, {\f1\fs20 Currency} values are automatically divided or multiplied by 10000.}
In fact, this type matches the corresponding {\f1\fs20 OLE} and {\f1\fs20 .Net} implementation of {\f1\fs20 currency}. It is still implemented the same in the {\i Win64} platform (since XE 2). The {\f1\fs20 Int64} binary representation of the {\f1\fs20 currency} type (i.e. {\f1\fs20 value*10000} as accessible via a typecast like {\f1\fs20 PInt64(@aCurrencyValue)^}) is a safe and fast implementation pattern.
In our framework, we tried to avoid any unnecessary conversion to float values when dealing with {\f1\fs20 currency} values. Some dedicated functions have been implemented - see @33@ - for fast and secure access to {\f1\fs20 currency} published properties via @*RTTI@, especially when converting values to or from @*JSON@ text. Using the {\f1\fs20 Int64} binary representation can be not only faster, but also safer: you will avoid any rounding problem which may be introduced by the conversion to a float type. Rounding issues are a nightmare to track - it sounds safe to have a framework handling natively a {\f1\fs20 currency} type from the ground up.
:  Record fields
Published properties of {\i records} are handled in our code, but Delphi doesn't create the corresponding @*RTTI@ for such properties so it won't work.
You could use a {\i @*dynamic array@} with only one element, in order to handle records within your {\f1\fs20 @*TSQLRecord@} class definition - see @21@.
: Working with Objects
To access a particular record, the following code can be used to handle @*CRUD@ statement ({\i Add/Update/Delete/Retrieve}), following the @*REST@ful pattern - see @9@:
!var Baby: TSQLBaby;
!  ID: integer;
!begin
!  // create a new record, since Smith, Jr was just born
!  Baby := TSQLBaby.Create;
!  try
!    Baby.Name := 'Smith';
!    Baby.Address := 'New York City';
!    Baby.BirthDate := Now;
!    Baby.Sex := sMale;
!!    ID := Client.Add(Baby);
!  finally
!    Baby.Free;
!  end;
!  // update record data
!  Baby := TSQLBaby.Create(Client,ID);
!  try
!    assert(Baby.Name='Smith');
!    Baby.Name := 'Smeeth';
!!    Client.Update(Baby);
!  finally
!    Baby.Free;
!  end;
!  // retrieve record data
!  Baby := TSQLBaby.Create;
!  try
!!    Client.Retrieve(ID,Baby);
!    // we may have written:  Baby := TSQLBaby.Create(Client,ID);
!    assert(Baby.Name='Smeeth');
!  finally
!    Baby.Free;
!  end;
!  // delete the created record
!!  Client.Delete(TSQLBaby,ID);
!end;
Of course, you can have a {\f1\fs20 TSQLBaby} instance alive during a longer time. The same {\f1\fs20 TSQLBaby} instance can be used to access several record content, and call {\f1\fs20 Retrieve / Add / Delete / Update} methods on purpose.
No @*SQL@ statement to write, just accessing objects via high-leve methods. This is the magic of @*ORM@.
: Queries
:  Return a list of objects
You can query your table with the {\f1\fs20 FillPrepare} or {\f1\fs20 @**CreateAndFillPrepare@} methods, for instance all babies with balls and a name starting with the letter 'A':
!!aMale := TSQLBaby.CreateAndFillPrepare(Client,
!!  'Name LIKE ? AND Sex = ?',['A%',ord(sMale)]);
!try
!!  while aMale.FillOne do
!    DoSomethingWith(aMale);
!finally
!  aMale.Free;
!end;
This request loops through all matching records, accessing each row content via a {\f1\fs20 TSQLBaby} instance.
The {\f1\fs20 mORMot} engine will create a SQL statement with the appropriate SELECT query, retrieve all data as JSON, transmit it between the Client and the Server (if any), then convert the values into properties of our {\f1\fs20 TSQLBaby} object instance. Internally, the {\f1\fs20 *FillPrepare} / {\f1\fs20 FillOne} methods use a list of records, retrieved as @*JSON@ from the Server, and parsed in memory one row a time (using an internal {\f1\fs20 @*TSQLTableJSON@} instance).
Note that there is an optional {\f1\fs20 aCustomFieldsCSV} parameter available in all {\f1\fs20 FillPrepare / CreateAndFillPrepare} methods, by which you may specify a CSV list of field names to be retrieved. It may save some remote bandwidth, if not all record fields values are needed in the loop. Note that you should use this {\f1\fs20 aCustomFieldsCSV} parameter only to retrieve some data, and that the other fields will remain untouched (i.e. void in case of {\f1\fs20 CreateAndFillPrepare}): any later call to {\f1\fs20 Update} should lead into a data loss, since the method will know that is has been called during a {\f1\fs20 FillPrepare / CreateAndFillPrepare} process, and only the retrieved filled will be updated on the server side.
:36  Query parameters
For safer and faster database process, the WHERE clause of the request expects some parameters to be specified. They are bound in the {\f1\fs20 ?} appearance order in the WHERE clause of the {\f1\fs20 [CreateAnd]FillPrepare} query method.
Standard simple kind of parameters ({\f1\fs20 RawUTF8, integer, double}..) can be bound directly - as in the sample code above for {\f1\fs20 Name} or {\f1\fs20 Sex} properties. The first parameter will be bound as {\f1\fs20 'A%' RawUTF8} TEXT, and the second as the {\f1\fs20 1} INTEGER value.
Any {\f1\fs20 @*TDateTime@} bound parameter shall better be specified using {\f1\fs20 @*DateToSQL@()}, {\f1\fs20 @*DateTimeToSQL@()} or {\f1\fs20 @*Iso8601ToSQL@()} functions, as such:
! aRec.CreateAndFillPrepare(Client,'Datum=?',[DateToSQL(EncodeDate(2012,5,4))]);
! aRec.CreateAndFillPrepare(Client,'Datum<=?',[DateTimeToSQL(Now)]);
! aRec.CreateAndFillPrepare(Client,'Datum<=?',[Iso8601ToSQL(Iso8601Now)]);
For {\f1\fs20 @*TTimeLog@ / @*TModTime@ / @*TCreateTime@} kind of properties, please use the underlying {\f1\fs20 Int64} value as bound parameter.
Any {\f1\fs20 @*sftBlob@} property should better be handled separately, via dedicated {\f1\fs20 RetrieveBlob} and {\f1\fs20 UpdateBlob} method calls, if the data is expected to be big (more than a few MB). But you can specify a small BLOB content using an explicit conversion to the corresponding TEXT format, by calling {\f1\fs20 @*BinToBase64WithMagic@()} overloaded functions when preparing such a query.
Note that there was a {\i breaking change} about the {\f1\fs20 TSQLRecord.Create / FillPrepare  / CreateAndFillPrepare} and {\f1\fs20 TSQLRest.OneFieldValue / MultiFieldValues} methods: for historical reasons, they expected parameters to be marked as {\f1\fs20 %} in the SQL WHERE clause, and inlined via {\f1\fs20 :(...):} as stated @61@ - since revision 1.17 of the framework, those methods expect parameters marked as {\f1\fs20 ?} and with no {\f1\fs20 :(...):}. Due to this {\i breaking change}, user code review is necessary if you want to upgrade the engine from 1.16 or previous. In all cases, using {\f1\fs20 ?} is less confusing for new users, and more close to the usual way of preparing database queries - e.g. as stated @27@. Both {\f1\fs20 TSQLRestClient.EngineExecuteFmt / ListFmt} methods are not affected by this change, since they are just wrappers to the {\f1\fs20 FormatUTF8()} function.
:  Introducing TSQLTableJSON
As we stated above, {\f1\fs20 *FillPrepare} / {\f1\fs20 FillOne} methods are implemented via an internal {\f1\fs20 @*TSQLTableJSON@} instance.
In short, {\f1\fs20 TSQLTableJSON} will expect some {\i @*JSON@} content as input, will parse it in rows and columns, associate it with one or more optional {\f1\fs20 @*TSQLRecord@} class types, then will let you access the data via its {\f1\fs20 Get*} methods.
You can use this {\f1\fs20 TSQLTableJSON} class as in the following example:
!procedure WriteBabiesStartingWith(const Letters: RawUTF8; Sex: TSex);
!var aList: TSQLTableJSON;
!    Row: integer;
!begin
!!  aList := Client.MultiFieldValues(TSQLBaby,'ID,BirthDate',
!!    'Name LIKE ? AND Sex = ?',[Letters+'%',ord(Sex)]);
!  if aList=nil then
!    raise Exception.Create('Impossible to retrieve data from Server');
!  try
!    for Row := 1 to aList.RowCount do
!      writeln('ID=',aList.GetAsInteger(Row,0),' BirthDate=',aList.Get(Row,1));
!  finally
!    aList.Free;
!  end;
!end;
For a record with a huge number of fields, specifying the needed fields could save some bandwidth. In the above sample code, the {\f1\fs20 ID} column has a field index of 0 (so is retrieved via {\f1\fs20 aList.GetAsInteger(Row,0)}) and the {\f1\fs20 BirthDate} column has a field index of 1 (so is retrieved as a {\f1\fs20 PUTF8Char} via {\f1\fs20 aList.Get(Row,1)}). All data rows are processed via a loop using the {\f1\fs20 RowCount} property count - first data row is indexed as 1, since the row 0 will contain the column names.
See also the following methods of {\f1\fs20 @*TSQLRest@}: {\f1\fs20 OneFieldValue}, {\f1\fs20 OneFieldValues}, {\f1\fs20 MultiFieldValue}, {\f1\fs20 MultiFieldValues} which are able to retrieve either a {\f1\fs20 TSQLTableJSON}, either a {\i @*dynamic array@} of {\f1\fs20 integer} or {\f1\fs20 @*RawUTF8@}. And also {\f1\fs20 List} and {\f1\fs20 ListFmt} methods of {\f1\fs20 TSQLRestClient}, if you want to make a {\f1\fs20 JOIN} against multiple tables at once.
A {\f1\fs20 @*TSQLTableJSON@} content can be associated to a {\f1\fs20 TGrid} in order to produce an User Interface taking advantage of the column types, as retrieved from the associated {\f1\fs20 @*TSQLRecord@} @*RTTI@. The {\f1\fs20 TSQLTableToGrid} class is able to associate any {\f1\fs20 TSQLTable} to a standard {\f1\fs20 TDrawGrid}, with some enhancements: themed drawing, handle Unicode, column types (e.g. {\f1\fs20 boolean} are displayed as check-boxes, dates as text, etc...), column auto size, column sort, incremental key lookup, optional hide IDs, selection...
:61  Note about query parameters
{\i (this paragraph is not mandatory to be read at first, so you can skip it if you do not need to know about the mORMot internals - just remember that ? bound parameters are inlined as {\f1\fs20 :(...):} in the JSON transmitted content so can be set directly as such in any WHERE clause)}
If you consider the first sample code:
!aMale := TSQLBaby.CreateAndFillPrepare(Client,
!  'Name LIKE ? AND Sex = ?',['A%',ord(sMale)]);
This will execute a SQL statement, with an ORM-generated SELECT, and a WHERE clause using two parameters bound at execution, containing {\f1\fs20 'A%' RawUTF8} text and 1 integer value.
In fact, from the SQL point of view, the {\f1\fs20 CreateAndFillPrepare()} method as called here is exactly the same as:
!aMale := TSQLBaby.CreateAndFillPrepare(Client,
!  'Name LIKE :(''A%''): AND Sex = :(1):');
or
!aMale := TSQLBaby.CreateAndFillPrepare(Client,
!  'Name LIKE :(%): AND Sex = :(%):',['''A%''',ord(sMale)],[]));
or
!aMale := TSQLBaby.CreateAndFillPrepare(Client,
!  FormatUTF8('Name LIKE :(%): AND Sex = :(%):',['''A%''',ord(sMale)]));
First point is that the {\f1\fs20 'A'} letter has been embraced with @*quotes@, as expected per the @*SQL@ syntax. In fact, {\f1\fs20 Name LIKE :(%): AND Sex = :(%):', ['''A%''',ord(sMale)]} is expected to be a valid WHERE clause of a SQL statement.
Note we used single quotes, but we may have used double quotes (") inside the {\f1\fs20 :( ):} statements. In fact, {\i @*SQLite3@} expects single @**quotes@ in its raw SQL statements, whereas our @**prepared@ statements {\f1\fs20 :( ):} will handle both single ' and double " quotes. Just to avoid any confusion, we'll always show single quotes in the documentation. But you can safely use double quotes within {\f1\fs20 :( ):} statements, which could be more convenient than single quotes, which should be doubled within a pascal constant string {\f1\fs20 ''}.
The only not-obvious syntax in the above code is the {\f1\fs20 :(%):} used for defining prepared parameters in the format string.
In fact, the format string will produce the following WHERE clause parameter as plain text:
!aMale := TSQLBaby.CreateAndFillPrepare(Client,
!  'Name LIKE :(''A%''): AND Sex = :(1):');
So that the following SQL query will be executed by the database engine, after translation by the @*ORM@ magic:
$ SELECT * FROM Baby WHERE Name LIKE ? AND Sex = ?;
With the first {\f1\fs20 ?} parameter bound with {\f1\fs20 'A%'} value, and the second with {\f1\fs20 1}.
In fact, when the framework finds some {\f1\fs20 :( ):} in the SQL statement string, it will prepare a SQL statement, and will bound the parameters before execution (in our case, text {\f1\fs20 A} and integer {\f1\fs20 1}), reusing any previous matching prepared SQL statement. See @14@ for more details about this mechanism.
To be clear, without any prepared statement, you could have used:
!aMale := TSQLBaby.CreateAndFillPrepare(Client,
!  'Name LIKE % AND Sex = %',['''A%''',ord(sMale)],[]);
or
!aMale := TSQLBaby.CreateAndFillPrepare(Client,
!  FormatUTF8('Name LIKE % AND Sex = %',['''A%''',ord(sMale)]));
which will produce the same as:
!aMale := TSQLBaby.CreateAndFillPrepare(Client,
!  'Name LIKE ''A%'' AND Sex = 1');
So that the following SQL statement will be executed:
$ SELECT * FROM Baby WHERE Name LIKE 'A%' AND Sex = 1;
Note that we prepared the SQL WHERE clause, so that we could use the same request statement for all females with name starting with the character 'D':
!aFemale := TSQLBaby.CreateAndFillPrepare(Client,
!  'Name LIKE :(%): AND Sex = :(%):', ['''D%''',ord(sFemale)]);
Using a prepared statement will speed up the database engine, because the SQL query would have to be parsed and optimized only once.
The second query method, i.e.
!  aList := Client.MultiFieldValues(TSQLBaby,'ID,BirthDate',
!    'Name LIKE ? AND Sex = ?',[Letters+'%',ord(Sex)]);
is the same as this code:
!  aList := Client.MultiFieldValues(TSQLBaby,'ID,BirthDate',
!    'Name LIKE :(%): AND Sex = :(%):',[QuotedStr(Letters+'%'),ord(Sex)],[]);
or
!  aList := Client.MultiFieldValues(TSQLBaby,'ID,BirthDate',
!    FormatUTF8('Name LIKE :(%): AND Sex = :(%):',[QuotedStr(Letters+'%'),ord(Sex)]));
In both cases, the parameters will be inlined, in order to prepare the statements, and improve execution speed.
We used the {\f1\fs20 QuotedStr} standard function to embrace the {\f1\fs20 Letters} parameter with quotes, as expected per the @*SQL@ syntax.
Of course, using '?' and bounds parameters is much easier than '%' and manual {\f1\fs20 :(%):} inlining with a {\f1\fs20 QuotedStr()} function call. In your client code, you should better use '?' - but if you find some {\f1\fs20 ':(%):'} in the framework source code and when a WHERE clause is expected within the transmited JSON content, you won't be surprised.
: Objects relationship: cardinality
All previous code is fine if your application requires "flat" data. But most of the time, you'll need to define master/child relationship, perhaps over several levels. In data modeling, the {\i @**cardinality@} of one data table with respect to another data table is a critical aspect of database design. Relationships between data tables define {\i cardinality} when explaining how each table links to another.
In the relational model, tables can have the following {\i cardinality}, i.e. can be related as any of:
- "@*One to one@".
- "Many to one" (rev. "@*One to many@");
- "Many to many" (or "@has many@").
Our {\i mORMot framework} handles all those kinds of {\i cardinality}.
:  "One to one" or "One to many"
In order to handle "{\i @**One to one@}" or "{\i @**One to many@}" relationship between tables (i.e. normalized @**Master/Detail@ in a classical @*RDBMS@ approach), you could define {\f1\fs20 @*TSQLRecord@} @*published properties@ in the object definition.
For instance, you could declare classes as such:
!  TSQLMyFileInfo = class(TSQLRecord)
!  private
!    FMyFileDate: TDateTime;
!    FMyFileSize: Int64;
!  published
!    property MyFileDate: TDateTime read FMyFileDate write FMyFileDate;
!    property MyFileSize: Int64 read FMyFileSize write FMyFileSize;
!  end;
!
!  TSQLMyFile = class(TSQLRecord)
!  private
!    FSecondOne: TSQLMyFileInfo;
!    FFirstOne: TSQLMyFileInfo;
!    FMyFileName: RawUTF8;
!  published
!    property MyFileName: RawUTF8 read FMyFileName write FMyFileName;
!!    property FirstOne: TSQLMyFileInfo read FFirstOne write FFirstOne;
!!    property SecondOne: TSQLMyFileInfo read FSecondOne write FSecondOne;
!  end;
As stated by @26@, {\f1\fs20 TSQLRecord} published properties do not contain an instance of the {\f1\fs20 TSQLRecord} class. They will instead contain {\f1\fs20 pointer(RowID)}, and will be stored as an {\f1\fs20 INTEGER} in the database.
So do not use directly such published properties, like a regular class instance: you'll have an access violation.
When creating such records, use temporary instances for each detail object, as such:
!var One, Two: TSQLMyFileInfo;
!     MyFile: TSQLMyFile;
!begin
!  One := TSQLMyFileInfo.Create;
!  Two := TSQLMyFileInfo.Create;
!  MyFile := TSQLMyFile.Create;
!  try
!    One.MyFileDate := ....
!    One.MyFileSize := ...
!!    MyFile.FirstOne := TSQLMyFileInfo(MyDataBase.Add(One,True)); // add One and store ID in MyFile.FirstOne
!    Two.MyFileDate := ....
!    Two.MyFileSize := ...
!!    MyFile.SecondOne:= TSQLMyFileInfo(MyDataBase.Add(Two,True)); // add Two and store ID in MyFile.SecondOne
!    MyDataBase.Add(MyFile);
!  finally
!     MyFile.Free;
!     Two.Free;
!     One.Free;
!  end;
!end;
When accessing the detail objects, you should not access directly to {\f1\fs20 FirstOne} or {\f1\fs20 SecondOne} properties (there are not class instances, but IDs), then use instead the {\f1\fs20 TSQLRecord. Create(aClient: TSQLRest; aPublishedRecord: TSQLRecord: ForUpdate: boolean=false)} overloaded constructor, as such:
!var One: TSQLMyFileInfo;
!    MyFile: TSQLMyFile;
!begin
!  MyFile := TSQLMyFile.Create(Client,aMyFileID);
!  try
!    // here MyFile.FirstOne.MyFileDate will trigger an access violation
!!    One := TSQLMyFileInfo.Create(Client,MyFile.FirstOne);
!    try
!      // here you can access One.MyFileDate or One.MyFileSize
!    finally
!      One.Free;
!    end;
!  finally
!    MyFile.Free;
!  end;
!end;
Or with a {\f1\fs20 with} statement:
!    with TSQLMyFileInfo.Create(Client,MyFile.FirstOne) do
!    try
!      // here you can access MyFileDate or MyFileSize
!    finally
!      Free;
!    end;
Mapping a {\f1\fs20 TSQLRecord} field into an {\f1\fs20 integer} ID is a bit difficult to learn at first. It was the only way we found out in order to define a "one to one" or "one to many" relationship within the class definition, without any property attribute features of the Delphi compiler (only introduced in newer versions). The main drawback is that the compiler won't be able to identify at compile time some potential GPF issues at run time. This is up to the developper to write correct code, when dealing with {\f1\fs20 TSQLRecord} properties.
:  "Has many" and "has many through"
As @http://en.wikipedia.org/wiki/Many-to-many_(data_model) wrote:
{\i In systems analysis, a many-to-many relationship is a type of cardinality that refers to the relationship between two entities (see also Entity-Relationship Model) A and B in which A may contain a parent row for which there are many children in B and vice versa. For instance, think of A as Authors, and B as Books. An Author can write several Books, and a Book can be written by several Authors. Because most database management systems only support one-to-many relationships, it is necessary to implement such relationships physically via a third and fourth junction table, say, AB with two one-to-many relationships A -> AB and B -> AB. In this case the logical primary key for AB is formed from the two foreign keys (i.e. copies of the primary keys of A and B).}
From the record point of view, and to follow the @*ORM@ vocabulary (in Ruby on Rails, Python, or other {\i ActiveRecord} clones), we could speak of "@**has many@" relationship. In the classic RDBMS implementation, a pivot table is created, containing two references to both related records. Additional information can be stored within this pivot table. It could be used, for instance, to store association time or corresponding permissions of the relationship. This is called a "@**has many through@" relationship.
In fact, there are several families of ORM design, when implementing the "many to many" @*cardinality@:
- Map collections into {\f1\fs20 JOIN}ed query from the ORM (i.e. pivot tables are abstracted from object lists or collections by the framework, to implement "has many" relationship, but you will have to define lazy loading and won't have "has many through" relationship at hand);
- Explicitly handle pivot tables as ORM classes, and provide methods to access to them (it will allow both "has many" and "has many through" relationship).
- Store collections within the ORM classes property (data @*sharding@).
In the {\i mORMot framework}, we did not implement the 1st implementation pattern, but the 2nd and 3rd:
- You can map the DB with dedicated {\f1\fs20 @*TSQLRecordMany@} classes, which allows some true pivot table to be available (that is the 2nd family), introducing true "has many through" cardinality;
- But for most applications, it sounds definitively more easy to use {\f1\fs20 TCollection} (of {\f1\fs20 TPersistent} classes) or {\i dynamic arrays} within one {\f1\fs20 TSQLRecord} class, and data sharding (i.e. the 3rd family).
Up to now, there is no explicit {\i @**Lazy Loading@} feature in our ORM. There is no native handling of {\f1\fs20 @*TSQLRecord@} collections or lists (as they do appear in the first family of ORMs). This could sound like a limitation, but it allows to manage exactly the data to be retrieved from the server in your code, and maintain bandwidth and memory use as low as possible. Use of a pivot table (via the {\f1\fs20 @*TSQLRecordMany@} kind of records) allows tuned access to the data, and implements optimal {\i lazy loading} feature. Note that the only case when some {\f1\fs20 TSQLRecord} instances are automatically created by the ORM is for those {\f1\fs20 TSQLRecordMany} published properties.
:29   Shared nothing architecture (or sharding)
Defining a pivot table is a classic and powerful use of relational database, and unleash its power (especially when linked data is huge).
But it is not easy nor natural to properly handle it, since it introduces some dependencies from the DB layer into the business model. For instance, it does introduce some additional requirements, like constraints / integrity checking and tables/classes inter-dependency.
Furthermore, in real life, we do not have such a separated storage, but we store all details within the main data. So for a @54@, which tries to map the real objects of its own domain, such a pivot table is breaking the business logic. With today's computer power, we can safely implement a centralized way of storing data into our data repository.
Let us quote what {\i wikipedia} states at @http://en.wikipedia.org/wiki/Shared_nothing_architecture
{\i A @**shared nothing architecture@ (SN) is a distributed computing architecture in which each node is independent and self-sufficient, and there is no single point of contention across the system. People typically contrast SN with systems that keep a large amount of centrally-stored state information, whether in a database, an application server, or any other similar single point of contention.}
As we stated in @26@, in our ORM, high-level types like @*dynamic array@s or {\f1\fs20 @**TPersistent@} / {\f1\fs20 @**TCollection@} properties are stored as BLOB or TEXT inside the main data row. There is no external linked table, no {\i @*Master/Detail@} to maintain. In fact, each {\f1\fs20 @*TSQLRecord@} instance content could be made self-contained in our ORM.
When the server starts to have an increasing number of clients, such a data layout could be a major benefit. In fact, the so-called {\i @**sharding@}, or horizontal partitioning of data, is a proven solution for web-scale databases, such as those in use by social networking sites. How does {\i EBay} or {\i Facebook} scale with so many users? Just by {\i sharding}.
A simple but very efficient {\i sharding} mechanism could therefore be implemented with our ORM. In-memory databases, or our {\i BigTable} component are good candidate for light speed data process. Even {\i SQLite} could scale very well in most cases.
Storing detailed data in BLOB or in TEXT as JSON could first sounds a wrong idea. It does break one widely accepted principle of the @*RDBMS@ architecture. But even {\i Google} had to break this dogma. And when {\i MySQL} or such tries to implement sharding, it does need a lot of effort. Others, like the NoSQL {\i MongoDB}, are better candidates: they are not tight to the SQL flat scheme.
Finally, this implementation pattern fits much better with a @*Domain-Driven@ Design.
Therefore, on second thought, having at hand a shared nothing architecture could be a great advantage. Our ORM is already ready to break the table-oriented of SQL. Let us go one step further.
:    Arrays, TPersistent, TCollection, TMyClass
The "{\i has many}" and "{\i has many through}" relationship we just described does follow the classic process of rows association in a relational database, using a pivot table. This does make sense if you have some DB background, but it is sometimes not worth it.
One drawback of this approach is that the data is split into several tables, and you should carefully take care of data integrity to ensure for instance that when you delete a record, all references to it are also deleted in the associated tables. Our @*ORM@ engine will take care of it, but could fail sometimes, especially if you play directly with the tables via SQL, instead of using high-level methods like {\f1\fs20 FillMany*} or {\f1\fs20 DestGetJoined}.
Another potential issue is that one business logical unit is split into several tables, therefore into several diverse {\f1\fs20 @*TSQLRecord@} and {\f1\fs20 @*TSQLRecordMany@} classes. From the @*ORM@ point of view, this could be confusing.
Starting with the revision 1.13 of the framework, {\i @*dynamic array@s}, {\f1\fs20 @*TStrings@} and {\f1\fs20 @*TCollection@} can be used as @*published properties@ in the {\f1\fs20 TSQLRecord} class definition. This won't be strong enough to implement all possible "Has many" architectures, but could be used in most case, when you need to add a list of records within a particular record, and when this list won't have to be referenced as a stand-alone table.
{\i @*Dynamic array@s} will be stored as @*BLOB@ fields in the database, retrieved with {\i Base64} encoding in the @*JSON@ content, the serialized using the {\f1\fs20 TDynArray} wrapper. Therefore, only Delphi clients would be able to use this field content: you'll loose the @*AJAX@ capability of the ORM, at the benefit of better integration with object pascal code. Some dedicated SQL functions have been added to the {\i SQLite} engine, like {\f1\fs20 @*IntegerDynArrayContains@}, to search inside this @*BLOB@ field content from the WHERE clause of any search (see @21@). Those functions are available from AJAX queries.
{\f1\fs20 @*TPersistent@, @*TStrings@} and {\f1\fs20 @*TCollection@} will be stored as TEXT fields in the database, following the {\f1\fs20 ObjectToJSON} function format (you can even serialize any @*TObject@ class, via a previous call to the {\f1\fs20 TJSONSerializer. @*RegisterCustomSerializer@} class method). This format contains only valid JSON arrays or objects: so it could be unserialized via an AJAX application, for instance.
About this (trolling?) subject, and why/when you should use plain Delphi objects or arrays instead of classic @*Master/Detail@ DB relationship, please read "{\i Objects, not tables}" and "{\i ORM is not DB}" paragraphs below.
:     Dynamic arrays fields
:      Dynamic arrays from Delphi Code
For instance, here is how the regression @*test@s included in the framework define a {\f1\fs20 @*TSQLRecord@} class with some additional {\i @*dynamic array@s} fields:
!  TFV = packed record
!    Major, Minor, Release, Build: integer;
!    Main, Detailed: string;
!  end;
!  TFVs = array of TFV;
!  TSQLRecordPeopleArray = class(TSQLRecordPeople)
!  private
!    fInts: TIntegerDynArray;
!    fCurrency: TCurrencyDynArray;
!    fFileVersion: TFVs;
!    fUTF8: RawUTF8;
!  published
!    property UTF8: RawUTF8 read fUTF8 write fUTF8;
!    property Ints: TIntegerDynArray index 1 read fInts write fInts;
!    property Currency: TCurrencyDynArray index 2 read fCurrency write fCurrency;
!    property FileVersion: TFVs index 3 read fFileVersion write fFileVersion;
!  end;
This {\f1\fs20 TSQLRecordPeopleArray} class inherits from {\f1\fs20 TSQLRecordPeople}, that is it will add {\f1\fs20 UTF8, Ints, Currency} and {\f1\fs20 FileVersion} fields to this root class.
Some content is added to the {\f1\fs20 PeopleArray} table, with the following code:
!var V: TSQLRecordPeople;
!    VA: TSQLRecordPeopleArray;
!    FV: TFV;
!  (...)
!  V2.FillPrepare(Client,'LastName=:(''Dali''):');
!  n := 0;
!  while V2.FillOne do
!  begin
!    VA.FillFrom(V2); // fast copy some content from TSQLRecordPeople
The {\f1\fs20 FillPrepare} / {\f1\fs20 FillOne} method are used to loop through all {\f1\fs20 People} table rows with a {\f1\fs20 LastName} column value equal to 'Dali' (with a @*prepared@ statement thanks to {\f1\fs20 :( ):}), then initialize a {\f1\fs20 TSQLRecordPeopleArray} instance with those values, using a {\f1\fs20 FillFrom} method call.
!    inc(n);
!    if n and 31=0 then
!    begin
!      VA.UTF8 := '';
!!      VA.DynArray('Ints').Add(n);
!      Curr := n*0.01;
!!      VA.DynArray(2).Add(Curr);
!      FV.Major := n;
!      FV.Minor := n+2000;
!      FV.Release := n+3000;
!      FV.Build := n+4000;
!      str(n,FV.Main);
!      str(n+1000,FV.Detailed);
!!      VA.DynArray('FileVersion').Add(FV);
!    end else
!      str(n,VA.fUTF8);
The {\f1\fs20 n} variable is used to follow the {\f1\fs20 PeopleArray} number, and will most of the type set its textual converted value in the {\f1\fs20 UTF8} column, and once per 32 rows, will add one item to both {\f1\fs20 VA} and {\f1\fs20 FV} {\i @*dynamic array@} fields.
We could have used normal access to V{\f1\fs20 VA} and {\f1\fs20 FV} {\i dynamic arrays}, as such:
!     SetLength(VA.Ints,length(VA.Ints)+1);
!     VA.Ints[high(VA.Ints)] := n;
But the {\f1\fs20 DynArray} method is used instead, to allow direct access to the {\i dynamic array} via a {\f1\fs20 TDynArray} wrapper. Those two lines behave therefore the same as this code:
!      VA.DynArray('Ints').Add(n);
Note that the {\f1\fs20 DynArray} method can be used via two overloaded set of parameters: either the field name ({\f1\fs20 'Ints'}), either an {\f1\fs20 index} value, as was defined in the class definition. So we could have written:
!      VA.DynArray(1).Add(n);
since the {\f1\fs20 Ints} published property has been defined as such:
!    property Ints: TIntegerDynArray index 1 read fInts write fInts;
Similarly, the following line will add a {\f1\fs20 @*currency@} value to the {\f1\fs20 Currency} field:
!      VA.DynArray(2).Add(Curr);
And a more complex {\f1\fs20 TFV} record is added to the {\f1\fs20 FileVersion} field {\i dynamic array} with just one line:
!      VA.DynArray('FileVersion').Add(FV);
Of course, using the {\f1\fs20 DynArray} method is a bit slower than direct {\f1\fs20 SetLength / Ints[]} use. Using {\f1\fs20 DynArray} with an index should be also a bit faster than using {\f1\fs20 DynArray} with a textual field name (like {\f1\fs20 'Ints'}), with the benefit of perhaps less keyboard errors at typing the property name. But if you need to fast add a lot of items to a {\i dynamic array}, you could use a custom {\f1\fs20 TDynArray} wrapper with an associated external {\f1\fs20 Count} value, or direct access to its content (like {\f1\fs20 SetLength + Ints[]}).
Then the {\f1\fs20 FillPrepare} / {\f1\fs20 FillOne} loop ends with the following line:
!!    Check(Client.Add(VA,true)=n);
!  end;
This will add the {\f1\fs20 VA} fields content into the database, creating a new row in the {\f1\fs20 PeopleArray} table, with an {\f1\fs20 ID} following the value of the {\f1\fs20 n} variable. All {\i dynamic array} fields will be serialized as BLOB into the database table.
:21      Dynamic arrays from SQL code
In order to access the @*BLOB@ content of the dynamic arrays directly from @*SQL@ statements, some new @**SQL function@s have been defined in {\f1\fs20 TSQLDataBase}, named after their native simple types:
- {\f1\fs20 ByteDynArrayContains(BlobField,I64)};
- {\f1\fs20 WordDynArrayContains(BlobField,I64)};
- {\f1\fs20 @**IntegerDynArrayContains@(BlobField,I64)};
- {\f1\fs20 CardinalDynArrayContains(BlobField,I64)};
- {\f1\fs20 CurrencyDynArrayContains(BlobField,I64)} - in this case, {\f1\fs20 I64} is not the {\f1\fs20 @*currency@} value directly converted into an {\f1\fs20 Int64} value (i.e. not {\f1\fs20 Int64(aCurrency)}), but the binary mapping of the {\f1\fs20 currency} value, i.e. {\f1\fs20 aCurrency*10000} or {\f1\fs20 PInt64(@aCurrency)^};
- {\f1\fs20 Int64DynArrayContains(BlobField,I64)};
- {\f1\fs20 RawUTF8DynArrayContainsCase(BlobField,'Text')};
- {\f1\fs20 RawUTF8DynArrayContainsNoCase(BlobField,'Text')}.
Those functions allow direct access to the BLOB content like this:
!  for i := 1 to n shr 5 do
!  begin
!    k := i shl 5;
!!    aClient.OneFieldValues(TSQLRecordPeopleArray,'ID',
!!      FormatUTF8('IntegerDynArrayContains(Ints,?)',[],[k]),IDs);
!    Check(length(IDs)=n+1-32*i);
!    for j := 0 to high(IDs) do
!      Check(IDs[j]=k+j);
!  end;
In the above code, the WHERE clause of the {\f1\fs20 OneFieldValues} method will use the dedicated {\f1\fs20 IntegerDynArrayContains} @*SQL function@ to retrieve all records containing the specified {\f1\fs20 integer} value {\f1\fs20 k} in its {\f1\fs20 Ints} BLOB column. With such a function, all the process is performed Server-side, with no slow data transmission nor JSON/Base64 @*serialization@.
For instance, using such a SQL function, you are able to store multiple {\f1\fs20 @*TSQLRecord@. ID} field values into one {\f1\fs20 TIntegerDynArray} property column, and have direct search ability inside the SQL statement. This could be a very handy way of implementing "one to many" or "many to many" relationship, without the need of a pivot table.
Those functions were implemented to be very efficient for speed. They won't create any temporary dynamic array during the search, but will access directly to the BLOB raw memory content, as returned by the {\i SQlite} engine. The {\f1\fs20 RawUTF8DynArrayContainsCase / RawUTF8DynArrayContainsNoCase} functions also will search directly inside the BLOB. With huge number of requests, this could be slower than using a {\f1\fs20 @*TSQLRecordMany@} pivot table, since the search won't use any index, and will have to read all BLOB field during the request. But, in practice, those functions behave nicely with a relative small amount of data (up to about 50,000 rows). Don't forget that BLOB column access are very optimized in {\i @*SQlite3@}.
For more complex dynamic array content handling, you'll have either to create your own @*SQL function@ using the {\f1\fs20 TSQLDataBase. RegisterSQLFunction} method and an associated {\f1\fs20 TSQLDataBaseSQLFunction} class, or via a dedicated @*Service@ or a @*stored procedure@ - see @22@ on how to implement it.
:     TPersistent/TCollection fields
For instance, here is the way regression @*test@s included in the framework define a {\f1\fs20 @*TSQLRecord@} class with some additional {\f1\fs20 @**TPersistent@}, {\f1\fs20 @**TCollection@} or {\f1\fs20 TRawUTF8List} fields ({\f1\fs20 TRawUTF8List} is just a {\f1\fs20 TStringList}-like component, dedicated to handle {\f1\fs20 @*RawUTF8@} kind of {\f1\fs20 string}):
!  TSQLRecordPeopleObject = class(TSQLRecordPeople)
!  private
!    fPersistent: TCollTst;
!    fUTF8: TRawUTF8List;
!  public
!    constructor Create; override;
!    destructor Destroy; override;
!  published
!    property UTF8: TRawUTF8List read fUTF8;
!    property Persistent: TCollTst read fPersistent;
!  end;
In order to avoid any memory leak or access violation, it's mandatory to initialize then release all internal property instances in the overridden {\f1\fs20 constructor} and {\f1\fs20 destructor} of the class:
!constructor TSQLRecordPeopleObject.Create;
!begin
!  inherited;
!  fPersistent := TCollTst.Create;
!  fUTF8 := TRawUTF8List.Create;
!end;
!
!destructor TSQLRecordPeopleObject.Destroy;
!begin
!  inherited;
!  FreeAndNil(fPersistent);
!  FreeAndNil(fUTF8);
!end;
Here is how the regression @*test@s are performed:
!var VO: TSQLRecordPeopleObject;
!  (...)
!if Client.TransactionBegin(TSQLRecordPeopleObject) then
!try
!  V2.FillPrepare(Client,'LastName=?',['Morse']);
!  n := 0;
!  while V2.FillOne do
!  begin
!    VO.FillFrom(V2); // fast copy some content from TSQLRecordPeople
!    inc(n);
!    VO.Persistent.One.Color := n+100;
!    VO.Persistent.One.Length := n;
!    VO.Persistent.One.Name := Int32ToUtf8(n);
!    if n and 31=0 then
!    begin
!      VO.UTF8.Add(VO.Persistent.One.Name);
!      with VO.Persistent.Coll.Add do
!      begin
!        Color := n+1000;
!        Length := n*2;
!        Name := Int32ToUtf8(n*3);
!      end;
!    end;
!!    Check(Client.Add(VO,true)=n);
!  end;
!  Client.Commit;
!except
!  Client.RollBack; // in case of error
!end;
This will add 1000 rows to the {\f1\fs20 PeopleObject} table.
First of all, the adding is nested inside a @**transaction@ call, to speed up @*SQL@ {\f1\fs20 INSERT} statements, via {\f1\fs20 TransactionBegin} and {\f1\fs20 Commit} methods. Please note that the {\f1\fs20 TransactionBegin} method returns a {\f1\fs20 boolean} value, and should be checked in a multi-threaded or Client-Server environment (in this part of the test suit, content is accessed in the same thread, so checking the result is not mandatory, but shown here for accuracy). In the current implementation of the framework, transactions should not be nested. The typical transaction usage should be the following:
!if Client.TransactionBegin(TSQLRecordPeopleObject) then
!try
!  //.... modify the database content, raise exceptions on error
!  Client.Commit;
!except
!  Client.RollBack; // in case of error
!end;
In a @*Client-Server@ environment with multiple Clients connected at the same time, you can use the dedicated {\f1\fs20 TSQLRestClientURI.TransactionBeginRetry} method:
!if Client.TransactionBeginRetry(TSQLRecordPeopleObject,20) then
!  ...
Note that the transactions are handled according to the corresponding client @*session@: the client should make the transaction block as short as possible (e.g. using a @*batch@ command), since any write attempt by other clients will wait for the transaction to be released (with either a commit or rollback).
The fields inherited from the {\f1\fs20 @*TSQLRecord@} class are retrieved via {\f1\fs20 FillPrepare} / {\f1\fs20 FillOne} method calls, for columns with the {\f1\fs20 LastName} matching {\f1\fs20 'Morse'}. One {\f1\fs20 TPersistent} property instance values are set ({\f1\fs20 VO.Persistent.One}), then, for every 32 rows, a new item is added to the {\f1\fs20 VO.Persistent.Coll} collection.
Here is the data sent for instance to the Server, when the item with {\f1\fs20 ID=32} is added:
${"FirstName":"Samuel Finley Breese31",
$"LastName":"Morse",
$"YearOfBirth":1791,
$"YearOfDeath":1872,
$"UTF8":["32"],
$"Persistent":{"One":{"Color":132,"Length":32,"Name":"32"},"Coll":[{"Color":1032,"Length":64,"Name":"96"}]}
$}
Up to revision 1.15 of the framework, the transmitted JSON content was not a true JSON object, but sent as {\f1\fs20 @*RawUTF8@} TEXT values (i.e. every double-quote ({\f1\fs20 "}) character is escaped as {\f1\fs20 \"} - e.g. {\f1\fs20 "UTF8":"[\"32\"]"}). Starting with revision 1.16 of the framework, the transmitted data is a true JSON object, to allow better integration with an AJAX client. That is, {\f1\fs20 UTF8} field is transmitted as a valid JSON array of string, and {\f1\fs20 Persistent} as a valid JSON object with nested objects and arrays.
When all 1000 rows were added to the database file, the following loop is called once with direct connection to the DB engine, once with a remote client connection (with all available connection protocols):
!  for i := 1 to n do
!  begin
!    VO.ClearProperties;
!!    Client.Retrieve(i,VO);
!    Check(VO.ID=i);
!    Check(VO.LastName='Morse');
!    Check(VO.UTF8.Count=i shr 5);
!    for j := 0 to VO.UTF8.Count-1 do
!      Check(GetInteger(pointer(VO.UTF8[j]))=(j+1) shl 5);
!    Check(VO.Persistent.One.Length=i);
!    Check(VO.Persistent.One.Color=i+100);
!    Check(GetInteger(pointer(VO.Persistent.One.Name))=i);
!    Check(VO.Persistent.Coll.Count=i shr 5);
!    for j := 0 to VO.Persistent.Coll.Count-1 do
!     with VO.Persistent.Coll[j] do
!     begin
!       k := (j+1) shl 5;
!       Check(Color=k+1000);
!       Check(Length=k*2);
!       Check(GetInteger(pointer(Name))=k*3);
!     end;
!  end;
All the magic is made in the {\f1\fs20 Client.Retrieve(i,VO)} method. Data is retrieved from the database as TEXT values, then unserialized from @*JSON@ arrays or objects into the internal {\f1\fs20 TRawUTF8List} and {\f1\fs20 TPersistent} instances.
When the {\f1\fs20 ID=33} row is retrieved, the following JSON content is received from the server:
${"ID":33,
$"FirstName":"Samuel Finley Breese32",
$"LastName":"Morse",
$"YearOfBirth":1791,
$"YearOfDeath":1872,
$"UTF8":"[\"32\"]",
$"Persistent":"{\"One\":{\"Color\":133,\"Length\":33,\"Name\":\"33\"},\"Coll\":[{\"Color\":1032,\"Length\":64,\"Name\":\"96\"}]}"}
In contradiction with POST content, this defines no valid nested JSON objects nor arrays, but {\f1\fs20 UTF8} and {\f1\fs20 Persistent} fields transmitted as JSON strings. This is a known limitation of the framework, due to the fact that it is much faster to retrieve directly the text from the database than process for this operation. For an AJAX application, this won't be difficult to use a temporary {\f1\fs20 string} property, and evaluate the JSON content from it, in order to replace the property with a corresponding object content. Implementation may change in the future.
:52     Custom TObject JSON serialization
Not only {\f1\fs20 TPersistent, TCollection} and {\f1\fs20 TSQLRecord} types can be serialized by writting all {\f1\fs20 published} properties.
In fact, any {\f1\fs20 @*TObject@} can be serialized as @*JSON@ in the whole framework: not only for the ORM part (for {\f1\fs20 published} properties), but also for SOA (as parameters of interface-based service methods). All JSON @**serialization@ is centralized in {\f1\fs20 ObjectToJSON()} and {\f1\fs20 JSONToObject()} (aka {\f1\fs20 TJSONSerializer.WriteObject}) functions.
In some cases, it may be handy to have a custom serialization, for instance if you want to manage some third-party classes, or to adapt the serialization scheme to a particular purpose, at runtime.
You can add a customized serialization of any {\f1\fs20 class}, by calling the {\f1\fs20 TJSONSerializer. @**RegisterCustomSerializer@} class method. Two callbacks are to be defined for a specific class type, and will be used to serialize or un-serialize the object instance. The callbacks are class methods ({\f1\fs20 procedure() of object}), and not plain functions (for some evolved objects, it may have sense to use a context during serialization).
In the current implementation of this feature, callbacks expect low-level implementation. That is, their implementation code shall follow function {\f1\fs20 JSONToObject()} patterns, i.e. calling low-level {\f1\fs20 GetJSONField()} function to decode the JSON content, and follow function {\f1\fs20 TJSONSerializer.WriteObject()} patterns, i.e. {\f1\fs20 aSerializer.Add/AddInstanceName/AddJSONEscapeString} to encode the class instance as JSON.
Note that the process is called outside the "{\f1\fs20 \{...\}}" JSON object layout, allowing any serialization scheme: even a class content can be serialized as a JSON string, JSON array or JSON number, on request.
For instance, we'd like to customize the serialization of this class (defined in {\f1\fs20 SynCommons.pas}):
!  TFileVersion = class
!  protected
!    fDetailed: string;
!    fBuildDateTime: TDateTime;
!  public
!    Major: Integer;
!    Minor: Integer;
!    Release: Integer;
!    Build: Integer;
!    BuildYear: integer;
!    Main: string;
!  published
!    property Detailed: string read fDetailed write fDetailed;
!    property BuildDateTime: TDateTime read fBuildDateTime write fBuildDateTime;
!  end;
By default, since it has been defined within {\f1\fs20 \{$M+\} ... \{$M-\}} conditionals, RTTI is available for the {\f1\fs20 published} properties (just as if it were inheriting from {\f1\fs20 TPersistent}). That is, the default JSON serialization will be for instance:
& {"Detailed":"1.2.3.4","BuildDateTime":"1911-03-14T00:00:00"}
This is what is expected when serialized within a {\f1\fs20 TSynLog} content, or for main use.
We would like to serialize this {\f1\fs20 class} as such:
& {"Major":1,"Minor":2001,"Release":3001,"Build":4001,"Main":"1","BuildDateTime":"1911-03-14"}
We will therefore define the {\i Writer} callback, as such:
!class procedure TCollTstDynArray.FVClassWriter(const aSerializer: TJSONSerializer;
!  aValue: TObject; aHumanReadable, aDontStoreDefault, aFullExpand: Boolean);
!var V: TFileVersion absolute aValue;
!begin
!  aSerializer.AddJSONEscape(['Major',V.Major,'Minor',V.Minor,'Release',V.Release,
!    'Build',V.Build,'Main',V.Main,'BuildDateTime',DateTimeToIso8601Text(V.BuildDateTime)]);
!end;
Most of the JSON serialization work will be made within the {\f1\fs20 AddJSONEscape} method, expecting the JSON object description as an array of name/value pairs.
Then the associated {\i Reader} callback could be, for instance:
!class function TCollTstDynArray.FVClassReader(const aValue: TObject; aFrom: PUTF8Char;
!  var aValid: Boolean): PUTF8Char;
!var V: TFileVersion absolute aValue;
!    Values: TPUtf8CharDynArray;
!begin
!  aValid := false;
!  aFrom := JSONDecode(aFrom,['Major','Minor','Release','Build','Main','BuildDateTime'],Values);
!  if aFrom=nil then
!    exit;
!  V.Major := GetInteger(Values[0]);
!  V.Minor := GetInteger(Values[1]);
!  V.Release := GetInteger(Values[2]);
!  V.Build := GetInteger(Values[3]);
!  V.Main := UTF8DecodeToString(Values[4],StrLen(Values[4]));
!  V.BuildDateTime := Iso8601ToDateTimePUTF8Char(Values[5]);
!  aValid := true;
!  result := aFrom;
!end;
Here, the {\f1\fs20 JSONDecode} function will un-serialize the JSON object into an array of {\f1\fs20 PUTF8Char} values, without any memory allocation (in fact, {\f1\fs20 Values[]} will point to un-escaped and #0 terminated content within the {\f1\fs20 aFrom} memory buffer. So decoding is very fast.
Then, the registration step will be defined as such:
!  TJSONSerializer.RegisterCustomSerializer(TFileVersion,
!    TCollTstDynArray.FVClassReader,TCollTstDynArray.FVClassWriter);
If you want to disable the custom serialization, you may call the same method as such:
!  TJSONSerializer.RegisterCustomSerializer(TFileVersion,nil,nil);
This will reset the JSON serialization of the specified class to the default serializer (i.e. writing of {\f1\fs20 published} properties).
The above code uses some low-level functions of the framework (i.e. {\f1\fs20 AddJSONEscape} and {\f1\fs20 JSONDecode}) to implement serialization as a JSON object, but you may use any other serialization scheme, on need. That is, you may serialize the whole class instance just as one JSON string or numerical value, or even a JSON array. It will depend of the implementation of the {\i Reader} and {\i Writer} registered callbacks.
:58   ORM implementation via pivot table
Data sharding just feels natural, from the @*ORM@ point of view.
But defining a pivot table is a classic and powerful use of relational database, and will unleash its power:
- When data is huge, you can query only for the needed data, without having to load the whole content (it is something similar to {\i lazy loading} in ORM terminology);
- In a master/detail data model, sometimes it can be handy to access directly to the detail records, e.g. for data consolidation;
- And, last but not least, the pivot table is the natural way of storing data associated with "@*has many through@" relationship (e.g. association time or corresponding permissions).
:    Introducing TSQLRecordMany
A dedicated class, inheriting from the standard {\f1\fs20 @*TSQLRecord@} class (which is the base of all objects stored in our ORM), has been created, named {\f1\fs20 @*TSQLRecordMany@}. This table will turn the "many to many" relationship into two "one to many" relationships pointing in opposite directions. It shall contain at least two {\f1\fs20 TSQLRecord} (i.e. INTEGER) @*published properties@, named "{\f1\fs20 Source}" and "{\f1\fs20 Dest}" (name is mandatory, because the ORM will share for exact matches). The first pointing to the source record (the one with a {\f1\fs20 TSQLRecordMany} published property) and the second to the destination record.
For instance:
! TSQLDest = class(TSQLRecord);
! TSQLSource = class;
!! TSQLDestPivot = class(TSQLRecordMany)
! private
!  fSource: TSQLSource;
!  fDest: TSQLDest;
!  fTime: TDateTime;
! published
!!   property Source: TSQLSource read fSource; // map Source column
!!   property Dest: TSQLDest read fDest; // map Dest column
!   property AssociationTime: TDateTime read fTime write fTime;
! end;
! TSQLSource = class(TSQLRecordSigned)
! private
!   fDestList: TSQLDestPivot;
! published
!   property SignatureTime;
!   property Signature;
!!   property DestList: TSQLDestPivot read fDestList;
! end;
!  TSQLDest = class(TSQLRecordSigned)
!  published
!    property SignatureTime;
!    property Signature;
!  end;
When a {\f1\fs20 TSQLRecordMany} published property exists in a {\f1\fs20 TSQLRecord}, it is initialized automatically during {\f1\fs20 TSQLRecord.Create} constructor execution into a real class instance. Note that the default behavior for a {\f1\fs20 TSQLRecord} published property is to contain an {\f1\fs20 INTEGER} value which is the ID of the corresponding record - creating a "one to one" or "many to one" relationship. But {\f1\fs20 TSQLRecordMany} is a special case. So don't be confused! :)
This {\f1\fs20 TSQLRecordMany} instance is indeed available to access directly the pivot table records, via {\f1\fs20 FillMany} then {\f1\fs20 FillRow, FillOne} and {\f1\fs20 FillRewind} methods to loop through records, or {\f1\fs20 FillManyFromDest} / {\f1\fs20 DestGetJoined} for most advanced usage.
Here is how the regression @*test@s are written in the {\f1\fs20 SQLite3} unit:
!procedure TestMany(aClient: TSQLRestClient);
!var MS: TSQLSource;
!    MD, MD2: TSQLDest;
!    i: integer;
!    sID, dID: array[1..100] of Integer;
!    res: TIntegerDynArray;
!begin
!  MS := TSQLSource.Create;
!  MD := TSQLDest.Create;
!  try
!    MD.fSignatureTime := Iso8601Now;
!    MS.fSignatureTime := MD.fSignatureTime;
!    Check(MS.DestList<>nil);
!    Check(MS.DestList.InheritsFrom(TSQLRecordMany));
!!    aClient.TransactionBegin(TSQLSource); // faster process
This code will create two {\f1\fs20 TSQLSource / TSQLDest} instances, then will begin a @*transaction@ (for faster database engine process, since there will be multiple records added at once). Note that during {\f1\fs20 TSQLSource.Create} execution, the presence of a {\f1\fs20 TSQLRecordMany} field is detected, and the {\f1\fs20 DestList} property is filled with an instance of {\f1\fs20 TSQLDestPivot}. This {\f1\fs20 DestList} property is therefore able to be directly used via the "{\i has-many}" dedicated methods, like {\f1\fs20 ManyAdd}.
!    for i := 1 to high(dID) do
!    begin
!      MD.fSignature := FormatUTF8('% %',[aClient.ClassName,i]);
!!      dID[i] := aClient.Add(MD,true);
!      Check(dID[i]>0);
!    end;
This will just add some rows to the {\f1\fs20 Dest} table.
!    for i := 1 to high(sID) do begin
!      MS.fSignature := FormatUTF8('% %',[aClient.ClassName,i]);
!      sID[i] := aClient.Add(MS,True);
!      Check(sID[i]>0);
!      MS.DestList.AssociationTime := i;
!!      Check(MS.DestList.ManyAdd(aClient,sID[i],dID[i])); // associate both lists
!      Check(not MS.DestList.ManyAdd(aClient,sID[i],dID[i],true)); // no dup
!    end;
!    aClient.Commit;
This will create some {\f1\fs20 Source} rows, and will call the {\f1\fs20 ManyAdd} method of the auto-created {\f1\fs20 DestList} instance to associate a {\f1\fs20 Dest} item to the {\f1\fs20 Source} item. The {\f1\fs20 AssociationTime} field of the {\f1\fs20 DestList} instance is set, to implement a "{\i has many through}" relationship.
Then the @*transaction@ is committed to the database.
!    for i := 1 to high(dID) do
!    begin
!!      Check(MS.DestList.SourceGet(aClient,dID[i],res));
!      if not Check(length(res)=1) then
!        Check(res[0]=sID[i]);
!!      Check(MS.DestList.ManySelect(aClient,sID[i],dID[i]));
!      Check(MS.DestList.AssociationTime=i);
!    end;
This code will validate the association of {\f1\fs20 Source} and {\f1\fs20 Dest} tables, using the dedicated {\f1\fs20 SourceGet} method to retrieve all {\f1\fs20 Source} items {\f1\fs20 IDs} associated to the specified {\f1\fs20 Dest ID}, i.e. one item, matching the {\f1\fs20 sID[]} values. It will also check for the {\f1\fs20 AssociationTime} as set for the "{\i has many through}" relationship.
!for i := 1 to high(sID) do
!begin
!!  Check(MS.DestList.DestGet(aClient,sID[i],res));
!  if Check(length(res)=1) then
!    continue; // avoid GPF
!  Check(res[0]=dID[i]);
The {\f1\fs20 DestGet} method retrieves all {\f1\fs20 Dest} items {\f1\fs20 IDs} associated to the specified {\f1\fs20 Source ID}, i.e. one item, matching the {\f1\fs20 dID[]} values.
!!  Check(MS.DestList.FillMany(aClient,sID[i])=1);
This will fill prepare the {\f1\fs20 DestList} instance with all pivot table instances matching the specified {\f1\fs20 Source ID}. It should return only one item.
!!  Check(MS.DestList.FillOne);
!  Check(Integer(MS.DestList.Source)=sID[i]);
!  Check(Integer(MS.DestList.Dest)=dID[i]);
!  Check(MS.DestList.AssociationTime=i);
!  Check(not MS.DestList.FillOne);
Those lines will fill the first (and unique) prepared item, and check that {\f1\fs20 Source, Dest} and {\f1\fs20 AssociationTime} properties match the expected values. Then the next call to {\f1\fs20 FillOne} should fail, since only one prepared row is expected for this {\f1\fs20 Source ID}.
!!  Check(MS.DestList.DestGetJoined(aClient,'',sID[i],res));
!  if not Check(length(res)=1) then
!    Check(res[0]=dID[i]);
This will retrieve all {\f1\fs20 Dest} items {\f1\fs20 IDs} associated to the specified {\f1\fs20 Source ID}, with no additional WHERE condition.
!!  Check(MS.DestList.DestGetJoined(aClient,'Dest.SignatureTime=:(0):',sID[i],res));
!  Check(length(res)=0);
This will retrieve all {\f1\fs20 Dest} items {\f1\fs20 IDs} associated to the specified {\f1\fs20 Source ID}, with an additional always invalid WHERE condition. It should always return no item in the {\f1\fs20 res} array, since {\f1\fs20 SignatureTime} is never equal to 0.
!!  Check(MS.DestList.DestGetJoined(aClient,
!!    FormatUTF8('Dest.SignatureTime=?',[],[MD.SignatureTime]),sID[i],res));
!  if Check(length(res)=1) then
!    continue; // avoid GPF
!  Check(res[0]=dID[i]);
This will retrieve all {\f1\fs20 Dest} items {\f1\fs20 IDs} associated to the specified {\f1\fs20 Source ID}, with an additional WHERE condition, matching the expected value. It should therefore return one item.
Note the call of the global {\f1\fs20 FormatUTF8()} function to get the WHERE clause. You may have written instead:
!  Check(MS.DestList.DestGetJoined(aClient,
!    'Dest.SignatureTime=:('+Int64ToUTF8(MD.SignatureTime)+'):',sID[i],res));
But in this case, using manual inlined {\f1\fs20 :(..):} values is less convenient than the '?' calling convention, especially for string ({\f1\fs20 @*RawUTF8@}) values.
!!  MD2 := MS.DestList.DestGetJoined(aClient,
!!    FormatUTF8('Dest.SignatureTime=?',[],[MD.SignatureTime]),sID[i]) as TSQLADest;
!  if Check(MD2<>nil) then
!    continue;
!  try
!!    Check(MD2.FillOne);
!    Check(MD2.ID=dID[i]);
!    Check(MD2.Signature=FormatUTF8('% %',[aClient.ClassName,i]));
!  finally
!    MD2.Free;
!  end;
!end;
This overloaded {\f1\fs20 DestGetJoined} method will return into {\f1\fs20 MD2} a {\f1\fs20 TSQLDest} instance, prepared with all the {\f1\fs20 Dest} record content associated to the specified {\f1\fs20 Source ID} , with an additional WHERE condition, matching the expected value. Then {\f1\fs20 FillOne} will retrieve the first (and unique) matching {\f1\fs20 Dest} record, and checks for its values.
!!    aClient.TransactionBegin(TSQLADests); // faster process
!    for i := 1 to high(sID) shr 2 do
!!      Check(MS.DestList.ManyDelete(aClient,sID[i*4],dID[i*4]));
!!    aClient.Commit;
!    for i := 1 to high(sID) do
!      if i and 3<>0 then
!      begin
!!        Check(MS.DestList.ManySelect(aClient,sID[i],dID[i]));
!        Check(MS.DestList.AssociationTime=i);
!      end else
!        Check(not MS.DestList.ManySelect(aClient,sID[i],dID[i]));
This code will delete one association per four, and ensure that {\f1\fs20 ManySelect} will retrieve only expected associations.
!  finally
!    MD.Free;
!    MS.Free;
!  end;
This will release associated memory, and also the instance of {\f1\fs20 TSQLDestPivot} created in the {\f1\fs20 DestList} property.
:    Automatic JOIN query
All those methods ({\f1\fs20 ManySelect, DestGetJoined...}) are used to retrieve the relations between tables from the pivot table point of view. This saves bandwidth, and can be used in most simple cases, but it is not the only way to perform requests on many-to-many relationships. And you may have several {\f1\fs20 @*TSQLRecordMany@} instances in the same main record - in this case, those methods won't help you.
It is very common, in the SQL world, to create a @**JOIN@ed request at the main "{\i Source}" table level, and combine records from two or more tables in a database. It creates a set that can be saved as a table or used as is. A JOIN is a means for combining fields from two or more tables by using values common to each. Writing such JOINed statements is not so easy by hand, especially because you'll have to work with several tables, and have to specify the exact fields to be retrieved; if you have several pivot tables, it may start to be a nightmare. Let's see how our @*ORM@ will handle it.
A dedicated {\f1\fs20 FillPrepareMany} method has been added to the {\f1\fs20 @*TSQLRecord@} class, in conjunction with a new {\f1\fs20 constructor} named {\f1\fs20 CreateAndFillPrepareMany}. This particular method will:
- Instantiate all {\f1\fs20 Dest} properties of each {\f1\fs20 TSQLRecordMany} instances - so that the JOINed request will be able to populate directly those values;
- Create the appropriate {\f1\fs20 SELECT} statement, with an optional WHERE clause.
Here is the test included in our regression suite, working with the same database:
!Check(MS.FillPrepareMany(aClient,
!  'DestList.Dest.SignatureTime<>% and id>=? and DestList.AssociationTime<>0 '+
!  'and SignatureTime=DestList.Dest.SignatureTime '+
!  'and DestList.Dest.Signature<>"DestList.AssociationTime"',[0],[sID[1]]));
Of course, the only useful parameter here is {\f1\fs20 id>=?} which is used to retrieve the just added relationships in the pivot table. All other conditions will always be true, but it will help testing the generated SQL.
Our {\i mORMot} will generate the following SQL statement:
$select A.ID AID,A.SignatureTime A00,A.Signature A01,
$  B.ID BID,B.AssociationTime B02,
$  C.ID CID,C.SignatureTime C00,C.Signature C01
$from ASource A,ADests B,ADest C
$where B.Source=A.ID and B.Dest=C.ID
$  and (C.SignatureTime<>0 and A.id>=:(1): and B.AssociationTime<>0
$  and A.SignatureTime=C.SignatureTime and C.Signature<>"DestList.AssociationTime")
You can notice the following:
- All declared {\f1\fs20 TSQLRecordMany} instances (renamed {\f1\fs20 B} in our case) are included in the statement, with all corresponding {\f1\fs20 Dest} instances (renamed as {\f1\fs20 C});
- Fields are aliased with short unique identifiers ({\f1\fs20 AID, A01, BID, B02...}), for all {\i simple} properties of every classes;
- The JOIN clause is created ({\f1\fs20 B.Source=A.ID and B.Dest=C.ID});
- Our manual WHERE clause has been translated into proper SQL, including the table internal aliases ({\f1\fs20 A,B,C}) - in fact, {\f1\fs20 DestList.Dest} has been replaced by {\f1\fs20 C}, the main {\f1\fs20 ID} property has been declared properly as {\f1\fs20 A.ID}, and the {\f1\fs20 "DestList.AssociationTime"} text remained untouched, because it was bounded with quotes.
That is, our @*ORM@ did make all the dirty work for you! You can use Delphi-level conditions in your query, and the engine will transparently convert them into a valid SQL statement. Benefit of this will become clear in case of multiple pivot tables, which are likely to occur in real-world applications.
After the statement has been prepared, you can use the standard {\f1\fs20 FillOne} method to loop through all returned rows of data, and access to the JOINed columns within the Delphi objects instances:
!  Check(MS.FillTable.RowCount=length(sID));
!  for i := 1 to high(sID) do begin
!!   MS.FillOne;
!    Check(MS.fID=sID[i]);
!    Check(MS.SignatureTime=MD.fSignatureTime);
!    Check(MS.DestList.AssociationTime=i);
!    Check(MS.DestList.Dest.fID=dID[i]);
!    Check(MS.DestList.Dest.SignatureTime=MD.fSignatureTime);
!    Check(MS.DestList.Dest.Signature=FormatUTF8('% %',[aClient.ClassName,i]));
!  end;
!!  MS.FillClose;
Note that in our case, an explicit call to {\f1\fs20 FillClose} has been added in order to release all {\f1\fs20 Dest} instances created in {\f1\fs20 FillPrepareMany}. This call is not mandatory if you call {\f1\fs20 MS.Free} directly, but it is required if the same {\f1\fs20 MS} instance is about to use some regular many-to-many methods, like {\f1\fs20 MS.DestList.ManySelect()} - it will prevent any GPF exception to occur with code expecting the {\f1\fs20 Dest} property not to be an instance, but a {\f1\fs20 pointer(DestID)} value.
: Calculated fields
It is often useful to handle some calculated fields. That is, having some field values computed when you set another field value. For instance, if you set an error code from an enumeration (stored in an INTEGER field), you may want the corresponding text (to be stored on a TEXT field). Or you may want a total amount to be computed automatically from some detailed records.
This should not be done on the Server side. In fact, the framework expects the transmitted JSON transmitted from client to be set directly to the database layer, as stated by this code from the {\f1\fs20 SQLite3} unit:
!function TSQLRestServerDB.EngineUpdate(Table: TSQLRecordClass; ID: integer;
!  const SentData: RawUTF8): boolean;
!begin
!  if (self=nil) or (Table=nil) or (ID<=0) then
!    result := false else begin
!    // this SQL statement use :(inlined params): for all values
!    result := EngineExecuteFmt('UPDATE % SET % WHERE RowID=:(%):;',
!!      [Table.RecordProps.SQLTableName,GetJSONObjectAsSQL(SentData,true,true),ID]);
!    if Assigned(OnUpdateEvent) then
!       OnUpdateEvent(self,seUpdate,Table,ID);
!  end;
!end;
The direct conversion from the received JSON content into the SQL {\f1\fs20 UPDATE} statement values is performed very quickly via the {\f1\fs20 GetJSONObjectAsSQL} procedure. It won't use any intermediary {\f1\fs20 @*TSQLRecord@}, so there will be no server-side field calculation possible.
Record-level calculated fields should be done on the Client side, using some setters.
There are at least three ways of updating field values before sending to the server:
- Either by using some dedicated setters method for {\f1\fs20 TSQLRecord} properties;
- Either by overriding the {\f1\fs20 ComputeFieldsBeforeWrite} virtual method of {\f1\fs20 TSQLRecord}.
- If the computed fields need a more complex implementation (e.g. if some properties of another record should be modified), a dedicated @*REST@ful @*service@ should be implemented - see @11@.
:  Setter for TSQLRecord
For instance, here we define a new table named {\i INVOICE}, with only two fields. A @*dynamic array@ containing the invoice details, then a field with the total amount. The dynamic array property will be stored as BLOB into the database, and no additional @*Master/Detail@ table will be necessary.
!type
!  TInvoiceRec = record
!    Ident: RawUTF8;
!    Amount: currency;
!  end;
!  TInvoiceRecs = array of TInvoiceRec;
!  TSQLInvoice = class(TSQLRecord)
!  protected
!    fDetails: TInvoiceRecs;
!    fTotal: Currency;
!    procedure SetDetails(const Value: TInvoiceRecs);
!  published
!    property Details: TInvoiceRecs read fDetails write SetDetails;
!    property Total: Currency read fTotal;
!  end;
Note that the {\f1\fs20 Total} property does not have any {\i setter} (aka {\f1\fs20 write} statement). So it will be read-only, from the ORM point of view. In fact, the following protected method will compute the {\f1\fs20 Total} property content from the {\f1\fs20 Details} property values, when they will be modified:
!procedure TSQLInvoice.SetDetails(const Value: TInvoiceRecs);
!var i: integer;
!begin
!  fDetails := Value;
!  fTotal := 0;
!  for i := 0 to high(Value) do
!    fTotal := fTotal+Value[i].Amount;
!end;
When the object content will be sent to the Server, the {\f1\fs20 Total} value of the JSON content sent will contain the expected value.
Note that with this implementation, the {\f1\fs20 SetDetails} must be called explicitly. That is, you should {\i not only} modify directly the {\f1\fs20 Details[]} array content, but either use a temporary array during edition then assign its value to {\f1\fs20 Invoice.Details}, either force the update with a line of code like:
! Invoice.Details := Invoice.Details; // force Total calculation
:  TSQLRecord.ComputeFieldsBeforeWrite
Even if a {\f1\fs20 @*TSQLRecord@} instance should not normally have access to the {\f1\fs20 TSQLRest} level, according to @*OOP@ principles, the following virtual method have been defined:
!  TSQLRecord = class(TObject)
!  public
!    procedure ComputeFieldsBeforeWrite(aRest: TSQLRest; aOccasion: TSQLEvent); virtual;
!  (...)
It will be called automatically on the Client side, just before a {\f1\fs20 TSQLRecord} content will be sent to the remote server, before adding or update.
In fact, the {\f1\fs20 TSQLRestClientURI.Add / Update / BatchAdd / BatchUpdate} methods will call this method before calling {\f1\fs20 TSQLRecord.GetJSONValues} and send the JSON content to the server.
On the Server-side, in case of some business logic involving the ORM, the {\f1\fs20 TSQLRestServer.Add / Update} methods will also call {\f1\fs20 ComputeFieldsBeforeWrite}.
By default, this method will compute the {\f1\fs20 @*TModTime@ / sftModTime} and {\f1\fs20 @*TCreateTime@ / sftCreateTime}  properties value from the current @*server time stamp@, as such:
!procedure TSQLRecord.ComputeFieldsBeforeWrite(aRest: TSQLRest; aOccasion: TSQLEvent);
!var F: integer;
!begin
!  if (self<>nil) and (aRest<>nil) then
!    with RecordProps do begin
!      if HasModTimeFields then
!        for F := 0 to high(FieldType) do
!        if FieldType[f]=sftModTime then
!!          SetInt64Prop(Self,Fields[F],aRest.ServerTimeStamp);
!      if HasCreateTimeField and (aOccasion=seAdd) then
!        for F := 0 to high(FieldType) do
!        if FieldType[f]=sftCreateTime then
!!          SetInt64Prop(Self,Fields[F],aRest.ServerTimeStamp);
!    end;
!end;
You may override this method for you own purpose, saved the fact that you call this inherited implementation to properly handle {\f1\fs20 TModTime} and {\f1\fs20 TCreateTime} @*published properties@.
: Daily ORM
When you compare @*ORM@ and standard @*SQL@, some aspects must be highlighted.
First, you do not have to worry about field orders and names, and can use field completion in the IDE. It's much more convenient to type {\f1\fs20 Baby}. then select the {\f1\fs20 Name} property, and access to its value.
The ORM code is much more readable than the SQL. You do not have to switch your mind from one syntax to another, in your code. Because @*SQL@ is a true language (see {\i SQL Is A High-Level Scripting Language} at @http://www.fossil-scm.org/index.html/doc/tip/www/theory1.wiki). You can even forget about the SQL itself for most projects; only some performance-related or complex queries should be written in SQL, but you will avoid it most of the time. Think object pascal. And happy coding. Your software architecture will thank you for it.
Another good impact is the naming consistency. For example, what about if you want to rename your table? Just change the class definition, and your IDE will do all refactoring for you, without any risk of missing a hidden SQL statement anywhere. Do you want to rename or delete a field? Change the class definition, and the Delphi compiler will let you know all places where this property was used in your code. Do you want to add a field to an existing database? Just add the property definition, and the framework will create the missing field in the database schema for you.
Another risk-related improvement is about the @**strong type@ checking, included into the Delphi language during compile time, and only during execution time for the SQL. You will avoid most runtime exceptions for your database access: your clients will thank you for that. In one word, forget about field typing mismatch or wrong type assignment in your database tables. Strong typing is great in such cases for code SQA, and if you worked with some scripting languages (like @*JavaScript@, Python or Ruby), you should have wished to have this feature in your project!
It's worth noting that our framework allows writing triggers and stored procedures (or like @*stored procedure@s) in Delphi code, and can create key indexing and perform foreign key checking in class definition.
Another interesting feature is the enhanced Grid component supplied with this framework, and the @*AJAX@-ready orientation, by using natively @*JSON@ flows for @*Client-Server@ data streaming. The @*REST@ protocol can be used in most application, since the framework provide you with an easy to use "Refresh" and caching mechanism. You can even work off line, with a local database replication of the remote data.
For Client-Server - see @6@ - you do not have to open a connection to the database, just create an instance of a {\f1\fs20 TSQLRestClient} object (with the communication layer you want to use: direct access, GDI messages, named pipe or @*HTTP@), and use it as any normal Delphi object. All the @*SQL@ coding or communication and error handling will be done by the framework. The same code can be used in the Client or Server side: the parent {\f1\fs20 @*TSQLRest@} object is available on both sides, and its properties and methods are strong enough to access the data.
:  ORM is not DB
It's worth emphasizing that you should not think about the @*ORM@ like a mapping of an existing DB schema. This is an usual mistake in ORM design.
The database is just one way of your objects persistence:
- Don't think about tables with simple types (text/number...), but objects with high level types;
- Don't think about @*Master/Detail@, but logical units;
- Don't think "@*SQL@", think about classes;
- Don't wonder "How will I store it", but "Which data do I need".
For instance, don't be tempted to always create a pivot table (via a {\f1\fs20 @*TSQLRecordMany@} property), but consider using a {\i @*dynamic array@}, {\f1\fs20 @*TPersistent@, @*TStrings@} or {\f1\fs20 @*TCollection@} @*published properties@ instead.
Or consider that you can use a {\f1\fs20 TRecordReference} property pointing to any registered class of the {\f1\fs20 @*TSQLModel@}, instead of creating one {\f1\fs20 @*TSQLRecord@} property per potential table.
:  Objects, not tables
With an @*ORM@, you should usually define fewer tables than in a "regular" relational database, because you can use the high-level type of the {\f1\fs20 @*TSQLRecord@} properties to handle some per-row data.
The first point, which may be shocking for a database architect, is that you should better {\ul not} create @*Master/Detail@ tables, but just one "master" object with the details stored within, as @*JSON@, via {\i @*dynamic array@}, {\f1\fs20 @*TPersistent@, @*TStrings@} or {\f1\fs20 @*TCollection@} properties.
Another point is that a table is not to be created for every aspect of your software configuration. Let's confess that some DB architects design one configuration table per module or per data table. In an ORM, you could design a configuration class, then use the unique corresponding table to store all configuration encoded as some JSON data, or some DFM-like data. And do not hesitate to separate the configuration from the data, for all not data-related configuration - see e.g. how the {\f1\fs20 SQLite3Options} unit works. With our framework, you can serialize directly any {\f1\fs20 @*TSQLRecord@} or {\f1\fs20 TPersistent} instance into JSON, without the need of adding this {\f1\fs20 TSQLRecord} to the {\f1\fs20 @*TSQLModel@} list. Since revision 1.13 of the framework, you can even define {\f1\fs20 TPersistent} @published properties@ in your {\f1\fs20 TSQLRecord} class, and it will be automatically serialized as TEXT in the database.
:  Methods, not SQL
At first, you should be tempted to write code as such (this code sample was posted on our forum, and is not bad code, just not using the @*ORM@ orientation of the framework):
!  DrivesModel := CreateDrivesModel();
!  GlobalClient := TSQLRestClientDB.Create(DrivesModel, CreateDrivesModel(), 'drives.sqlite', TSQLRestServerDB);
!  TSQLRestClientDB(GlobalClient).Server.DB.Execute(
!    'CREATE TABLE IF NOT EXISTS drives ' +
!    '(id INTEGER PRIMARY KEY, drive TEXT NOT NULL UNIQUE COLLATE NOCASE);');
!  for X := 'A' to 'Z' do
!  begin
!    TSQLRestClientDB(GlobalClient).Server.DB.Execute(
!      'INSERT OR IGNORE INTO drives (drive) VALUES ("' + StringToUTF8(X) + ':")');
!  end;
Please, don't do that!
The correct ORM-oriented implementation should be the following:
!  DrivesModel := TSQLModel.Create([TDrives], 'root');
!  GlobalClient := TMyClient.Create(DrivesModel, nil, 'drives.sqlite', TSQLRestServerDB);
!!  GlobalClient.CreateMissingTables(0);
!!  if GlobalClient.TableRowCount(TDrives)=0 then
!  begin
!    D := TDrives.Create;
!    try
!      for X := 'A' to 'Z' do
!      begin
!        D.Drive := X;
!!        GlobalClient.Add(D,true);
!      end;
!    finally
!      D.Free;
!    end;
!  end;
In the above lines, no SQL was written. It's up to the @*ORM@ to:
- Create all missing tables, via the {\f1\fs20 CreateMissingTables} method - and not compute by hand a "{\f1\fs20 CREATE TABLE IF NOT EXISTS...}" SQL statement;
- Check if there is some rows of data, via the {\f1\fs20 TableRowCount} method - instead of a "{\f1\fs20 SELECT COUNT(*) FROM DRIVES}";
- Append some data using an high-level {\f1\fs20 TDrives} Delphi instance and the {\f1\fs20 Add} method - and not any "{\f1\fs20 INSERT OR IGNORE INTO DRIVES...}".
Then, in order to retrieve some data, you'll be tempted to code something like that (extracted from the same forum article):
!procedure TMyClient.FillDrives(aList: TStrings);
!var
!  table: TSQLTableJSON;
!  X, FieldIndex: Integer;
!begin
!  table := TSQLRestClientDB(GlobalClient).ExecuteList([TSQLDrives], 'SELECT * FROM drives');
!  if (table <> nil) then
!  try
!    FieldIndex := table.FieldIndex('drive');
!    if (FieldIndex >= 0) then
!      for X := 1 to table.RowCount do
!        Items.Add(UTF8ToString(table.GetU(X, FieldIndex)));
!  finally
!    table.Free;
!  end;
!end;
Thanks to the {\f1\fs20 TSQLTableJSON} class, code is somewhat easy to follow. Using a temporary {\f1\fs20 FieldIndex} variable make also it fast inside the loop execution.
But it could also be coded as such, using the {\f1\fs20 @*CreateAndFillPrepare@} then {\f1\fs20 FillOne} method in a loop:
!procedure TMyClient.FillDrives(aList: TStrings);
!begin
!  aList.BeginUpdate;
!  try
!    aList.Clear;
!!    with TSQLDrives.CreateAndFillPrepare(GlobalClient,'') do
!    try
!!      while FillOne do
!!        aList.Add(UTF8ToString(Drive));
!    finally
!      Free;
!    end;
!  finally
!    aList.EndUpdate;
!  end;
!end;
We even added the {\f1\fs20 BeginUpdate / EndUpdate} VCL methods, to have even cleaner and faster code (if you work with a {\f1\fs20 TListBox} e.g.).
Note that in the above code, an hidden {\f1\fs20 TSQLTableJSON} class is created in order to retrieve the data from the server. The abstraction introduced by the ORM methods makes the code not slowest, but less error-prone (e.g. {\f1\fs20 Drive} is now a {\f1\fs20 @*RawUTF8@} property), and easier to understand.
But @*ORM@ is not perfect in all cases.
For instance, if the {\f1\fs20 Drive} field is the only column content to retrieve, it could make sense to ask only for this very column. One drawback of the {\f1\fs20 CreateAndFillPrepare} method is that, by default, it retrieves all columns content from the server, even if you need only one. This is a common potential issue of an ORM: since the library doesn't know which data is needed, it will retrieve all object data, which is some cases is not worth it.
You can specify the optional {\f1\fs20 aCustomFieldsCSV} parameter as such, in order to retrieve only the {\f1\fs20 Drive} property content, and potentially save some bandwidth:
!!    with TSQLDrives.CreateAndFillPrepare(GlobalClient,'','Drive') do
Note that for this particular case, you have an even more high-level method, handling directly a {\f1\fs20 TStrings} property as the recipient:
!procedure TMyClient.FillDrives(aList: TStrings);
! begin
!!  GlobalClients.OneFieldValues(TSQLDrives,'drive','',aList);
! end;
The whole query is made in one line, with no {\f1\fs20 SELECT} statement to write.
For a particular ID range, you may have written, with a specific WHERE clause using a @*prepared@ statement:
!!  GlobalClients.OneFieldValues(TSQLDrives,'drive',
!!    'ID>=? AND ID<=?',[],[aFirstID,aLastID],aList);
It's certainly worth reading all the (verbose) interface part of the {\f1\fs20 SQLite3Commons.pas} unit, e.g. the {\f1\fs20 TSQLRest} class, to make your own idea about all the high-level methods available. In the following pages, you'll find all needed documentation about this particular unit. Since our framework is used in real applications, most useful methods should already have been made available. If you need additional high-level features, feel free to ask for them, if possible with source code sample, in our forum, freely available at @http://synopse.info
:  Think multi-tier
And do not forget the framework is able to have several level of objects, thanks to our @*Client-Server@ architecture - see @6@. Such usage is not only possible, but strongly encouraged.
You should have business-logic level objects at the Client side. Then both business-logic and DB objects at the Server side.
If you have a very specific database schema, business-logic objects can be of very high level, encapsulating some @*SQL@ views for reading, and accessed via some @*REST@ful @*service@ commands for writing - see @11@.
Another possibility to access your high-level type, is to use either custom {\i SQLite3} @*SQL function@s either @*stored procedure@s - see @23@ - both coded in Delphi.
:38 ORM Cache
Here is the definition of "cache", as stated by {\i Wikipedia}:
{\i In computer engineering, a @**cache@ is a component that transparently stores data so that future requests for that data can be served faster. The data that is stored within a cache might be values that have been computed earlier or duplicates of original values that are stored elsewhere. If requested data is contained in the cache (cache hit), this request can be served by simply reading the cache, which is comparatively faster. Otherwise (cache miss), the data has to be recomputed or fetched from its original storage location, which is comparatively slower. Hence, the greater the number of requests that can be served from the cache, the faster the overall system performance becomes.}
{\i To be cost efficient and to enable an efficient use of data, caches are relatively small. Nevertheless, caches have proven themselves in many areas of computing because access patterns in typical computer applications have locality of reference. References exhibit temporal locality if data is requested again that has been recently requested already. References exhibit spatial locality if data is requested that is physically stored close to data that has been requested already.}
In our @*ORM@ framework, since performance was one of our goals since the beginning, cache has been implemented at four levels:
- @*Statement@ cache for implementing @*SQL@ @*prepared@ statements, and parameters bound on the fly - see @36@ and @14@ - note that this cache is available not only for the {\i SQlite3} database engine, but also for any external engine - see @27@;
- Global @*JSON@ result cache at the database level, which is flushed globally on any {\f1\fs20 INSERT / UPDATE} - see @37@;
- Tuned record cache at the @*CRUD@/@*REST@ful level for specified tables or records on the {\i server} side - see @39@;
- Tuned record cache at the CRUD/RESTful level for specified tables or records on the {\i client} side - see @39@.
Thanks to those specific caching abilities, our framework is able to minimize the number of client-server requests, therefore spare bandwidth and network access, and scales well in a concurrent rich client access architecture. In such perspective, a @*Client-Server@ ORM does make sense, and is of huge benefit in comparison to a basic ORM used only for data persistence and automated SQL generation.
: MVC pattern
:  Creating a Model
According to the {\i Model-View-Controller} (@*MVC@) pattern - see @10@ - the database schema should be handled separately from the User Interface.
The {\f1\fs20 @**TSQLModel@} class centralizes all {\f1\fs20 @*TSQLRecord@} inherited classes used by an application, both database-related and business-logic related.
Since this class should be used on both Client and Server sides, it's a good practice to use a common unit to define all {\f1\fs20 TSQLRecord} types, and have a common function to create the related {\f1\fs20 TSQLModel} class.
For instance, here is the corresponding function as defined in the first samples available in the source code repository (unit {\f1\fs20 SampleData.pas}):
!function CreateSampleModel: TSQLModel;
!begin
!  result := TSQLModel.Create([TSQLSampleRecord]);
!end;
For a more complex model, using for instance some User Interface auto-creation, this function could be written as such - extracted from the main demo application, unit {\f1\fs20 FileTables.pas}:
!function CreateFileModel(Owner: TSQLRest): TSQLModel;
!var Classes: array[0..high(FileTabs)] of TSQLRecordClass;
!    i: integer;
!begin
!  for i := 0 to high(FileTabs) do
!    Classes[i] := FileTabs[i].Table;
!!  result := TSQLModel.Create(Classes,'synfile');
!  result.Owner := Owner;
!  result.SetActions(TypeInfo(TFileAction));
!  result.SetEvents(TypeInfo(TFileEvent));
!end;
An {\f1\fs20 Owner} property is also available, and will allow access to the current running client or server {\f1\fs20 @*TSQLRest@} instance associated with this model.
This model will also centralize the available User actions and associated events, for the User Interface and Business Logic handling.
In order to follow the @*MVC@ pattern, the {\f1\fs20 TSQLModel} instance is to be used when you have to deal at table level. For instance, do not try to use low-level {\f1\fs20 TSQLDataBase.GetTableNames} or {\f1\fs20 TSQLDataBase.GetFieldNames} methods in your code. In fact, the tables declared in the Model may not be available in the {\i @*SQLite3@} database schema, but may have been defined as {\f1\fs20 @*TSQLRestServerStaticInMemory@} instance via the {\f1\fs20 TSQLRestServer.StaticDataCreate} method, or being external tables - see @27@. So, in order to access all tables properties, you may instead use code like this:
!var i: integer;
!    Props: TSQLRecordProperties;
!begin
!  ...
!  for i := 0 to high(Model.TableProps) do begin
!    Props := Model.TableProps[i];
!    // now you can access Props.SQLTableName or Props.Fields[] ...
!  end;
!end;
In fact, the {\f1\fs20 TSQLModel.TableProps[]} array maps {\f1\fs20 TSQLModel.Tables[].RecordProps}, and allow fast direct access to all the needed ORM properties of every {\f1\fs20 TSQLRecord} handled by this model, as retrieved from @5@. See {\f1\fs20 TSQLRecordProperties} fields and methods to see all the available information.
:56  Filtering and Validating
According to the n-Tier architecture - see @7@ - data @**filtering@ and @**validation@ should be implemented in the business logic, not in the User Interface.
If you were used to develop RAD database application using Delphi, you may have to change a bit your habits here. Data filtering and validation should be implemented not in the User Interface, but in pure Delphi code.
In order to make this easy, a dedicated set of classes are available in the {\f1\fs20 SynCommons.pas} unit, and allow to define both filtering and validation. They all will be children of any of those both classes:
\graph HierTSynFilter Filtering and Validation classes hierarchy
\TSynValidate\TSynFilterOrValidate
\TSynFilter\TSynFilterOrValidate
\
{\f1\fs20 @*TSQLRecord@} field content validation is handled in the new {\f1\fs20 TSQLRecord. Validate} virtual method, or via some {\f1\fs20 TSQLValidate} classes.
{\f1\fs20 TSQLRecord} field content filtering is handled in the new {\f1\fs20 TSQLRecord. Filter} virtual method, or via some {\f1\fs20 TSQLFilter} classes.
Some "standard" classes are already defined in the {\f1\fs20 SynCommons} and {\f1\fs20 SQLite3Commons} unit, to be used for most common usage:
\graph HierTSynFilters Default filters and Validation classes hierarchy
\TSynValidatePassWord\TSynValidateText
\TSynValidateText\TSynValidate
\TSynValidateTableUniqueField\TSynValidateTable
\TSynValidateTable\TSynValidate
\TSynValidateUniqueField\TSynValidateRest
\TSynValidateRest\TSynValidate
\TSynValidatePatternI\TSynValidatePattern
\TSynValidatePattern\TSynValidate
\TSynValidateIPAddress\TSynValidate
\TSynValidateEmail\TSynValidate
\TSynValidate\TSynFilterOrValidate
\TSynFilterUpperCaseU\TSynFilter
\TSynFilterUpperCase\TSynFilter
\TSynFilterTrim\TSynFilter
\TSynFilterLowerCaseU\TSynFilter
\TSynFilterLowerCase\TSynFilter
\TSynFilter\TSynFilterOrValidate
rankdir=LR;
\
You have powerful validation classes for IP Address, Email (with TLD+domain name), simple {\i regex} pattern, textual validation, strong password validation...
Note that some database-related filtering are existing, like {\f1\fs20 TSynValidateUniqueField} which inherits from {\f1\fs20 TSynValidateRest}.
Of course, the {\f1\fs20 SQLite3UIEdit} unit now handles {\f1\fs20 @*TSQLRecord@} automated filtering (using {\f1\fs20 TSQLFilter} classes) and validation (using one of the {\f1\fs20 TSQLValidate} classes).
The unique field validation is now in {\f1\fs20 TSQLRecord. Validate} and not in {\f1\fs20 SQLite3UIEdit} itself (to have a better multi-tier architecture).
To initialize it, you can add some filters/validators to your {\f1\fs20 @*TSQLModel@} creation function:
!function CreateFileModel(Owner: TSQLRest): TSQLModel;
!var Classes: array[0..high(FileTabs)] of TSQLRecordClass;
!    i: integer;
!begin
!  for i := 0 to high(FileTabs) do
!    Classes[i] := FileTabs[i].Table;
!  result := TSQLModel.Create(Classes,'synfile');
!  result.Owner := Owner;
!  result.SetActions(TypeInfo(TFileAction));
!  result.SetEvents(TypeInfo(TFileEvent));
!  TSQLFile.AddFilterOrValidate('Name',TSQLFilterLowerCase);
!  TSQLUser.AddFilterOrValidate('Email',TSQLValidateEmail);
!end;
In order to perform the filtering of some content, you'll have to call the {\f1\fs20 aRecord.Filter()} method, and {\f1\fs20 aRecord.Validate()} to test for valid content.
For instance, this is how {\f1\fs20 SQLite3UIEdit.pas} unit filters and validates the user interface input:
!procedure TRecordEditForm.BtnSaveClick(Sender: TObject);
! (...)
!  // perform all registered filtering
!!  Rec.Filter(ModifiedFields);
!  // perform content validation
!  FieldIndex := -1;
!!  ErrMsg := Rec.Validate(Client,ModifiedFields,@FieldIndex);
!  if ErrMsg<>'' then begin
!    // invalid field content -> show message, focus component and abort saving
!    if cardinal(FieldIndex)<cardinal(length(fFieldComponents)) then begin
!      C := fFieldComponents[FieldIndex];
!      C.SetFocus;
!      Application.ProcessMessages;
!      ShowMessage(ErrMsg,format(sInvalidFieldN,[fFieldCaption[FieldIndex]]),true);
!    end else
!      ShowMessage(ErrMsg,format(sInvalidFieldN,['?']),true);
!  end else
!    // close window on success
!    ModalResult := mrOk;
!end;
It is up to your code to filter and validate the record content. By default, the {\i mORMot} @*CRUD@ operations won't call the registered filters or validators.
:  Views
This framework also handles directly the creation of Ribbon-like interfaces, with full data view and navigation as visual Grids. Reporting and edition windows can be generated in an automated way. The whole User Interface is designed in code, by some constant definitions.
:5   RTTI
The Delphi language (aka Object Pascal) provided Runtime Type Information (@**RTTI@) more than a decade ago. In short, Runtime Type Information is information about an object's data type that is set into memory at run-time. The RTTI support in Delphi has been added first and foremost to allow the design-time environment to do its job, but developers can also take advantage of it to achieve certain code simplifications. Our framework makes huge use of RTTI, from the database level to the User Interface. Therefore, the resulting program has the advantages of very fast development (Rails-like), but with the robustness of @*strong type@ syntax, and the speed of one of the best compiler available.
In short, it allows the software logic to be extracted from the code itself. Here are the places where this technology was used:
- All database structures are set in the code by normal classes definition, and most of the needed @*SQL@ code is created on the fly by the framework, before calling the {\i @*SQLite3@} database engine, resulting in a true Object-relational mapping (@*ORM@) framework;
- All User Interface is generated by the code, by using some simple data structures, relying on enumerations (see next paragraph);
- Most of the text displayed on the screen does rely on RTTI, thanks to the @*Camel@ approach (see below), ready to be translated into local languages;
- All internal Event process (such as Button press) relies on enumerations RTTI;
- Options and program parameters are using RTTI for data persistence and screen display (e.g. the Settings window of your program can be created by pure code): adding an option is a matter of a few code lines.
In Delphi, enumeration types or {\i Enum} provides a way of to define a list of values. The values have no inherent meaning, and their ordinality follows the sequence in which the identifiers are listed. These values are written once in the code, then used everywhere in the program, even for User Interface generation.
For example, some tool-bar actions can be defined with:
!type
!  /// item toolbar actions
!  TBabyAction = (
!    paCreateNew, paDelete, paEdit, paQuit);
Then this {\f1\fs20 TBabyAction} @*enumerated@ type is used to create the User Interface ribbon of the main window, just by creating an array of set of this kind:
!BarEdit: array[0..1] of set of TBabyAction = (
!    [paCreateNew, paDelete, paEdit],
!    [paQuit] );
The caption of the buttons to be displayed on the screen is then extracted by the framework using "@**Camel@ Case": the second button, defined by the {\f1\fs20 paCreateNew} identifier in the source code, is displayed as "{\i Create new}" on the screen, and this "{\i Create new}" is used for direct @*i18n@ of the software. For further information about "Camel Case" and its usage in Object Pascal, Java, Dot Net, Python see @http://en.wikipedia.org/wiki/CamelCase
Advantages of the RTTI can therefore by sum up:
- Software maintainability, since the whole program logic is code-based, and the User Interface is created from it. It therefore avoid RAD (Rapid Application Development) abuse, which mix the User Interface with data logic, and could lead into "write fast, try to maintain" scenarios;
- Enhanced code @*security@, thanks to Object Pascal @*strong type@ syntax;
- Direct database access from the language object model, without the need of writing @*SQL@ or use of a @*MVC@ framework;
- User Interface coherency, since most screen are created on the fly;
- Easy @*i18n@ of the software, without additional components or systems.
:   User Interface
User Interface generation from RTTI and the integrated reporting features will be described @31@, during presentation of the Main Demo application design.
: One ORM to rule them all
Just before entering deeper into the {\i mORMot} material in the following pages (Database layer, Client-Server, Services), you may find out that this implementation may sounds restricted.
Some common (and founded) criticisms are the following (quoting from our forum):
- "One of the things I don't like so much about your approach to the @*ORM@ is the mis-use of existing Delphi constructs like "{\f1\fs20 index n}" attribute for the maximum length of a string-property. Other ORMs solve this i.e. with official {\f1\fs20 Class}-attributes";
- "You have to inherit from {\f1\fs20 TSQLRecord}, and can't persist any plain class";
- "There is no way to easily map an existing complex database".
Those concerns are pretty understandable. Our {\i mORMot} framework is not meant to fit any purpose, but it is worth understanding why it has been implemented as such, and why it may be quite unique within the family of ORMs - which almost all are following the {\i Hibernate} way of doing.
:  Rude class definition
Attributes do appear in Delphi 2010, and it is worth saying that FPC has an alternative syntax. Older versions of Delphi (still very deployed) do not have attributes available in the language, so it was not possible to be compatible with Delphi 6 up to latest versions (as we wished for our units).
It is perfectly right to speak about 'mis-use of {\f1\fs20 index}' - but this was the easiest and only way we found out to have such information, just using RTTI. Since this parameter was ignored and not used for most classes, it was re-used (also for dynamic array properties, to have faster lookup).\line There is another "mis-use" for the "{\f1\fs20 stored false}" property, which is used to identify unique mandatory columns.
Using attributes is one of the most common way of describing tables in most ORMs.\line On the other hand, some coders have a concern about such class definitions. They are mixing DB and logic: you are somewhat polluting the business-level class definition with DB-related stuff.
That is why other kind of ORMs provide a way of mapping classes to tables using external files (some ORMs provide both ways of definition). And why those days, even code gurus identified the attributes overuse as a potential weakness of code maintainability.\line Attributes do have a huge downsize, when you are dealing with a Client-Server ORM, like ours: on the Client side, those attributes are pointless (client does not need to know anything about the database), and you need to link to all the DB plumbing code to your application. For {\i mORMot}, it was some kind of strong argument.
For the very same reasons, the column definitions (uniqueness, indexes, required) are managed in {\i mORMot} at two levels:
- At {\i ORM level} for {\i DB related stuff} (like indexes, which is a DB feature, not a business feature);
- At {\i Model level} for {\i Business related stuff} (like uniqueness, validators and filters).
When you take a look at the supplied validators and filters - see @56@ - you'll find out that this is much powerful than the attributes available in "classic" ORMs: how could you validate an entry to be an email, or to match a pattern, or to ensure that it will be stored in uppercase within the DB?
Other question worth asking is about the security. If you access the data remotely, a global access to the DB is certainly not enough. Our framework handle per-table CRUD level access for its ORM, above the DB layer (and has also complete security attributes for services) - see @43@. It works however the underneath DB grants are defined (even an DB with no user rights - like in-memory or {\i SQLite3} is able to do it).
The {\i mORMot} point of view (which is not the only one), is to let the DB persist the data, as safe and efficient as possible, but rely on higher levels layers to implement the business logic. It will make it pretty database-agnostic (you can even not use a SQL database at all), and will make the framework code easier to debug and maintain, since we don't have to deal with all the DB engine particularities. In short, this is the @*REST@ point of view, and main cause of success: @*CRUD@ is enough.
:  Several ORMs at once
To be clear, {\i mORMot} offers three kind of table definitions:
- Via {\f1\fs20 @*TSQLRecord@} / {\f1\fs20 @*TSQLRecordVirtual@} "native ORM" classes: data storage is using either fast in-memory lists via {\f1\fs20 @*TSQLRestServerStaticInMemory@}, either {\i SQLite3} tables (in memory, on file, or virtual). In this case, we do not use {\f1\fs20 index} for strings (column length is not used by any of those engines).
- Via {\f1\fs20 @*TSQLRecord@} "external ORM-managed" classes: after registration via a call to the {\f1\fs20 @*VirtualTableExternalRegister@()} function, external DB tables are created and managed by the ORM, via SQL - see @27@. These classes will allow creation of tables in any supported external database engine ({\i SQlite3, Oracle, MS SQL, Jet}, whatever {\i OleDB} or {\i ODBC} provider). In this case, we use {\f1\fs20 index} for text column length. This is the only needed parameter to be defined for such a basic implementation, in regard to {\f1\fs20 TSQLRecord} kind of classes.
- Via {\f1\fs20 @*TSQLRecordMappedAutoID@} / {\f1\fs20 @*TSQLRecordMappedForcedID@} "external mapped" classes: DB tables are not created by the ORM, but already existing in the DB, with sometimes a very complex layout. This feature is not yet implemented, but on the road-map. For this kind of classes we won't probably use attributes, nor even external files, but we will rely on definition from code, either with a fluent definition, either with dedicated classes (or interface).
The concern of not being able to persist any class (it needs to inherit from {\f1\fs20 TSQLRecord}) does perfectly make sense.
On the other hand, from the implementation point of view, it is very powerful, since you have a lot of methods included within this class definition. It does also make sense to have a common ancestor able to identify all three kind of {\i mORMot}'s table definitions: the same abstract ancestor is used, and clients won't even need to know that they are implemented in-memory, using a {\i SQLite3} engine, or even a MS SQL / Oracle database. Another benefit of using a parent {\f1\fs20 class} is to enforce code safety using Delphi's {\i @*strong type@} abilities: you won't be able to pass a non-persistent type to methods which expect one.
From the @*Domain-Driven@ / @*SOA@ point of view, it is now an established rule to make a distinction between @*DTO@ (Data Transfer Objects) and @*Domain Values@ ({\i Entity objects} or {\i Aggregates}). In most implementations, persistence objects (aka ORM objects) should be either the aggregate roots themselves (you do not store Entity objects and even worse DTOs), either dedicated classes. Do not mix layers, unless you like your software to be a maintenance nightmare!
Some @*Event-Sourcing@ architectures even implement {\i several DB back-end at once}:
- It will store the status on one DB (e.g. high-performance in-memory) for most common requests to be immediate;
- And store the modification events in another @*ACID@ DB (e.g. {\i SQLite3}, {\i MS SQL} or {\i Oracle});
- And even sometimes fill some dedicated consolidation DBs for further analysis.
AFAIK it could be possible to directly access ORM objects remotely (e.g. the consolidation DB), mostly in a read-only way, for dedicated reporting, e.g. from consolidated data - this is one potential @*CQRS@ implementation pattern with {\i mORMot.} Thanks to the framework security, remote access will be safe: your clients won't be able to change the consolidation DB content!
As can be easily guessed, such design models are far away from a basic ORM built only for class persistence.
:  The best ORM is the one you need
Therefore, we may sum up some potential use of ORM, depending of your intent:
- If your understanding of ORM is just to persist some existing objects, {\i mORMot} won't help you directly (but we identified that some users are using the built-in JSON serialization feature of the framework to create their own dedicated Client-Server ORM-like platform);
- If you want to persist some data objects (not tied to complex business logic), the framework will be a light and fast candidate, via {\i SQLite3, Oracle, MS SQL} or even with no SQL engine, using {\f1\fs20 @*TSQLRestServerStaticInMemory@} class which is able to persist its content with small files - see @57@;
- If you need (perhaps not now, but probably in the future) to create some kind of scalable domain-driven architecture, you'll have all needed features at hand with {\i mORMot};
- If your expectation is to map an existing complex DB, {\i mORMot} will handle it soon (it is planned and prepared within the framework architecture).
Therefore, {\i mORmot} is not just an ORM, nor just a "classic" ORM.
\page
:42Database layer
: SQLite3-powered, not SQLite3-limited
:  SQLite3 as core
This framework uses a compiled version of the official {\i SQLite3} library source code, and includes it natively into Delphi code. This framework therefore adds some very useful capabilities to the Standard {\i SQLite3} database engine, but keeping all its advantages, as listed in the previous paragraph of this document:
- Faster database access, through unified memory model, and usage of the {\f1\fs20 FastMM4} memory manager (which is almost 10 times faster than the default Windows memory manager for memory allocation);
- Optional direct encryption of the data on the disk (up to AES-256 level, that is Top-Secret @*security@);
- Database layout is declared once in the Delphi source code (as @*published properties@ of classes), avoiding common field or table names mismatch;
- Locking of the database at the record level ({\i SQLite3} only handles file-level locking);
- Of course, the main enhancement added to the {\i SQLite3} engine is that it can be deployed in a stand-alone or @*Client-Server@ architecture, whereas the default {\i SQLite3} library works only in stand-alone mode.
From the technical point of view, here are the current compilation options used for building the {\i SQLite3} engine:
- Uses @*ISO 8601@:2004 format to properly handle date/time values in TEXT field, or in faster and smaller {\f1\fs20 Int64} custom types ({\f1\fs20 @*TTimeLog@ / @*TModTime@ / @*TCreateTime@});
- {\i SQLite3} library unit was compiled including @*RTREE@ extension for doing very fast range queries;
- It can include @*FTS@3/FTS4 full text search engine (MATCH operator), with integrated @*SQL@ optimized ranking function;
- The framework makes use only of newest API ({\f1\fs20 sqlite3_prepare_v2}) and follows {\i SQLite3} official documentation;
- Additional {\i collations} (i.e. sorting functions) were added to handle efficiently not only @*UTF-8@ text, but also e.g. @*ISO 8601@ time encoding, fast {\i Win1252} diacritic-agnostic comparison and native slower but accurate Windows UTF-16 functions;
- Additional @*SQL@ functions like {\i Soundex} for English/French/Spanish phonemes, {\f1\fs20 MOD} or {\f1\fs20 CONCAT}, and some dedicated functions able to directly search for data within @*BLOB@ fields containing an Delphi high-level type (like a serialized dynamic array);
- Custom @*SQL@ functions can be defined in Delphi code;
- Automatic SQL statement parameter preparation, for execution speed up;
- {\f1\fs20 TSQLDatabase} can cache the last results for SELECT statements, or use a tuned client-side or server-side per-record caching, in order to speed up most read queries, for lighter web server or client User Interface e.g.;
- User @*authentication@ handling ({\i SQLite3} is user-free designed);
- {\i SQLite3} source code was compiled without thread mutex: the caller has to be @*thread-safe@ aware; this is faster on most configurations, since mutex has to be acquired once): low level {\f1\fs20 sqlite3_*()} functions are not thread-safe, as {\f1\fs20 TSQLRequest} and {\f1\fs20 TSQLBlobStream} which just wrap them; but {\f1\fs20 TSQLDataBase} is thread-safe, as {\f1\fs20 TSQLTableDB}/{\f1\fs20 TSQLRestServerDB}/{\f1\fs20 TSQLRestClientDB} which call {\f1\fs20 TSQLDataBase};
- Compiled with {\f1\fs20 SQLITE_OMIT_SHARED_CACHE} define, since with the new @*Client-Server@ approach of this framework, no concurrent access could happen, and an internal efficient caching algorithm is added, avoiding most call of the {\i SQLite3} engine in multi-user environment (any @*AJAX@ usage should benefit of it);
- The embedded {\i SQLite3} database engine can be easily updated from the official {\i SQLite3} source code available at @http://sqlite.org - use the amalgamation C file with a few minor changes (documented in the {\f1\fs20 SynSQLite3.pas} unit) - the resulting C source code delivered as {\f1\fs20 .obj} is also available in the official {\i Synopse} source code repository.
The overhead of including {\i SQlite3} in your server application will be worth it: just some KB to the executable, but with so many nice features, even if only external databases are used.
:  Extended by SQLite3 virtual tables
Since the framework is truly object oriented, another database engine could be used instead of the framework. You could easily write your own {\f1\fs20 TSQLRestServer} descendant (as an example, we included a fast in-memory database engine as {\f1\fs20 @*TSQLRestServerFullMemory@}) and link to a another engine (like {\i FireBird}, or a private one). You can even use our framework without any link to the {\i @*SQLite3@} engine itself, via our provided very fast in memory dataset (which can be made persistent by writing and reading @*JSON@ files on disk). The {\i SQLite3} engine is implemented in a separate unit, named {\f1\fs20 SynSQLite3.pas}, and the main unit of the framework is {\f1\fs20 SQLite3Commons.pas}. A bridge between the two units is made with {\f1\fs20 SQLite3.pas}, which will found our ORM framework using {\i SQLite3} as its core.
The framework ORM is able to access any database class (internal or external), via the powerful {\i SQLite3} Virtual Table mechanisms - see @20@. For instance, any external database (via @*OleDB@ / @*ODBC@ providers or direct {\i @*Oracle@} connection) can be accessed via our {\f1\fs20 @*SynDB@}-based dedicated units, as stated @27@.
As a result, the framework has several potential database back-ends, in addition to the default {\i SQLite3} file-based engine. Each engine may have its own purpose, according to the application expectations.
:59  Data access benchmark
On an average desktop computer, depending on the backend database interfaced, {\i mORMot} excels in speed:
- You can persist up to 150,000 objects per second, or retrieve  240,000 objects per second (for our pure Delphi in-memory engine);
- When data is retrieved from server or client @38@, you can read 450,000 objects per second;
- With a high-performance database like Oracle and our direct access classes, you can write 53,000 and read 72,000 objects per second, over a 100 MB network.
Difficult to find a faster ORM, I suspect.
The following tables try to sum up all available possibilities, and give some @**benchmark@ (average objects/second for writing or read).
In these tables:
- 'internal' means use of the internal {\i SQLite3} engine, either via a 'file' back-end or in 'mem'ory;
- 'external' stands for an external access via {\f1\fs20 SynDB} - see @27@;
- 'file off' stands for the file back-end, with {\f1\fs20 Synchronous := smOff};
- '{\f1\fs20 TObjectList}' indicates a {\f1\fs20 TSQLRestServerStaticInMemory} instance - see @57@ - either static (with no SQL support) or virtual (i.e. SQL featured via {\i SQLite3} virtual table mechanism) which may persist the data on disk as JSON or compressed binary;
- numbers are expressed in rows/second (or objects/second), 'k' standing for 1000 (e.g. '15k' = 15,000 objects per second);
- 'trans' stands for {\i Transaction}, i.e. when the write process is nested within {\f1\fs20 BeginTransaction / Commit} calls;
- 'batch' mode will be described @28@;
- 'read one' states that one object is read per call (ORM generates a {\f1\fs20 SELECT * FROM table WHERE ID=?});
- 'read all' is when all 5000 objects are read in a single call (i.e. running {\f1\fs20 SELECT * FROM table});
- {\f1\fs20 @**ACID@} is an acronym for "{\i Atomicity Consistency Isolation Durability}" properties, which guarantee that database transactions are processed reliably: for instance, in case of a power loss or hardware failure, the data will be saved on disk in a consistent way, with no potential loss of data.
Benchmark was run on a good old Core 2 Duo workstation (no SSD), with anti-virus and background applications, over a 100 Mb corporate network, linked to a shared {\i Oracle} 11g database. So it was a development environment, very similar to production site, not dedicated to give best performance.  As a result, rates and timing may vary depending on network and server load, but you get results similar to what could be expected on customer side.\line You can compile the {\f1\fs20 15 - External DB performance} supplied sample code, and run the very same benchmark on your own configuration.
|%30%10%10%10%10%10%10%10
|\b Database|ACID|Persist|Write one|Write trans|Write batch|Read one|Read all\b0
|internal SQLite3 file|Yes|Yes|8|15k|16k|10k|170k
|internal SQLite3 file off|Yes|Yes|400|35k|38k|10k|170k
|internal SQLite3 mem|No|No|30k|35k|45k|44k|170k
|{\f1\fs20 TObjectList} static|No|Yes|97k|145k|147k|100k|234k
|{\f1\fs20 TObjectList} virtual|No|Yes|97k|145k|147k|99k|234k
|external SQLite3 file|Yes|Yes|8|13k|15k|45k|160k
|external SQLite3 file off|Yes|Yes|400|31k|38k|10k|170k
|external SQLite3 mem|No|No|26k|32k|40k|47k|160k
|external Oracle|Yes|Yes|460|715|53k|800|72k
|external Jet|No|Yes|688|900|900|1000|76k
|%
Due to its ACID implementation, {\i SQLite3} process on file waits for the hard-disk to have finished flushing its data, therefore it is the reason why it is so slow (less than 10 objects per second) outside the scope of a transaction. So if you want to reach the best writing performance in your application with the default engine, you should better use transactions and regroup all writing into services or a BATCH process. Another possibility could be to execute {\f1\fs20 DB.Synchronous := smOff} at {\i SQLite3} engine level before process: in case of power loss at wrong time it may corrupt the database file, but it will increase the rate up to 400 objects per second, as stated by the "file off" rows of the table - see @60@.
Therefore, the typical use may be the following:
|%20%37%45
|\b Database|Created by|Use\b0
|int. SQLite3 file|default|General safe data handling
|int. SQLite3 mem|{\f1\fs20 :memory:}|Fast data handling with no persistence (e.g. for testing)
|{\f1\fs20 TObjectList} static|{\f1\fs20 StaticDataCreate}|Best possible performance for small amount of data, without ACID nor SQL
|{\f1\fs20 TObjectList} virtual|{\f1\fs20 VirtualTableRegister}|Best possible performance for small amount of data, if ACID is not required nor complex SQL
|ext. SQLite3 file|{\f1\fs20 VirtualTableExternalRegister}|External back-end, e.g. for disk spanning
|ext. SQLite3 mem|{\f1\fs20 VirtualTableExternalRegister}|Fast external back-end (e.g. for testing)
|ext. Oracle|{\f1\fs20 VirtualTableExternalRegister}|Fast, secure and industry standard; can be shared outside {\i mORMot}
|ext. Jet|{\f1\fs20 VirtualTableExternalRegister}|Could be used as a data exchange format (e.g. with Office applications)
|%
For both writing and reading, {\f1\fs20 TObjectList} / {\f1\fs20 TSQLRestServerStaticInMemory} engine gives impressive results, but has the weakness of being in-memory, so it is not ACID by design, and the data has to fit in memory. Note that indexes are available for IDs and {\f1\fs20 stored false} properties. As a consequence search of not unique values may be slow: the engine has to loop through all rows of data. But for unique values (defined as {\f1\fs20 stored false}), both insertion and search speed is awsome, due to its optimized O(1) hash algorithm - see the following benchmark, especially the "By name" row, which corresponds to a search of an unique {\f1\fs20 RawUTF8} property value.
|%9%10%10%10%10%10%10%10%10%9%9
| |\b SQLite3 (file full)|SQLite3 (file off)|SQLite3 (mem)|TObjectList (static) |TObjectList (virtual)|SQLite3 (ext file full)|SQLite3 (ext file off)|SQLite3 (ext mem)|Oracle|Jet\b0
|{\b By one}|10461|10549|44737|103577|103553|43367|44099|45220|901|1074
|{\b By name}|9694|9651|32350|70534|60153|22785|22240|23055|889|1071
|{\b All Virtual}|167095|162956|168651|253292|118203|97083|90592|94688|56639|52764
|{\b All Direct}|167123|144250|168577|254284|256383|170794|165601|168856|88342|75999
|%
When declared as virtual table (via a {\f1\fs20 VirtualTableRegister} call), you have the full power of SQL (including JOINs) at hand, with incredibly fast CRUD operations: 100,000 requests per second for objects read and write, including serialization and Client-Server communication!
In the above list, the {\i MS SQL Server} is not integrated, but may be used instead of {\i Oracle} (minus the fact that BULK insert is not implemented yet for it, whereas @*array bind@ing boosts {\i Oracle} writing BATCH process performance by 100 times). Any other @*OleDB@ or @*ODBC@ providers may also be used, with direct access (without {\i DBExpress} / {\i BDE} layer nor heavy {\f1\fs20 TDataSet} instance).
Note that all those tests were performed locally and in-process, via a {\f1\fs20 TSQLRestClientDB} instance. For both insertion and reading, a Client-Server architecture (e.g. using HTTP/1.1 for {\i mORMot} clients) will give even better results for BATCH and retrieve all modes. During the tests, internal caching - see @37@ and @38@ - was disabled, so you may expect speed enhancements for real applications, when data is more read than written: for instance, when an object is retrieved from the cache, you achieve 450,000 read requests per second, whatever database is used.
: SQLite3 implementation
Beginning with the revision 1.15 of the framework, the {\i @**SQLite3@} engine itself has been separated from our {\f1\fs20 SQLite3.pas} unit, and defined as a stand-alone unit named {\f1\fs20 SynSQLite3.pas}. See @SDD-DI-2.2.1@.
It can be used therefore:
- Either stand-alone with direct access of all its features, even using its lowest-level C API, via {\f1\fs20 SynSQLite3.pas} - but you won't be able to switch to another database engine easily;
- Either stand-alone with high-level SQL access, using our {\f1\fs20 @*SynDB@} generic access classes, via {\f1\fs20 SynDBSQLite3.pas} - so you will be able to change to any other database engine (e.g. MSSQL or @*Oracle@) when needed;
- Either Client-Server based access with all our @*ORM@ features - see {\f1\fs20 SQLite3.pas}.
We'll define here some highlights specific to our own implementation of the {\i SQLite3} engine, and let you consult the official documentation of this great Open Source project at @http://sqlite.org for general information about its common features.
:14  Prepared statement
In order to speed up the time spent in the {\i SQLite3} engine (it may be useful for high-end servers), the framework is able to natively handle @*prepared@ @*SQL@ statements.
Starting with version 1.12 of the framework, we added an internal SQL statement @*cache@ in the database access, available for all SQL request. Previously, only the one-record SQL {\f1\fs20 SELECT * FROM ... WHERE RowID=...} was prepared (used e.g. for the {\f1\fs20 @*TSQLRest@. Retrieve} method).
That is, if a previous SQL statement is run with some given parameters, a prepared version, available in cache, is used, and new parameters are bounded to it before the execution by {\i SQLite3}.
In some cases, it can speed the {\i SQLite3} process a lot. From our profiling, prepared statements make common requests (i.e. select / insert / update on one row) at least two times faster, on an in-memory database ({\f1\fs20 ':memory:'} specified as file name).
In order to use this statement caching, any SQL statements must have the parameters to be surrounded with '{\f1\fs20 :(}' and '{\f1\fs20 ):}'. The SQL format was indeed enhanced by adding an optional way of marking parameters inside the SQL request, to enforce statement preparing and caching.
Therefore, there are now two ways of writing the same SQL request:
Write the SQL statement as usual:
$SELECT * FROM TABLE WHERE ID=10;
in this case, the SQL will be parsed by the {\i SQLite3} engine, a statement will be compiled, then run.
Use the new optional markers to identify the changing parameter:
$SELECT * FROM TABLE WHERE ID=:(10):;
in this case, any matching already prepared statement will be re-used for direct run.
In the later case, an internal pool of prepared {\f1\fs20 TSQLRequest} statements is maintained. The generic SQL code used for the matching will be this one:
$SELECT * FROM TABLE WHERE ID=?;
and the integer value 10 will be bounded to the prepared statement before execution.
Example of possible inlined values are (note double " @*quotes@ are allowed for the text parameters, whereas SQL statement should only use single ' quotes):
$:(1234): :(12.34): :(12E-34): :("text"): :('It''s great'):
All internal SQL statement generated by the @*ORM@ are now using this new parameter syntax.
For instance, here is how an object deletion is implemented:
!function TSQLRestServerDB.EngineDelete(Table: TSQLRecordClass; ID: integer): boolean;
!begin
!  if Assigned(OnUpdateEvent) then
!    OnUpdateEvent(self,seDelete,Table,ID); // notify BEFORE deletion
!  result := EngineExecuteFmt('DELETE FROM % WHERE RowID=:(%):;',[Table.SQLTableName,ID]);
!end;
Using {\f1\fs20 :(%):} will let the {\f1\fs20 DELETE FROM table_name WHERE RowID=?} statement be prepared and reused between calls.
In your code, you should better use, for instance:
! aName := OneFieldValue(TSQLMyRecord,'Name','ID=:(%):',[aID]);
 or even easier
! aName := OneFieldValue(TSQLMyRecord,'Name','ID=?',[],[aID]);
 instead of
! aName := OneFieldValue(TSQLMyRecord,'Name','ID=%',[aID]);
 or instead of a plain
! aName := OneFieldValue(TSQLMyRecord,'Name','ID='+Int32ToUtf8(aID));
In fact, from your client code, you may not use directly the {\f1\fs20 :(...):} expression in your request, but would rather use the overloaded {\f1\fs20 TSQLRecord.Create, TSQLRecord.FillPrepare, TSQLRecord.CreateAndFillPrepare, TSQLRest.OneFieldValue, TSQLRest.MultiFieldValues, TQLRestClient.EngineExecuteFmt} and {\f1\fs20 TSQLRestClient.ListFmt} methods, available since revision 1.15 of the framework, which will accept both '%' and '?' characters in the SQL WHERE format text, inlining '?' parameters with proper {\f1\fs20 :(...):} encoding and quoting the {\f1\fs20 @*RawUTF8@} / strings parameters on purpose.
I found out that this SQL format enhancement is much easier to use (and faster) in the Delphi code than using parameters by name or by index, like in this classic VCL code:
$SQL.Text := 'SELECT Name FROM Table WHERE ID=:Index';
$SQL.ParamByName('Index').AsInteger := aID;
At a lowest-level, inlining the bounds values inside the statement enabled better serialization in a Client-Server architecture, and made caching easier on the Server side: the whole SQL query contains all parameters within one unique {\f1\fs20 RawUTF8} value, and can be therefore directly compared to the cached entries. As such, our framework is able to handle prepared statements without keeping bound parameters separated from the main SQL text.
It's also worth noting that external databases (see next paragraph) will also benefit from this statement preparation. Inlined values will be bound separately to the external SQL statement, to achieve the best speed possible.
:  R-Tree inclusion
Since the 2010-06-25 source code repository update, the @*RTREE@ extension is now compiled by default within all supplied {\f1\fs20 .obj} files.
An R-Tree is a special index that is designed for doing range queries. R-Trees are most commonly used in geospatial systems where each entry is a rectangle with minimum and maximum X and Y coordinates. Given a query rectangle, an R-Tree is able to quickly find all entries that are contained within the query rectangle or which overlap the query rectangle. This idea is easily extended to three dimensions for use in CAD systems. R-Trees also find use in time-domain range look-ups. For example, suppose a database records the starting and ending times for a large number of events. A R-Tree is able to quickly find all events, for example, that were active at any time during a given time interval, or all events that started during a particular time interval, or all events that both started and ended within a given time interval. And so forth. See @http://www.sqlite.org/rtree.html
A dedicated @*ORM@ class, named {\f1\fs20 TSQLRecordRTree}, is available to create such tables. It inherits from {\f1\fs20 TSQLRecordVirtual}, like the other @*virtual table@s types (e.g. {\f1\fs20 TSQLRecordFTS3}).
Any record which inherits from this {\f1\fs20 TSQLRecordRTree} class must have only {\f1\fs20 sftFloat} (i.e. Delphi {\f1\fs20 double}) @*published properties@ grouped by pairs, each as minimum- and maximum-value, up to 5 dimensions (i.e. 11 columns, including the ID property). Its {\f1\fs20 ID: integer} property must be set before adding a {\f1\fs20 TSQLRecordRTree} to the database, e.g. to link an R-Tree representation to a regular {\f1\fs20 @*TSQLRecord@} table containing the main data.
Queries against the ID or the coordinate ranges are almost immediate: so you can e.g. extract some coordinates box from the main regular {\f1\fs20 TSQLRecord} table, then use a {\f1\fs20 TSQLRecordRTree}-joined query to make the process faster; this is exactly what the {\f1\fs20 TSQLRestClient. RTreeMatch} method offers: for instance, running with {\f1\fs20 aMapData. @*Blob@Field} filled with {\f1\fs20 [-81,-79.6,35,36.2]} the following lines:
! aClient.RTreeMatch(TSQLRecordMapData,'BlobField',TSQLRecordMapBox,
!   aMapData.BlobField,ResultID);
will execute the following @*SQL@ statement:
$ SELECT MapData.ID From MapData, MapBox WHERE MapData.ID=MapBox.ID
$  AND minX>=:(-81.0): AND maxX<=:(-79.6): AND minY>=:(35.0): AND :(maxY<=36.2):
$  AND MapBox_in(MapData.BlobField,:('\uFFF0base64encoded-81,-79.6,35,36.2'):);
The {\f1\fs20 MapBox_in} @*SQL function@ is registered in {\f1\fs20 @*TSQLRestServerDB@. Create} constructor for all {\f1\fs20 TSQLRecordRTree} classes of the current database model. Both {\f1\fs20 BlobToCoord} and {\f1\fs20 ContainedIn} class methods are used to handle the box storage in the BLOB. By default, it will process a raw {\f1\fs20 array of double}, with a default box match (that is {\f1\fs20 ContainedIn} method will match the simple {\f1\fs20 minX>=...maxY<=...} where clause).
:8  FTS3/FTS4
@**FTS@3/FTS4 are {\i @*SQLite3@} @*virtual table@ modules that allow users to perform full-text searches on a set of documents. The most common (and effective) way to describe full-text searches is "what Google, Yahoo and Altavista do with documents placed on the World Wide Web". Users input a term, or series of terms, perhaps connected by a binary operator or grouped together into a phrase, and the full-text query system finds the set of documents that best matches those terms considering the operators and groupings the user has specified. See @http://www.sqlite.org/fts3.html as reference material about FTS3 usage in {\i SQLite3}.
Since version 1.5 of the framework, the {\f1\fs20 sqlite3fts3.obj} file is always available in the distribution file: just define the {\f1\fs20 INCLUDE_FTS3} conditional globaly for your application (it is expected e.g. in {\f1\fs20 SQLite3.pas} and {\f1\fs20 SynSQLite3.pas}) to enable {\i FTS3} in your application.
Leave it undefined if you do not need this feature, and will therefore spare some KB of code.
FTS3 and FTS4 are nearly identical. FTS4 is indeed an enhancement to FTS3, added with SQLite version 3.7.4, and included in the release v.1.11 of the framework. They share most of their code in common, and their interfaces are the same. The differences are:
- FTS4 contains query performance optimizations that may significantly improve the performance of full-text queries that contain terms that are very common (present in a large percentage of table rows).
- FTS4 supports some additional options that may used with the {\f1\fs20 matchinfo()} function.
Because it stores extra information on disk in two new shadow tables in order to support the performance optimizations and extra {\f1\fs20 matchinfo()} options, FTS4 tables may consume more disk space than the equivalent table created using FTS3. Usually the overhead is 1-2% or less, but may be as high as 10% if the documents stored in the FTS table are very small. The overhead may be reduced by using a {\f1\fs20 TSQLRecordFTS3} table type instead of {\f1\fs20 TSQLRecordFTS4} declaration, but this comes at the expense of sacrificing some of the extra supported {\f1\fs20 matchinfo()} options.
:   Dedicated FTS3/FTS4 record type
In order to allow easy use of the @*FTS@ feature, some types have been defined:
- {\f1\fs20 TSQLRecordFTS3} to create a FTS3 table with default "simple" stemming;
- {\f1\fs20 TSQLRecordFTS3Porter} to create a FTS3 table using the {\i Porter Stemming} algorithm (see below);
- {\f1\fs20 TSQLRecordFTS4} to create a FTS4 table with default "simple" stemming;
- {\f1\fs20 TSQLRecordFTS4Porter} to create a FTS4 table using the {\i Porter Stemming} algorithm.
The following graph will detail this class hierarchy:
\graph FTSORMClasses FTS3/FTS4 ORM classes
\TSQLRecord\TSQLRecordVirtual
\TSQLRecordVirtual\TSQLRecordFTS3
\TSQLRecordFTS3\TSQLRecordFTS3Porter
\TSQLRecordFTS3\TSQLRecordFTS4
\TSQLRecordFTS4\TSQLRecordFTS4Porter
\
:   Stemming
The "stemming" algorithm - see @http://sqlite.org/fts3.html#tokenizer - is the way the english text is parsed for creating the word index from raw text.
The {\i simple} (default) tokenizer extracts tokens from a document or basic @*FTS@ full-text query according to the following rules:
- A term is a contiguous sequence of eligible characters, where eligible characters are all alphanumeric characters, the "_" character, and all characters with UTF codepoints greater than or equal to 128. All other characters are discarded when splitting a document into terms. Their only contribution is to separate adjacent terms.
- All uppercase characters within the ASCII range (UTF codepoints less than 128), are transformed to their lowercase equivalents as part of the tokenization process. Thus, full-text queries are case-insensitive when using the simple tokenizer.
For example, when a document containing the text "{\i Right now, they're very frustrated.}", the terms extracted from the document and added to the full-text index are, in order, "{\f1\fs20 right now they re very frustrated}". Such a document would match a full-text query such as "{\f1\fs20 MATCH 'Frustrated'}", as the simple tokenizer transforms the term in the query to lowercase before searching the full-text index.
The {\i Porter Stemming algorithm} tokenizer uses the same rules to separate the input document into terms, but as well as folding all terms to lower case it uses the {\i Porter Stemming} algorithm to reduce related English language words to a common root. For example, using the same input document as in the paragraph above, the porter tokenizer extracts the following tokens: "{\f1\fs20 right now thei veri frustrat}". Even though some of these terms are not even English words, in some cases using them to build the full-text index is more useful than the more intelligible output produced by the simple tokenizer. Using the porter tokenizer, the document not only matches full-text queries such as "{\f1\fs20 MATCH 'Frustrated'}", but also queries such as "{\f1\fs20 MATCH 'Frustration'}", as the term "{\i Frustration}" is reduced by the Porter stemmer algorithm to "{\i frustrat}" - just as "{\i Frustrated}" is. So, when using the porter tokenizer, FTS is able to find not just exact matches for queried terms, but matches against similar English language terms. For more information on the Porter Stemmer algorithm, please refer to the @http://tartarus.org/~martin/PorterStemmer page.
:24   FTS searches
A good approach is to store your data in a regular {\f1\fs20 @*TSQLRecord@} table, then store your text content in a separated @*FTS@3 table, associated to this {\f1\fs20 TSQLRecordFTS3} table via its {\f1\fs20 ID / DocID} property. Note that for {\f1\fs20 TSQLRecordFTS*} types, the {\f1\fs20 ID} property was renamed as {\f1\fs20 DocID}, which is the internal name for the FTS virtual table definition of its unique integer key {\f1\fs20 ID} property.
For example (extracted from the regression @*test@ code), you can define this new class:
!  TSQLFTSTest = class(TSQLRecordFTS3)
!  private
!    fSubject: RawUTF8;
!    fBody: RawUTF8;
!  published
!    property Subject: RawUTF8 read fSubject write fSubject;
!    property Body: RawUTF8 read fBody write fBody;
!  end;
Note that FTS tables must only content @*UTF-8@ text field, that is {\f1\fs20 @*RawUTF8@} (under Delphi 2009/2010/XE/XE2, you could also use the Unicode string type, which is mapped as a UTF-8 text field for the {\i SQLite3} engine).
Then you can add some {\i Body/Subject} content to this FTS3 table, just like any regular {\f1\fs20 TSQLRecord} content, via the @*ORM@ feature of the framework:
!  FTS := TSQLFTSTest.Create;
!  try
!    Check(aClient.TransactionBegin(TSQLFTSTest)); // MUCH faster with this
!    for i := StartID to StartID+COUNT-1 do
!    begin
!!      FTS.DocID := IntArray[i];
!      FTS.Subject := aClient.OneFieldValue(TSQLRecordPeople,'FirstName',FTS.DocID);
!      FTS.Body := FTS.Subject+' bodY'+IntToStr(FTS.DocID);
!      aClient.Add(FTS,true);
!    end;
!    aClient.Commit; // Commit must be BEFORE OptimizeFTS3, memory leak otherwize
!    Check(FTS.OptimizeFTS3Index(Client.fServer));
The steps above are just typical. The only difference with a "standard" @*ORM@ approach is that the {\f1\fs20 DocID} property must be set {\i before} adding the {\f1\fs20 TSQLRecordFTS3} instance: there is no ID automatically created by {\i SQLite}, but an ID must be specified in order to link the FTS record to the original {\f1\fs20 TSQLRecordPeople} row, from its ID.
To support full-text queries, FTS maintains an inverted index that maps from each unique term or word that appears in the dataset to the locations in which it appears within the table contents. The dedicated {\f1\fs20 OptimizeFTS3Index} method is called to merge all existing index b-trees into a single large b-tree containing the entire index. This can be an expensive operation, but may speed up future queries: you should not call this method after every modification of the FTS tables, but after some text has been added.
Then the FTS search query will use the custom {\f1\fs20 FTSMatch} method:
!  Check(aClient.FTSMatch(TSQLFTSTest,'Subject MATCH ''salVador1''',IntResult));
The matching IDs are stored in the {\f1\fs20 IntResult} integer {\i dynamic array}. Note that you can use a regular @*SQL@ query instead. Use of the {\f1\fs20 FTSMatch} method is not mandatory: in fact, it's just a wrapper around the {\f1\fs20 OneFieldValues} method, just using the "neutral" {\i RowID} column name for the results:
!function TSQLRest.FTSMatch(Table: TSQLRecordFTS3Class;
!  const WhereClause: RawUTF8; var DocID: TIntegerDynArray): boolean;
!begin // FTS3 tables do not have any ID, but RowID or DocID
!  result := OneFieldValues(Table,'RowID',WhereClause,DocID);
!end;
An overloaded {\f1\fs20 FTSMatch} method has been defined, and will handle detailed matching information, able to use a ranking algorithm. With this method, the results will be sorted by relevance:
!  Check(aClient.FTSMatch(TSQLFTSTest,'body1*',IntResult,[1,0.5]));
This method expects some additional constant parameters for weighting each FTS table column (there must be the same number of {\f1\fs20 PerFieldWeight} parameters as there are columns in the {\f1\fs20 TSQLRecordFTS3} table). In the above sample code, the {\f1\fs20 Subject} field will have a weight of 1.0, and he {\f1\fs20 Body} will be weighted as 0.5, i.e. any match in the '{\i body}' column content will be ranked twice less than any match in the '{\i subject}', which is probably of higher density.
The above query will call the following SQL statement:
$ SELECT RowID FROM FTSTest WHERE FTSTest MATCH 'body1*'
$ ORDER BY rank(matchinfo(FTSTest),1.0,0.5) DESC
The {\f1\fs20 rank} internal @*SQL function@ has been implemented in Delphi, following the guidelines of the official {\i SQLite3} documentation - as available from their Internet web site at @http://www.sqlite.org/fts3.html#appendix_a - to implement the most efficient way of implementing ranking. It will return the {\f1\fs20 RowID} of documents that match the full-text query sorted from most to least relevant. When calculating relevance, query term instances in the '{\i subject}' column are given twice the weighting of those in the '{\i body}' column.
:  NULL handling
Since you access Delphi properties, NULL doesn't exist as such (it's a @*SQL@ concept). So you will have 0 for an integer field, nil for a field referring to another record, and '' for a string field. At the SQL and @*JSON@ levels, the NULL value does exist and are converted as expected. At higher level (Delphi code or @*JavaScript@/@*AJAX@ code) the NULL value is to be handled explicitly.
In {\i @*SQLite3@} itself, NULL is handled as stated in @http://www.sqlite.org/lang_expr.html (see e.g. IS and IS NOT operators).
There is no direct way of making a difference between NULL and '' for a string field, for example. It can be performed by using a simple SQL statement, which can be added to your database class, as a method common to all your application tables classes. These methods are not yet implemented.
But it's worth noting that NULL handling is not consistent among databases... so we should recommend not using it in any database statements, or only in a 100% compatible way.
:60  ACID and speed
As stated above in @59@, the default {\i SQlite3} write speed is quite slow, when running on a normal hard drive. By default, the engine will pause after issuing a OS-level write command. This guarantees that the data is written to the disk, and features the @*ACID@ properties of the database engine.
You can overwrite this default behavior by setting the {\f1\fs20 TSQLDataBase.Synchronous} property to {\f1\fs20 smOff} instead of the default {\f1\fs20 smFull} setting. When {\f1\fs20 Synchronous} is set to {\f1\fs20 smOff}, {\i SQLite} continues without syncing as soon as it has handed data off to the operating system. If the application running {\i SQLite} crashes, the data will be safe, but the database might become corrupted if the operating system crashes or the computer loses power before that data has been written to the disk surface. On the other hand, some operations are as much as 50 or more times faster with this setting.
When the tests performed during @59@ use {\f1\fs20 Synchronous := smOff}, "Write one" speed is enhanced from 8-9 rows per second into about 400 rows per second, on a physical hard drive (SSD or NAS drives may not suffer from this delay).
So depending on your application requirements, you may switch Synchronous setting to off.
To change the main {\i SQLite3} engine synchronous parameter, you may code for instance:
!Client := TSQLRestClientDB.Create(Model,nil,MainDBFileName,TSQLRestServerDB,false,'');
!!Client.Server.DB.Synchronous := smOff;
Note that this setting is common to a whole {\f1\fs20 TSQLDatabase} instance, so will affect all tables handled by the {\f1\fs20 TSQLRestServerDB} instance.
But if you defined some {\i SQLite3} external tables - see @27@, you can define the setting for a particular external connection, for instance:
!Props := TSQLDBSQLite3ConnectionProperties.Create(DBFileName,'''','');
!VirtualTableExternalRegister(Model,TSQLRecordSample,Props,'SampleRecord');
!Client := TSQLRestClientDB.Create(Model,nil,MainDBFileName,TSQLRestServerDB,false,'');
!!TSQLDBSQLite3Connection(Props.MainConnection).Synchronous := smOff;
By default, the slow but truly {\f1\fs20 ACID} setting will be used with {\i mORMot}, just as with {\i SQlite3}. We do not change this policy, since it will ensure best safety, in the expense of slow writing outside a transaction.
If you can afford loosing some data in very rare border case, or if you are sure your hardware configuration is safe (e.g. if the server is connected to a power inverter and has RAID disks) and that you have backups at hand, setting {\f1\fs20 Synchronous := smOff} would help your application scale. Consider using an external and dedicated database (like {\i Oracle} or MS SQL) if your security expectations are very high, and if the default {\f1\fs20 Synchronous := smFull} safe but slow setting is not enough for you.
In all cases, do not forget to perform @*backup@s as often as possible (at least several times a day). You may use {\f1\fs20 TSQLRestServerDB.Backup} or {\f1\fs20 TSQLRestServerDB.BackupGZ} methods for a fast backup of a running database. Adding a backup feature on the server side is as simple as running:
!Client.Server.BackupGZ(MainDBFileName+'.gz');
Server will stop working during this phase, so a lower-level backup mechanism could be used instead, if you need 100% of service availability. Using an external database would perhaps keep you main {\i mORMot} database small in size, so that its backup time will remain unnoticeable on the client side.
:20 Virtual Tables magic
The {\i @*SQlite3@} engine has the unique ability to create @**Virtual Table@s from code. From the perspective of an @*SQL@ statement, the virtual table object looks like any other table or view. But behind the scenes, queries from and updates to a virtual table invoke callback methods on the virtual table object instead of reading and writing to the database file.
The virtual table mechanism allows an application to publish interfaces that are accessible from SQL statements as if they were tables. SQL statements can in general do anything to a virtual table that they can do to a real table, with the following exceptions:
- One cannot create a trigger on a virtual table.
- One cannot create additional indices on a virtual table. (Virtual tables can have indices but that must be built into the virtual table implementation. Indices cannot be added separately using {\f1\fs20 CREATE INDEX} statements.)
- One cannot run {\f1\fs20 ALTER TABLE ... ADD COLUMN} commands against a virtual table.
- Particular virtual table implementations might impose additional constraints. For example, some virtual implementations might provide read-only tables. Or some virtual table implementations might allow {\f1\fs20 INSERT} or {\f1\fs20 DELETE} but not {\f1\fs20 UPDATE}. Or some virtual table implementations might limit the kinds of {\f1\fs20 UPDATE}s that can be made.
Example of virtual tables, already included in the {\i SQLite3} engine, are @*FTS@ or @*RTREE@ tables.
Our framework introduces new types of custom virtual table. You'll find classes like {\f1\fs20 @*TSQLVirtualTableJSON@} or {\f1\fs20 @*TSQLVirtualTableBinary@} which handle in-memory data structures. Or it might represent a view of data on disk that is not in the {\i SQLite3} format (e.g. {\f1\fs20 TSQLVirtualTableLog}). It can be used to access any external database, just as if they were native {\i SQLite3} tables - see @27@. Or the application might compute the content of the virtual table on demand.
Thanks to the generic implementation of Virtual Table in {\i SQLite3}, you can use such tables in your SQL statement, and even safely execute a {\f1\fs20 SELECT} statement with {\f1\fs20 JOIN} or custom functions, mixing normal {\i SQLite3} tables and any other Virtual Table. From the @*ORM@ point of view, virtual tables are just tables, i.e. they inherit from {\f1\fs20 TSQLRecordVirtual}, which inherits from the common base {\f1\fs20 TSQLRecord} class.
:  Virtual Table module classes
A dedicated mechanism has been added to the framework, beginning with revision 1.13, in order to easily add such virtual tables with pure Delphi code.
In order to implement a new @*Virtual Table@ type, you'll have to define a so called {\i Module} to handle the fields and data access and an associated {\i Cursor} for the {\f1\fs20 SELECT} statements. This is implemented by the two {\f1\fs20 TSQLVirtualTable} and {\f1\fs20 TSQLVirtualTableCursor} classes as defined in the @!TSQLVirtualTable,TSQLVirtualTableCursor,TSQLVirtualTableJSON,TSQLVirtualTableBinary,TSQLVirtualTableLog,TSQLVirtualTableCursorLog,TSQLVirtualTableCursorJSON,TSQLVirtualTableCursorIndex!Lib\SQLite3\SQLite3Commons.pas@ unit.
For instance, here are the default Virtual Table classes deriving from those classes:
\graph HierTSQLVirtualTable Virtual Tables classes hierarchy
\TSQLVirtualTableJSON\TSQLVirtualTable
\TSQLVirtualTableBinary\TSQLVirtualTableJSON
\TSQLVirtualTableLog\TSQLVirtualTable
\TSQLVirtualTableCursorIndex\TSQLVirtualTableCursor
\TSQLVirtualTableCursorJSON\TSQLVirtualTableCursorIndex
\TSQLVirtualTableCursorLog\TSQLVirtualTableCursorIndex
\
{\f1\fs20 @*TSQLVirtualTableJSON@, @*TSQLVirtualTableBinary@} and {\f1\fs20 TSQLVirtualTableCursorJSON} classes will implement a Virtual Table using a {\f1\fs20 @*TSQLRestServerStaticInMemory@} instance to handle fast in-memory @*static@ databases. Disk storage will be encoded either as UTF-8 @*JSON@ (for the {\f1\fs20 TSQLVirtualTableJSON} class, i.e. the '{\f1\fs20 JSON}' module), either in a proprietary @*SynLZ@ compressed format (for the {\f1\fs20 TSQLVirtualTableBinary} class, i.e. the '{\f1\fs20 Binary}' module). File extension on disk will be simply {\f1\fs20 .json} for the '{\f1\fs20 JSON}' module, and {\f1\fs20 .data} for the '{\f1\fs20 Binary}' module. Just to mention the size on disk difference, the 502 KB {\f1\fs20 People.json} content (as created by included regression tests) is stored into a 92 KB {\f1\fs20 People.data} file, in our proprietary optimized format.
Note that the virtual table module name is retrieved from the class name. For instance, the {\f1\fs20 TSQLVirtualTableJSON} class will have its module named as 'JSON' in the SQL code.
To handle external databases, two dedicated classes, named {\f1\fs20 TSQLVirtualTableExternal} and {\f1\fs20 TSQLVirtualTableCursorExternal} will be defined in a similar manner - see @%%HierExternalTables@ @30@.
As you probably have already stated, all those Virtual Table mechanism is implemented in {\f1\fs20 SQLite3Commons}. Therefore, it is independent from the {\i SQLite3} engine, even if, to my knowledge, there is no other SQL database engine around able to implement this pretty nice feature.
:  Defining a Virtual Table module
Here is how the {\f1\fs20 TSQLVirtualTableLog} class type is defined, which will implement a @*Virtual Table@ module named "{\f1\fs20 Log}". Adding a new module is just made by overriding some Delphi methods:
!  TSQLVirtualTableLog = class(TSQLVirtualTable)
!  protected
!    fLogFile: TSynLogFile;
!  public
!    class procedure GetTableModuleProperties(
!      var aProperties: TVirtualTableModuleProperties); override;
!    constructor Create(aModule: TSQLVirtualTableModule; const aTableName: RawUTF8;
!      FieldCount: integer; Fields: PPUTF8CharArray); override;
!    destructor Destroy; override;
!  end;
This module will allow direct Read-Only access to a {\f1\fs20 .log} file content, which file name will be specified by the corresponding SQL table name.
The following method will define the properties of this Virtual Table Module:
!class procedure TSQLVirtualTableLog.GetTableModuleProperties(
!  var aProperties: TVirtualTableModuleProperties);
!begin
!  aProperties.Features := [vtWhereIDPrepared];
!  aProperties.CursorClass := TSQLVirtualTableCursorLog;
!  aProperties.RecordClass := TSQLRecordLogFile;
!end;
The supplied feature set defines a read-only module (since {\f1\fs20 vtWrite} is not selected), and {\f1\fs20 vtWhereIDPrepared} indicates that any {\f1\fs20 RowID=?} SQL statement will be handled as such in the cursor class (we will use the log row as ID number, start counting at 1, so we can speed up {\f1\fs20 RowID=?} WHERE clause easily). The associated cursor class is returned. And a {\f1\fs20 @*TSQLRecord@} class is specified, to define the handled fields - its @*published properties@ definition will be used by the inherited {\f1\fs20 Structure} method to specify to the {\i @*SQLite3@} engine which kind of fields are expected in the SQL statements:
!  TSQLRecordLogFile = class(TSQLRecordVirtualTableAutoID)
!  protected
!    fContent: RawUTF8;
!    fDateTime: TDateTime;
!    fLevel: TSynLogInfo;
!  published
!    /// the log event time stamp
!    property DateTime: TDateTime read fDateTime;
!    /// the log event level
!    property Level: TSynLogInfo read fLevel;
!    /// the textual message associated to the log event
!    property Content: RawUTF8 read fContent;
!  end;
You could have overridden the {\f1\fs20 Structure} method in order to provide the {\f1\fs20 CREATE TABLE} SQL statement expected. But using Delphi class RTTI allows the construction of this SQL statement with the appropriate column type and collation, common to what the rest of the @*ORM@ will expect.
Of course, this {\f1\fs20 RecordClass} property is not mandatory. For instance, the {\f1\fs20 TSQLVirtualTableJSON.GetTableModuleProperties} method won't return any associated {\f1\fs20 TSQLRecordClass}, since it will depend on the table it is implementing, i.e. the running {\f1\fs20 @*TSQLRestServerStaticInMemory@} instance. Instead, the {\f1\fs20 Structure} method is overridden, and will return the corresponding field layout of each associated table.
Here is how the {\f1\fs20 Prepare} method is implemented, and will handle the {\f1\fs20 vtWhereIDPrepared} feature:
!function TSQLVirtualTable.Prepare(var Prepared: TSQLVirtualTablePrepared): boolean;
!begin
!  result := Self<>nil;
!  if result then
!!    if (vtWhereIDPrepared in fModule.Features) and
!!       Prepared.IsWhereIDEquals(true) then
!    with Prepared.Where[0] do begin // check ID=?
!      Value.VType := varAny; // mark TSQLVirtualTableCursorJSON expects it
!      OmitCheck := true;
!      Prepared.EstimatedCost := 1;
!    end else
!      Prepared.EstimatedCost := 1E10; // generic high cost
!end;
Then here is how each '{\f1\fs20 log}' virtual table module instance is created:
!constructor TSQLVirtualTableLog.Create(aModule: TSQLVirtualTableModule;
!  const aTableName: RawUTF8; FieldCount: integer; Fields: PPUTF8CharArray);
!var aFileName: TFileName;
!begin
!  inherited;
!  if (FieldCount=1) then
!    aFileName := UTF8ToString(Fields[0]) else
!    aFileName := aModule.FileName(aTableName);
!  fLogFile := TSynLogFile.Create(aFileName);
!end;
It only associates a {\f1\fs20 TSynLogFile} instance according to the supplied file name (our SQL {\f1\fs20 CREATE VIRTUAL TABLE} statement only expects one parameter, which is the {\f1\fs20 .log} file name on disk - if this file name is not specified, it will use the SQL table name instead).
The {\f1\fs20 TSQLVirtualTableLog.Destroy} destructor will free this {\f1\fs20 fLogFile} instance:
!destructor TSQLVirtualTableLog.Destroy;
!begin
!  FreeAndNil(fLogFile);
!  inherited;
!end;
Then the corresponding cursor is defined as such:
!  TSQLVirtualTableCursorLog = class(TSQLVirtualTableCursorIndex)
!  public
!    function Search(const Prepared: TSQLVirtualTablePrepared): boolean; override;
!    function Column(aColumn: integer; var aResult: TVarData): boolean; override;
!  end;
Since this class inherits from {\f1\fs20 TSQLVirtualTableCursorIndex}, it will have the generic {\f1\fs20 fCurrent / fMax} protected fields, and will have the {\f1\fs20 HasData, Next} and {\f1\fs20 Search} methods using those properties to handle navigation throughout the cursor.
The overridden {\f1\fs20 Search} method consists only in:
!function TSQLVirtualTableCursorLog.Search(
!  const Prepared: TSQLVirtualTablePrepared): boolean;
!begin
!  result := inherited Search(Prepared); // mark EOF by default
!  if result then begin
!    fMax := TSQLVirtualTableLog(Table).fLogFile.Count-1;
!    if Prepared.IsWhereIDEquals(false) then begin
!      fCurrent := Prepared.Where[0].Value.VInt64-1; // ID=? -> index := ID-1
!      if cardinal(fCurrent)<=cardinal(fMax) then
!        fMax := fCurrent else // found one
!        fMax := fCurrent-1;   // out of range ID
!    end;
!  end;
!end;
The only purpose of this method is to handle {\f1\fs20 RowID=?} statement {\f1\fs20 SELECT WHERE} clause, returning {\f1\fs20 fCurrent=fMax=ID-1} for any valid {\f1\fs20 ID}, or {\f1\fs20 fMax<fCurrent}, i.e. no result if the {\f1\fs20 ID} is out of range. In fact, the {\f1\fs20 Search} method of the cursor class must handle all cases which has been notified as handled during the call to the {\f1\fs20 Prepare} method. In our case, since we have set the {\f1\fs20 vtWhereIDPrepared} feature and the {\f1\fs20 Prepare} method identified it in the request and set the {\f1\fs20 OmitCheck} flag, our {\f1\fs20 Search} method MUST handle the {\f1\fs20 RowID=?} case.
If the {\f1\fs20 WHERE} clause is not {\f1\fs20 RowID=?} (i.e. if {\f1\fs20 Prepared.IsWhereIDEquals} returns false), it will return {\f1\fs20 fCurrent=0} and {\f1\fs20 fMax=fLogFile.Count-1}, i.e. it will let the {\i SQLite3} engine loop through all rows searching for the data.
Each column value is retrieved by this method:
!function TSQLVirtualTableCursorLog.Column(aColumn: integer;
!  var aResult: TVarData): boolean;
!var LogFile: TSynLogFile;
!begin
!  result := false;
!  if (self=nil) or (fCurrent>fMax) then
!    exit;
!  LogFile := TSQLVirtualTableLog(Table).fLogFile;
!  if LogFile=nil then
!    exit;
!  case aColumn of
!   -1: SetColumn(aResult,fCurrent+1); // ID = index + 1
!    0: SetColumn(aResult,LogFile.EventDateTime(fCurrent));
!    1: SetColumn(aResult,ord(LogFile.EventLevel[fCurrent]));
!    2: SetColumn(aResult,LogFile.LinePointers[fCurrent],LogFile.LineSize(fCurrent));
!    else exit;
!  end;
!  result := true;
!end;
As stated by the documentation of the {\f1\fs20 TSQLVirtualTableCursor} class, {\f1\fs20 -1} is the column index for the {\f1\fs20 RowID}, and then will follow the columns as defined in the text returned by the {\f1\fs20 Structure} method (in our case, the {\f1\fs20 DateTime, Level, Content} fields of {\f1\fs20 TSQLRecordLogFile}).
The {\f1\fs20 SetColumn} overloaded methods can be used to set the appropriate result to the {\f1\fs20 aResult} variable. For UTF-8 text, it will use a temporary in-memory space, to ensure that the text memory will be still available at least until the next {\f1\fs20 Column} method call.
:  Using a Virtual Table module
From the low-level {\i @*SQLite3@} point of view, here is how this "{\f1\fs20 Log}" @virtual table@ module can be used, directly from the {\i SQLite3} engine.
First we will register this module to a DB connection (this method is to be used only in case of such low-level access - in our @*ORM@ you should never call this method, but {\f1\fs20 TSQLModel. VirtualTableRegister} instead, {\i cf.} next paragraph):
! RegisterVirtualTableModule(TSQLVirtualTableLog,Demo);
Then we can execute the following SQL statement to create the virtual table for the {\f1\fs20 Demo} database connection:
! Demo.Execute('CREATE VIRTUAL TABLE test USING log(temptest.log);');
This will create the virtual table. Since all fields are already known by the {\f1\fs20 TSQLVirtualTableLog} class, it's not necessary to specify the fields at this level. We only specify the log file name, which will be retrieved by {\f1\fs20 TSQLVirtualTableLog. Create} constructor.
! Demo.Execute('select count(*) from test',Res);
! Check(Res=1);
! s := Demo.ExecuteJSON('select * from test');
! s2 := Demo.ExecuteJSON('select * from test where rowid=1');
! s3 := Demo.ExecuteJSON('select * from test where level=3');
You can note that there is no difference with a normal {\i SQLite3} table, from the SQL point of view. In fact, the full power of the SQL language as implemented by {\i SQLite3} - see @http://sqlite.org/lang.html - can be used with any kind of data, if you define the appropriate methods of a corresponding Virtual Table module.
:  Virtual Table, ORM and TSQLRecord
The framework @*ORM@ is able to use @*Virtual Table@ modules, just by defining some {\f1\fs20 @*TSQLRecord@}, inheriting from some {\f1\fs20 TSQLRecordVirtual} dedicated classes:
\graph HierTSQLRecordVirtualTableForcedID Custom Virtual Tables records classes hierarchy
\TSQLRecordVirtualTableAutoID\TSQLRecordVirtual
\TSQLRecordVirtual\TSQLRecord
\TSQLRecordLogFile\TSQLRecordVirtualTableAutoID
\TSQLRecordVirtualTableForcedID\TSQLRecordVirtual
\
{\f1\fs20 TSQLRecordVirtualTableAutoID} children can be defined for Virtual Table implemented in Delphi, with a new {\f1\fs20 ID} generated automatically at {\f1\fs20 INSERT}.
{\f1\fs20 TSQLRecordVirtualTableForcedID} children can be defined for Virtual Table implemented in Delphi, with an {\f1\fs20 ID} value forced at {\f1\fs20 INSERT} (in a similar manner than for {\f1\fs20 TSQLRecordRTree} or {\f1\fs20 TSQLRecordFTS3/4}).
{\f1\fs20 TSQLRecordLogFile} was defined to map the column name as retrieved by the {\f1\fs20 TSQLVirtualTableLog} ('{\f1\fs20 log}') module, and should not to be used for any other purpose.
The Virtual Table module associated from such classes is retrieved from an association made to the server {\f1\fs20 @*TSQLModel@}. In a @*Client-Server@ application, the association is not needed (nor to be used, since it may increase code size) on the Client side. But on the server side, the {\f1\fs20 TSQLModel. VirtualTableRegister} method must be called to associate a {\f1\fs20 TSQLVirtualTableClass} (i.e. a Virtual Table module implementation) to a {\f1\fs20 TSQLRecordVirtualClass} (i.e. its ORM representation).
For instance, the following code will register two {\f1\fs20 TSQLRecord} classes, the first using the '{\f1\fs20 JSON}' virtual table module, the second using the '{\f1\fs20 Binary}' module:
!  Model.VirtualTableRegister(TSQLRecordDali1,TSQLVirtualTableJSON);
!  Model.VirtualTableRegister(TSQLRecordDali2,TSQLVirtualTableBinary);
This registration should be done on the Server side only, {\i before} calling {\f1\fs20 TSQLRestServer.Create} (or {\f1\fs20 TSQLRestClientDB.Create}, for a stand-alone application). Otherwise, an exception is raised at virtual table creation.
:57  In-Memory "static" process
We have seen that the {\f1\fs20 @*TSQLVirtualTableJSON@, @*TSQLVirtualTableBinary@} and {\f1\fs20 TSQLVirtualTableCursorJSON} classes implement a @*Virtual Table@ module using a {\f1\fs20 @**TSQLRestServerStaticInMemory@} instance to handle fast @**static@ in-memory database.
Why use such a database type, when you can create a {\i @*SQLite3@} in-memory table, using the {\f1\fs20 :memory:} file name? That is the question...
- {\i SQlite3} in-memory tables are not persistent, whereas our {\f1\fs20 JSON} or {\f1\fs20 Binary} virtual table modules can be written on disk on purpose, if the {\f1\fs20 aServer.StaticVirtualTable[aClass].CommitShouldNotUpdateFile} property is set to {\f1\fs20 true} - in this case, file writing should be made by calling explicitely the {\f1\fs20 aServer.StaticVirtualTable[aClass].UpdateToFile} method;
- {\i SQlite3} in-memory tables will need two database connections, or call to the {\f1\fs20 @*ATTACH DATABASE@} SQL statement - both of them are not handled natively by our @*Client-Server@ framework;
- {\i SQlite3} in-memory tables are only accessed via SQL statements, whereas {\f1\fs20 @*TSQLRestServerStaticInMemory@} tables can have faster direct access for most common @*REST@ful commands ({\f1\fs20 GET / POST / PUT / DELETE} individual rows) - this could make a difference in server CPU load, especially with the @*Batch@ feature of the framework;
- On the server side, it could be very convenient to have a direct list of in-memory {\f1\fs20 @*TSQLRecord@} instances to work with in pure Delphi code; this is exactly what {\f1\fs20 TSQLRestServerStaticInMemory} allows, and definitively makes sense for an @*ORM@ framework;
- On the client or server side, you could create calculated fields easily with {\f1\fs20 TSQLRestServerStaticInMemory} dedicated "getter" methods written in Delphi, whereas {\i SQlite3} in-memory tables would need additional SQL coding;
- {\i SQLite3} tables are stored in the main database file - in some cases, it could be much convenient to provide some additional table content in some separated database file (for a round robin table, a configuration table written in JSON, some content to be shared among users...): this is made possible using our {\f1\fs20 JSON} or {\f1\fs20 Binary} virtual table modules (but, to be honest, the {\f1\fs20 @*ATTACH DATABASE@} statement could provide a similar feature);
- The {\f1\fs20 TSQLRestServerStaticInMemory} class can be used stand-alone, i.e. without the {\i SQLite3} engine so it could be used to produce small efficient server software - see the "{\f1\fs20 SQLite3\\Samples\\01 - In Memory ORM}" folder.
:   In-Memory tables
A first way of using @*static@ tables, independently from the {\i SQLite3} engine, is to call the {\f1\fs20 TSQLRestServer. StaticDataCreate} method.
This method is only to be called server-side, of course. For the Client, there is no difference between a regular and a static table.
The in-memory {\f1\fs20 @*TSQLRestServerStaticInMemory@} instance handling the storage can be accessed later via the {\f1\fs20 StaticDataServer[]} property array of {\f1\fs20 TSQLRestServer}.
As we just stated, this primitive but efficient database engine can be used without need of the {\i SQLite3} database engine to be linked to the executable, saving some KB of code if necessary. It will be enough to handle most basic @*REST@ful requests.
:   In-Memory virtual tables
A more advanced and powerful way of using @*static@ tables is to define some classes inheriting from {\f1\fs20 TSQLRecordVirtualTableAutoID}, and associate them with some {\f1\fs20 TSQLVirtualTable} classes. The {\f1\fs20 TSQLRecordVirtualTableAutoID} parent class will specify that associated @*virtual table@ modules will behave like normal {\i SQLite3} tables, so will have their {\f1\fs20 RowID} property computed at {\f1\fs20 INSERT}).
For instance, the supplied regression tests define such two tables with three columns, named {\f1\fs20 FirstName}, {\f1\fs20 YearOfBirth} and {\f1\fs20 YearOfDeath}, after the @*published properties@ definition:
!  TSQLRecordDali1 = class(TSQLRecordVirtualTableAutoID)
!  private
!    fYearOfBirth: integer;
!    fFirstName: RawUTF8;
!    fYearOfDeath: word;
!  published
!    property FirstName: RawUTF8 read fFirstName write fFirstName;
!    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
!    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
!  end;
!  TSQLRecordDali2 = class(TSQLRecordDali1);
Both class types are then added to the {\f1\fs20 @*TSQLModel@} instance of the application, common to both Client and Server side:
!  ModelC := TSQLModel.Create(
!    [TSQLRecordPeople,  (...)
!     TSQLRecordDali1,TSQLRecordDali2],'root');
Then, on the Server side, the corresponding Virtual Table modules are associated with those both classes:
!  ModelC.VirtualTableRegister(TSQLRecordDali1,TSQLVirtualTableJSON);
!  ModelC.VirtualTableRegister(TSQLRecordDali2,TSQLVirtualTableBinary);
Thanks to the {\f1\fs20 VirtualTableRegister} calls, on the server side, the '{\f1\fs20 JSON}' and '{\f1\fs20 Binary}' Virtual Table modules will be launched automatically when the {\i @*SQLite3@} DB connection will be initialized:
!  Client := TSQLRestClientDB.Create(ModelC,nil,Demo,TSQLRestServerTest);
This {\f1\fs20 @*TSQLRestClientDB@} has in fact a {\f1\fs20 @*TSQLRestServerDB@} instance running, which will be used for all Database access, including Virtual Table process.
Two files will be created on disk, named '{\f1\fs20 Dali1.json}' and '{\f1\fs20 Dali2.data}'. As stated above, the JSON version will be much bigger, but also more easy to handle from outside the application.
From the code point of view, there is no difference in our @*ORM@ with handling those virtual tables, compared to regular {\f1\fs20 @*TSQLRecord@} tables. For instance, here is some code extracted from the supplied regression tests:
!!  if aClient.TransactionBegin(TSQLRecordDali1) then
!  try
!    // add some items to the file
!    V2.FillPrepare(aClient,'LastName=:("Dali"):');
!    n := 0;
!    while V2.FillOne do begin
!      VD.FirstName := V2.FirstName;
!      VD.YearOfBirth := V2.YearOfBirth;
!      VD.YearOfDeath := V2.YearOfDeath;
!      inc(n);
!!      Check(aClient.Add(VD,true)=n,Msg);
!    end;
!    // update some items in the file
!    for i := 1 to n do begin
!!      Check(aClient.Retrieve(i,VD),Msg);
!      Check(VD.ID=i);
!      Check(IdemPChar(pointer(VD.FirstName),'SALVADOR'));
!      Check(VD.YearOfBirth=1904);
!      Check(VD.YearOfDeath=1989);
!      VD.YearOfBirth := VD.YearOfBirth+i;
!      VD.YearOfDeath := VD.YearOfDeath+i;
!!      Check(aClient.Update(VD),Msg);
!    end;
!    // check SQL requests
!    for i := 1 to n do begin
!!      Check(aClient.Retrieve(i,VD),Msg);
!      Check(VD.YearOfBirth=1904+i);
!      Check(VD.YearOfDeath=1989+i);
!    end;
!!    Check(aClient.TableRowCount(TSQLRecordDali1)=1001);
!!    aClient.Commit;
!  except
!!    aClient.RollBack;
!  end;
A {\f1\fs20 Commit} is needed from the Client side to write anything on disk. From the Server side, in order to create disk content, you'll have to explicitly call such code on purpose:
As we already noticed, data will be written by default on disk with our {\f1\fs20 @*TSQLRestServerStaticInMemory@}-based virtual tables. In fact, the {\f1\fs20 Commit} method in the above code will call {\f1\fs20 TSQLRestServerStaticInMemory.UpdateFile}.
Please note that the {\i @*SQlite3@} engine will handle any Virtual Table just like regular {\i SQLite3} tables, concerning the @*atomic@ity of the data. That is, if no explicit @*transaction@ is defined (via {\f1\fs20 TransactionBegin / Commit} methods), such a transaction will be performed for every database modification (i.e. all @*CRUD@ operations, as {\f1\fs20 INSERT / UPDATE / DELETE}). The {\f1\fs20 TSQLRestServerStaticInMemory. UpdateToFile} method is not immediate, because it will write all table data each time on disk. It's therefore mandatory, for performance reasons, to nest multiple modification to a Virtual Table with such a transaction, for better performance. And in all cases, it's the standard way of using the ORM. If for some reason, you later change your mind and e.g. move your table from the {\f1\fs20 TSQLVirtualTableJSON / TSQLVirtualTableBinary} engine to the default {\i SQlite3} engine, your code could remain untouched.
It's possible to force the In-Memory virtual table data to stay in memory, and the {\f1\fs20 COMMIT} statement to write nothing on disk, using the following property:
! Server.StaticVirtualTable[TSQLRecordDali1].CommitShouldNotUpdateFile := true;
In order to create disk content, you'll then have to explicitly call the corresponding method on purpose:
! Server.StaticVirtualTable[TSQLRecordDali1].UpdateToFile;
Since {\f1\fs20 StaticVirtualTable} property is only available on the Server side, you are the one to blame if your client updates the table data and this update never reachs the disk!
:  Virtual Tables to access external databases
As will be stated @27@, some external databases may be accessed by our ORM.
The @*Virtual Table@ feature of {\i SQLite3} will allow those remote tables to be accessed just like "native" {\i SQLite3} tables - in fact, you may be able e.g. to write a valid SQL query with a {\f1\fs20 JOIN} between {\i SQlite3} tables, {\i Microsoft SQL Server}, {\i MySQL} and {\i Oracle} databases, even with multiple connections and several remote servers. Think as an ORM-based {\i Business Intelligence} from any database source. Added to our code-based reporting engine (able to generate @*pdf@), it could be a very powerful way of consolidating any kind of data.
In order to define such {\i external} tables, you define your regular {\f1\fs20 @*TSQLRecord@} classes as usual, then a call to the {\f1\fs20 @**VirtualTableExternalRegister@()} function will define this class to be managed as a virtual table, from an external database engine. Using a dedicated external database server may allow better response time or additional features (like data sharing with other applications or languages). Server-side may omit a call to {\f1\fs20 VirtualTableExternalRegister()} if the need of an internal database is expected: it will allow custom database configuration at runtime, depending on the customer's expectations (or license).
:27 External database access
:  Database agnosticism
Since revision 1.15, our @*ORM@ @*REST@ful framework is able to access any available database engine, via a set of generic units and classes.
The framework still relies on {\i @*SQLite3@} as its SQL core on the server, but a dedicated mechanism allows access to any remote database, and mix those tables content with the native ORM tables of the framework. Thanks to the unique @*Virtual Table@s mechanism of {\i SQLite3}, those external tables may be accessed as native {\i SQLite3} tables in our SQL statements. See @%%mORMotDesign3@.
The current list of available external database classes is:
- Any {\i @*OleDB@} provider (including {\i @*MS SQL@ Server, Jet} or others);
- Any {\i @*ODBC@} provider (including {\i FireBird, MySQL}, or others);
- {\i @*Oracle@} direct access (via OCI);
- A {\i SQLite3} database file.
This list is not closed, and may be completed in the near future. Any help is welcome here: it's not difficult to implement a new unit, following the patterns already existing. You may start from an existing driver (e.g. {\i Zeos} or {\i Alcinoe} libraries). Open Source contribution are always welcome!
In fact, {\i OleDB} is a good candidate for database access with good performance, Unicode native, with a lot of available providers. Thanks to {\i OleDB}, we are already able to access to almost any existing database. The code overhead in the server executable will also be much less than with adding any other third-party Delphi library. And we will let Microsoft or the {\i OleDB} provider perform all the testing and debugging for each driver.
Since revision 1.17, direct access to the {\i ODBC} layer has been included to the framework database units. It has a wider range of free providers (including e.g. {\i MySQL} or {\i FireBird}), and is the official replacement for {\i OleDB} (next version of {\i MS SQL Server} will provide only ODBC providers, as far as {\i Microsoft} warned its customers).
An {\i Oracle} dedicated direct access was added, because all available OleDB providers for Oracle (i.e. both Microsoft's and Oracle's) do have problems with handling BLOB, and we wanted our Clients to have a light-weight and as fast as possible access to this great database.
Thanks to the design of our classes, it was very easy (and convenient) to implement {\i SQLite3} direct access. It's even used for our regression tests, in order to implement stand-alone unitary testing.
:   Direct access to any Database engine
The {\f1\fs20 @**SynDB@} units have the following features:
- Direct fast access to {\i OleDB, ODBC, Oracle} (via OCI) or {\i SQLite3} (statically linked) databases;
- Generic abstract @*OOP@ layout, able to work with any SQL-based database engine;
- Tested with @*MS SQL@ Server 2008, Oracle 11g, and the latest {\i SQLite3} engine;
- Could access any local or remote Database, from any edition of Delphi (even {\i Delphi 7 personal}, the {\i Turbo Explorer} or {\i Starter edition}), just for free (in fact, it does not use the {\f1\fs20 DB.pas} standard unit and all its dependencies);
- Ability to be truly Unicode, even with pre-Unicode version of Delphi (like Delphi 7 or 2007) - use internally UTF-8 encoding;
- Handle NULL or BLOB content for parameters and results, including stored procedures;
- Avoid most memory copy or unnecessary allocation: we tried to access the data directly from the retrieved data buffer, just as given from {\i OleDB / ODBC} or the low-level database client (e.g. OCI for Oracle, or the {\i SQLite3} engine);
- Designed to achieve the best possible performance on 32 bit or @*64 bit@ Windows: most time is spent in the database provider (OleDB, ODBC, OCI, {\i SQLite3}) - the code layer added to the database client is very thin and optimized;
- Could be safely used in a multi-threaded application/server (with dedicated thread-safe methods, usable even if the database client is not officially multi-thread);
- Allow parameter bindings of @*prepared@ requests, with fast access to any parameter or column name (thanks to {\f1\fs20 @*TDynArrayHashed@});
- Column values accessible with most Delphi types, including {\f1\fs20 Variant} or generic {\f1\fs20 string / WideString}.
- Available {\f1\fs20 ISQLDBRows} interface - to avoid typing {\f1\fs20 try...finally Query.Free end;} and allow one-line SQL statement;
- Late-binding column access, via a custom variant type;
- Direct @*JSON@ content creation, with no temporary data copy nor allocation (this feature will be the most used in our JSON-based ORM server);
- High-level catalog / database layout abstract methods, able to retrieve the table and column properties (including indexes), for database reverse-engineering; provide also SQL statements to create a table or an index in a database-abstract manner; those features will be used directly by our ORM;
- Designed to be used with our ORM, but could be used stand-alone (a full Delphi 7 client executable is just about 200 KB), or even in any existing Delphi application, thanks to a {\f1\fs20 TQuery}-like wrapper;
- {\f1\fs20 TQuery} {\i emulation class}, for direct re-use with existing code, in replacement to the deprecated BDE technology;
- Free {\f1\fs20 @*SynDBExplorer@} tool provided, which is a small but efficient way of running queries in a simple User Interface, about all available engines; it is also a good sample program of a stand-alone usage of those libraries.
:   Data types
Of course, our ORM does not need a whole feature set (do not expect to use this database classes with your VCL DB RAD components), but handles directly the basic SQL column types, as needed by our ORM (derived from SQLite's internal column types): {\f1\fs20 NULL, Int64, Double, @*Currency@, DateTime, @*RawUTF8@} and {\f1\fs20 BLOB}.
They are defined as such in {\f1\fs20 @*SynDB@}:
!  TSQLDBFieldType =
!    (ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUTF8, ftBlob);
Those types will map low-level database-level access types, not high-level Delphi types as {\f1\fs20 TSQLFieldType} defined in {\f1\fs20 SQLite3Commons}, or the generic huge {\f1\fs20 TFieldType} as defined in the standard VCL {\f1\fs20 DB.pas} unit. In fact, it is more tied to the standard {\i @*SQLite3@} generic types, i.e. NULL, INTEGER, REAL, TEXT, BLOB (with the addition of a {\f1\fs20 ftCurrency} and {\f1\fs20 ftDate} type, for better support of most DB engines) see @http://www.sqlite.org/datatype3.html.
You can note that the only {\f1\fs20 string} type handled here uses UTF-8 encoding (implemented using our {\f1\fs20 RawUTF8} type), for cross-Delphi true Unicode process. Code can access to the textual data via {\f1\fs20 variant, string} or {\f1\fs20 widestring} variables and parameters, but our units will use UTF-8 encoding internally. It will therefore interface directly with our ORM, which uses the same encoding.
BLOB columns or parameters are accessed as {\f1\fs20 RawByteString} variables, which may be mapped to a standard {\f1\fs20 TStream} via our {\f1\fs20 TRawByteStringStream}.
:   SynDB Units
Here are the units implementing the external database-agnostic features:
|%30%70
|\b File|Description\b0
|{\f1\fs20 SynDB}|abstract database direct access classes
|{\f1\fs20 SynOleDB}|@*OleDB@ direct access classes
|{\f1\fs20 SynDBODBC}|@*ODBC@ direct access classes
|{\f1\fs20 SynDBOracle}|@*Oracle@ DB direct access classes (via OCI)
|{\f1\fs20 SynDBSQLite3}|@*SQLite3@ direct access classes
|%
It's worth noting that those units only depend on {\f1\fs20 SynCommons}, therefore are independent of the ORM part of our framework. They may be used separately, accessing all those external databases with regular SQL code. Since all their classes inherit from abstract classes defined in {\f1\fs20 SynDB}, switching from one database engine to another is just a matter of changing a class type.
:   Classes and generic use
The data is accessed via three families of classes:
- {\i Connection properties}, which store the database high-level properties (like database implementation classes, server and database name, user name and password);
- {\i Connections}, which implements an actual connection to a remote database, according to the specified {\i Connection properties} - of course, there can be multiple {\i connections} for the same {\i connection properties} instance;
- {\i Statements}, which are individual SQL queries or requests, which may be multiple for one existing {\i connection}.
In practice, you define a {\f1\fs20 TSQLDBConnectionProperties} instance, then you derivate {\f1\fs20 TSQLDBConnection} and {\f1\fs20 TSQLDBStatement} instances using dedicated {\f1\fs20 NewConnection} / {\f1\fs20 ThreadSafeConnection} / {\f1\fs20 NewStatement} methods.
Here is some working sample program, using our {\f1\fs20 SynOleDB} unit to connect to a local {\i @*MS SQL@ Server 2008 R2 Express edition}, which will write a file with the JSON representation of the {\f1\fs20 Person.Address} table of the sample database {\i AdventureWorks2008R2}:
!program TestOleDB;
!{$APPTYPE CONSOLE}
!uses
!  SysUtils,
!  Classes,
!  SynCommons,
!  SynOleDB;
!
!var Props: TOleDBConnectionProperties;
!    Conn: TSQLDBConnection;
!    Query: TSQLDBStatement;
!    F: TFileStream;
!begin
!  with OleDBSynLogClass.Family do begin
!    Level := LOG_VERBOSE;
!    AutoFlushTimeOut := 10;
!  end;
!!  Props := TOleDBMSSQLConnectionProperties.Create('.\SQLEXPRESS','AdventureWorks2008R2','','');
!  try
!    //Props.ConnectionStringDialogExecute;
!!    Conn := Props.NewConnection;
!    try
!!      Query := Conn.NewStatement;
!      try
!!        Query.Execute('select * from Person.Address',true,[]);
!        F := TFileStream.Create(ChangeFileExt(paramstr(0),'.json'),fmCreate);
!        try
!!          Query.FetchAllToJSON(F,false);
!        finally
!          F.Free;
!        end;
!      finally
!        Query.Free;
!      end;
!    finally
!      Conn.Free;
!    end;
!  finally
!    Props.Free;
!  end;
!end.
Here are the general class hierarchy, for all available remote {\i connection properties}:
\graph HierTSQLDBConnectionProperties TSQLDBConnectionProperties classes hierarchy
\TOleDBMySQLConnectionProperties\TOleDBConnectionProperties
\TSQLDBSQLite3ConnectionProperties\TSQLDBConnectionProperties
\TSQLDBOracleConnectionProperties\TSQLDBConnectionPropertiesThreadSafe
\TODBCConnectionProperties\TSQLDBConnectionPropertiesThreadSafe
\TOleDBMSOracleConnectionProperties\TOleDBOracleConnectionProperties
\TOleDBOracleConnectionProperties\TOleDBConnectionProperties
\TOleDBODBCSQLConnectionProperties\TOleDBConnectionProperties
\TOleDBMSSQLConnectionProperties\TOleDBConnectionProperties
\TOleDBConnectionProperties\TSQLDBConnectionPropertiesThreadSafe
\TSQLDBConnectionPropertiesThreadSafe\TSQLDBConnectionProperties
\
Then the following {\i connection} classes are defined - the usable classes are {\f1\fs20 TOleDBConnection, TODBCConnection, TSQLDBOracleConnection} and {\f1\fs20 TSQLDBSQLite3Connection}:
\graph HierTSQLDBConnection TSQLDBConnection classes hierarchy
\TSQLDBConnectionThreadSafe\TSQLDBConnection
\TOleDBConnection\TSQLDBConnectionThreadSafe
\TODBCConnection\TSQLDBConnectionThreadSafe
\TSQLDBOracleConnection\TSQLDBConnectionThreadSafe
\TSQLDBSQLite3Connection\TSQLDBConnection
\
Each connection may create a corresponding {\i statement} instance:
\graph HierTSQLDBStatement TSQLDBStatement classes hierarchy
\TOleDBStatement\TSQLDBStatement
\TSQLDBSQLite3Statement\TSQLDBStatement
\TSQLDBStatementWithParams\TSQLDBStatement
\TSQLDBStatementWithParamsAndColumns\TSQLDBStatementWithParams
\TODBCStatement\TSQLDBStatementWithParamsAndColumns
\TSQLDBOracleStatement\TSQLDBStatementWithParamsAndColumns
\
You can specify parameters, bound to the request, as such:
!Query.Execute('select * from Person.Customer where Name like ?',true,['B%']);
Or using direct {\i Bind*()} methods over a prepared statement. All bound parameters will appear within the SQL statement, when @*log@ged using our {\f1\fs20 TSynLog} classes - see @16@.
:   ISQLDBRows interface
In order to allow shorter code, an interface type can be used to reference a statement instance.
It allows writing code as such:
!procedure WriteFamily(const aName: RawUTF8);
!var I: ISQLDBRows;
!begin
!  I := MyConnProps.Execute('select * from table where name=?',[aName]);
!  while I.Step do
!    writeln(I['FirstName'],' ',DateToStr(I['BirthDate']));
!end;
In this procedure, no {\f1\fs20 TSQLDBStatement} is defined, and there is no need to add a {\f1\fs20 try ... finally Query.Free; end;} block.
In fact, the {\f1\fs20 MyConnProps.Execute} method returns a {\f1\fs20 TSQLDBStatement} instance as a {\f1\fs20 ISQLDBRows}, which methods can be used to loop for each result row, and retrieve individual column values. In the code above, {\f1\fs20 I['FirstName']} will in fact call the {\f1\fs20 I.Column[]} default property, which will return the column value as a {\f1\fs20 variant}. You have other dedicated methods, like {\f1\fs20 ColumnUTF8} or {\f1\fs20 ColumnInt}, able to retrieve directly the expected data.
:   Late binding
We implemented late binding access of column values, via a custom variant time. It uses the internal mechanism used for {\i Ole Automation}, here to access column content as if column names where native object properties.
The resulting Delphi code to write is just clear and obvious:
!Props := TOleDBMSSQLConnectionProperties.Create('.\SQLEXPRESS','AdventureWorks2008R2','','');
!procedure TestISQLDBRowsVariant;
!var Row: Variant;
!begin
!  OleDBSynLogClass.Enter;
!  with Props.Execute('select * from Sales.Customer where AccountNumber like ?',
!    ['AW000001%'],@Row) do
!    while Step do
!      assert(Copy(Row.AccountNumber,1,8)='AW000001');
!end;
Note that {\f1\fs20 Props.Execute} returns an {\f1\fs20 ISQLDBRows} interface, so the code above will initialize (or reuse an existing) thread-safe connection (OleDB uses a per-thread model), initialize a statement, execute it, access the rows via the {\f1\fs20 Step} method and the {\f1\fs20 Row} variant, retrieving the column value via a direct {\f1\fs20 Row.AccountNumber} statement.
The above code is perfectly safe, and all memory will be released with the reference count garbage-collector feature of the {\f1\fs20 ISQLDBRows} interface. You are not required to add any {\f1\fs20 try..finally Free; end} statements in your code.
This is the magic of late-binding in Delphi. Note that a similar feature is available for our {\f1\fs20 SynBigTable} unit.
In practice, this code is slower than using a standard property based access, like this:
!while Step do
!  assert(Copy(ColumnUTF8('AccountNumber'),1,8)='AW000001');
But the first version, using late-binding of column name, just sounds more natural.
Of course, since it's {\i late}-binding, we are not able to let the compiler check at compile time for the column name. If the column name in the source code is wrong, an error will be triggered at runtime only.
First of all, let's see the fastest way of accessing the row content.
In all cases, using the textual version of the column name ({\f1\fs20 'AccountNumber'}) is slower than using directly the column index. Even if our {\f1\fs20 @*SynDB@} library uses a fast lookup using hashing, the following code will always be faster:
!var Customer: Integer;
!begin
!  with Props.Execute(
!    'select * from Sales.Customer where AccountNumber like ?',
!    ['AW000001%'],@Customer) do begin
!    Customer := ColumnIndex('AccountNumber');
!    while Step do
!      assert(Copy(ColumnString(Customer),1,8)='AW000001');
!  end;
!end;
But to be honest, after profiling, most of the time is spend in the {\f1\fs20 Step} method, especially in {\f1\fs20 fRowSet.GetData}. In practice, I was not able to notice any speed increase worth mentioning, with the code above.
Our name lookup via a hashing function (i.e. {\f1\fs20 TDynArrayHashed}) just does its purpose very well.
On the contrary the {\i Ole-Automation} based late binding was found out to be much slower, after profiling. In fact, the {\f1\fs20 Row.AccountNumber} expression calls an hidden {\f1\fs20 DispInvoke} function, which is slow when called multiple times. Our {\f1\fs20 SynCommons} unit is able to hack the VCL, and by patching the VCL code in-memory, will call an optimized version of this function. Resulting speed is very close to direct {\f1\fs20 Column['AccountNumber']} call. See @SDD-DI-2.2.3@.
:  Database access
:   OleDB or ODBC to rule them all
{\i @**OleDB@} (Object Linking and Embedding, Database, sometimes written as OLE DB or OLE-DB) is an API designed by Microsoft for accessing data from a variety of sources in a uniform manner.
;It was designed as a higher-level replacement for, and successor to, ODBC, extending its feature set to support a wider variety of non-relational databases, such as object databases and spreadsheets that do not necessarily implement SQL.
Of course, you have got the {\i Microsoft SQL Native Client} to access the @**MS SQL@ Server 2005/2008, but {\i @*Oracle@} provides a native OleDB provider (even if we found out that this Oracle provider, including the Microsoft's version, have problems with BLOBs). Do not forget about the {\i Advantage Sybase OleDB} driver and such...
{\i @**ODBC@} ({\i Open DataBase Connectivity}) is a standard C programming language middleware API for accessing database management systems (DBMS). {\i ODBC} was originally developed by Microsoft during the early 1990s, then was deprecated in favor to {\i OleDB}. More recently, Microsoft is officially deprecating {\i OleDB}, and urge all developers to switch to the open and cross-platform {\i ODBC} API for native connection. Back & worse strategy from Micro$oft... one more time!\line @http://blogs.msdn.com/b/sqlnativeclient/archive/2011/08/29/microsoft-is-aligning-with-odbc-for-native-relational-data-access.aspx
By using our own {\i OleDB} and {\i ODBC} implementations, we will for instance be able to convert directly the {\i OleDB} or {\i ODBC} binary rows to @*JSON@, with no temporary conversion into the Delphi high-level types (like temporary string or variant allocations). The resulting performance is much higher than using standard {\f1\fs20 TDataSet} or other components, since we will bypass most of the layers introduced by BDE/dbExpress/AnyDAC/ZDBC component sets.
Most {\i OleDB / ODBC} providers are free (even maintained by the database owner), others would need a paid license.
It is worth saying that, when used in a {\i mORMot} Client-Server architecture, object persistence using an {\i OleDB} or {\i ODBC} remote access expects only the database instance to be reachable on the Server side. Clients could communicate via standard HTTP, so won't need any specific port forwarding or other IT configuration to work as expected.
:   Oracle via OCI
For our framework, and in completion to our {\f1\fs20 SynOleDB / SynDBODBC} units, the {\f1\fs20 SynDBOracle} unit has been implemented. It allows direct access to any remote @**Oracle@ server, using the {\i Oracle Call Interface}.
{\i Oracle Call Interface} (OCI) is the most comprehensive, high performance, native unmanaged interface to the Oracle Database that exposes the full power of the Oracle Database. A direct interface to the {\f1\fs20 oci.dll} library was written, using our DB abstraction classes introduced in {\f1\fs20 @*SynDB@}.
We tried to implement all best-practice patterns detailed in the official {\i Building High Performance Drivers for Oracle} reference document.
Resulting speed is quite impressive: for all requests, {\f1\fs20 SynDBOracle} is 3 to 5 times faster than a {\f1\fs20 SynOleDB} connection using the native {\i OleDB Provider} supplied by Oracle. A similar (even worse) speed penalty has been observer in comparison with the official ODBC driver from Oracle, via a {\f1\fs20 SynDBODBC}-based connection. We noted also that our implementation is 10 times faster than the one provided with ZEOS/ZDBC, which is far from optimized (it retrieves the rows one by one from OCI).
You can use the latest version of the {\i Oracle Instant Client} (OIC) provided by Oracle - see @http://www.oracle.com/technetwork/database/features/instant-client - which allows to run client applications without installing the standard (huge) Oracle client or having an {\f1\fs20 ORACLE_HOME}. Just deliver the few {\f1\fs20 dll} files in the same directory than the application (probably a {\i mORMot} server), and it will work at amazing speed, with all features of Oracle (other stand-alone direct Oracle access library rely on deprecated Oracle 8 protocol).
\graph OCIdirect Oracle Connectivity with SynDBOracle
\RAD Application\DBExpress¤or BDE
\DBExpress¤or BDE\ installed¤Oracle Client
\ installed¤Oracle Client\ Oracle Server\TCP/IP
\mORMot Application\installed¤Oracle Client
\installed¤Oracle Client\Oracle Server \TCP/IP
\mORMot Application¤with OIC dlls\Oracle Server\TCP/IP
\
It is worth saying that, when used in a {\i mORMot} Client-Server architecture, object persistence using an {\i Oracle} database expects only the Oracle instance to be reachable on the Server side, just like with {\i OleDB} or {\i ODBC}.
Here are the main features of this unit:
- {\i Direct access} to the {\i Oracle Call Interface} (OCI) client, with no BDE, Midas, DBExpress, nor {\i OleDB / ODBC} provider necessary;
- Dedicated to work with {\i any version} of the Oracle OCI interface, starting from revision 8;
- {\i Optimized for the latest features} of Oracle 11g (e.g. using native {\f1\fs20 Int64} for retrieving NUMBER fields with no decimal);
- Able to work with the {\i Oracle Instant Client} for {\i No Setup} applications (installation via file/folder copy);
- {\i Natively Unicode} (uses internal UTF-8 encoding), for all version of Delphi, with special handling of each database char-set;
- Tried to achieve {\i best performance available} from every version of the Oracle client;
- Designed to work under {\i any version of Windows}, either in 32 or @*64 bit@ architecture (but the OCI library must be installed in the same version than the compiled Delphi application, i.e. only 32 bit for this current version);
- {\i Late-binding} access to column names, using a new dedicated {\f1\fs20 Variant} type (similar to Ole Automation runtime properties);
- Connections are {\i multi-thread ready} with low memory and CPU resource overhead;
- Can use connection strings like {\f1\fs20 '//host[:port]/[service_name]'}, avoiding use of the {\f1\fs20 TNSNAME.ORA} file;
- Use {\i Rows Array} and {\i BLOB fetching}, for best performance (ZEOS/ZDBC did not handle this, for instance);
- Implements {\i @*Array Bind@ing} for very fast bulk modifications - insert, update or deletion of a lot of rows at once - see @59@;
- Handle {\i Prepared Statements} - but by default, we rely on OCI-side statement cache, if available;
- Native {\i export to @*JSON@} methods, which will be the main entry point for our @*ORM@ framework.
:   SQLite3
For our ORM framework, we implemented an efficient {\i @*SQLite3@} wrapper, joining statically (i.e. without any external {\f1\fs20 dll}, but within the main {\f1\fs20 exe}) the {\i SQLite3} engine to the executable.
It was an easy task to let the {\f1\fs20 SynSQLite3.pas} unit be called from our {\f1\fs20 @*SynDB@} database abstract classes. Adding such another Database is just a very thin layer, implemented in the {\f1\fs20 SynDBSQLite3.pas} unit.
To create a {\i connection property} to an existing {\i SQLite3} database file, call the {\f1\fs20 TSQLDBSQLite3ConnectionProperties. Create} constructor, with the actual {\i SQLite3} database file as {\f1\fs20 ServerName} parameter, and (optionally the proprietary encryption password in {\f1\fs20 Password} - available since rev. 1.16); others ({\f1\fs20 DataBaseName, UserID}) are just ignored.
This classes will implement an internal statement cache, just as the one used for {\f1\fs20 @*TSQLRestServerDB@}. In practice, using the cache can make process up to two times faster (when processing small requests).
:  ORM Integration
:   Transparent use
An {\i external} record can be defined as such:
!type
!  TSQLRecordPeopleExt = class(TSQLRecord)
!  private
!    fData: TSQLRawBlob;
!    fFirstName: RawUTF8;
!    fLastName: RawUTF8;
!    fYearOfBirth: integer;
!    fYearOfDeath: word;
!    fLastChange: TModTime;
!    fCreatedAt: TCreateTime;
!  published
!    property FirstName: RawUTF8 index 40 read fFirstName write fFirstName;
!    property LastName: RawUTF8 index 40 read fLastName write fLastName;
!    property Data: TSQLRawBlob read fData write fData;
!    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
!    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
!    property LastChange: TModTime read fLastChange write fLastChange;
!    property CreatedAt: TCreateTime read fCreatedAt write fCreatedAt;
!  end;
As you can see, there is no difference with an {\i internal} ORM class: it inherits from {\f1\fs20 @*TSQLRecord@}, but you may want it to inherit from {\f1\fs20 TSQLRecordMany} to use @58@ for instance.
The only difference is this {\f1\fs20 index 40} attribute in the definition of {\f1\fs20 FirstName} and {\f1\fs20 LastName} @*published properties@: this will define the length (in {\f1\fs20 WideChar}) to be used when creating the external field for TEXT column. In fact, {\i SQLite3} does not care about textual field length, but almost all other database engines expect a maximum length to be specified when defining a {\f1\fs20 VARCHAR} column in a table. If you do not specify any length in your field definition (i.e. if there is no {\f1\fs20 index ???} attribute), the ORM will create a column with an unlimited length (e.g. {\f1\fs20 varchar(max)} for {\i @*MS SQL@ Server} in this case, code will work, but performance and disk usage may be degraded.
Here is an extract of the regression test corresponding to external databases:
!var RInt: TSQLRecordPeople;
!    RExt: TSQLRecordPeopleExt;
!  (...)
!fConnection := TSQLDBSQLite3ConnectionProperties.Create(':memory:','','','');
!!VirtualTableExternalRegister(fModel,TSQLRecordPeopleExt,fConnection,'PeopleExternal');
!fClient := TSQLRestClientDB.Create(fModel,nil,'test.db3',TSQLRestServerDB);
!!fClient.Server.StaticVirtualTableDirect := StaticVirtualTableDirect;
!!fClient.Server.CreateMissingTables;
!(...)
!while RInt.FillOne do begin
!  RExt.Data := RInt.Data;
!(...)
!!  aID := fClient.Add(RExt,true);
!(...)
!!  Check(fClient.Retrieve(aID,RExt));
!(...)
!end;
!!Check(fClient.Server.CreateSQLMultiIndex(
!!  TSQLRecordPeopleExt,['FirstName','LastName'],false));
!Check(RInt.FillRewind);
!while RInt.FillOne do begin
!!  RExt.FillPrepare(fClient,'FirstName=? and LastName=?',
!!    [RInt.FirstName,RInt.LastName]); // query will use index -> fast :)
!!  while RExt.FillOne do begin
!!    Check(RExt.FirstName=RInt.FirstName);
!(...)
!  end;
!end;
!Now := fClient.ServerTimeStamp;
!for i := 1 to aID do
!  if i mod 100=0 then begin
!!    Check(fClient.Retrieve(i,RExt,true),'for update');
!    RExt.YearOfBirth := RExt.YearOfDeath;
!!    Check(fClient.Update(RExt),'Update 1/100 rows');
!!    Check(fClient.UnLock(RExt));
!  end;
!for i := 1 to aID do
!  if i and 127=0 then
!!    Check(fClient.Delete(TSQLRecordPeopleExt,i),'Delete 1/128 rows');
!for i := 1 to aID do begin
!!  ok := fClient.Retrieve(i,RExt,false);
!  Check(ok=(i and 127<>0),'deletion');
!  if ok then begin
!    Check(RExt.CreatedAt<=Now);
!    if i mod 100=0 then begin
!      Check(RExt.YearOfBirth=RExt.YearOfDeath,'Updated');
!      Check(RExt.LastChange>=Now);
!    end else begin
!      Check(RExt.YearOfBirth<>RExt.YearOfDeath,'Not Updated');
!      Check(RExt.LastChange<=Now);
!    end;
!  end;
!end;
As you can see, there is no difference with using the local {\i SQLite3} engine or a remote database engine. From the Client point of view, you just call the usual RESTful methods, i.e. {\f1\fs20 Add / Retrieve / Update / UnLock / Delete}, and you can even handle advanced methods like a {\f1\fs20 FillPrepare} with a complex WHERE clause, or {\f1\fs20 CreateSQLMultiIndex / CreateMissingTables} on the server side. Even the creation of the table in the remote database (the {\f1\fs20 'CREATE TABLE...'} SQL statement) is performed by the framework, with the appropriate column properties according to the database expectations (e.g. a {\f1\fs20 TEXT} for {\i SQLite3} will be a {\f1\fs20 NVARCHAR2} field for {\i Oracle}).
The only specific instruction is the global {\f1\fs20 @*VirtualTableExternalRegister@} function, which has to be run on the server side (it does not make any sense to run it on the client side, since for the client there is no difference between any tables - in short, the client do not care about storage; the server does). In order to work as expected, {\f1\fs20 VirtualTableExternalRegister()} shall be called {\i before} {\f1\fs20 TSQLRestServer.Create} constructor: when the server initializes, the ORM server must know whenever an {\i internal} or {\i external} database shall be managed.
Note that in the above code, the {\f1\fs20 LastChange} field was defined as a {\f1\fs20 TModTime}: in fact, the current date and time will be stored each time the record is updated, i.e. for each {\f1\fs20 fClient.Add} or {\f1\fs20 fClient.Update} calls. This is tested by both {\f1\fs20 RExt.LastChange>=Now} and {\f1\fs20 RExt.LastChange<=Now} checks in the latest loop. The time used is the "server-time", i.e. the current time and date on the server (not on the client), and, in the case of external databases, the time of the remote server (it will execute e.g. a {\f1\fs20 select getdate()} under @*MS SQL@ to synchronize the date to be inserted for {\f1\fs20 LastChange}). In order to retrieve this server-side time stamp, we use {\f1\fs20 Now := fClient.ServerTimeStamp} instead of the local {\f1\fs20 Iso8601Now} function.
A similar feature is tested for the {\f1\fs20 CreatedAt} published field, which was defined as {\f1\fs20 TCreateTime}: it will be set automatically to the current server time at record creation (and not changed on modifications). This is the purpose of the {\f1\fs20 RExt.CreatedAt<=Now} check in the above code.
:30   Behind the scene
The {\f1\fs20 SQLite3DB.pas} unit, introduced in revision 1.15, implements @*Virtual Table@s access for any {\f1\fs20 @*SynDB@}-based external database for the framework.
In fact, the new {\f1\fs20 TSQLRestServerStaticExternal, TSQLVirtualTableCursorExternal} and {\f1\fs20 TSQLVirtualTableExternal} classes will implement this feature:
\graph HierExternalTables External Databases classes hierarchy
\TSQLRecordVirtual\TSQLRecord
\TSQLRecordVirtualTableAutoID\TSQLRecordVirtual
\TSQLVirtualTableCursorExternal\TSQLVirtualTableCursor
\TSQLVirtualTableExternal\TSQLVirtualTable
\
In order to be stored in an external database, the ORM records can inherit from any {\f1\fs20 TSQLRecord} class. Even if this class does not inherit from {\f1\fs20 TSQLRecordVirtualTableAutoID}, it will behave as such: i.e. it will therefore have an {\f1\fs20 Integer RowID} published property, auto-incremented at every record insertion (auto-increment will be handled via a {\f1\fs20 select max(rowid) from tablename}, since not all databases handle such fields - e.g. {\i @*Oracle@}).
The registration of the class is done by a call to the following new global procedure:
!procedure VirtualTableExternalRegister(aModel: TSQLModel; aClass: TSQLRecordClass;
!  aExternalDB: TSQLDBConnectionProperties; const aExternalTableName: RawUTF8);
This procedure will register on the Server-side an external database for an ORM class:
- It will define the supplied class to behave like a {\f1\fs20 TSQLRecordVirtualTableAutoID} class (i.e. its {\f1\fs20 TSQLRecordProperties.Kind} property will be ovewritten to {\f1\fs20 rCustomAutoID});
- It will associate the supplied class with a {\f1\fs20 TSQLVirtualTableExternal} module;
- The {\f1\fs20 TSQLDBConnectionProperties} instance should be shared by all classes, and released globally when the ORM is no longer needed;
- The full table name, as expected by the external database, should be provided here ({\f1\fs20 SQLTableName} will be used internally as table name when called via the associated {\i SQLite3} Virtual Table) - if no table name is specified (''), will use {\f1\fs20 SQLTableName} (e.g. 'Customer' for a class named {\f1\fs20 TSQLCustomer});
- Internal adjustments will be made to convert SQL on the fly from internal ORM representation into the expected external SQL format (e.g. table name or {\f1\fs20 ID} property).
Typical usage may be for instance:
!aProps := TOleDBMSSQLConnectionProperties.Create('.\SQLEXPRESS','AdventureWorks2008R2','','');
!aModel := TSQLModel.Create([TSQLCustomer],'root');
!VirtualTableExternalRegister(aModel,TSQLCustomer,aProps,'Sales.Customer');
!aServer := TSQLRestServerDB.Create(aModel,'application.db'),true)
All the rest of the code will use the "regular" ORM classes, methods and functions, as stated by @3@.
You do not have to know where and how the data persistence is stored. The framework will do all the low-level DB work for you. And thanks to the Virtual Table feature of {\i SQlite3}, internal and external tables can be mixed in the SQL statements. Depending on the implementation need, classes could be persistent either via the internal {\i SQLite3} engine, either via external databases, just via a call to {\f1\fs20 VirtualTableExternalRegister()} before server initialization.
In fact, {\f1\fs20 TSQLVirtualTableCursorExternal} will convert any query on the external table into a proper optimized SQL query, according to the indexes existing on the external database. {\f1\fs20 TSQLVirtualTableExternal} will also convert individual SQL modification statements (like insert / update / delete) at the {\i SQLite3} level into remote SQL statements to the external database.
Most of the time, all @*REST@ful methods ({\f1\fs20 GET/POST/PUT/DELETE}) will be handled directly by the {\f1\fs20 TSQLRestServerStaticExternal} class, and won't use the virtual table mechanism. In practice, most access to the external database will be as fast as direct access, but the virtual table will always be ready to interpret any cross-database complex request or statement.
Here is an extract of the test regression log file (see code above, in previous paragraph), which shows the difference between RESTful call and virtual table call, working with more than 11,000 rows of data:
$  - External via REST: 198,767 assertions passed  1.39s
$  - External via virtual table: 198,767 assertions passed  3.41s
The first run is made with {\f1\fs20 TSQLRestServer. StaticVirtualTableDirect} set to TRUE (which is the default) - i.e. it will call directly {\f1\fs20 TSQLRestServerStaticExternal} for RESTful commands, and the second will set this property to FALSE - i.e. it will call the {\i SQLite3} engine and let its virtual table mechanism convert it into another SQL calls. It's worth saying that this test is using an in-memory {\i SQLite3} database as its external DB, so what we test here is mostly the communication overhead, not the external database speed. With real file-based or remote databases (like @*MS SQL@), the overhead of remote connection won't make noticeable the use of Virtual Tables. In all cases, letting the default {\f1\fs20 StaticVirtualTableDirect=true} will ensure the best possible performance. As stated by @59@, using a virtual or direct call won't affect the CRUD operation speed: it will by-pass the virtual engine whenever possible.
\page
:6Client-Server
: Involved technologies
Before describing the Client-Server design of this framework, we may have to detail some standards it is based on:
- JSON as its data transmission format;
- REST as its Client-Server architecture.
:2  JSON
:   Why use JSON?
As we just stated, the @**JSON@ format is used internally in this framework. By definition, the {\i JavaScript Object Notation} (JSON) is a standard, open and lightweight computer data interchange format.
Usage of this layout, instead of other like XML or any proprietary format, results in several particularities:
- Like XML, it's a text-based, human-readable format for representing simple data structures and associative arrays (called objects);
- It's easier to read (for both human beings and machines), quicker to implement, and much smaller in size than XML for most use;
- It's a very efficient format for data caching;
- Its layout allows to be rewritten in place into individual zero-terminated @*UTF-8@ strings, with almost no wasted space: this feature is used for very fast JSON to text conversion of the tables results, with no memory allocation nor data copy;
- It's natively supported by the @*JavaScript@ language, making it a perfect @*serialization@ format in any @*AJAX@ (i.e. Web 2.0) application;
- The JSON format is specified in this RFC
- The default text encoding for both JSON and {\i @*SQLite3@} is UTF-8, which allows the full Unicode char-set to be stored and communicated;
- It is the default data format used by ASP.NET AJAX services created in Windows Communication Foundation (WCF) since .NET framework 3.5; so it's Microsoft officially "ready";
- For binary @*BLOB@ transmission, we simply encode the binary data as {\i Base64}; please note that, by default, BLOB fields are not transmitted with other fields, see @1@ (only exception is {\i @*dynamic array@} fields).
:   JSON format density
Most common @*REST@ful @*JSON@ used a verbose format for the JSON content: see for example @http://bitworking.org/news/restful_json which proposed to put whole URI in the JSON content;
$[
$  "http://example.org/coll/1",
$  "http://example.org/coll/2",
$  "http://example.org/coll/3",
$  ...
$  "http://example.org/coll/N",
$]
The REST implementation of the framework will return most concise JSON content:
$ [{"ID":1},{"ID":2},{"ID":3},{"ID":4}]
which preserves bandwidth and human readability: if you were able to send a GET request to the URI {\f1\fs20 http://example.org/coll} you will be able to append this URI at the beginning of every future request, doesn't it make sense?
In all cases, the {\i Synopse mORMot Framework} always returns the JSON content just as a pure response of a @*SQL@ query, with an array and field names.
:   JSON format layouts
Note that our @*JSON@ content has two layouts, which can be produced according to the {\f1\fs20 TSQLRestServer.NoAJAXJSON} property:
1. the {\i "expanded" or standard/@*AJAX@ layout}, which allows you to create pure @*JavaScript@ objects from the JSON content, because the field name / JavaScript object property name is supplied for every value:
$ [{"ID":0,"Int":0,"Test":"abcde+¬ef+á+¬","Unicode":"abcde+¬ef+á+¬","Ansi":"abcde+¬ef+á+¬","ValFloat":3.14159265300000E+0000,"ValWord":1203,"ValDate":"2009-03-10T21:19:36","Next":0},{..}]
2. the {\i "not expanded" layout}, which reflects exactly the layout of the @*SQL@ request: first line/row are the field names, then all next lines.row are the field content:
$ {"fieldCount":9,"values":["ID","Int","Test","Unicode","Ansi","ValFloat","ValWord","ValDate","Next",0,0,"abcde+¬ef+á+¬","abcde+¬ef+á+¬","abcde+¬ef+á+¬",3.14159265300000E+0000,1203,"2009-03-10T21:19:36",0,..]}
By default, the {\f1\fs20 NoAJAXJSON} property is set to {\f1\fs20 true} when the {\f1\fs20 TSQLRestServer. ExportServerNamedPipe} is called: if you use named pipes for communication, you probably won't use JavaScript because all browser communicate via @*HTTP@!
But otherwise, {\f1\fs20 NoAJAXJSON} property is set to {\f1\fs20 false}. You could force its value to {\f1\fs20 true} and you will save some bandwidth if JavaScript is never executed: even the parsing of the JSON Content will be faster with Delphi if JSON content is not expanded.
In this "not expanded" layout, the following JSON content:
$ [{"ID":1},{"ID":2},{"ID":3},{"ID":4},{"ID":5},{"ID":6},{"ID":7}]
will be transfered as shorter:
$ {"fieldCount":1,"values":["ID",1,2,3,4,5,6,7]}
:37   JSON global cache
A global @*cache@ is used to enhance the framework scaling, and will use @*JSON@ for its result encoding.
In order to speed-up the server response time, especially in a concurrent client access, the internal database engine is not to be called on every request. In fact, a global cache has been introduced to store in memory the latest @*SQL@ {\f1\fs20 SELECT} statements results, directly in JSON.
The {\i @*SQLite3@} engine access is protected at SQL/JSON cache level, via {\f1\fs20 DB.LockJSON()} calls in most {\f1\fs20 @*TSQLRestServerDB@} methods.
A {\f1\fs20 TSynCache} instance is instantiated within the {\f1\fs20 TSQLDataBase} internal global instance, with the following line:
!constructor TSQLRestServerDB.Create(aModel: TSQLModel; aDB: TSQLDataBase;
!  aHandleUserAuthentication: boolean);
!begin
!  fStatementCache.Init(aDB.DB);
!!  aDB.UseCache := true; // we better use caching in this JSON oriented use
!  (...)
This will enable a global JSON cache at the SQL level. This cache will be reset on every {\f1\fs20 INSERT, UPDATE} or {\f1\fs20 DELETE} SQL statement, whatever the corresponding table is.
In practice, this global cache was found to be efficient, even if its implementation is some kind of "naive". It is in fact much more tuned than other HTTP-level caching mechanisms used in most client-server solutions (using e.g. a {\i Squid} proxy) - since our caching is at the SQL level, it is shared among all @*CRUD@ / @*Rest@ful queries, and is also indenpendent from the authentication scheme, which pollutes the URI. Associated with the other levels of cache - see @38@ - the framework scaling was found to be very good.
:9  REST
:   RESTful implementation
{\i Representational state transfer} (@**REST@) is a style of software architecture for distributed hypermedia systems such as the World Wide Web. As such, it is not just a method for building "web @*service@s". The terms "representational state transfer" and "REST" were introduced in 2000 in the doctoral dissertation of Roy Fielding, one of the principal authors of the Hypertext Transfer Protocol (@**HTTP@) specification, on which the whole Internet rely.
The {\i Synopse mORMot Framework} was designed in accordance with Fielding's REST architectural style without using HTTP and without interacting with the World Wide Web. Such Systems which follow REST principles are often referred to as "RESTful". Optionally, the Framework is able to serve standard HTTP/1.1 pages over the Internet (by using the {\f1\fs20 SQLite3Http} unit and the {\f1\fs20 TSQLite3HttpServer} and {\f1\fs20 TSQLite3HttpClient} classes), in an embedded low resource and fast HTTP server.
The standard RESTful methods are implemented:
- GET to list the members of the collection;
- PUT to update a member of the collection;
- POST to create a new entry in the collection;
- DELETE to delete a member of the collection.
The following methods were added to the standard REST definition, for locking individual records and for handling database @*transaction@s (which speed up database process):
- LOCK to lock a member of the collection;
- UNLOCK to unlock a member of the collection;
- BEGIN to initiate a transaction;
- END to commit a transaction;
- ABORT to rollback a transaction.
The GET method has an optional pagination feature, compatible with the YUI DataSource Request Syntax for data pagination - see {\f1\fs20 TSQLRestServer.URI} method and @http://developer.yahoo.com/yui/datatable/#data .
From the Delphi code point of view, a RESTful @*Client-Server@  architecture is implemented by inheriting some common methods and properties from a main class.
\graph HierTSQLRestClient TSQLRestClient classes hierarchy
\TSQLRestServer\TSQLRest
\TSQLRestClientURI\TSQLRestClient
\TSQLRestClient\TSQLRest
\
This diagram states how the {\f1\fs20 @*TSQLRest@} class implements a common ancestor for both Client and Server classes.
:1   REST and BLOB fields
@**BLOB@ fields are defined as {\f1\fs20 TSQLRawBlob} @*published properties@ in the classes definition. But their content is not included in standard @*REST@ful methods of the framework, to spare network bandwidth.
The RESTful protocol allows BLOB to be retrieved (GET) or saved (PUT) via a specific URL, like "{\f1\fs20 ModelRoot/TableName/ID/BlobFieldName}". This is even better than the standard @*JSON@ encoding, which works well but convert BLOB to/from hexadecimal values, therefore need twice the normal size of it. By using such dedicated URL, data can be transfered as full binary.
Some dedicated methods of the generic {\f1\fs20 @*TSQLRest@} class handle BLOB fields: {\f1\fs20 RetrieveBlob} and {\f1\fs20 UpdateBlob}.
:   REST and JSON
The @*HTTP@ @*Client-Server@ sample application available in the framework source code tree can be used to show how the framework is @*AJAX@-ready, and can be proudly compared to any other @*REST@ server (like {\i CouchDB}) also based on {\f1\fs20 JSON}:
- Start the {\f1\fs20 Project04Server.exe} program: the background HTTP server, together with its {\i SQLite3} database engine;
- Start any {\f1\fs20 Project04Client.exe} instances, and add/find any entry, to populate the database a little;
- Close the {\f1\fs20 Project04Client.exe} programs, if you want;
- Open your browser, and type into the address bar:
$  http://localhost:8080/root
- You'll see an error message:
$TSQLite3HttpServer Server Error 400
- Type into the address bar:
$  http://localhost:8080/root/SampleRecord
- You'll see the result of all {\f1\fs20 SampleRecord} IDs, encoded as a JSON list, e.g.
$ [{"ID":1},{"ID":2},{"ID":3},{"ID":4}]
- Type into the address bar:
$  http://localhost:8080/root/SampleRecord/1
- You'll see the content of the {\f1\fs20 SampleRecord} of ID=1, encoded as JSON, e.g.
${"ID":1,"Time":"2010-02-08T11:07:09","Name":"AB","Question":"To be or not to be"}
- Type into the address bar any other REST command, and the database will reply to your request...
You have got a full HTTP/SQLite3 RESTful JSON server within less than 400 KB.
:15   REST is Stateless
Our framework is implementing @*REST@ as a @**stateless@ protocol, just as the @*HTTP@/1.1 protocol it could use as its communication layer.
A {\i stateless} server is a server that treats each request as an independent @*transaction@ that is unrelated to any previous request.
At first, you could find it a bit disappointing from a classic @*Client-Server@ approach. In a stateless world, you are never sure that your Client data is up-to-date. The only place where the data is safe is the server. In the web world, it's not confusing. But if you are coming from a rich Client background, this may concern you: you should have the habit of writing some synchronization code from the server to replicate all changes to all its clients. This is not necessary in a stateless architecture any more.
The main rule of this architecture is to ensure that the Server is the only reference, and that the Client is able to retrieve any pending update from the Server side. That is, always modify a record content on a server side, then refresh the client to retrieve the modified value. Do {\i not} modify the client side directly, but always pass through the Server. The UI components of the framework follow these principles. Client-side modification could be performed, but must be made in a separated autonomous table/database. This will avoid any synchronization problem in case of concurrent client modification.
:    Server side synchronization
Even if @*REST@ is @*stateless@, it's always necessary to have some event triggered on the server side when a record is edited.
On the server side, you can use this method prototype:
!type
!  ///  used to define how to trigger Events on record update
!  // - see TSQLRestServer.OnUpdateEvent property
!  // - returns true on success, false if an error occured (but action must continue)
!  TNotifySQLEvent = function(Sender: TSQLRestServer; Event: TSQLEvent;
!    aTable: TSQLRecordClass; aID: integer): boolean of object;
!
!  TSQLRestServer = class(TSQLRest)
! (...)
!    /// a method can be specified here to trigger events after any table update
!    OnUpdateEvent: TNotifySQLEvent;
:    Client side synchronization
But if you want all clients to be notified from any update, there is no direct way of broadcasting some event from the server to all clients.
It's not even technically possible with pipe-oriented transport layer, like named pipes or the TCP/IP - @*HTTP@ protocol.
What you can do easily, and is what should be used in such case, is to have a timer in your client applications which will call {\f1\fs20 TSQLRestClientURI. UpdateFromServer()} method to refresh the content of any {\f1\fs20 @*TSQLRecord@} or {\f1\fs20 @*TSQLTableJSON@} instance:
!/// check if the data may have changed of the server for this objects, and
!// update it if possible
!// - only working types are TSQLTableJSON and TSQLRecord descendants
!// - make use of the InternalState function to check the data content revision
!// - return true if Data is updated successfully, or false on any error
!// during data retrieval from server (e.g. if the TSQLRecord has been deleted)
!// - if Data contains only one TSQLTableJSON, PCurrentRow can point to the
!// current selected row of this table, in order to refresh its value
!function UpdateFromServer(const Data: array of TObject; out Refreshed: boolean;
!  PCurrentRow: PInteger = nil): boolean;
With a per-second timer, it's quick and reactive, even over a remote network.
The @*stateless@ aspect of @*REST@ allows this approach to be safe, by design.
This is handled natively by our Client User Interface classes, with the following parameter defining the User interface:
!/// defines the settings for a Tab
!  TSQLRibbonTabParameters = object
!  (...)
!    /// by default, the screens are not refreshed automaticaly
!    // - but you can enable the auto-refresh feature by setting this
!    // property to TRUE, and creating a WM_TIMER timer to the form
!    AutoRefresh: boolean;
This parameter will work only if you handle the {\f1\fs20 WM_TIMER} message in your main application form, and call {\f1\fs20 Ribbon.WMRefreshTimer}.
See for example this method in the main demo (@!Lib\SQLite3\Samples\MainDemo\FileMain.pas@ unit):
!procedure TMainForm.WMRefreshTimer(var Msg: TWMTimer);
!begin
!  Ribbon.WMRefreshTimer(Msg);
!end;
In a multi-threaded client application, and even on the server side, a @*stateless@ approach makes writing software easier. You do not have to care about forcing data refresh in your client screens. It's up to the screens to get refreshed. In practice, I found it very convenient to rely on a timer instead of calling the somewhat "delicate" {\f1\fs20 TThread. Synchronize} method.
:46  Interfaces
:   Delphi and interfaces
:    Declaring an interface
No, interface(-book) is not another social network, sorry.
In Delphi OOP model, an {\f1\fs20 @**interface@} defines a type that comprises abstract virtual methods. The short, easy definition is that an interface is a declaration of functionality without an implementation of that functionality. It defines "what" is available, not "how" it is made available. This is the so called "abstraction" benefit of interfaces (there are another benefits, like orthogonality of interfaces to classes, but we'll see it later).
In Delphi, we can declare an interface like so:
!type
!  ICalculator = interface(IInvokable)
!    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
!    /// add two signed 32 bit integers
!    function Add(n1,n2: integer): integer;
!  end;
It just sounds like a class definition, but, as you can see:
- It is named {\f1\fs20 ICalculator}, and not {\f1\fs20 TCalculator}: it is a common convention to start an interface name with a {\f1\fs20 I}, to make a difference with a {\f1\fs20 T} for a class or other implementation-level type definition;
- There is no visibility attribute (no {\f1\fs20 private / protected / public / published} keywords): in fact, it is just as if all methods were published;
- There is no fields, just methods (fields are part of the implementation, not of the interface): in fact, you can have properties in your interface definition, but those properties shall redirect to existing getter and setter methods, via {\f1\fs20 read} and {\f1\fs20 write} keywords;
- There is a strange number below the interface name, called a {\f1\fs20 @*GUID@}: this is an unique identifier of the interface - you can create such a genuine constant on the editor cursor position by pressing {\f1\fs20 Ctrl + Shift + G} in the Delphi IDE;
- But the methods are just defined as usual.
:    Implementing an interface with a class
Now that we have an interface, we need to create an implementation.
Our interface is very basic, so we may implement it like this:
!type
!!  TServiceCalculator = class(TInterfacedObject, ICalculator)
!  protected
!    fBulk: string;
!  public
!!    function Add(n1,n2: integer): integer;
!    procedure SetBulk(const aValue: string);
!  end;
!
!function TServiceCalculator.Add(n1, n2: integer): integer;
!begin
!  result := n1+n2;
!end;
!
!procedure TServiceCalculator.SetBulk(const aValue: string);
!begin
!  fBulk := aValue;
!end;
You can note the following:
- We added {\f1\fs20 ICalculator} name to the {\f1\fs20 class()} definition: this class inherits from {\f1\fs20 TInterfacedObject}, and implements the {\f1\fs20 ICalculator} interface;
- Here we have {\f1\fs20 protected} and {\f1\fs20 public} keywords - but the {\f1\fs20 Add} method can have any visibility, from the interface point of view: it will be used as implementation of an interface, even if the method is declared as {\f1\fs20 private} in the implementation class;
- There is a {\f1\fs20 SetBulk} method which is not part of the {\f1\fs20 ICalculator} definition - so we can add other methods to the implementation class, and we can even implement several interfaces within the same method (just add other interface names after like {\f1\fs20 class(TInterfacedObject, ICalculator, IAnotherInterface)};
- There a {\f1\fs20 fBulk} protected field member within this class definition, which is not used either, but could be used for the class implementation.
- Here we have to code an implementation for the {\f1\fs20 TServiceCalculator.Add()} method (otherwise the compiler will complain for a missing method), whereas there is no implementation expected for the {\f1\fs20 ICalculator.Add} method - it is perfectly "abstract".
:    Using an interface
Now we have two ways of using our {\f1\fs20 TServiceCalculator} class:
- The classic way;
- The abstract way (using an interface).
The "classic" way, using an explicit class instance:
!function MyAdd(a,b: integer): integer;
!var Calculator: TServiceCalculator;
!begin
!  Calculator := TServiceCalculator.Create;
!  try
!    result := Calculator.Add(a,b);
!  finally
!    Calculator.Free;
!  end;
!end;
Note that we used a {\f1\fs20 try..finally} block to protect the instance memory resource.
Then we can use an interface:
!function MyAdd(a,b: integer): integer;
!var Calculator: ICalculator;
!begin
!  ICalculator := TServiceCalculator.Create;
!  result := Calculator.Add(a,b);
!end;
What's up over there?
- We defined the local variable as {\f1\fs20 ICalculator}: so it will be an {\f1\fs20 interface}, not a regular class instance;
- We assigned a {\f1\fs20 TServiceCalculator} instance to this {\f1\fs20 interface} variable: the variable will now handle the instance life time;
- We called the method just as usual - in fact, the computation is performed with the same exact expression: {\f1\fs20 result := Calculator.Add(a,b)};
- We do not need any {\f1\fs20 try...finally} block here: in Delphi, interface variables are {\i @*reference-counted@}: that is, the use of the interface is tracked by the compiler and the implementing instance, once created, is automatically freed when the compiler realizes that the number of references to a given interface variable is zero;
- And the performance cost is negligible: this is more or less the same as calling a virtual method (just one more redirection level).
In fact, the compiler creates an hidden {\f1\fs20 try...finally} block in the {\f1\fs20 MyAdd} function, and the instance will be released as soon as the {\f1\fs20 Calculator} variable is out of scope. The generated code could look like this:
!function MyAdd(a,b: integer): integer;
!var Calculator: TServiceCalculator;
!begin
!  Calculator := TServiceCalculator.Create;
!  try
!    Calculator.FRefCount := 1;
!    result := Calculator.Add(a,b);
!  finally
!    dec(Calculator.FRefCount);
!    if Calculator.FRefCount=0 then
!      Calculator.Free;
!  end;
!end;
Of course, this is a bit more optimized than this (and thread-safe), but you have got the idea.
:    There is more than one way to do it
One benefit of interfaces we have already told about, is that it is "orthogonal" to the implementation.
In fact, we can create another implementation class, and use the same interface:
!type
!!  TOtherServiceCalculator = class(TInterfacedObject, ICalculator)
!  protected
!!    function Add(n1,n2: integer): integer;
!  end;
!
!function TOtherServiceCalculator.Add(n1, n2: integer): integer;
!begin
!  result := n2+n1;
!end;
Here the computation is not the same: we use {\f1\fs20 n2+n1} instead of {\f1\fs20 n1+n2}... of course, this will result into the same value, but we can use this another method for our very same interface, by using its {\f1\fs20 TOtherServiceCalculator} class name:
!function MyOtherAdd(a,b: integer): integer;
!var Calculator: ICalculator;
!begin
!!  ICalculator := TOtherServiceCalculator.Create;
!  result := Calculator.Add(a,b);
!end;
:    Here comes the magic
Now you may begin to see the point of using interfaces in a client-server framework like ours.
Our {\i mORMot} is able to use the same {\f1\fs20 interface} definition on both client and server side, calling all expected methods on both sides, but having all the implementation logic on the server side. The client application will transmit method calls (using JSON instead of much more complicated XML/@*SOAP@) to the server (using a "fake" implementation class created on the fly by the framework), then the execution will take place on the server (with obvious benefits), and the result will be sent back to the client, as JSON. The same interface can be used on the server side, and in this case, execution will be in-place, so very fast.
By creating a whole bunch of interfaces for implementing the business logic of your project, you will benefit of an open and powerful implementation pattern.
More on this later on... first we'll take a look at good principles of playing with interfaces.
:47   SOLID design principles
The acronym @**SOLID@ is derived from the following @*OOP@ principles (quoted from the corresponding {\i Wikipedia} article):
- {\i Single responsibility principle}: the notion that an object should have only a single responsibility;
- {\i Open/closed principle}: the notion that “software entities ... should be open for extension, but closed for modification”;
- {\i Liskov substitution principle}: the notion that “objects in a program should be replaceable with instances of their subtypes without altering the correctness of that program” - also named as "{\i design by contract}";
- {\i @*Interface@ segregation principle}: the notion that “many client specific interfaces are better than one general purpose interface.”;
- {\i Dependency inversion principle}: the notion that one should “Depend upon Abstractions. Do not depend upon concretions.”. {\i Dependency injection} is one method of following this principle.
If you have some programming skills, those principles are general statements you may already found out by yourself. If you start doing serious object-oriented coding, those principles are best-practice guidelines you would gain following.
They certainly help to fight the three main code weaknesses:
- {\i Rigidity} – Hard to change something because every change affects too many other parts of the system;
- {\i Fragility} – When you make a change, unexpected parts of the system break;
- {\i Immobility} – Hard to reuse in another application because it cannot be disentangled from the current application.
:    Single responsibility principle
When you define a class, it shall be designed to implement only one feature. The so-called feature can be seen as an "{\i axis of change}" or a "{\i a reason for change}".
Therefore:
- One class shall have only one reason that justifies changing its implementation;
- Classes shall have few dependencies on other classes;
- Classes shall be abstract from the particular layer they are running - see @7@.
For instance, a {\f1\fs20 TRectangle} object should not have both {\f1\fs20 ComputeArea} and {\f1\fs20 Draw} methods defined at once - they would define two responsibilities or axis of change: the first responsibility is to provide a mathematical model of a rectangle, and the second is to render it on GUI.
When you define an @*ORM@ object, do not put GUI methods within. In fact, the fact that our {\f1\fs20 @*TSQLRecord@} class definitions are common to both Client and Server sides makes this principle mandatory. You won't have any GUI related method on the Server side, and the Client side could use the objects instances with several GUI implementations (Delphi Client, AJAX Client...).
Therefore, if you want to change the GUI, you won't have to recompile the {\f1\fs20 TSQLRecord} class and the associated database model.
Another example is how our database classes are defined in {\f1\fs20 @*SynDB@.pas} - see @27@:
- The {\i connection properties} feature is handled by {\f1\fs20 TSQLDBConnectionProperties} classes;
- The actual {\i living connection} feature is handled by {\f1\fs20 TSQLDBConnection} classes;
- And {\i database requests} feature is handled by {\f1\fs20 TSQLDBStatement} instances using dedicated {\f1\fs20 NewConnection} / {\f1\fs20 ThreadSafeConnection} / {\f1\fs20 NewStatement} methods.
Therefore, you may change how a database connection is defined (e.g. add a property to a {\f1\fs20 TSQLDBConnectionProperties} child), and you won't have to change the statement implementation itself.
Following this {\i Single responsibility principle} may sound simple and easy, but in fact, it is one of the hardest principles to get right. Naturally, we tend to join responsibilities in our class definitions. Our ORM architecture will enforce you, by its @*Client-Server@ nature, to follow this principle, but it is always up to the end coder to design properly his/her interfaces.
:    Open/closed principle
When you define a class or a unit, at the same time:
- They shall be {\i open for extension};
- But {\i closed for modification}.
When designing our ORM, we tried to follow this principle. In fact, you should not have to modify its implementation. You should define your own units and classes, without the need to {\i hack} the framework source code.
Even if {\i Open Source} paradigm allows you to modify the supplied code, this shall not be done unless you are either fixing a bug or adding a new common feature. This is in fact the purpose of our @http://synopse.info web site, and most of the framework enhancements have come from user requests.
The framework Open Source @*license@ - see @34@ - may encourage user contributions in order to fulfill the Open/closed design principle:
- Your application code extends the {\i Synopse mORMot Framework} by defining your own classes or event handlers - this is how it is {\i open for extension};
- The main framework units shall remain inviolate, and common to all users - this illustrates the {\i closed for modification} design.
Furthermore, this principle will ensure your code to be ready to follow the main framework updates (which are quite regular). When a new version is available, you would be able to retrieve it for free from our web site, replace your files locally, then build a new enhanced version of your application. Even the source code repository is available - at @http://synopse.info/fossil - and allows you to follow the current step of evolvment of the framework.
In short, abstraction is the key. All your code shall not depend on a particular implementation.
In order to implement this principle, several conventions could be envisaged:
- You shall better define some abstract classes, then use specific overridden classes for each and every implementation: this is for instance how @*Client-Server@ classes were implemented - see @35@;
- All object members shall be declared {\f1\fs20 private} or {\f1\fs20 protected} - this is a good idea to use @17@ for defining server-side process, and/or make the {\f1\fs20 @*TSQLRecord@} published properties read-only and using some client-side {\f1\fs20 constructor} with parameters;
- No singleton nor global variable - {\i ever};
- RTTI is dangerous - that is, let our framework use RTTI functions for its own cooking, but do not use it in your code.
Some other guidelines may be added, but you got the main idea. Conformance to this open/closed principle is what yields the greatest benefit of @*OOP@, i.e.:
- Code re-usability;
- Code maintainability;
- Code extendibility.
Following this principle will make your code far away from a regular RAD style. But benefits will be huge.
:    Liskov substitution principle
Even if her name is barely unmemorable, {\i Barbara Liskov} is a great computer scientist, we should better learn from.
Her "substitution principle" states that, if {\f1\fs20 TChild} is a subtype of {\f1\fs20 TParent}, then objects of type {\f1\fs20 TParent} may be replaced with objects of type {\f1\fs20 TChild} (i.e., objects of type {\f1\fs20 TChild} may be substitutes for objects of type {\f1\fs20 TParent}) without altering any of the desirable properties of that program (correctness, task performed, etc.).
For our framework, it would signify that {\f1\fs20 TSQLRestServer} or {\f1\fs20 TSQLRestClient} instances can be substituted to a {\f1\fs20 TSQLRest} object. Most @*ORM@ methods expect a {\f1\fs20 TSQLRest} parameter to be supplied.
Your code shall refer to abstractions, not to implementations. By using only methods and properties available at classes parent level, your code won't need to change because of a specific implementation.
The main advantages of this coding pattern are the following:
- Thanks to this principle, you will be for instance able to {\i stub} or {\i mock} an @*interface@ or a class - this principle is therefore mandatory for implementing unitary @*test@ing to your project;
- Furthermore, testing would be available not only at isolation level (testing each child class), but also at abstracted level, i.e. from the client point of view - you can have implementation which behave correctly when tested individually, but which failed when tested at higher level if the Liskov principle was broken;
- If this principle is violated, the open/close principle will be - the parent class would need to be modified whenever a new derivative of the base class is defined;
- Code re-usability is enhanced by method re-usability: a method defined at a parent level does not require to be implemented for each child.
Some patterns which shall not appear in your code:
- Statements like {\f1\fs20 if aObject is TAClass then begin .... end else if aObject is TAnotherClass then ...} in a parent method;
- Use an @*enumerated@ item and a {\f1\fs20 case ... of } or nested {\f1\fs20 if ... then} to change a method behavior (this will also probably break the single responsibility principle: each enumeration shall be defined as a class);
- Define a method which will stay {\f1\fs20 abstract} for some children;
- Need to explicitly add all child classes units to the parent class unit {\f1\fs20 uses} clause.
In order to fulfill this principle, you should:
- Use the "behavior" design pattern, when defining your objects hierarchy - for instance, if a square may be a rectangle, a {\f1\fs20 TSquare} object is definitively {\i not} a {\f1\fs20 TRectangle} object, since the behavior of a {\f1\fs20 TSquare} object is not consistent with the behavior of a {\f1\fs20 TRectangle} object (square width always equals its height, whereas it is not the case for most rectangles);
- Write your tests using abstract local variables (and this will allow test code reuse for all children classes);
- Follow the concept of {\i Design by Contract}, i.e. the Meyer's rule defined as "{\i when redefining a routine [in a derivative], you may only replace its precondition by a weaker one, and its postcondition by a stronger one}" - use of preconditions and postconditions also enforce testing model;
- Separate your classes hierarchy: typically, you may consider using separated object types for implementing persistence and object creation (this is the common separation between {\i Factory} and {\i Repository}).
The @*SOA@ and @*ORM@ concepts as used by our framework are compatible with the Liskov substitution principle.
Furthermore, a more direct {\i Design by Contract} implementation pattern is also available (involving a more wide usage of {\f1\fs20 @*interface@s}).
:    Interface segregation principle
This principle states that once an @*interface@ has become too 'fat' it shall be split into smaller and more specific interfaces so that any clients of the interface will only know about the methods that pertain to them. In a nutshell, no client should be forced to depend on methods it does not use.
As a result, it will help a system stay decoupled  and thus easier to re-factor, change, and redeploy.
Beginning with revision 1.16, our framework allows direct use of {\f1\fs20 interfaces} to implement services. This great @*Client-Server@ @*SOA@ implementation pattern - see @11@ - helps decoupling all services to individual small methods. In this case also, the @*stateless@ used design will also reduce the use of 'fat' session-related processes: an object life time can be safely driven by the {\f1\fs20 interface} scope.
By defining Delphi {\f1\fs20 interface} instead of plain {\f1\fs20 class}, it helps creating small and business-specific contracts, which can be executed on both client and server side, with the same exact code.
:    Dependency Inversion Principle
Another form of decoupling is to invert the dependency between high and low level of a software design:
- High-level modules should not depend on low-level modules. Both should depend on abstractions;
- Abstractions should not depend upon details. Details should depend upon abstractions.
In conventional application architecture, lower-level components are designed to be consumed by higher-level components which enable increasingly complex systems to be built. This design limits the reuse opportunities of the higher-level components, and certainly breaks the Liskov's substitution principle.
The goal of the {\i dependency inversion principle} is to decouple high-level components from low-level components such that reuse with different low-level component implementations becomes possible. A simple implementation pattern could be to use only @*interface@s owned by, and existing only with the high-level component package.
In other languages (like Java or .Net), various patterns such as {\i Plug-in, Service Locator}, or {\i Dependency Injection} are then employed to facilitate the run-time provisioning of the chosen low-level component implementation to the high-level component.
Our @*Client-Server@ architecture facilitated this decoupling pattern, and allows the use of native Delphi {\f1\fs20 interface} to call services from an abstract @*factory@.
:   Circular reference and (zeroing) weak pointers
:    Weak pointers
The memory allocation model of the Delphi {\f1\fs20 interface} type uses some kind of {\i Automatic Reference Counting} (@*ARC@). In order to avoid memory and resource leaks and potential random errors in the applications (aka the terrible {\f1\fs20 EAccessViolation} exception on customer side) when using @46@, a @*SOA@ framework like {\i mORMot} has to offer so-called {\i @**Weak pointers@} and {\i @**Zeroing Weak pointers@} features.
By default in Delphi, all references are either:
- {\i weak references} for pointer and class instances;
- explicit copy for low-level value types like {\f1\fs20 integer, Int64, currency, double} or {\f1\fs20 record} (and old deprecated {\f1\fs20 object} or {\f1\fs20 shortstring});
- {\i copy-on-write} with {\i reference counting} for high-level value types (e.g. {\f1\fs20 string, widestring, variant} or a {\i dynamic array});
- {\i strong reference} with {\i reference counting} for {\f1\fs20 interface} instances.
The main issue with {\i strong reference counting} is the potential {\i circular reference} problem.\line This occurs when an {\f1\fs20 interface} has a strong pointer to another, but the target {\f1\fs20 interface} has a strong pointer back to the original. Even when all other references are removed, they still will hold on to one another and will not be released. This can also happen indirectly, by a chain of objects that might have the last one in the chain referring back to an earlier object.
See the following {\f1\fs20 interface} definition for instance:
!  IParent = interface
!    procedure SetChild(const Value: IChild);
!    function GetChild: IChild;
!    function HasChild: boolean;
!    property Child: IChild read GetChild write SetChild;
!  end;
!
!  IChild = interface
!    procedure SetParent(const Value: IParent);
!    function GetParent: IParent;
!    property Parent: IParent read GetParent write SetParent;
!  end;
The following implementation will definitively leak memory:
!procedure TParent.SetChild(const Value: IChild);
!begin
!  FChild := Value;
!end;
!
!procedure TChild.SetParent(const Value: IParent);
!begin
!  FParent := Value;
!end;
In Delphi, most common kind of reference-copy variables (i.e. {\f1\fs20 variant}, {\i dynamic array} or {\f1\fs20 string}) solve this issue by implementing {\i copy-on-write}. Unfortunately, this pattern is not applicable to {\f1\fs20 interface}, which are not value objects, but reference objects, tied to an implementation {\f1\fs20 class}, which can't be copied.
One common solution is to use {\i Weak pointers}, by which the {\f1\fs20 interface} is assigned to a property without incrementing the reference count.
Note that garbage collector based languages (like Java or C#) do not suffer from this problem, since the circular references are handled by their memory model: objects lifetime are maintained globally by the memory manager. Of course, it will increase memory use, slowdown the process due to additional actions during allocation and assignments (all objects and their references have to be maintained in internal lists), and may slow down the application when garbage collector enters in action. In order to avoid such issues when performance matters, experts tend to pre-allocate and re-use objects: this is one common limitation of this memory model, and why Delphi is still a good candidate (like unmanaged C or C++ - and also {\i Objective C}) when it deals with performance and stability. In some cases (e.g. when using an object cache), such languages have to introduce some kind of "weak pointers", to allow some referenced objects to be reclaimed by garbage collection: but it is a diverse mechanism, under the same naming.
:    Handling weak pointers
In order to easily create a weak pointer, the following function was added to {\f1\fs20 SQLite3Commons.pas}:
!procedure SetWeak(aInterfaceField: PIInterface; const aValue: IInterface);
!begin
!  PPointer(aInterfaceField)^ := Pointer(aValue);
!end;
It will assign the `interface` to a field by assigning the `pointer` of this instance to the internal field. It will by-pass the reference counting, so memory won't be leaked any more.
Therefore, it could be used as such:
!procedure TParent.SetChild(const Value: IChild);
!begin
!  SetWeak(@FChild,Value);
!end;
!
!procedure TChild.SetParent(const Value: IParent);
!begin
!  SetWeak(@FParent,Value);
!end;
:    Zeroing weak pointers
But there are still some cases where it is not enough. Under normal circumstances, a {\f1\fs20 class} instance should not be deallocated if there are still outstanding references to it. But since weak references don't contribute to an {\f1\fs20 interface} reference count, a {\f1\fs20 class} instance can be released when there are outstanding weak references to it. Some memory leak or even random access violations could occur. A debugging nightmare...
In order to solve this issue, ARC's {\i Zeroing Weak pointers} come to mind.\line It means that weak references will be set to {\f1\fs20 nil} when the object they reference is released. When this happens, the automatic zeroing of the outstanding weak references prevents them from becoming dangling pointers. And {\i voilà}! No access violation any more!
Such a {\i Zeroing} ARC model has been implemented in {\i Objective C} by Apple, starting with Mac OS X 10.7 Lion, in replacement (and/or addition) to the previous manual memory handling implementation pattern: in its Apple's flavor, ARC is available not only for interfaces, but for objects, and is certainly more sophisticated than the basic implementation available in the Delphi compiler: it is told (at least from the marketing paper point of view) to use some deep knowledge of the software architecture to provide an accurate access to all instances - whereas the Delphi compiler just relies on a {\i out-of-scope} pattern. In regard to classic {\i garbage collector} memory model, ARC is told to be much more efficient, due to its deterministic nature: Apple's experts ensure that it does make a difference, in term of memory use and program latency - which both are very sensitive on "modest" mobile devices. In short, thanks to ARC, your phone UI won't glitch during background garbage recycling. So {\f1\fs20 mORMot} will try to offer a similar feature, even if the Delphi compiler does not implement it (yet).
In order to easily create a so-called zeroing weak pointer, the following function was defined in {\f1\fs20 SQLite3Commons.pas}:
!procedure SetWeakZero(aObject: TObject; aObjectInterfaceField: PIInterface;
!  const aValue: IInterface);
A potential use case could be:
!procedure TParent.SetChild(const Value: IChild);
!begin
!  SetWeakZero(self,@FChild,Value);
!end;
!
!procedure TChild.SetParent(const Value: IParent);
!begin
!  SetWeakZero(self,@FParent,Value);
!end;
We also defined a {\f1\fs20 class helper} around the {\f1\fs20 TObject} class, to avoid the need of supplying the {\f1\fs20 self} parameter, but unfortunately, the {\f1\fs20 class helper} implementation is so buggy it won't be even able to compile before Delphi XE version of the compiler. But it will allow to write code as such:
!procedure TParent.SetChild(const Value: IChild);
!begin
!  SetWeak0(@FChild,Value);
!end;
For instance, the following code is supplied in the regression tests, and will ensure that weak pointers are effectively zeroed when {\f1\fs20 SetWeakZero()} is used:
!function TParent.HasChild: boolean;
!begin
!  result := FChild<>nil;
!end;
!
!  Child := nil; // here Child is destroyed
!  Check(Parent.HasChild=(aWeakRef=weakref),'ZEROed Weak');
Here, {\f1\fs20 aWeakRef=weakref} is {\f1\fs20 true} when {\f1\fs20 SetWeak()} has been called, and equals {\f1\fs20 false} when {\f1\fs20 SetWeakZero()} has been used to assign the {\f1\fs20 Child} element to its {\f1\fs20 Parent} interface.
:    Weak pointers functions implementation details
The {\f1\fs20 SetWeak()} function itself is very simple. The Delphi RTL/VCL itself use similar code when necessary.
But the {\f1\fs20 SetWeakZero()} function has a much more complex implementation, due to the fact that a list of all weak references has to be maintained per {\f1\fs20 class} instance, and set to {\f1\fs20 nil} when this referring instance is released.
The {\i mORMot} implementation tries to implement:
- Best performance possible when processing the {\i Zeroing} feature;
- No performance penalty for other classes not involved within weak references;
- Low memory use, and good scalability when references begin to define huge graphs;
- Thread safety - which is mandatory at least on the server side of our framework;
- Compatible with Delphi 6 and later (avoid syntax tricks like {\f1\fs20 generic}).
Some good existing implementations can be found on the Internet:
- {\i Andreas Hausladen} provided a classical and complete implementation at @http://andy.jgknet.de/blog/2009/06/weak-interface-references using some nice tricks (like per-instance optional speed up using a void {\f1\fs20 IWeakInterface interface} whose VMT slot will refer to the references list), is thread-safe and is compatible with most Delphi versions - but it will slow down all {\f1\fs20 TObject.FreeInstance} calls (i.e. within {\f1\fs20 Free / Destroy}) and won't allow any overriden {\f1\fs20 FreeInstance} method implementation;
- {\i Vincent Parrett} proposed at @http://www.finalbuilder.com/Resources/Blogs/PostId/410/WeakRefence-in-Delphi-solving-circular-interfac.aspx a {\f1\fs20 generic}-based solution (not thread-safe nor optimized for speed), but requiring to inherit from a base class for any {\f1\fs20 class} that can have a weak reference pointing to it;
- More recently, {\i Stefan Glienke} published at @http://delphisorcery.blogspot.fr/2012/06/weak-interface-references.html another {\f1\fs20 generic}-based solution, not requiring to inherit from a base class, but not thread-safe and suffering from the same limitations related to {\f1\fs20 TObject.FreeInstance}.
The implementation included within {\i mORMot} uses several genuine patterns, when compared to existing solutions:
- It will hack the {\f1\fs20 TObject.FreeInstance} at the {\f1\fs20 class} VMT level, so will only slow down the exact {\f1\fs20 class} which is used as a weak reference, and not others (also its inherited {\f1\fs20 classes} won't be overridden) - and it will allow custom override of the {\f1\fs20 virtual FreeInstance} method;
- It makes use of our {\f1\fs20 TDynArrayHashed} wrapper to provide a very fast lookup of instances and references, without using {\f1\fs20 generic} definitions - hashing will start when it will be worth it, i.e. for any list storing more than 32 items;
- The unused {\f1\fs20 vmtAutoTable} VMT slot is used to handle the class-specific orientation of this feature (similar to {\f1\fs20 TSQLRecordProperties} lookup as implemented for @DI-2.1.3@), for best speed and memory use.
See the {\f1\fs20 TSetWeakZeroClass} and {\f1\fs20 TSetWeakZeroInstance} implementation in {\f1\fs20 SQlite3Commons.pas} for the details.
:35 Client-Server implementation
The framework can be used either stand-alone, either in a @**Client-Server@ model, via several communication layers:
- Fast in-process access (an executable file using a common library, for instance);
- GDI messages, only locally on the same computer, which is very fast;
- Named pipes, which can be used locally between a Server running as a Windows service and some Client instances;
- @*HTTP@/1.1 over TCP/IP, for remote access.
See @%%mORMotDesign1@ about this Client-Server architecture.
:  Implementation design
A typical Client-Server @*REST@ful POST / Add request over HTTP/1.1 will be implemented as such, on both Client and Server side:
\graph ArchClient Client-Server implementation - Client side
subgraph cluster {
\Client.Add\TSQLite3HttpClient.URI\ORM to JSON over REST
\TSQLite3HttpClient.URI\Http Server\HTTP protocol¤POST
\Http Server\TSQLite3HttpClient.URI
\TSQLite3HttpClient.URI\Client.Add\return¤new ID
label = "Client";
}
\
\graph ArchServer Client-Server implementation - Server side
subgraph cluster {
\Http Server\TSQLRestServer.URI\dispatch
\TSQLRestServer.URI\TSQLRestServerDB.EngineAdd\decode¤POST JSON
\TSQLRestServerDB.EngineAdd\SQLite3 SQL\SQL insert
\SQLite3 SQL\SQLite3 BTREE\prepare + execute
\SQLite3 BTREE\Database file\atomic write
\SQLite3 BTREE\TSQLRestServer.URI\return new ID
\TSQLRestServer.URI\Http Server\return 200 OK + ID
label = "Server";
}
\
Of course, several clients can access to the same server.
It's possible to by-pass the whole Client-Server architecture, and let the application be stand-alone, by defining a {\f1\fs20 @*TSQLRestClientDB@} class, which will embed a {\f1\fs20 @*TSQLRestServerDB@} instance in the same executable:
\graph ArchStandAlone Client-Server implementation - Stand-Alone application
subgraph cluster {
\Client.Add\TSQLite3HttpClientDB.URI\ORM to JSON over REST
\TSQLite3HttpClientDB.URI\TSQLRestServerDB.URI\direct call
\TSQLRestServerDB.URI\TSQLRestServerDB.EngineAdd\decode¤POST JSON
\TSQLRestServerDB.EngineAdd\SQLite3 SQL\SQL insert
\SQLite3 SQL\SQLite3 BTREE\prepare + execute
\SQLite3 BTREE\Database file\atomic write
\SQLite3 BTREE\TSQLRestServerDB.URI\return new ID
\TSQLRestServerDB.URI\Client.Add\return new ID
label = "Stand-Alone application";
}
\
In case of a @*Virtual Table@ use (either in-memory or for accessing an external database), the client side remains identical. Only the server side is modified as such:
\graph ArchServerVirtual Client-Server implementation - Server side with Virtual Tables
subgraph cluster {
\Http Server\TSQLRestServer.URI\dispatch
\TSQLRestServer.URI\TSQLRestServerDB.EngineAdd\decode¤POST JSON
\TSQLRestServerDB.EngineAdd\SQLite3 SQL\SQL insert
\SQLite3 SQL\SQLite3 engine\prepare + execute
\SQLite3 engine\SQLite3 BTREE\Is a SQLite3 table?
\SQLite3 BTREE\Database file\atomic write
\SQLite3 BTREE\TSQLRestServer.URI\return new ID
\SQLite3 engine\TSQLVirtualTableExternal\Is a Virtual table?
\TSQLVirtualTableExternal\TSQLDBConnectionProperties\external database
\TSQLDBConnectionProperties\TSQLDBStatement\compute SQL insert
\TSQLDBStatement\OleDB/ODBC or other\execute SQL
\OleDB/ODBC or other\Database Client\store data
\Database Client\OleDB/ODBC or other
\OleDB/ODBC or other\TSQLDBStatement
\TSQLDBStatement\TSQLVirtualTableExternal
\TSQLVirtualTableExternal\TSQLRestServer.URI\return new ID
\TSQLRestServer.URI\Http Server\return 200 OK + ID
label = "Server";
}
\
In fact, the above function correspond to a database model with only external virtual tables, and with {\f1\fs20 StaticVirtualTableDirect=false}, i.e. calling the Virtual Table mechanism of SQlite3 for each request.
Most of the time, i.e. for RESTful / @*CRUD@ commands, the execution is more direct:
\graph ArchVirtualDirect Client-Server implementation - Server side with "static" Virtual Tables
subgraph cluster {
\Http Server\TSQLRestServer.URI\dispatch
\TSQLRestServer.URI\TSQLRestServerDB.EngineAdd\Is a SQLite3 table?
\TSQLRestServer.URI\TSQLRestServerStaticExternal.EngineAdd\Is a static table?
\TSQLRestServerDB.EngineAdd\SQLite3 SQL\SQL insert
\SQLite3 SQL\SQLite3 engine\Is a SQLite3 table?
\SQLite3 engine\SQLite3 BTREE\prepare + execute
\SQLite3 BTREE\Database file\atomic write
\SQLite3 BTREE\TSQLRestServer.URI\return new ID
\TSQLRestServerStaticExternal.EngineAdd\TSQLDBConnectionProperties\external database
\TSQLDBConnectionProperties\TSQLDBStatement\compute SQL
\TSQLDBStatement\OleDB/ODBC or other\execute SQL
\OleDB/ODBC or other\Database Client\store data
\Database Client\OleDB/ODBC or other
\OleDB/ODBC or other\TSQLDBStatement
\TSQLDBStatement\TSQLRestServerStaticExternal.EngineAdd
\TSQLRestServerStaticExternal.EngineAdd\TSQLRestServer.URI\return new ID
\TSQLRestServer.URI\Http Server\return 200 OK + ID
label = "Server";
}
\
As stated in @27@, the @*static@ {\f1\fs20 TSQLRestServerStaticExternal} instance is called for most RESTful access. In practice, this design will induce no speed penalty, when compared to a direct database access. It could be even faster, if the server is located on the same computer than the database: in this case, use of JSON and REST could be faster - even faster when using @28@.
In order to be exhaustive, here is a more complete diagram, showing how native {\i SQLite3}, in-memory or external tables are handled on the server side. You'll find out how CRUD statements are handled directly for better speed, whereas any SQL JOIN query can also be processed among all kind of tables.
\graph ArchServerFull Client-Server implementation - Server side
subgraph cluster {
\Http Server\TSQLRestServer.URI\dispatch
\TSQLRestServer.URI\TSQLRestServerDB.Engine*\Refers to¤a SQLite3 table or¤a JOINed query?
\TSQLRestServer.URI\TSQLRestServerStatic.¤Engine*\CRUD over¤a static table?
\TSQLRestServerDB.Engine*\SQLite3 SQL\decode into SQL
\SQLite3 SQL\SQLite3 engine\prepare + execute
\SQLite3 engine\SQLite3 BTREE\For any¤SQLite3¤table
\SQLite3 engine\TSQLRestServer.URI
\SQLite3 BTREE\Database file\atomic¤read/write
\SQLite3 BTREE\SQLite3 engine
\SQLite3 engine\TSQLVirtualTableJSON¤TSQLVirtualTableBinary\For any in-memory¤Virtual Table
\SQLite3 engine\TSQLVirtualTableExternal\For any external¤Virtual table
\TSQLVirtualTableExternal\TSQLDBConnectionProperties\external¤database
\TSQLDBConnectionProperties\TSQLDBStatement\compute SQL
\TSQLDBStatement\OleDB/ODBC or other\execute SQL
\OleDB/ODBC or other\Database Client\handle data
\Database Client\OleDB/ODBC or other
\OleDB/ODBC or other\TSQLDBStatement
\TSQLDBStatement\SQLite3 engine
\TSQLRestServer.URI\Http Server\return 200 OK +¤result (JSON)
\TSQLRestServerStatic.¤Engine*\TSQLRestServerStaticExternal.¤Engine*\Is an external¤Virtual Table?
\TSQLRestServerStatic.¤Engine*\TSQLRestServerStaticInMemory.¤Engine*\Is an in-memory table?¤(Virtual or static)
\TSQLRestServerStaticExternal.¤Engine*\TSQLDBConnectionProperties¤TSQLDBStatement\compute SQL
\TSQLDBConnectionProperties¤TSQLDBStatement\OleDB/ODBC¤or other\execute SQL
\OleDB/ODBC¤or other\TSQLRestServer.URI
\OleDB/ODBC¤or other\Database¤Client\handle data
\Database¤Client\OleDB/ODBC¤or other
\TSQLRestServerStaticInMemory.¤Engine*\CRUD¤in-memory¤TSQLRecord
\CRUD¤in-memory¤TSQLRecord\TSQLRestServer.URI
\TSQLVirtualTableJSON¤TSQLVirtualTableBinary\Process¤in-memory¤TSQLRecord
\Process¤in-memory¤TSQLRecord\SQLite3 engine
label = "Server";
}
\
You will find out some speed numbers resulting from this unique architecture in the supplied @59@.
:  Client-Server classes
This architecture is implemented by a hierarchy of classes, implementing the @*REST@ful pattern - see @9@ - for either stand-alone, client or server side, all inheriting from a {\f1\fs20 @*TSQLRest@} common ancestor:
\graph ClientServerRESTClasses Client-Server RESTful classes
rankdir=LR;
\TSQLRestServerStaticInMemory\TSQLRestServerStatic
\TSQLRestServerStatic\TSQLRestServer
\TSQLRestServerTest\TSQLRestServerDB
\TSQLRestServerDB\TSQLRestServer
\TSQLRestServerRemoteDB\TSQLRestServer
\TSQLRestServerFullMemory\TSQLRestServer
\TSQLRestServer\TSQLRest
\TSQLRestClientURINamedPipe\TSQLRestClientURI
\TSQLRestClientURIMessage\TSQLRestClientURI
\TSQLRestClientURIDll\TSQLRestClientURI
\TSQLRestClientDB\TSQLRestClientURI
\TSQLite3HttpClientWinINet\TSQLite3HttpClientWinGeneric
\TSQLite3HttpClientWinHTTP\TSQLite3HttpClientWinGeneric
\TSQLite3HttpClientWinGeneric\TSQLite3HttpClientGeneric
\TSQLite3HttpClientWinSock\TSQLite3HttpClientGeneric
\TSQLite3HttpClientGeneric\TSQLRestClientURI
\TSQLRestClientURI\TSQLRestClient
\TSQLRestClient\TSQLRest
\
For a stand-alone application, create a {\f1\fs20 @*TSQLRestClientDB@}. This particular class will initialize an internal {\f1\fs20 @*TSQLRestServerDB@} instance, and you'll have full access to the database in the same process, with no speed penalty.
For a @*Client-Server@ application, create a {\f1\fs20 TSQLRestServerDB} instance, then use the corresponding {\f1\fs20 ExportServer, ExportServerNamedPipe, ExportServerMessage} method to instantiate either a in-process, Named-Pipe or GDI message server. For @*HTTP@/1.1 over TCP/IP, creates a {\f1\fs20 TSQLite3HttpServer} instance, and associate your running {\f1\fs20 TSQLRestServerDB} to it. Then create either a {\f1\fs20 TSQLite3HttpClient}, {\f1\fs20 TSQLRestClientURIDll, TSQLRestClientURINamedPipe} or {\f1\fs20 TSQLRestClientURIMessage} instance to access to your data according to the communication protocol used for the server.
In practice, in order to implement the business logic, you should better create a new class, inheriting from {\f1\fs20 TSQLRestServerDB}. If your purpose is not to have a full {\i SQLite3} engine available, you may create your server from a {\f1\fs20 @*TSQLRestServerFullMemory@} class instead of {\f1\fs20 TSQLRestServerDB}: this will implement a fast in-memory engine (using {\f1\fs20 TSQLRestServerStaticInMemory} instances), with basic CRUD features (for ORM), and persistence on disk as JSON or optimized binary files - this kind of server is enough to handle authentication, and host @*service@s in a stand-alone way. If your services need to have access to a remote ORM server, it may use a {\f1\fs20 @*TSQLRestServerRemoteDB@} class instead: this server will use an internal {\f1\fs20 TSQLRestClient} instance to handle all ORM operations- it can be used e.g. to host some services on a stand-alone server, with all ORM and data access retrieved from another server: it will allow to easily implement a proxy architecture (for instance, as a DMZ for publishing services, but letting ORM process stay out of scope).
All those classes will implement a RESTful access to a remote database, with associated services and business logic.
:  HTTP client
In fact, there are several implementation of a @**HTTP@/1.1 client, according to this class hierarchy:
\graph ClientRESTClasses HTTP/1.1 Client RESTful classes
\TSQLite3HttpClientWinINet\TSQLite3HttpClientWinGeneric
\TSQLite3HttpClientWinHTTP\TSQLite3HttpClientWinGeneric
\TSQLite3HttpClientWinGeneric\TSQLite3HttpClientGeneric
\TSQLite3HttpClientWinSock\TSQLite3HttpClientGeneric
\
So you can select either {\f1\fs20 TSQLite3HttpClientWinSock}, {\f1\fs20 TSQLite3HttpClientWinINet} or {\f1\fs20 TSQLite3HttpClientWinHTTP} for a HTTP/1.1 client.
Each class has its own architecture, and attaches itself to a Windows communication library, all based on {\i WinSock} API. As stated by their name, {\f1\fs20 TSQLite3HttpClientWinSock} will call directly the {\i WinSock} API, {\f1\fs20 TSQLite3HttpClientWinINet} will call {\i WinINet} API (as used by IE 6) and {\f1\fs20 TSQLite3HttpClientWinHTTP} will cal the latest {\i WinHTTP} API:
- {\i WinSock} is the common user-space API to access the sockets stack of Windows, i.e. IP connection - it's able to handle any IP protocol, including TCP/IP, UDP/IP, and any protocol over it (including HTTP);
- {\i WinINet} was designed as an HTTP API client platform that allowed the use of interactive message dialogs such as entering user credentials - it's able to handle HTTP and FTP protocols;
- {\i WinHTTP}'s API set is geared towards a non-interactive environment allowing for use in service-based applications where no user interaction is required or needed, and is also much faster than {\i WinINet} - it only handles HTTP protocol.
\graph ClientHTTPClasses HTTP/1.1 Client architecture
\WinINet\WinSock
\UrlMon\WinINet
\RASAPI\WinINet
\WinHTTP\WinSock
\AutoProxy APIs\WinHTTP
\
Here are some PROs and CONs of those solutions:
|%25%20%30%25
|\b Criteria|WinSock|WinINet|WinHTTP\b0
|API Level|Low|High|Medium
|Local speed|Fastest|Slow|Fast
|Network speed|Slow|Medium|Fast
|Minimum OS|Win95/98|Win95/98|Win2000
|HTTPS|Not available|Available|Available
|Integration with IE|None|Excellent (proxy)|Available (see below)
|User interactivity|None|Excellent (authentication, dial-up)|None
|%
As stated above, there is still a potential performance issue to use the direct {\f1\fs20 TSQLite3HttpClientWinSock} class over a network. It has been reported on our forum, and root cause was not identified yet.
Therefore, the {\f1\fs20 TSQLite3HttpClient} class maps by default to the {\f1\fs20 TSQLite3HttpClientWinHTTP} class. This is the recommended usage from a Delphi client application.
Note that even if {\i WinHTTP} does not share by default any proxy settings with Internet Explorer, it can import the current IE settings.  The {\i WinHTTP} proxy configuration is set by either {\f1\fs20 proxycfg.exe} on Windows XP and Windows Server 2003 or earlier, either {\f1\fs20 netsh.exe} on Windows Vista and Windows Server 2008 or later; for instance, you can run "{\f1\fs20 proxycfg -u}" or "{\f1\fs20 netsh winhttp import proxy source=ie}" to use the current user's proxy settings for Internet Explorer. Under @*64 bit@ Vista/Seven, to configure applications using the 32 bit {\i WinHttp} settings, call {\f1\fs20 netsh} or {\f1\fs20 proxycfg} bits from {\f1\fs20 %SystemRoot%\\SysWOW64} folder explicitly.
:  HTTP server using http.sys
:   Presentation
Since {\i Windows XP SP2} and {\i Windows Server 2003}, the Operating System provides a kernel stack to handle @**HTTP@ requests. This {\f1\fs20 http.sys} driver is in fact a full featured HTTP server, running in kernel mode. It is part of the networking subsystem of the {\i Windows} operating system, as a core component.
The {\f1\fs20 SynCrtSock} unit can implement a HTTP server based on this component. Of course, the {\i Synopse mORMot framework} will use it. If it's not available, it will launch our pure Delphi optimized HTTP server, using I/O completion ports and a Thread Pool.
What’s good about https.sys?
- {\i Kernel-mode request queuing}: Requests cause less overhead in context switching, because the kernel forwards requests directly to the correct worker process. If no worker process is available to accept a request, the kernel-mode request queue holds the request until a worker process picks it up.
- {\i Enhanced stability}: When a worker process fails, service is not interrupted; the failure is undetectable by the user because the kernel queues the requests while the WWW service starts a new worker process for that application pool.
- {\i Faster process}: Requests are processed faster because they are routed directly from the kernel to the appropriate user-mode worker process instead of being routed between two user-mode processes, i.e. the good old WinSock library and the worker process.
All is encapsulated into a single class, named {\f1\fs20 THttpApiServer}. It provides one {\f1\fs20 OnRequest} property event, in which all high level process is to take place - it expects some input parameters, then will compute the output content to be sent as response:
!TOnHttpServerRequest = function(
!   const InURL, InMethod, InContent, InContentType: TSockData;
!   out OutContent, OutContentType, OutCustomHeader: TSockData): cardinal of object;
This event handler prototype is shared by both {\f1\fs20 TThread} classes instances able to implement a {\f1\fs20 HTTP/1.1} server:
\graph HierTHttpServer HTTP Server classes hierarchy
\THttpServerGeneric\TThread
\THttpApiServer\THttpServerGeneric
\THttpServer\THttpServerGeneric
\
When the Two steps are performed:
- The HTTP Server API is first initialized (if needed) during {\f1\fs20 THttpApiServer.Create} constructor call. The {\f1\fs20 HttpApi.dll} library (which is the wrapper around {\f1\fs20 http.sys}) is loaded dynamically: so if you are running an old system ({\i Windows XP SP1} for instance), you could still be able to use the server.
- We then register some URI matching the @*REST@ful model - @9@ - via the {\f1\fs20 THttpApiServer.AddUrl} method. In short, the {\f1\fs20 @*TSQLModel@. Root} property is used to compute the RESTful URI needed, just by the book.
You can register several {\f1\fs20 TSQLRestServer} instances, each with its own {\f1\fs20 TSQLModel. Root}, if you need it.
If any of the two first point fails (e.g. if http.sys is not available, or if it was not possible to register the URLs), our framework will fall back into using our {\f1\fs20 THttpServer} class, which is a plain Delphi multi-threaded server. It won't be said that we will let you down!
Inside {\f1\fs20 http.sys} all the magic is made... it will listen to any incoming connection request, then handle the headers, then check against any matching URL.
Our {\f1\fs20 THttpApiServer} class will then receive the request, and pass it to the {\f1\fs20 TSQLRestServer} instance matching the incoming URI request, via the {\f1\fs20 THttpApiServer.OnRequest} event handler.
All @*JSON@ content will be processed, and a response will be retrieved from the internal @*cache@ of the framework, or computed using the {\i @*SQLite3@} database engine.
The resulting JSON content will be compressed using our very optimized {\i @*SynLZ@} algorithm (20 times faster than Zip/Deflate for compression), if the client is a Delphi application knowing about {\i SynLZ} - for an @*AJAX@ client, it won't be compressed by default (even if you can enable the deflate algorithm - which may slow down the server).
Then the response will be marked as to be sent back to the Client...
And {\f1\fs20 http.sys} will handle all the communication by itself, leaving the server free to process the next request.
:   UAC and Vista/Seven support
This works fine under XP. Performances are very good, and stability is there.
But... here comes the UAC nightmare again. Security settings have changed since XP. Now only applications running with Administrator rights can register URLs to {\f1\fs20 http.sys}. That is, no real application.
So the URI registration step will always fail with the default settings, under Vista and Seven.
Our {\f1\fs20 SynCrtSock} unit provides a dedicated method to authorize a particular URI prefix to be registered by any user. Therefore, a program can be easily created and called once with administrator rights to make http.sys work with our framework. This could be for instance part of your Setup program. Then when your server application will be launched (for instance, as a background Windows service), it will be able to register all needed URL.
Nice and easy.
Here is a sample program which can be launched to allow our {\f1\fs20 TestSQL3.dpr} to work as expected - it will allow any connection via the 888 port, using {\f1\fs20 @*TSQLModel@. Root} set as 'root'- that is, an URI prefix of {\f1\fs20 http://+:888/root/} as expected by the kernel server:
!program TestSQL3Register;
!uses
!  SynCrtSock,
!  SysUtils;
!
!// force elevation to Administrator under Vista/Seven
!{$R VistaAdm.res}
!
!begin
!!  THttpApiServer.AddUrlAuthorize('root','888',false,'+'));
!end.
:28  BATCH sequences for adding/updating/deleting records
:   BATCH process
When use the so-called @**BATCH@ sequences?
In a standard @*Client-Server@ architecture, especially with the common understanding (and most implementations) of a @*REST@ful service, any {\f1\fs20 Add / Update / Delete} method call requires a back and forth flow to then from the remote server. A so-called {\i round-trip} occurs: a message is sent to the client, the a response is sent back to the client.
In case of a remote connection via the Internet (or a slow network), you could have up to 100 ms of latency: it's just the "ping" timing, i.e. the time spent for your IP packet to go to the server, then back to you.
If you are making a number of such calls (e.g. add 1000 records), you'll have 100*1000 ms = 100 s = 1:40 min just because of this network latency!
\graph BATCHRoundTrip1 BATCH mode Client-Server latency
\ORM CRUD operation\ORM HTTP Client\In-process¤no latency
\ORM HTTP Client\ORM CRUD operation
\ORM HTTP Client\ORM HTTP Server\Internet¤100 ms latency
\ORM HTTP Server\ORM HTTP Client
\ORM HTTP Server\ORM database core\In-process¤no latency
\ORM database core\ORM HTTP Server
\
The BATCH sequence allows you to regroup those statements into just ONE remote call. Internally, it builds a @*JSON@ stream, then post this stream at once to the server. Then the server answers at once, after having performed all the modifications.
Some new {\f1\fs20 TSQLRestClientURI} methods have been added to implement BATCH sequences to speed up database modifications: after a call to {\f1\fs20 BatchStart}, database modification statements are added to the sequence via {\f1\fs20 BatchAdd / BatchUpdate / BatchDelete}, then all statements are sent as one to the remote server via {\f1\fs20 BatchSend} - this is MUCH faster than individual calls to {\f1\fs20 Add / Update / Delete} in case of a slow remote connection (typically @*HTTP@ over Internet).
Since the statements are performed at once, you can't receive the result (e.g. the ID of the added row) on the same time as you append the request to the BATCH sequence. So you'll have to wait for the {\f1\fs20 BatchSend} method to retrieve all results, {\i at once}, in a {\i dynamic} {\f1\fs20 array of integer}.
As you may guess, it's also a good idea to use a @*transaction@ for the whole process. By default, the BATCH sequence is not embedded into a transaction. It's up to the caller to use a {\f1\fs20 TransactionBegin} ... {\f1\fs20 try}... {\f1\fs20 Commit  except RollBack} block.
Here is a typical use (extracted from the regression @*test@s in {\f1\fs20 SQLite3.pas}:
!// start the transaction
!!if ClientDist.TransactionBegin(TSQLRecordPeople) then
!try
!  // start the BATCH sequence
!!  Check(ClientDist.BatchStart(TSQLRecordPeople));
!  // delete some elements
!  for i := 0 to n-1 do
!!    Check(ClientDist.BatchDelete(IntArray[i])=i);
!  // update some elements
!  nupd := 0;
!  for i := 0 to aStatic.Count-1 do
!  if i and 7<>0 then
!  begin // not yet deleted in BATCH mode
!    Check(ClientDist.Retrieve(aStatic.ID[i],V));
!    V.YearOfBirth := 1800+nupd;
!!    Check(ClientDist.BatchUpdate(V)=nupd+n);
!    inc(nupd);
!  end;
!  // add some elements
!  V.LastName := 'New';
!  for i := 0 to 1000 do
!  begin
!    V.FirstName := RandomUTF8(10);
!    V.YearOfBirth := i+1000;
!!    Check(ClientDist.BatchAdd(V,true)=n+nupd+i);
!  end;
!  // send the BATCH sequences to the server
!!  Check(ClientDist.BatchSend(Results)=200);
!  // now Results[] contains the results of every BATCH statement...
!  Check(Length(Results)=n+nupd+1001);
!  // Results[0] to Results[n-1] should be 200 = deletion OK
!  // Results[n] to Results[n+nupd-1] should be 200 = update OK
!  // Results[n+nupd] to Results[high(Results)] are the IDs of each added record
!  for i := 0 to n-1 do
!    Check(not ClientDist.Retrieve(IntArray[i],V),'BatchDelete');
!    for i := 0 to high(Results) do
!      if i<nupd+n then
!        Check(Results[i]=200) else
!      begin
!        Check(Results[i]>0);
!        ndx := aStatic.IDToIndex(Results[i]);
!        Check(ndx>=0);
!        with TSQLRecordPeople(aStatic.Items[ndx]) do
!        begin
!          Check(LastName='New','BatchAdd');
!          Check(YearOfBirth=1000+i-nupd-n);
!        end;
!      end;
!  // in case of success, apply the Transaction
!!  ClientDist.Commit;
!except
!  // In case of error, rollback the Transaction
!!  ClientDist.RollBack;
!end;
In the above code, all @*CRUD@ operations are performed as usual, using {\f1\fs20 Batch*()} methods instead of plain {\f1\fs20 Add / Delete / Update}. The ORM will take care of all internal process, including serialization.
:   Implementation details
As described above, all {\f1\fs20 Batch*()} methods are serialized as JSON on the client side, then sent as once to the server, where it will be processed without any client-server {\i round-trip }and slow latency.
Here is a typical JSON stream sent to the server:
${"People":["DELETE":2,"DELETE":13,"DELETE":24,
$   (...)  all DELETE actions
$  ,"DELETE":11010,
$  "PUT":{"RowID":3,"FirstName":"Sergei1","LastName":"Rachmaninoff","YearOfBirth":1800, "YearOfDeath":1943},
$  "PUT":{"RowID":4,"FirstName":"Alexandre1","LastName":"Dumas","YearOfBirth":1801, "YearOfDeath":1870},
$   (...)  all PUT = update actions
$  "PUT":{"RowID":11012,"FirstName":"Leonard","LastName":"da VinÃ§i","YearOfBirth":9025, "YearOfDeath":1519},
$  "POST":{"FirstName":"â€š@â€¢Å"Hâ€ mÂ£Â g","LastName":"New","YearOfBirth":1000, "YearOfDeath":1519},
$  "POST":{"FirstName":"@â€¦,KAÂ½Ã #Â¶f","LastName":"New","YearOfBirth":1001, "YearOfDeath":1519},
$   (...)  all POST = add actions
$ "POST":{"FirstName":"+ÂtqCXW3Ã‚\"","LastName":"New","YearOfBirth":2000, "YearOfDeath":1519}
$  ]}
Here is a typical JSON stream receiver from the server, on success:
$ [200,200,...]
All the JSON generation (client-side) and parsing (server-side) is very optimized and fast. With the new internal {\i @*SynLZ@} compression (available by default in our @*HTTP@ Client-Server classes), used bandwidth is minimal.
Thanks to these methods, most time is now spent into the database engine itself, and not in the communication layer.
Beginning with revision 1.16 of the framework, the {\f1\fs20 BatchUpdate} method will only update the mapped fields if called on a record in which a {\f1\fs20 FillPrepare} was performed, and not unmapped (i.e. with no call to {\f1\fs20 FillClose}). For instance, in the following code, {\f1\fs20 V.FillPrepare} will retrieve only {\f1\fs20 ID} and {\f1\fs20 YearOfBirth} fields of the {\f1\fs20 TSQLRecordPeople} table, so subsequent {\f1\fs20 BatchUpdate(V)} calls will only update the {\f1\fs20 YearOfBirth} field:
!  // test BATCH update from partial FillPrepare
!!  V.FillPrepare(ClientDist,'LastName=:("New"):','ID,YearOfBirth');
!  if ClientDist.TransactionBegin(TSQLRecordPeople) then
!  try
!    Check(ClientDist.BatchStart(TSQLRecordPeople));
!    n := 0;
!    V.LastName := 'NotTransmitted';
!    while V.FillOne do begin
!      Check(V.LastName='NotTransmitted');
!      Check(V.YearOfBirth=n+1000);
!      V.YearOfBirth := n;
!!      ClientDist.BatchUpdate(V); // will update only V.YearOfBirth
!      inc(n);
!    end;
!  (...)
:   Array binding
When used in conjunction with @27@, BATCH methods can be implemented as {\i @**array bind@ing} if the corresponding {\f1\fs20 TSQLDBConnection} implementation implements the feature (only {\f1\fs20 SynDBOracle} unit has it by now). In fact, when using a remote database on a physical network, you won't be able to achieve more than 500-600 requests per second when performing {\f1\fs20 INSERT}, {\f1\fs20 DELETE} or {\f1\fs20 UPDATE} statements: the same {\f1\fs20 round-trip} occurs, this time between the ORM server side and the external Database engine.
\graph BATCHRoundTrip2 BATCH mode latency issue on external DB
\ORM CRUD operation\ORM HTTP Client\In-process¤no latency
\ORM HTTP Client\ORM CRUD operation
\ORM HTTP Client\ORM HTTP Server\Internet¤100 ms latency
\ORM HTTP Server\ORM HTTP Client
\ORM HTTP Server\ORM database core\In-process¤no latency
\ORM database core\ORM HTTP Server
\ORM database core\External DB\Local Network¤1 ms latency
\External DB\ORM database core
\
Of course, this 1 ms latency due to the external datatabase additional round-trip may sounds negligeable, but in @17@ when most process is done on the server side, it may introduce a huge performance difference. Your customers would not understand why using a {\i SQLite3} engine would be much faster than a dedicated {\i Oracle} instance they do pay for.
Our {\f1\fs20 SynDB} unit has been enhanced to introduce new {\f1\fs20 TSQLDBStatement.BindArray()} methods, introducing {\i array binding} for faster database batch modifications. It is working in conjuction with our BATCH methods, so CRUD modification actions are grouped within one {\i round-trip} over the network.
Thanks to this enhancement, inserting records within {\i Oracle} comes from 400-500 rows per second to more than 50000 rows per second, in our @*benchmark@s. See @59@ and for details and performance charts the article at @http://blog.synopse.info/post/2012/07/19/Oracle-Array-Binding-and-BATCH-performance.
In fact, some modern database engine (e.g. {\i Oracle} or MS SQL) are even faster when using {\i array binding}, not only due to the network latency reduce, but to the fact that in such operations, integrity checking and indexes update is performed at the end of the bulk process. If your table has several indexes and constraints, it will make using this feature even faster than a "naive" stored procedure with statements within a loop.
If you want to use a {\i @*map/reduce@} algorithm in your application, in addition to ORM data access, all those enhancements may speed up a lot your process. Reading and writing huge amount of data has never been so fast and easy: you may even be tempted to replace stored-procedure process by high-level code implemented in your Domain service. N-tier separation would benefit from it.
:39  CRUD level cache
:   Where to cache
Starting with revision 1.16 of the framework, tuned record cache has been implemented at the @*CRUD@/@*REST@ful level, for specific tables or records, on both the {\i server} and {\i client} sides. See @38@ for the other cache patterns available in the framework.
In fact, a unique caching mechanism is shared at the {\f1\fs20 TSQLRest} level, for both {\f1\fs20 TSQLRestClient} and {\f1\fs20 TSQLRestServer} kind of classes. Therefore, Delphi clients can have their own cache, and the Server can also have its own cache. A client without any cache (e.g. a rough AJAX client) will take advantage of the server cache, at least.
\graph mORMotCaching CRUD caching in mORMot
\Internet (VPN)\Local Network
node [shape=box];
\Local Network\Server
subgraph cluster_0 {
"Client 1\n(Delphi)";
"client\ncache";
label="PC 1";
}
subgraph cluster_1 {
"Client 2\n(AJAX)";
label="PC 2";
}
subgraph cluster_2 {
\Server\DB
"server\ncache";
label="PC Server";
}
subgraph cluster_3 {
"Client n\n(Delphi)";
"client\n cache";
label="PC n";
}
\Client 1¤(Delphi)\Local Network\JSON + REST¤over HTTP/1.1
\Client 2¤(AJAX)\Internet (VPN)\JSON + REST¤over HTTP/1.1
\Client n¤(Delphi)\Internet (VPN)
\client¤cache\Client 1¤(Delphi)
\Client n¤(Delphi)\client¤ cache
\client¤ cache\Client n¤(Delphi)
\Client 1¤(Delphi)\client¤cache
\server¤cache\Server
\Server\server¤cache
\Server\external DB
\
When caching is set {\i on the server} for a particular record or table, in-memory values could be retrieved from this cache instead of calling the database engine each time. When properly used, this would increase global server responsiveness and allow more clients to be served with the same hardware.
{\i On the client} side, a local in-memory cache could be first checked when a record is to be retrieved. If the item is found, the client uses this cached value. If the data item is not in the local cache, the query is then sent to the server, just as usual. Due to the high latency of a remote client-server request, adding caching on the client side does make sense. Client caching properties can be tuned in order to handle properly remote HTTP access via the Internet, which may be much slower than a local Network.
Our caching implementation is transparent to the CRUD code. The very same usual ORM methods are to be called to access a record ({\f1\fs20 Retrieve  / Update / Add}), then either client or server cache will be used, if available. For applications that frequently access the same data - a large category - record-level caching improves both performance and scalability.
:   When to cache
The main problem with cache is about data that both changes and is accessed simultaneously by multiple clients.
In the current implementation, a "pessimistic" concurrency control is used by our framework, relying on explicit locks, and (ab)use of its @15@ general design. It is up to the coder to ensure that no major confusion could arise from concurrency issues.
You must tune caching at both Client and Server level - each side will probably require its own set of cache options.
In your project project implementation, caching should better not to be used at first, but added on need, when performance and efficiency was found to be required. Adding a cache shall imply having automated regression tests available, since in a Client-Server multi-threaded architecture, "{\i premature optimization is the root of all evil}" (Donald Knuth).
The main rules may be simply:
- {\i Not to cache if it may break something relevant} (like a global monetary balance value);
- {\i Not to cache unless you need to} (see Knuth's wisdom);
- {\i Ensure that caching is worth it} (if a value is likely to be overridden often, it could be even slower to cache it);
- Test once, test twice, always test and do not forget to test even more.
:   What to cache
A typical content of these two tuned caches can be any global configuration settings, or any other kind of unchanging data which is not likely to vary often, and is accessed simultaneously by multiple clients, such as catalog information for an online retailer.
Another good use of caching is to store data that changes but is accessed by only one client at a time. By setting a cache at the client level for such content, the server won't be called often to retrieve the client-specific data. In such case, the problem of handling concurrent access to the cached data doesn't arise.
Profiling can be necessary to identify which data is to be registered within those caches, either at the client and/or the server side. The logging feature - see @16@ - integrated to {\i mORMot} can be very handy to tune the caching settings, due to its unique customer-side profiling ability.
But most of the time, an human guess at the business logic level is enough to set which data is to be cached on each side, and ensure content coherency.
:   How to cache
A dedicated {\f1\fs20 TSQLRestCache} instance can be created, and will maintain such a tuned caching mechanism, for both {\f1\fs20 TSQLRestClient} and {\f1\fs20 TSQLRestServer} classes.
A call to {\f1\fs20 TSQLRest.Cache}'s {\f1\fs20 SetCache()} and {\f1\fs20 SetTimeOut()} methods is enough to specify which table(s) or record(s) are to be cached, either at the client or the server level.
For instance, here is how the Client-side caching is tested about one individual record:
!    (...)
!!    Client.Cache.SetCache(TSQLRecordPeople); // cache whole table
!    TestOne;
!    Client.Cache.Clear; // reset cache settings
!!    Client.Cache.SetCache(Rec); // cache one record
!    // same as Client.Cache.SetCache(TSQLRecordPeople,Rec.ID);
!    TestOne;
!    (...)
!!    Database.Cache.SetCache(TSQLRecordPeople); // server-side
!    (...)
Note that in the above code, {\f1\fs20 Client.Cache.Clear} is used to reset all cache settings (i.e. not only flush the cache content, but delete all settings previously made with {\f1\fs20 Cache.SetCache()} or {\f1\fs20 Cache.SetTimeOut()} calls. So in the above code, a global cache is first enabled for the whole {\f1\fs20 TSQLRecordPeople} table, then the cache settings are reset, then cache is enabled for only the particular {\f1\fs20 Rec} record.
It's worth warning once again that it's up to the code responsibility to ensure that these caches are consistent over the network. Server side and client side have their own coherency profile to be ensured.
{\i On the Client side}, only local CRUD operations are tracked. According to the stateless design, adding a time out value does definitively make sense, unless the corresponding data is known to be dedicated to this particular client (like a @*session@ data). If no time out period is set, it's up to the client to flush its own cache on purpose, by using {\f1\fs20 TSQLRestClient.Cache.Flush()} methods.
{\i On the Server side}, all CRUD operations of the @*ORM@ (like {\f1\fs20 Add / Update / Delete}) will be tracked, and cache will be notified of any data change. But direct SQL statements changing table contents (like a {\f1\fs20 UPDATE} or a {\f1\fs20 DELETE} over one or multiple rows with a {\f1\fs20 WHERE} clause) are not tracked by the current implementation: in such case, you'll have to manually flush the server cache content, to enforce data coherency. If such statements did occur on the server side, {\f1\fs20 TSQLRestServer.Cache.Flush()} methods are to be called, e.g. in the services which executed the corresponding SQL. If such non-CRUD statements did occur on the client side, it is possible to ensure that the server content is coherent with the client side, via a dedicated {\f1\fs20 TSQLRestClientURI.ServerCacheFlush()} method, which will call a dedicated standard service on the server to flush its cache content on purpose.
:23  Server side process (aka stored procedure)
The framework is able to handle a custom type of "@**stored procedure@" in pure Delphi code, like any powerful @*Client-Server@ solution.
In short, a {\i stored procedure} is a way of moving some data-intensive process on the server side. A client will ask for some data to be retrieved or processed on the server, and all actions will be taken on the server: since no data has to be exchanged between the client and the server, such a feature will be much faster than a pure client-sided solution.
According to the current state of our framework, there are several ways of handling such a server-side {\i stored procedure} process:
- Write your own @*SQL function@ to be used in {\i @*SQLite3@} WHERE statements;
- Low-level dedicated Delphi stored procedures;
- Define some @11@, as {\f1\fs20 TSQLRestServer} @*published method@s (in fact, such a service can have fast direct access to the database on the Server side, so can be identified as a kind of stored procedure).
The Server-Side service appears to be the more @*REST@ful compatible way of implementing a stored procedure mechanism in our framework. But custom SQL may help the client code to be more generic, and particular queries to be more easily written.
:22   Custom SQL functions
The {\i @*SQLite3@} engine defines some standard @**SQL function@s, like {\f1\fs20 abs() min() max()} or {\f1\fs20 upper()}. A complete list is available at @http://www.sqlite.org/lang_corefunc.html
One of the greatest {\i SQLite3} feature is the ability to define custom SQL functions in high-level language. In fact, its C API allows implementing new functions which may be called within a SQL query. In other database engine, such functions are usually named UDF (for {\i User Defined Functions}).
Some custom already defined SQL functions are defined by the framework. You may have to use, on the Server-side:
- {\f1\fs20 Rank} used for page ranking in @24@;
- {\f1\fs20 Concat} to process fast string concatenation;
- {\f1\fs20 Soundex SoundexFR SoundexES} for computing the English / French / Spanish soundex value of any text;
- {\f1\fs20 @*IntegerDynArrayContains@, ByteDynArrayContains, WordDynArrayContains, CardinalDynArrayContains, Int64DynArrayContains, CurrencyDynArrayContains, RawUTF8DynArrayContainsCase, RawDynArrayContainsNoCase, } for direct search inside a BLOB column containing some @*dynamic array@ binary content (expecting either an INTEGER either a TEXT search value as 2nd parameter).
Those functions are no part of the {\i SQlite3} engine, but are available inside our ORM to handle BLOB containing @*dynamic array@ properties, as stated in @21@.
Since you may use such SQL functions in an {\f1\fs20 UPDATE} or {\f1\fs20 INSERT} SQL statement, you may have an easy way of implementing server-side process of complex data, as such:
$ UPDATE MyTable SET SomeField=0 WHERE IntegerDynArrayContains(IntArrayField,:(10):)
:    Implementing a function
Let us implement a {\f1\fs20 CharIndex()} SQL function, defined as such:
! CharIndex ( SubText, Text [ , StartPos ] )
In here, {\f1\fs20 SubText} is the string of characters to look for in {\f1\fs20 Text}. {\f1\fs20 StartPos} indicates the starting index where {\f1\fs20 charindex()} should start looking for {\f1\fs20 SubText} in {\f1\fs20 Text}. Function shall return the position where the match occurred, 0 when no match occurs. Characters are counted from 1, just like in {\f1\fs20 PosEx()} Delphi function.
The SQL function implementation pattern itself is explained in the {\f1\fs20 sqlite3_create_function_v2()} and {\f1\fs20 TSQLFunctionFunc}:
- {\f1\fs20 argc} is the number of supplied parameters, which are available in {\f1\fs20 argv[]} array (you can call {\f1\fs20 ErrorWrongNumberOfArgs(Context)} in case of unexpected incoming number of parameters);
- Use {\f1\fs20 sqlite3_value_*(argv[*])} functions to retrieve a parameter value;
- Then set the result value using {\f1\fs20 sqlite3_result_*(Context,*)} functions.
Here is a typical implementation code of the {\f1\fs20 CharIndex()} SQL function, calling the expected low-level {\i SQLite3} API (note the {\b {\f1\fs20 cdecl}} calling convention, since it is a {\i SQLite3} / C callback function):
!procedure InternalSQLFunctionCharIndex(Context: TSQLite3FunctionContext;
!  argc: integer; var argv: TSQLite3ValueArray); cdecl;
!var StartPos: integer;
!begin
!  case argc of
!  2: StartPos := 1;
!  3: begin
!!    StartPos := sqlite3_value_int64(argv[2]);
!    if StartPos<=0 then
!      StartPos := 1;
!  end;
!  else begin
!    ErrorWrongNumberOfArgs(Context);
!    exit;
!  end;
!  end;
!  if (sqlite3_value_type(argv[0])=SQLITE_NULL) or
!     (sqlite3_value_type(argv[1])=SQLITE_NULL) then
!    sqlite3_result_int64(Context,0) else
!!    sqlite3_result_int64(Context,SynCommons.PosEx(
!!      sqlite3_value_text(argv[0]),sqlite3_value_text(argv[1]),StartPos));
!end;
This code just get the parameters values using {\f1\fs20 sqlite3_value_*()} functions, then call the {\f1\fs20 PosEx()} function to return the position of the supplied text, as an INTEGER, using {\f1\fs20 sqlite3_result_int64()}.
The local {\f1\fs20 StartPos} variable is used to check for an optional third parameter to the SQL function, to specify the character index to start searching from.
The special case of a {\f1\fs20 NULL} parameter is handled by checking the incoming argument type, calling {\f1\fs20 sqlite3_value_type(argv[])}.
:    Registering a function
:     Direct low-level SQLite3 registration
Since we have a {\f1\fs20 InternalSQLFunctionCharIndex()} function defined, we may register it with direct {\i SQLite3} API calls, as such:
!  sqlite3_create_function_v2(Demo.DB,
!    'CharIndex',2,SQLITE_ANY,nil,InternalSQLFunctionCharIndex,nil,nil,nil);
!  sqlite3_create_function_v2(Demo.DB,
!    'CharIndex',3,SQLITE_ANY,nil,InternalSQLFunctionCharIndex,nil,nil,nil);
The function is registered twice, one time with 2 parameters, then with 3 parameters, to add an overloaded version with the optional {\f1\fs20 StartPos} parameter.
:     Class-driven registration
It's possible to add some custom SQL functions to the {\i SQlite3} engine itself, by creating a {\f1\fs20 TSQLDataBaseSQLFunction} custom class and calling the {\f1\fs20 TSQLDataBase.RegisterSQLFunction} method.
The standard way of using this is to override the {\f1\fs20 @*TSQLRestServerDB@.InitializeEngine virtual} method, calling {\f1\fs20 DB.RegisterSQLFunction()} with an defined {\f1\fs20 TSQLDataBaseSQLFunction} custom class.
So instead of calling low-level {\f1\fs20 sqlite3_create_function_v2()} API, you can declare the {\f1\fs20 CharIndex} SQL function as such:
!  Demo.RegisterSQLFunction(InternalSQLFunctionCharIndex,2,'CharIndex');
!  Demo.RegisterSQLFunction(InternalSQLFunctionCharIndex,3,'CharIndex');
The two lines above will indeed wrap the following code:
!  Demo.RegisterSQLFunction(TSQLDataBaseSQLFunction.Create(InternalSQLFunctionCharIndex,2,'CharIndex'));
!  Demo.RegisterSQLFunction(TSQLDataBaseSQLFunction.Create(InternalSQLFunctionCharIndex,3,'CharIndex'));
The {\f1\fs20 RegisterSQLFunction()} method is called twice, one time with 2 parameters, then with 3 parameters, to add an overloaded version with the optional {\f1\fs20 StartPos} parameter, as expected.
:     Custom class definition
The generic function definition may be completed, in our framework, with a custom class definition, which is handy to have some specific context, not only relative to the current SQL function context, but global and static to the whole application process.
\graph HierTSQLDataBaseSQLFunctionDynArray TSQLDataBaseSQLFunction classes hierarchy
\TSQLDataBaseSQLFunction\TObject
\TSQLDataBaseSQLFunctionDynArray\TSQLDataBaseSQLFunction
\
For instance, the following method will register a SQL function able to search into a BLOB-stored custom dynamic array type:
!procedure TSQLDataBase.RegisterSQLFunction(aDynArrayTypeInfo: pointer;
!  aCompare: TDynArraySortCompare; const aFunctionName: RawUTF8);
!begin
!  RegisterSQLFunction(
!    TSQLDataBaseSQLFunctionDynArray.Create(aDynArrayTypeInfo,aCompare,aFunctionName));
!end;
We specify directly the {\f1\fs20 TSQLDataBaseSQLFunctionDynArray} class instance to work with, which adds two needed protected fields to the {\f1\fs20 TSQLDataBaseSQLFunction} root class:
- A {\f1\fs20 fDummyDynArray TDynArray} instance which will handle the dynamic array RTTI handling;
- A {\f1\fs20 fDummyDynArrayValue pointer}, to be used to store the dynamic array reference values to be used during the dynamic array process.
Here is the corresponding class definition:
!  /// to be used to define custom SQL functions for dynamic arrays BLOB search
!  TSQLDataBaseSQLFunctionDynArray = class(TSQLDataBaseSQLFunction)
!  protected
!    fDummyDynArray: TDynArray;
!    fDummyDynArrayValue: pointer;
!  public
!    /// initialize the corresponding SQL function
!    // - if the function name is not specified, it will be retrieved from the type
!    // information (e.g. TReferenceDynArray will declare 'ReferenceDynArray')
!    // - the SQL function will expect two parameters: the first is the BLOB
!    // field content, and the 2nd is the array element to search (set with
!    // TDynArray.ElemSave() or with BinToBase64WithMagic(aDynArray.ElemSave())
!    // if called via a Client and a JSON prepared parameter)
!    // - you should better use the already existing faster SQL functions
!    // Byte/Word/Integer/Cardinal/Int64/CurrencyDynArrayContains() if possible
!    // (this implementation will allocate each dynamic array into memory before
!    // comparison, and will be therefore slower than those optimized versions)
!    constructor Create(aTypeInfo: pointer; aCompare: TDynArraySortCompare;
!      const aFunctionName: RawUTF8=''); override;
!  end;
And the constructor implementation:
!constructor TSQLDataBaseSQLFunctionDynArray.Create(aTypeInfo: pointer;
!  aCompare: TDynArraySortCompare; const aFunctionName: RawUTF8);
!begin
!  fDummyDynArray.Init(aTypeInfo,fDummyDynArrayValue);
!  fDummyDynArray.Compare := aCompare;
!  inherited Create(InternalSQLFunctionDynArrayBlob,2,aFunctionName);
!end;
The {\f1\fs20 InternalSQLFunctionDynArrayBlob} function is a low-level {\i SQlite3} engine SQL function prototype, which will retrieve a BLOB content, then un-serialize it into a dynamic array (using the {\f1\fs20 fDummyDynArrayValue. LoadFrom} method), then call the standard {\f1\fs20 ElemLoadFind} method to search the supplied element, as such:
! (...)
!  with Func.fDummyDynArray do
!  try
!!    LoadFrom(DynArray); // temporary allocate all dynamic array content
!    try
!!      if ElemLoadFind(Elem)<0 then
!        DynArray := nil;
!    finally
!      Clear; // release temporary array content in fDummyDynArrayValue
!    end;
! (...)
You can define a similar class in order to implement your own custom SQL function.
Here is how a custom SQL function using this {\f1\fs20 TSQLDataBaseSQLFunctionDynArray} class is registered in the supplied unitary tests to an existing database connection:
!  Demo.RegisterSQLFunction(TypeInfo(TIntegerDynArray),SortDynArrayInteger,
!    'MyIntegerDynArrayContains');
This new SQL function expects two BLOBs arguments, the first being a reference to the BLOB column, and the 2nd the searched value. The function can be called as such (lines extracted from the framework regression tests):
!    aClient.OneFieldValues(TSQLRecordPeopleArray,'ID',
!      FormatUTF8('MyIntegerDynArrayContains(Ints,:("%"):)',
!        [BinToBase64WithMagic(@k,sizeof(k))]),IDs);
Note that since the 2nd parameter is expected to be a BLOB representation of the searched value, the {\f1\fs20 BinToBase64WithMagic} function is used to create a BLOB parameter, as expected by the ORM. Here, the element type is an {\f1\fs20 integer}, which is a pure binary variable (containing no reference-counted internal fields): so we use direct mapping from its binary in-memory representation; for more complex element type, you should use the generic {\f1\fs20 BinToBase64WithMagic(aDynArray.ElemSave())} expression instead, calling {\f1\fs20 TDynArray. ElemSave} method.
Note that we did not use here the overloaded {\f1\fs20 OneFieldValues} method expecting '?' bound parameters here, but we may have use it as such:
!    aClient.OneFieldValues(TSQLRecordPeopleArray,'ID',
!      FormatUTF8('MyIntegerDynArrayContains(Ints,?)',[],
!        [BinToBase64WithMagic(@k,sizeof(k))]),IDs);
Since the {\f1\fs20 MyIntegerDynArrayContains} function will create a temporary dynamic array in memory from each row (stored in {\f1\fs20 fDummyDynArrayValue}), the dedicated {\f1\fs20 IntegerDynArrayContains} SQL function is faster.
:   Low-level Delphi stored procedure
To implement a more complete request, and handle any kind of stored data in a column (for instance, some TEXT format to be parsed), a {\f1\fs20 TOnSQLStoredProc} event handler can be called for every row of a prepared statement, and is able to access directly to the database request. Code inside this event handler should not use the @*ORM@ methods of the framework, but direct low-level {\i SQLite} access. This event handler should be specified to the corresponding overloaded {\f1\fs20 @*TSQLRestServerDB@. EngineExecute} method.
This will allow direct content modification during the {\f1\fs20 SELECT} statement. Be aware that, up to now, @20@ {\f1\fs20 TSQLVirtualTableCursorJSON} cursors are not safe to be used if the Virtual Table data is modified.
See the description of those event {\f1\fs20 TOnSQLStoredProc} handler and associated method in the next pages.
:   External stored procedure
If the application relies on external databases - see @27@ - the external database may be located on a remote computer.
In such situation, all RESTful Server-sided solutions could produce a lot of network traffic. In fact, custom SQL functions or stored procedures both use the {\i SQLite3} engine as root component.
In order to speed up the process, you may define some RDMS stored procedures in the external database format (.Net, Java, P/SQL or whatever), then define some @11@ to launch those functions. Note that in this case, you'll loose the database-independence of the framework, and switching to another database engine may cost.
:11 Client-Server services
In order to implement @17@ in our framework, the business logic can be implemented in two ways in the framework:
- Via some {\f1\fs20 @*TSQLRecord@} inherited classes, inserted into the database {\i model}, and accessible via some @*REST@ful URI - this will implement @*ORM@ architecture;
- By some RESTful @**service@s, implemented in the Server as {\i published methods}, and consumed in the Client via native Delphi methods;
- Defining some {\i service @*contract@s} as standard Delphi {\f1\fs20 interface}, and then run it seamlesly on both client and client sides.
The first of the two last items can be compared to the {\i DataSnap} Client-Server features, also @*JSON@-based, introduced in Delphi 2010. See for instance the following example available on the Internet at @http://docwiki.embarcadero.com/RADStudio/en/Developing_DataSnap_Applications
The second is purely interface-based, so matches the "designed by contract" principle - see @47@ - as implemented by Microsoft's @*WCF@ technology. {\i Windows Communication Foundation} is the unified programming model provided by Microsoft for building service-oriented applications - see @http://msdn.microsoft.com/en-us/library/dd456779. We included most of the nice features made available in WCF in {\i mORMot}, in a KISS manner.
:49  Client-Server services via methods
To implement a service in the {\i Synopse mORMot framework}, the first method is to define @**published method@ Server-side, then use easy functions about JSON or URL-parameters to get the request encoded and decoded as expected, on Client-side.
We'll implement the same example as in the official Embarcadero docwiki page above. Add two numbers. Very useful service, isn't it?
:   My Server is rich
On the server side, we need to customize the standard {\f1\fs20 TSQLRestServer} class definition (more precisely a {\f1\fs20 @*TSQLRestServerDB@} class which includes a {\i SQlite3} engine, or a lighter {\f1\fs20 @*TSQLRestServerFullMemory@} kind of server, which is enough for our purpose), by adding a new {\f1\fs20 published} method:
!type
!  TSQLRestServerTest = class(TSQLRestServerFullMemory)
!   (...)
!!  published
!!    function Sum(var aParams: TSQLRestServerCallBackParams): Integer;
!  end;
The method name ("Sum") will be used for the URI encoding, and will be called remotely from {\i ModelRoot/Sum} URL. The {\i ModelRoot} is the one defined in the {\f1\fs20 Root} parameter of the {\i model} used by the application.
This method, like all Server-side methods, MUST have the same exact parameter definition as in the {\f1\fs20 TSQLRestServerCallBack} prototype:
!type
!  TSQLRestServerCallBack = function(var aParams: TSQLRestServerCallBackParams): Integer of object;
Then we implement this method:
!function TSQLRestServerTest.Sum(var aParams: TSQLRestServerCallBackParams): Integer;
!var a,b: Extended;
!begin
!  if not UrlDecodeNeedParameters(aParams.Parameters,'A,B') then
!  begin
!    result := 404; // invalid Request
!    aParams.ErrorMsg^ := 'Missing Parameter';
!    exit;
!  end;
!  while aParameters<>nil do
!  begin
!    UrlDecodeExtended(aParams.Parameters,'A=',a);
!    UrlDecodeExtended(aParams.Parameters,'B=',b,@aParams.Parameters);
!  end;
!  aParams.Resp := JSONEncodeResult([a+b]);
!  // same as : aResp := JSONEncode(['result',a+b],TempMemoryStream);
!  result := 200; // success
!end;
The only not obvious part of this code is the parameters marshaling, i.e. how the values are retrieved from the incoming {\f1\fs20 aParams.Parameters} text buffer, then converted into native local variables.
On the Server side, typical implementation steps are therefore:
- Use the {\f1\fs20 UrlDecodeNeedParameters} function to check that all expected parameters were supplied by the caller in {\f1\fs20 aParams.Parameters};
- Call {\f1\fs20 UrlDecodeInteger / UrlDecodeInt64 / UrlDecodeExtended / UrlDecodeValue / UrlDecodeObject} functions (all defined in {\f1\fs20 SynCommons.pas}) to retrieve each individual parameter from standard JSON content;
- Implement the service (here it is just the {\f1\fs20 a+b} expression);
- Then return the result into {\f1\fs20 aParams.Resp} variable.
The powerful {\f1\fs20 UrlDecodeObject} function (defined in {\f1\fs20 SQLite3Commons.pas}) can be used to un-serialize most class instance from its textual JSON representation ({\f1\fs20 @*TPersistent@, @*TSQLRecord@, TStringList}...).
Note that due to this implementation pattern, the {\i mORMot} service implementation is very fast, and not sensitive to the "Hash collision attack" security issue, as reported with {\i Apache} - see @http://blog.synopse.info/post/2011/12/30/Hash-collision-attack for details.
The implementation must return the HTTP error code (e.g. 200 on success) as an integer value, and any response in {\f1\fs20 aParams.Resp} as a serialized JSON object by default (using e.g. {\f1\fs20 TSQLRestServer.JSONEncodeResult}), since default mime-type is {\f1\fs20 JSON_CONTENT_TYPE}:
$ {"Result":"OneValue"}
or a JSON object containing an array:
$ {"Result":["One","two"]}
So you can consume these services, implemented Server-Side in fast Delphi code, with any @*AJAX@ application on the client side (if you use HTTP as communication protocol).
The {\f1\fs20 aParams.Head^} parameter may be overridden on the server side to set a custom header which will be provided to the client - it may be useful for instance to specify another mime-type than the default constant {\f1\fs20 JSON_CONTENT_TYPE}, i.e. {\f1\fs20 'application/json; charset=UTF-8'}, and returns plain text, HTML or binary.
In case of an error on the server side (only two valid status codes are {\f1\fs20 200} and {\f1\fs20 201}), the client will receive a corresponding serialized JSON error object, as such:
${
$ "ErrorCode":404,
$ "ErrorText":"Missing Parameter"
$}
The {\f1\fs20 aParams.ErrorMsg^} parameter can be overridden on the server side to specify a custom error message in plain English, which will be returned to the client in case of an invalid status code. If no custom {\f1\fs20 ErrorMsg} is specified, the framework will return the corresponding generic HTTP status text.
The {\f1\fs20 aParams.Context} parameter may contain at calling time the expected {\f1\fs20 TSQLRecord} ID (as decoded from {\i RESTful} URI), and the current session, user and group IDs. If @*authentication@ - see @18@ - is not used, this parameter is meaningless: in fact, {\f1\fs20 aParams.Context.Session} will contain either 0 if any @*session@ is not yet started, or 1 if authentication mode is not active. Server-side implementation can use the {\f1\fs20 TSQLRestServer.SessionGetUser} method to retrieve the corresponding user details (note that when using this method, the returned {\f1\fs20 TSQLAuthUser} instance is a local thread-safe copy which shall be freed when done).
An {\i important point} is to remember that the implementation of the callback method {\b must be thread-safe} - as stated by @25@. In fact, the {\f1\fs20 TSQLRestServer.URI} method expects such callbacks to handle the thread-safety on their side. It's perhaps some more work to handle a critical section in the implementation, but, in practice, it's the best way to achieve performance and scalability: the resource locking can be made at the tiniest code level.
:   The Client is always right
The client-side is implemented by calling some dedicated methods, and providing the service name ({\f1\fs20 'sum'}) and its associated parameters:
!function Sum(aClient: TSQLRestClientURI; a, b: double): double;
!var err: integer;
!begin
!  val(aClient.CallBackGetResult('sum',['a',a,'b',b]),Result,err);
!end;
You could even implement this method in a dedicated client method - which make sense:
!type
!  TMyClient = class(TSQLite3HttpClient) // could be TSQLRestClientURINamedPipe
!  (...)
!    function Sum(a, b: double): double;
!  (...)
!
!function TMyClient.Sum(a, b: double): double;
!var err: integer;
!begin
!  val(CallBackGetResult('sum',['a',a,'b',b]),Result,err);
!end;
This later implementation is to be preferred on real applications.
You have to create the server instance, and the corresponding {\f1\fs20 TSQLRestClientURI} (or {\f1\fs20 TMyClient}), with the same database model, just as usual...
On the Client side, you can use the {\f1\fs20 CallBackGetResult} method to call the service from its name and its expected parameters, or create your own caller using the {\f1\fs20 UrlEncode()} function. Note that you can specify most class instance into its JSON representation by using some {\f1\fs20 TObject} into the method arguments:
!function TMyClient.SumMyObject(a, b: TMyObject): double;
!var err: integer;
!begin
!  val(CallBackGetResult('summyobject',['a',a,'b',b]),Result,err);
!end;
This Client-Server protocol uses JSON here, but you can serve any kind of data, binary, HTML, whatever... just by overriding the content type on the server.
The usual protocols of our framework can be used: @*HTTP@/1.1, Named Pipe, Windows GDI messages, direct in-memory/in-process access.
Of course, these services can be related to any table/class of our @*ORM@ framework, so you would be able to create easily any RESTful compatible requests on URI like {\f1\fs20 ModelRoot/TableName/ID/MethodName}. The ID of the corresponding record is decoded from its {\i RESTful} scheme into {\f1\fs20 aParams.Context.ID}. For example, here we return a @*BLOB@ field content as hexadecimal:
!function TSQLRestServerTest.DataAsHex(var aParams: TSQLRestServerCallBackParams): Integer;
!var aData: TSQLRawBlob;
!begin
!  result := 404; // invalid Request
!  if (self=nil) or (aParams.Table=nil) or
!      not aParams.Table.InheritsFrom(TSQLRecord) or
!    (aParams.Context.ID<0) then
!    exit; // we need a valid record and its ID
!  if not RetrieveBlob(TSQLRecordPeople,aParams.Context.ID,'Data',aData) then
!    exit; // impossible to retrieve the Data BLOB field
!  aParams.Resp := JSONEncodeResult([SynCommons.BinToHex(aData)]);
!  // idem: aResp := JSONEncode(['result',BinToHex(aRecord.fData)],TempMemoryStream);
!  result := 200; // success
!end;
:  Interface based services
The @49@ implementation gives full access to the lowest-level of the {\i mORMot}'s core, so it has some advantages:
- It can be tuned to fit any purpose (such as retrieving or returning some HTML or binary data, or modifying the HTTP headers on the fly);
- It is integrated into the @*REST@ful URI model, so it can be related to any table/class of our @*ORM@ framework (like {\f1\fs20 DataAsHex} service above), or it can handle any remote query (e.g. any @*AJAX@ or @*SOAP@ requests);
- It has a very low performance overhead, so can be used to reduce server workload for some common tasks.
But this implementation pattern has some drawbacks:
- Most content marshaling is to be done by hand, so may introduce implementation issues;
- Client and server side code does not have the same implementation pattern, so you will have to code explicitly data marshaling twice, for both client and server;
- The services do not have any hierarchy, and are listed as a plain list, which is not very convenient;
- It is difficult to synchronize several service calls within a single context, e.g. when a workflow is to be handled during the application process (you have to code some kind of state machine on both sides);
- @*Security@ is handled globally for the user, or should be checked by hand in the implementation method (using the {\f1\fs20 aParams.Context} values).
You can get rid of those limitations with the interface-based service implementation of {\i mORMot}. For a detailed introduction and best practice guide to @*SOA@, you can consult this classic article: @http://www.ibm.com/developerworks/webservices/library/ws-soa-design1
According to this document, all expected SOA features are now available in the current implementation of the {\i mORMot} framework (including service catalog aka "broker").
:   Implemented features
Here are the key features of the current implementation of services using interfaces in the {\i Synopse mORMot framework}:
|%25%75
|\b Feature|Remarks\b0
|Service Orientation|Allow loosely-coupled relationship
|Design by contract|Data Contracts are defined in Delphi code as standard {\f1\fs20 interface} custom types
|Factory driven|Get an implementation instance from a given interface
|Server factory|You can get an implementation on the server side
|Client factory|You can get a "fake" implementation on the client side, remotely calling the server to execute the process
|Auto marshaling|The contract is transparently implemented: no additional code is needed e.g. on the client side, and will handle simple types (strings, numbers, dates, sets and enumerations) and high-level types (objects, collections, records, dynamic arrays) from Delphi 6 up to XE2
|Flexible|Methods accept per-value or per-reference parameters
|Instance lifetime|An implementation class can be:\line - Created on every call,\line - Shared among all calls,\line - Shared for a particular user or group,\line - Stay alive as long as the client-side interface is not released,\line - or as long as an @*authentication@ session exists
|Stateless|Following a standard request/reply pattern
|Signed|The contract is checked to be consistent before any remote execution
|Secure|Every service and/or methods can be enabled or disabled on need
|Safe|Using extended RESTful authentication - see @18@
|Multi-hosted\line (with DMZ)|Services are hosted by default within the main @*ORM@ server, but can have their own process, with a dedicated connection to the ORM core
|Broker ready|Service meta-data can be optionally revealed by the server
|Multiple transports|All Client-Server protocols of mORMot are available, i.e. direct in-process connection, GDI messages, named pipes, TCP/IP-HTTP
|JSON based|Transmitted data uses JavaScript Object Notation
|Routing choice|Services are identified either at the URI level (the RESTful way), either in a JSON-RPC model (the @*AJAX@ way)
|AJAX and RESTful|JSON and HTTP combination allows services to be consumed from AJAX rich clients
|Light & fast|Performance and memory consumption are very optimized, in order to ensure scalability and ROI
|%
:   How to make services
The typical basic tasks to perform are the following:
- Define the service contract;
- Implement the contract;
- Configure and host the service;
- Build a client application.
We will describe those items.
:   Defining a data contract
In a @*SOA@, services tend to create a huge list of operations. In order to facilitate implementation and maintenance, operations shall be grouped within common services.
Before defining how such services are defined within {\i mORMot}, it is worth applying the @17@ main principles, i.e. loosely-coupled relationship. When you define {\i mORMOt} contracts, ensure that this contract will stay un-coupled with other contracts. It will help writing @*SOLID@ code, enhance maintenability, and allow introducing other service providers on demand (some day or later, you'll certainly be asked to replace one of your service with a third-party existing implementation of the corresponding feature: you shall at least ensure that your own implementation would be easily re-coded with external code, using e.g. a @*SOAP@/WSDL @*gateway@).
:    Define an interface
The data contract is to be defined as a plain Delphi {\f1\fs20 interface} type. In fact, the sample type as stated above - see @46@ - can be used directly:
!type
!  ICalculator = interface(IInvokable)
!    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
!    /// add two signed 32 bit integers
!    function Add(n1,n2: integer): integer;
!  end;
This {\f1\fs20 ICalculator.Add} method will define one "{\i Add}" operation, under the "{\i ICalculator}" service (which will be named internally {\f1\fs20 'Calculator'} by convention). This operation will expect two numbers as input, and then return the sum of those numbers.
The current implementation of service has the following expectations:
- Any interface inheriting from {\f1\fs20 IInvokable}, with a GUID, can be used - we expect the RTTI to be available, so {\f1\fs20 IInvokable} is a good parent type;
- You can inherit an interface from an existing one: in this case, the inherited methods will be part of the child interface, and will be expected to be implemented (just as with standard Delphi code);
- Only plain ASCII names are allowed for the type definition (as it is conventional to use English spelling for service and operation naming);
- Calling convention shall be {\f1\fs20 register} (the Delphi's default) - nor {\f1\fs20 stdcall} nor {\f1\fs20 cdecl} is available yet, but this won't be a restriction since the {\f1\fs20 interface} definition is dedicated to Delphi code scope;
- Methods can have a result, and accept per-value or per-reference parameters.
In fact, parameters expectations are the following:
- Simple types (strings, numbers, dates, sets and enumerations) and high-level types (objects, collections, records and dynamic arrays) are handled - see below for the details;
- They can be defined as {\f1\fs20 const}, {\f1\fs20 var} or {\f1\fs20 out} - in fact, {\f1\fs20 const} and {\f1\fs20 var} parameters values will be sent from the client to the server as JSON, and {\f1\fs20 var} and {\f1\fs20 out} parameters values will be returned as JSON from the server;
- {\f1\fs20 procedure} or {\f1\fs20 function} kind of method definition are allowed;
- Only exception is that you can't have a function returning a {\f1\fs20 class} instance (how will know when to release the instance in this case?), but such instances can be passed as {\f1\fs20 const}, {\f1\fs20 var} or {\f1\fs20 out} parameters (and {\f1\fs20 published} properties will be serialized within the JSON message);
- In fact, the {\f1\fs20 @*TCollection@} kind of parameter is not directly handled by the framework: you shall define a {\f1\fs20 @*TInterfacedCollection@} class, overriding its {\f1\fs20 GetClass} abstract virtual method (otherwise the server side won't be able to create the kind of collection as expected);
- Special {\f1\fs20 @*TServiceCustomAnswer@} kind of record can be used as {\f1\fs20 function} result to specify a custom content (with specified encoding, to be used e.g. for @*AJAX@ or HTML consumers) - in this case, no {\f1\fs20 var} nor {\f1\fs20 out} parameters values shall be defined in the method (only the BLOB value is returned).
:    Available types for methods parameters
Handled types of parameters are:
|%30%70
|\b Delphi type|Remarks\b0
|{\f1\fs20 boolean}|Transmitted as @*JSON@ true/false
|{\f1\fs20 integer cardinal Int64 double currency TDateTime}|Transmitted as JSON numbers
|enumerations|Transmitted as JSON number
|set|Transmitted as JSON number - one bit per element (up to 32 elements)
|{\f1\fs20 @*RawUTF8@ @*WideString@}|Transmitted as JSON text (UTF-8 encoded)
|{\f1\fs20 string}|Transmitted as UTF-8 JSON text, but prior to Delphi 2009, the framework will ensure that both client and server sides use the same ANSI code page - so you should better use {\f1\fs20 RawUTF8}
|{\f1\fs20 @*TPersistent@}|Published properties will be transmitted as JSON object
|{\f1\fs20 @*TSQLRecord@}|All fields (including ID) will be transmitted as JSON object
|any {\f1\fs20 @*TObject@}|Via {\f1\fs20 TJSONSerializer.@*RegisterCustomSerializer@}
|{\f1\fs20 @*TCollection@}|Not allowed: use {\f1\fs20 @*TInterfacedCollection@} instead
|{\f1\fs20 @*TInterfacedCollection@}|Transmitted as a JSON array of JSON objects - see @55@
|dynamic arrays|Transmitted as JSON arrays - see @48@
|{\f1\fs20 @*record@}|Transmitted as binary with Base-64 encoding or custom JSON serialization, needed to have RTTI (so a string or dynamic array field within), just like with regular Delphi {\f1\fs20 interface} expectations
|{\f1\fs20 @*TServiceCustomAnswer@}|If used as a {\f1\fs20 function} result (not as parameter), the supplied content will be transmitted directly to the client (with no JSON @*serialization@); in this case, no {\f1\fs20 var} nor {\f1\fs20 out} parameters are allowed in the method - it will be compatible with both our {\f1\fs20 TServiceFactoryClient} implementation, and any other service consumers (e.g. @*AJAX@)
|%
You can therefore define complex {\f1\fs20 interface} types, as such:
!type
!  ICalculator = interface(IInvokable)
!    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
!    /// add two signed 32 bit integers
!    function Add(n1,n2: integer): integer;
!    /// multiply two signed 64 bit integers
!    function Multiply(n1,n2: Int64): Int64;
!    /// substract two floating-point values
!    function Subtract(n1,n2: double): double;
!    /// convert a currency value into text
!    procedure ToText(Value: Currency; var Result: RawUTF8);
!    /// convert a floating-point value into text
!    function ToTextFunc(Value: double): string;
!    /// do some work with strings, sets and enumerates parameters,
!    // testing also var (in/out) parameters and set as a function result
!    function SpecialCall(Txt: RawUTF8; var Int: integer; var Card: cardinal; field: TSynTableFieldTypes;
!      fields: TSynTableFieldTypes; var options: TSynTableFieldOptions): TSynTableFieldTypes;
!    /// test integer, strings and wide strings dynamic arrays, together with records
!    function ComplexCall(const Ints: TIntegerDynArray; Strs1: TRawUTF8DynArray;
!      var Str2: TWideStringDynArray; const Rec1: TVirtualTableModuleProperties;
!      var Rec2: TSQLRestCacheEntryValue): TSQLRestCacheEntryValue;
!  end;
Note how {\f1\fs20 SpecialCall} and {\f1\fs20 ComplexCall} methods have quite complex parameters definitions, including dynamic arrays, sets and records. The framework will handle {\f1\fs20 const} and {\f1\fs20 var} parameters as expected, i.e. as input/output parameters, also on the client side. And simple types of dynamic arrays (like {\f1\fs20 TIntegerDynArray}, {\f1\fs20 TRawUTF8DynArray}, or {\f1\fs20 TWideStringDynArray}) will be serialized as plain JSON arrays - the framework is able to handle any dynamic array definition, but will serialize those simple types in a more AJAX compatible way.
:51    Custom JSON serialization of records
By default, any {\f1\fs20 @*record@} parameter or function result will be serialized with our proprietary binary (and optimized layout) - i.e. {\f1\fs20 RecordLoad} and {\f1\fs20 RecordSave} functions - then encoded in Base-64, to be stored as plain text within the JSON stream.
But custom record @*JSON@ @**serialization@ can be defined, as with any {\f1\fs20 class} - see @52@ - or {\i dynamic array} - see @53@.
In fact, there are two ways of specifying a custom JSON serialization for {\f1\fs20 record}:
- When setting a custom dynamic array JSON serializer, the associated {\f1\fs20 record} will also use the same {\f1\fs20 Reader} and {\f1\fs20 Writer} callbacks;
- By setting explicitly serialization callbacks for the {\f1\fs20 TypeInfo()} of the record, with the very same {\f1\fs20 TTextWriter. @*RegisterCustomJSONSerializer@} method.
For instance, you can serialize the following record definition:
!  TSQLRestCacheEntryValue = record
!    ID: integer;
!    TimeStamp: cardinal;
!    JSON: RawUTF8;
!  end;
With the following code:
!  TTextWriter.RegisterCustomJSONSerializer(TypeInfo(TSQLRestCacheEntryValue),
!    TTestServiceOrientedArchitecture.CustomReader,
!    TTestServiceOrientedArchitecture.CustomWriter);
The expected format will be as such:
& {"ID":1786554763,"TimeStamp":323618765,"JSON":"D:\\TestSQL3.exe"}
Therefore, the writer callback could be:
!class procedure TTestServiceOrientedArchitecture.CustomWriter(
!  const aWriter: TTextWriter; const aValue);
!var V: TSQLRestCacheEntryValue absolute aValue;
!begin
!  aWriter.AddJSONEscape(['ID',V.ID,'TimeStamp',Int64(V.TimeStamp),'JSON',V.JSON]);
!end;
In the above code, the {\f1\fs20 cardinal} field named {\f1\fs20 TimeStamp} is type-casted to a {\f1\fs20 Int64}: in fact, as stated by the documentation of the {\f1\fs20 AddJSONEscape} method, an {\f1\fs20 array of const} will handle by default any {\f1\fs20 cardinal} as an {\f1\fs20 integer} value (this is a limitation of the {\i Delphi} compiler). By forcing the type to be an {\f1\fs20 Int64}, the expected {\f1\fs20 cardinal} value will be transmitted, and not a wrongly negative versions for numbers {\f1\fs20 > $7fffffff}.
On the other side, the corresponding reader callback would be like:
!class function TTestServiceOrientedArchitecture.CustomReader(P: PUTF8Char;
!  var aValue; out aValid: Boolean): PUTF8Char;
!var V: TSQLRestCacheEntryValue absolute aValue;
!    Values: TPUtf8CharDynArray;
!begin
!  result := JSONDecode(P,['ID','TimeStamp','JSON'],Values);
!  if result=nil then
!    aValid := false else begin
!    V.ID := GetInteger(Values[0]);
!    V.TimeStamp := GetCardinal(Values[1]);
!    V.JSON := Values[2];
!    aValid := true;
!  end;
!end;
Note that the callback signature used for {\i records} matches the one used for {\i dynamic arrays} serializations - see @53@ - as it will be shared between the two of them.
Even if older versions of {\i Delphi} are not able to generate the needed RTTI information for such serialization, the {\i mORMot} framework offers a common way of implementing any custom serialization of records.
When records are used as {\i Data Transfer Objects} within services (which is a good idea in common SOA implementation patterns), such a custom serialization format can be handy, and makes more natural service consumption with AJAX clients.
:55    TInterfacedCollection kind of parameter
Due to the current implementation pattern of the {\f1\fs20 TCollection} type in Delphi, it was not possible to implement directly this kind of parameter.
In fact, the {\f1\fs20 TCollection} constructor is defined as such:
! constructor Create(ItemClass: TCollectionItemClass);
And, on the server side, we do not know which kind of {\f1\fs20 TCollectionItemClass} is to be passed. Therefore, the {\f1\fs20 TServiceFactoryServer} is unable to properly instantiate the object instances, supplying the expected item class.
So a dedicated {\f1\fs20 TInterfacedCollection} abstract type has been defined:
!  TInterfacedCollection = class(TCollection)
!  protected
!    function GetClass: TCollectionItemClass; virtual; abstract;
!  public
!    constructor Create; reintroduce; virtual;
!  end;
In order to use a collection of objects, you will have to define at least the abstract method, for instance:
!  TCollTests = class(TInterfacedCollection)
!  protected
!    function GetClass: TCollectionItemClass; override;
!  end;
!
!function TCollTests.GetClass: TCollectionItemClass;
!begin
!  result := TCollTest;
!end;
Or, if you want a more complete / convenient implementation:
!  TCollTests = class(TInterfacedCollection)
!  private
!    function GetCollItem(Index: Integer): TCollTest;
!  protected
!    function GetClass: TCollectionItemClass; override;
!  public
!    function Add: TCollTest;
!    property Item[Index: Integer]: TCollTest read GetCollItem; default;
!  end;
Then you will be able to define a contract as such:
!procedure Collections(Item: TCollTest; var List: TCollTests; out Copy: TCollTests);
A typical implementation of this contract may be:
!procedure TServiceComplexCalculator.Collections(Item: TCollTest;
!  var List: TCollTests; out Copy: TCollTests);
!begin
!  CopyObject(Item,List.Add);
!  CopyObject(List,Copy);
!end;
That is, it will append the supplied {\f1\fs20 Item} object to the provided {\f1\fs20 List} content, then return a copy in the {\f1\fs20 Copy} content:
- Setting {\f1\fs20 Item} without {\f1\fs20 var} or {\f1\fs20 out} specification is doing the same as {\f1\fs20 const}: it will be serialized from client to server (and not back from server to client);
- Setting {\f1\fs20 List} as {\f1\fs20 var} parameter will let this collection to be serialized from client to server, and back from server to the client;
- Setting {\f1\fs20 Copy} as {\f1\fs20 out} parameter will let this collection to be serialized only from server to client.
Note that {\f1\fs20 const / var / out} kind of parameters are used at the contract level in order to specify the direction of serialization, and not as usual (i.e. to define if it is passed {\i by value} or {\i by reference}). All {\f1\fs20 class} parameters shall be instantiated before method call: you can not pass any object parameter as nil (nor use it in a function result): it will raise an error.
:   Server side
:    Implementing service contract
In order to have an operating service, you'll need to implement a Delphi class which matches the expected {\f1\fs20 interface}.
In fact, the sample type as stated above - see @46@ - can be used directly:
!type
!  TServiceCalculator = class(TInterfacedObject, ICalculator)
!  public
!    function Add(n1,n2: integer): integer;
!  end;
!
!function TServiceCalculator.Add(n1, n2: integer): integer;
!begin
!  result := n1+n2;
!end;
And... That is all we need. The Delphi IDE will check at compile time that the class really implements the specified {\f1\fs20 interface} definition, so you'll be sure that your code meets the service contract expectations. Exact match (like handling type of parameters) will be checked by the framework when the service factory will be initialized, so you won't face any runtime exception due to a wrong definition.
Here the class inherits from {\f1\fs20 TInterfacedObject}, but you could use any plain Delphi class: the only condition is that it implements the {\f1\fs20 ICalculator} interface.
:    Set up the Server factory
In order to have a working service, you'll need to initialize a server-side factory, as such:
! Server.ServiceRegister(TServiceCalculator,[TypeInfo(ICalculator)],sicShared);
The {\f1\fs20 Server} instance can be any {\f1\fs20 TSQLRestServer} inherited class, implementing any of the supported protocol of {\i mORMot}'s @35@, embedding a full {\i SQLite3} engine (i.e. a {\f1\fs20 @*TSQLRestServerDB@} class) or a lighter in-memory engine (i.e. a {\f1\fs20 @*TSQLRestServerFullMemory@} class - which is enough for @*hosting@ services with authentication).
The code line above will register the {\f1\fs20 TServiceCalculator} class to implement the {\f1\fs20 ICalculator} service, with a single shared instance life time (specified via the {\f1\fs20 sicShared} parameter). An optional time out value can be specified, in order to automatically release a deprecated instance after some inactivity.
Whenever a service is executed, an implementation class is to be available. The life time of this implementation class is defined on both client and server side, by specifying a {\f1\fs20 TServiceInstanceImplementation} value. This setting must be the same on both client and server sides (it will be checked by the framework).
:    Instances life time implementation
The available instance management options are the following:
|%24%76
|\b Lifetime|Description\b0
|{\f1\fs20 sicSingle}|One class instance is created per call:\line - This is the most expensive way of implementing the service, but is safe for simple workflows (like a one-type call);\line - This is the default setting for {\f1\fs20 TSQLRestServer.ServiceRegister} method.
|{\f1\fs20 sicShared}|One object instance is used for all incoming calls and is not recycled subsequent to the calls - the implementation should be thread-safe on the server side
|{\f1\fs20 sicClientDriven}|One object instance will be created in synchronization with the client-side lifetime of the corresponding interface: when the interface will be released on client (either when it comes out of scope or set to {\f1\fs20 nil}), it will be released on the server side - a numerical identifier will be transmitted with all JSON requests
|{\f1\fs20 sicPerSession}|One object instance will be maintained during the whole running @*session@
|{\f1\fs20 sicPerUser}|One object instance will be maintained and associated with the running user
|{\f1\fs20 sicPerGroup}|One object instance will be maintained and associated with the running user's authorization group
|%
Of course, {\f1\fs20 sicPerSession}, {\f1\fs20 sicPerUser} and {\f1\fs20 sicPerGroup} modes will expect a specific user to be authenticated. Those implementation patterns will therefore only be available if the RESTful authentication is enabled between client and server.
Typical use of each mode may be the following:
|%24%76
|\b Lifetime|Use case\b0
|{\f1\fs20 sicSingle}|An asynchronous process (may be resource consuming)
|{\f1\fs20 sicShared}|Either a very simple process, or requiring some global data
|{\f1\fs20 sicClientDriven}|The best candidate to implement a Business Logic workflow
|{\f1\fs20 sicPerSession}|To maintain some data specific to the client application
|{\f1\fs20 sicPerUser}|Access to some data specific to one user
|{\f1\fs20 sicPerGroup}|Access to some data shared by a user category (e.g. administrator, or guests)
|%
In the current implementation of the framework, the class instance is allocated in memory.
This has two consequences:
- In client-server architecture, it is very likely that a lot of such instances will be created. It is therefore mandatory that it won't consume a lot of resource, especially with long-term life time: e.g. you should not store any BLOB within these instances, but try to restrict the memory use to the minimum. For a more consuming operation (a process which may need memory and CPU power), the {\f1\fs20 sicSingle} mode is preferred.
- There is no built-in data durability yet: service implementation shall ensure that data remaining in memory (e.g. in {\f1\fs20 sicShared}, {\f1\fs20 sicPerUser} or {\f1\fs20 sicPerGroup} mode) won't be missing in case of server shutdown. It is up to the class to persist the needed data - using e.g. @3@.
In order to illustrate {\f1\fs20 sicClientDriven} implementation mode, let's introduce the following interface and its implementation (extracted from the supplied regression tests of the framework):
!type
!  IComplexNumber = interface(IInvokable)
!    ['{29D753B2-E7EF-41B3-B7C3-827FEB082DC1}']
!    procedure Assign(aReal, aImaginary: double);
!    function GetImaginary: double;
!    function GetReal: double;
!    procedure SetImaginary(const Value: double);
!    procedure SetReal(const Value: double);
!    procedure Add(aReal, aImaginary: double);
!    property Real: double read GetReal write SetReal;
!    property Imaginary: double read GetImaginary write SetImaginary;
!  end;
Purpose of this interface is to store a complex number within its internal fields, then retrieve their values, and define a "{\f1\fs20 Add}" method, to perform an addition operation. We used properties, with associated getter and setter methods, to provide object-like behavior on {\f1\fs20 Real} and {\f1\fs20 Imaginary} fields, in the code.
This interface is implemented on the server side by the following class:
!type
!  TServiceComplexNumber = class(TInterfacedObject,IComplexNumber)
!  private
!    fReal: double;
!    fImaginary: double;
!    function GetImaginary: double;
!    function GetReal: double;
!    procedure SetImaginary(const Value: double);
!    procedure SetReal(const Value: double);
!  public
!    procedure Assign(aReal, aImaginary: double);
!    procedure Add(aReal, aImaginary: double);
!    property Real: double read GetReal write SetReal;
!    property Imaginary: double read GetImaginary write SetImaginary;
!  end;
!
!{ TServiceComplexNumber }
!
!procedure TServiceComplexNumber.Add(aReal, aImaginary: double);
!begin
!  fReal := fReal+aReal;
!  fImaginary := fImaginary+aImaginary;
!end;
!
!procedure TServiceComplexNumber.Assign(aReal, aImaginary: double);
!begin
!  fReal := aReal;
!  fImaginary := aImaginary;
!end;
!
!function TServiceComplexNumber.GetImaginary: double;
!begin
!  result := fImaginary;
!end;
!
!function TServiceComplexNumber.GetReal: double;
!begin
!  result := fReal;
!end;
!
!procedure TServiceComplexNumber.SetImaginary(const Value: double);
!begin
!  fImaginary := Value;
!end;
!
!procedure TServiceComplexNumber.SetReal(const Value: double);
!begin
!  fReal := Value;
!end;
This interface is registered on the server side as such:
! Server.ServiceRegister(TServiceComplexNumber,[TypeInfo(IComplexNumber)],sicClientDriven);
Using the {\f1\fs20 sicClientDriven} mode, also the client side will be able to have its own life time handled as expected. That is, both {\f1\fs20 fReal} and {\f1\fs20 fImaginary} field will remain allocated on the server side as long as needed.
When any service is executed, a global {\f1\fs20 threadvar} named {\f1\fs20 ServiceContext} can be accessed to retrieve the currently running context on the server side. You will have access to the following information, which could be useful for {\f1\fs20 sicPerSession, sicPerUser} and {\f1\fs20 sicPerGroup} instance life time modes:
!  TServiceRunningContext = record
!    /// the currently running service factory
!    // - it can be used within server-side implementation to retrieve the
!    // associated TSQLRestServer instance
!    Factory: TServiceFactoryServer;
!    /// the currently runnning session identifier which launched the method
!    // - make available the current session or authentication parameters
!    // (including e.g. user details via Factory.RestServer.SessionGetUser)
!    Session: ^TSQLRestServerSessionContext;
!  end;
When used, a local copy or a {\f1\fs20 PServiceRunningContext} pointer should better be created, since accessing a {\f1\fs20 threadvar} has a non negligible performance cost.
:    Using services on the Server side
Once the service is registered on the server side, it is very easy to use it in your code.
In a complex @17@, it is not a good practice to have services calling each other. Code decoupling is a key to maintainability here. But in some cases, you'll have to consume services on the server side, especially if your software architecture has several layers (like in a @54@): your application services could be decoupled, but the @*Domain-Driven@ services (those implementing the business model) could be on another Client-Server level, with a dedicated protocol, and could have nested calls.
In this case, according to the @47@, you'd better rely on abstraction in your code, i.e. not call the service implementation, but the service abstract interface. You can use the following method of your {\f1\fs20 TSQLRest.Services} instance (note that this method is available on both client and server sides, so is the right access point to all services):
! function TServiceFactory.Get(out Obj): Boolean;
You have several methods to retrieve a {\f1\fs20 TServiceFactory} instance, either from the service name, its GUID, or its index in the list.
That is, you may code:
!var I: ICalculator;
!begin
!  if Server.Services['Calculator'].Get(I)) then
!    result := I.Add(10,20);
!end;
or, for a more complex service:
!var CN: IComplexNumber;
!begin
!  if not Server.Services.Info(TypeInfo(IComplexNumber)).Get(CN) then
!    exit; // IComplexNumber interface not found
!  CN.Real := 0.01;
!  CN.Imaginary := 3.1415;
!  CN.Add(100,200);
!  assert(SameValue(CN.Real,100.01));
!  assert(SameValue(CN.Imaginary,203.1415));
!end; // here CN will be released
You can of course cache your {\f1\fs20 TServiceFactory} instance within a local field, if you wish.
:   Client side
There is no implementation at all on the client side. This is the magic of {\i mORMot}'s services: no Wizard to call (as in {\i DataSnap}), nor client-side methods to write - as with our @49@.
In fact, a hidden "fake" {\f1\fs20 TInterfaceObject} class will be created by the framework (including its internal {\i VTable} and low-level assembler code), and used to interact with the remote server. But you do not have to worry about this process: it is transparent to your code.
:    Set up the Client factory
On the client side, you have to register the corresponding interface, as such:
! Client.ServiceRegister([TypeInfo(ICalculator)],sicShared);
It is very close to the Server-side registration, despite the fact that we do not provide any implementation class here. Implementation will remain on the server side.
Note that the implementation mode (here {\f1\fs20 sicShared}) shall match the one used on the server side. An error will occur if this setting is not coherent.
The other interface we talked about, i.e. {\f1\fs20 IComplexNumber}, is registered as such for the client:
! Client.ServiceRegister([TypeInfo(IComplexNumber)],sicClientDriven);
This will create the corresponding {\f1\fs20 TServiceFactoryClient} instance, ready to serve fake implementation classes to the client process.
To be more precise, this registration step is indeed not mandatory on the client side. If you use the {\f1\fs20 TServiceContainerClient.Info()} method, the client-side implementation will auto-register the supplied interface, in {\f1\fs20 sicClientDriven} implementation mode.
:    Using services on the Client side
Once the service is registered on the client side, it is very easy to use it in your code.
You can use the same methods as on the server side to retrieve a {\f1\fs20 TServiceFactory} instance.
That is, you may code:
!var I: ICalculator;
!begin
!  if Client.Services['Calculator'].Get(I)) then
!    result := I.Add(10,20);
!end;
or, for a more complex service, initialized in {\f1\fs20 sicClientDriven}:
!var CN: IComplexNumber;
!begin
!  if not Client.Services.Info(TypeInfo(IComplexNumber)).Get(CN) then
!    exit; // IComplexNumber interface not found
!  CN.Real := 0.01;
!  CN.Imaginary := 3.1415;
!  CN.Add(100,200);
!  assert(SameValue(CN.Real,100.01));
!  assert(SameValue(CN.Imaginary,203.1415));
!end; // here CN will be released on both client AND SERVER sides
You can of course cache your {\f1\fs20 TServiceFactory} instance within a local field, if you wish.
The code is just the same as on the server. The only functional change is that the execution will take place on the server side (using the registered {\f1\fs20 TServiceComplexNumber} implementation class), and the corresponding class instance will remain active until the {\f1\fs20 CN} local interface will be released on the client.
As we stated in the previous paragraph, since the {\f1\fs20 IComplexNumber} is to be executed as {\f1\fs20 sicClientDriven}, it is not mandatory to call the {\f1\fs20 Client.ServiceRegister} method for this interface. In fact, during {\f1\fs20 Client.Services.Info(TypeInfo(IComplexNumber))} method execution, the registration will take place, if it has not been done explicitly before. For code readability, it may be a good idea to explicitly register the interface on the client side also, just to emphasize that this interface is about to be used, and in which mode.
:   Sample code
You can find in the "{\f1\fs20 SQLite3/Samples/14 - Interface based services}" folder of the supplied source code distribution, a dedicated sample about this feature.
Purpose of this code is to show how to create a client-server service, using interfaces, over named pipe communication.
:    The shared contract
First, you'll find a common unit, shared by both client and server applications:
!unit Project14Interface;
!
!interface
!
!type
!  ICalculator = interface(IInvokable)
!    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
!    function Add(n1,n2: integer): integer;
!  end;
!
!const
!  ROOT_NAME = 'service';
!  APPLICATION_NAME = 'RestService';
!
!implementation
!
!end.
Unique purpose of this unit is to define the service {\f1\fs20 interface}, and the {\f1\fs20 ROOT_NAME} used for the ORM Model (and therefore RESTful URI scheme), and the {\f1\fs20 APPLICATION_NAME} used for named-pipe communication.
:    The server sample application
The server is implemented as such:
!program Project14Server;
!
!{$APPTYPE CONSOLE}
!
!uses
!  SysUtils,
!  SQLite3Commons,
!  SQLite3,
!  Project14Interface;
!
!type
!  TServiceCalculator = class(TInterfacedObject, ICalculator)
!  public
!    function Add(n1,n2: integer): integer;
!  end;
!
!function TServiceCalculator.Add(n1, n2: integer): integer;
!begin
!  result := n1+n2;
!end;
!
!var
!  aModel: TSQLModel;
!begin
!  aModel := TSQLModel.Create([],ROOT_NAME);
!  try
!    with TSQLRestServerDB.Create(aModel,ChangeFileExt(paramstr(0),'.db'),true) do
!    try
!      CreateMissingTables; // we need AuthGroup and AuthUser tables
!!      ServiceRegister(TServiceCalculator,[TypeInfo(ICalculator)],sicShared);
!      if ExportServerNamedPipe(APPLICATION_NAME) then
!        writeln('Background server is running.'#10) else
!        writeln('Error launching the server'#10);
!      write('Press [Enter] to close the server.');
!      readln;
!    finally
!      Free;
!    end;
!  finally
!    aModel.Free;
!  end;
!end.
It will instantiate a {\f1\fs20 @*TSQLRestServerDB@} class, containing a {\i SQLite3} database engine. In fact, since we need authentication, both {\f1\fs20 AuthGroup} and {\f1\fs20 AuthUser} tables are expected to be available.
Then a call to {\f1\fs20 ServiceRegister()} will define the {\f1\fs20 ICalculator} contract, and the {\f1\fs20 TServiceCalculator} class to be used as its implementation. The {\f1\fs20 sicShared} mode is used, since the same implementation class can be shared during all calls (there is no shared nor private data to take care).
Note that since the database expectations of this server are basic (only CRUD commands are needed to handle authentication tables), we may use a {\f1\fs20 @*TSQLRestServerFullMemory@} class instead of {\f1\fs20 TSQLRestServerDB}. This is what is the purpose of the {\f1\fs20 Project14ServerInMemory.dpr} sample:
!program Project14ServerInMemory;
!  (...)
!!    with TSQLRestServerFullMemory.Create(aModel,'test.json',false,true) do
!    try
!!      ServiceRegister(TServiceCalculator,[TypeInfo(ICalculator)],sicShared);
!      if ExportServerNamedPipe(APPLICATION_NAME) then
!  (...)
Using this class will include the {\f1\fs20 CreateMissingTables} call to create both {\f1\fs20 AuthGroup} and {\f1\fs20 AuthUser} tables needed for authentication. But the resulting executable will be lighter: only 200 KB when compiled with Delphi 7 and our LVCL classes, for a full service provider.
:    The client sample application
The client is just a simple form with two {\f1\fs20 TEdit} fields ({\f1\fs20 edtA} and {\f1\fs20 edtB}), and a "{\i Call}" button, which {\f1\fs20 OnClick} event is implemented as:
!procedure TForm1.btnCallClick(Sender: TObject);
!var a,b: integer;
!    err: integer;
!    I: ICalculator;
!begin
!  val(edtA.Text,a,err);
!  if err<>0 then begin
!    edtA.SetFocus;
!    exit;
!  end;
!  val(edtB.Text,b,err);
!  if err<>0 then begin
!    edtB.SetFocus;
!    exit;
!  end;
!  if Client=nil then begin
!    if Model=nil then
!!      Model := TSQLModel.Create([],ROOT_NAME);
!!    Client := TSQLRestClientURINamedPipe.Create(Model,APPLICATION_NAME);
!!    Client.SetUser('User','synopse');
!!    Client.ServiceRegister([TypeInfo(ICalculator)],sicShared);
!  end;
!!  if Client.Services['Calculator'].Get(I) then
!!    lblResult.Caption := IntToStr(I.Add(a,b));
!end; // here local I will be released
The client code is initialized as such:
- A {\f1\fs20 TSQLRestClientURINamedPipe} instance is created, with an associate {\f1\fs20 TSQLModel} and the given {\f1\fs20 APPLICATION_NAME} to access the proper server via a named pipe communication;
- The connection is authenticated with the default {\f1\fs20 'User'} rights;
- The {\f1\fs20 ICalculator} interface is defined in the client's internal factory, in {\f1\fs20 sicShared} mode (just as in the server).
Once the client is up and ready, the local {\f1\fs20 I: ICalculator} variable instance is retrieved, and the remote service is called directly via a simple {\f1\fs20 I.Add(a,b)} statement.
You can imagine how easy and safe it will be to implement a @17@ for your future applications, using {\i mORMot}.
:   Implementation details
:    Involved classes
You will find out in {\f1\fs20 SQLite3Commons.pas} all classes implementing this interface communication.
\graph HierTServiceContainerServer Services implementation classes hierarchy
\TServiceFactoryServer\TServiceFactory
\TServiceFactoryClient\TServiceFactory
\TServiceContainerClient\TServiceContainer
\TServiceContainerServer\TServiceContainer
\
There are two levels of implementation:
- A {\i services catalog}, available in {\f1\fs20 TSQLRest.Services} property, declared as {\f1\fs20 TServiceContainer} (with two {\f1\fs20 inherited} versions, one for each side);
- A {\i service factory} for each interface, declared as {\f1\fs20 TServiceFactory} (also with two {\f1\fs20 inherited} versions, one for each side).
In fact, {\f1\fs20 TServiceFactory.Create constructor} will retrieve all needed RTTI information of the given interface, i.e. GUID, name and all methods (with their arguments). It will compute the low-level stack memory layout needed at execution to emulate a call of a native Delphi {\f1\fs20 interface}. And the corresponding "contract" will be computed from the signature of all interfaces and methods, to validate that both client and server expect the exact same content.
On the server side, {\f1\fs20 TServiceFactoryServer.ExecuteMethod} method (and then a nested {\f1\fs20 TServiceMethod.InternalExecute} call) is used to prepare a valid call to the implementation class code from a remote JSON request.
On the client side, a {\f1\fs20 TInterfacedObjectFake} class will be created, and will emulate a regular Delphi interface call using some on-the-fly asm code generated in the {\f1\fs20 TServiceFactoryClient.Create} constructor. For technical information about how interfaces are called in {\i Delphi}, see @http://sergworks.wordpress.com/2010/07/06/delphi-interfaces-on-binary-level and the {\f1\fs20 FakeCall} method implementation.
Here is the core of this client-side implementation of the "call stubs":
!  for i := 0 to fMethodsCount-1 do begin
!    fFakeVTable[i+RESERVED_VTABLE_SLOTS] := P;
!    P^ := $68ec8b55; inc(P);                 // push ebp; mov ebp,esp
!    P^ := i; inc(P);                         // push {MethodIndex}
!    P^ := $e2895251; inc(P);                 // push ecx; push edx; mov edx,esp
!    PByte(P)^ := $e8; inc(PByte(P));         // call FakeCall
!    P^ := PtrUInt(@TInterfacedObjectFake.FakeCall)-PtrUInt(P)-4; inc(P);
!    P^ := $c25dec89; inc(P);                 // mov esp,ebp; pop ebp
!    P^ := fMethods[i].ArgsSizeInStack or $900000;  // ret {StackSize}; nop
!    inc(PByte(P),3);
!  end;
Just for fun... I could not resist posting this code here; if you are curious, take a look at the "official" {\f1\fs20 RTTI.pas} or {\f1\fs20 RIO.pas} units as provided by Embarcadero, and you will probably find out that the {\i mORMot} implementation is much easier to follow, and also faster (it does not recreate all the stubs or virtual tables at each call, for instance). :)
:    Security
As stated above, in the features grid, a complete @*security@ pattern is available when using client-server services. In a @17@, securing messages between clients and services is essential to protecting data.
Security is implemented at several levels:
- For communication stream - e.g. when using HTTPS protocol at the @35@, or a custom cypher within HTTP content-encoding;
- At RESTful / URI @*authentication@ level - see @18@; introducing {\i Group} and {\i User} notions;
- At {\f1\fs20 interface} or method (service/operation) level - we'll discuss this part now.
By default, all services and operations (i.e. all interfaces and methods) are allowed to execution.
Then, on the server side (it's an implementation detail), the {\f1\fs20 TServiceFactoryServer} instance (available from {\f1\fs20 TSQLRestServer.Services} property) provides the following methods to change the security policy for each {\f1\fs20 interface}:
- {\f1\fs20 AllowAll()} and {\f1\fs20 Allow()} to enable methods execution globally;
- {\f1\fs20 DenyAll()} and {\f1\fs20 Deny()} to disable methods execution globally;
- {\f1\fs20 AllowAllByID()} and {\f1\fs20 AllowByID()} to enable methods execution by Group IDs;
- {\f1\fs20 DenyAllByID()} and {\f1\fs20 DenyByID()} to disable methods execution by Group IDs;
- {\f1\fs20 AllowAllByName()} and {\f1\fs20 AllowByName()} to enable methods execution by Group names;
- {\f1\fs20 DenyAllByName()} and {\f1\fs20 DenyByName()} to disable methods execution by Group names.
The first four methods will affect everybody. The next {\f1\fs20 *ByID()} four methods accept a list of {\i authentication Group} IDs (i.e. {\f1\fs20 TSQLAuthGroup.ID} values), where as the {\f1\fs20 *ByName()} methods will handle {\f1\fs20 TSQLAuthGroup.Ident} property values.
In fact, the execution can be authorized for a particular group of authenticated users. Your service can therefore provide some basic features, and then enables advanced features for administrators or supervisors only. Since the User / Group policy is fully customizable in our RESTful authentication scheme - see @18@, {\i mORMot} provides a versatile and inter-operable security pattern.
Here is some extract of the supplied regression tests:
! (...)
!!  S := fClient.Server.Services['Calculator'] as TServiceFactoryServer;
!  Test([1,2,3,4,5],'by default, all methods are allowed');
!!  S.AllowAll;
!  Test([1,2,3,4,5],'AllowAll should change nothing');
!  S.DenyAll;
!  Test([],'DenyAll will reset all settings');
!  S.AllowAll;
!  Test([1,2,3,4,5],'back to full acccess for everybody');
!  S.DenyAllByID([GroupID]);
!  Test([],'our current user shall be denied');
!  S.AllowAll;
!  Test([1,2,3,4,5],'restore allowed for everybody');
!  S.DenyAllByID([GroupID+1]);
!  Test([1,2,3,4,5],'this group ID won''t affect the current user');
!  S.DenyByID(['Add'],GroupID);
!  Test([2,3,4,5],'exclude a specific method for the current user');
!  S.DenyByID(['totext'],GroupID);
!  Test([2,3,5],'exclude another method for the current user');
! (...)
The {\f1\fs20 Test()} procedure is used to validate the corresponding methods of {\f1\fs20 ICalculator} (1={\i Add}, 2={\i Multiply}, 3={\i Subtract}, 4={\i ToText}...).
In this above code, the {\f1\fs20 GroupID} value is retrieved as such:
!  GroupID := fClient.MainFieldID(TSQLAuthGroup,'User');
And the current authenticated user is member of the {\f1\fs20 'User'} group:
!  fClient.SetUser('User','synopse'); // default user for Security tests
Since {\f1\fs20 TSQLRestServer.ServiceRegister} method returns the first created {\f1\fs20 TServiceFactoryServer} instance, and since all {\f1\fs20 Allow* / AllowAll* / Deny* / DenyAll*} methods return also a {\f1\fs20 TServiceFactoryServer} instance, you can use some kind of "fluent interface" in your code to set the security policy, as such:
!  Server.ServiceRegister(TServiceCalculator,[TypeInfo(ICalculator)],sicShared).
!    DenyAll.AllowAllByName(['Supervisor']);
This will allow access to the {\f1\fs20 ICalculator} methods only for the {\i Supervisor} group of users.
:    Transmission content
All data is transmitted as @*JSON@ arrays or objects, according to the requested URI.
We'll discuss how data is expected to be transmitted, at the application level.
:     Request format
As stated above, there are two mode of routing, defined by {\f1\fs20 TServiceRoutingMode}. The routing to be used is defined globally in the {\f1\fs20 TSQLRest.Routing} property.
|%20%40%40
||\b {\f1\fs20 rmREST}|{\f1\fs20 rmJSON_RPC}\b0
|Description|URI-based layout|JSON-RPC mode
|Default|Yes|No
|URI scheme|/Model/Interface.Method[/ClientDrivenID]|/Model/Interface
|Body content|JSON array of parameters|{\f1\fs20 \{"method":"{\i MethodName}","params":[...][,"id":{\i ClientDrivenID}]\}}
|Security|RESTful @*authentication@ for each method|RESTful authentication for the whole service (interface)
|Speed|10% faster|10% slower
|%
In the default {\f1\fs20 rmREST} mode, both service and operation (i.e. interface and method) are identified within the URI. And the message body is a standard JSON array of the supplied parameters (i.e. all {\f1\fs20 const} and {\f1\fs20 var} parameters).
Here is a typical request for {\f1\fs20 ICalculator.Add}:
$ POST /root/Calculator.Add
$ (...)
$ [1,2]
Here we use a {\f1\fs20 POST} verb, but the framework will also allow {\f1\fs20 GET}, if needed (e.g. from a ). The pure Delphi client implementation will use only {\f1\fs20 POST}.
For a {\f1\fs20 sicClientDriven} mode service, the needed instance ID is appended to the URI:
$ POST /root/ComplexNumber.Add/1234
$ (...)
$ [20,30]
Here, {\f1\fs20 1234} is the identifier of the server-side instance ID, which is used to track the instance life-time, in {\f1\fs20 sicClientDriven} mode.
One benefit of using URI is that it will be more secure in our RESTful authentication scheme - see @18@: each method (and even any client driven session ID) will be signed properly.
In this {\f1\fs20 rmREST} mode, the server is also able to retrieve the parameters from the URI, if the message body is left void. This is not used from a Delphi client (since it will be more complex and therefore slower), but it can be used for a client, if needed:
$ POST root/Calculator.Add?+%5B+1%2C2+%5D
In the above line, {\f1\fs20 +%5B+1%2C2+%5D} will be decoded as {\f1\fs20 [1,2]} on the server side. In conjunction with the use of a {\f1\fs20 GET} verb, it may be more suitable for a remote @*AJAX@ connection.
If {\f1\fs20 rmJSON_RPC} mode is used, the URI will define the interface, and then the method name will be inlined with parameters, e.g.
$ POST /root/Calculator
$ (...)
$ {"method":"Add","params":[1,2],"id":0}
Here, the {\f1\fs20 "id"} field can be not set (and even not existing), since it has no purpose in {\f1\fs20 sicShared} mode.
For a {\f1\fs20 sicClientDriven} mode service:
$ POST /root/ComplexNumber
$ (...)
$ {"method":"Add","params":[20,30],"id":1234}
This mode will be a little bit slower, but will probably be more AJAX ready.
It's up to you to select the right routing scheme to be used.
:     Response format
:      Standard answer as JSON object
The framework will always return the data in the same format, whatever the routing mode used.
Basically, this is a JSON object, with one nested {\f1\fs20 "result":} property, and the client driven {\f1\fs20 "id":} value (e.g. always 0 in {\f1\fs20 sicShared} mode):
$ POST /root/Calculator.Add
$ (...)
$ [1,2]
will be answered as such:
$ {"result":[3],"id":0}
The {\i result} array contains all {\f1\fs20 var} and {\f1\fs20 out} parameters values (in their declaration order), and then the method main result.
For instance, here is a transmission stream for a {\f1\fs20 ICalculator.ComplexCall} request in {\f1\fs20 rmREST} mode:
$ POST root/Calculator.ComplexCall
$ (...)
$ [[288722014,1231886296], ["one","two","three"], ["ABC","DEF","GHIJK"], "ïƒ¿BgAAAAAAAAAAAAAAAAAAACNEOlxEZXZcbGliXFNRTGl0ZTNcZXhlXFRlc3RTUUwzLmV4ZQ==", "ïƒ¿Xow1EdkXbUkDYWJj"]
will be answered as such:
$ '{"result":[ ["ABC","DEF","GHIJK","one,two,three"], "ïƒ¿X4w1EdgXbUkUMjg4NzIyMDE0LDEyMzE4ODYyOTY=", "ïƒ¿Xow1EdkXbUkjRDpcRGV2XGxpYlxTUUxpdGUzXGV4ZVxUZXN0U1FMMy5leGU="], "id":0}'
It matches the {\f1\fs20 var / const / out} parameters declaration of the method:
! function ComplexCall(const Ints: TIntegerDynArray; Strs1: TRawUTF8DynArray;
!   var Str2: TWideStringDynArray; const Rec1: TVirtualTableModuleProperties;
!   var Rec2: TSQLRestCacheEntryValue): TSQLRestCacheEntryValue;
And its implementation:
!function TServiceCalculator.ComplexCall(const Ints: TIntegerDynArray;
!  Strs1: TRawUTF8DynArray; var Str2: TWideStringDynArray; const Rec1: TVirtualTableModuleProperties;
!  var Rec2: TSQLRestCacheEntryValue): TSQLRestCacheEntryValue;
!var i: integer;
!begin
!  result := Rec2;
!  result.JSON := StringToUTF8(Rec1.FileExtension);
!  i := length(Str2);
!  SetLength(Str2,i+1);
!  Str2[i] := UTF8ToWideString(RawUTF8ArrayToCSV(Strs1));
!  inc(Rec2.ID);
!  dec(Rec2.TimeStamp);
!  Rec2.JSON := IntegerDynArrayToCSV(Ints,length(Ints));
!end;
Note that {\f1\fs20 TIntegerDynArray}, {\f1\fs20 TRawUTF8DynArray} and {\f1\fs20 TWideStringDynArray} values were marshaled as JSON arrays, whereas complex records (like {\f1\fs20 TSQLRestCacheEntryValue}) have been Base-64 encoded.
The framework is able to handle class instances as parameters, for instance with the following interface, using a {\f1\fs20 TPersistent} child class with published properties (it would be the same for {\f1\fs20 @*TSQLRecord@} @*ORM@ instances):
!type
!  TComplexNumber = class(TPersistent)
!  private
!    fReal: Double;
!    fImaginary: Double;
!  public
!    constructor Create(aReal, aImaginary: double); reintroduce;
!  published
!    property Real: Double read fReal write fReal;
!    property Imaginary: Double read fImaginary write fImaginary;
!  end;
!
!  IComplexCalculator = interface(ICalculator)
!    ['{8D0F3839-056B-4488-A616-986CF8D4DEB7}']
!    /// purpose of this unique method is to substract two complex numbers
!    // - using class instances as parameters
!    procedure Substract(n1,n2: TComplexNumber; out Result: TComplexNumber);
!  end;
As stated above, it is not possible to return a class as a result of a {\f1\fs20 function} (who will be responsible of handling its life-time?). So in this method declaration, the result is declared as {\f1\fs20 out} parameter.
During the transmission, published properties of {\f1\fs20 TComplexNumber} parameters will be serialized as standard JSON objects:
$ POST root/ComplexCalculator.Substract
$ (...)
$ [{"Real":2,"Imaginary":3},{"Real":20,"Imaginary":30}]
will be answered as such:
$ {"result":[{"Real":-18,"Imaginary":-27}],"id":0}
Those content have perfectly standard JSON declarations, so can be generated and consumed directly in any @*AJAX@ client.
In case of an error, the standard message object will be returned:
${
$ "ErrorCode":400,
$ "ErrorText":"Error description"
$}
The following error descriptions may be returned by the service implementation from the server side:
|%35%65
|\b {\f1\fs20 ErrorText}|Description\b0
|Method name required|{\f1\fs20 rmJSON_RPC} call without {\f1\fs20 "method":} field
|Unknown method|{\f1\fs20 rmJSON_RPC} call with invalid method name (in {\f1\fs20 rmRest} mode, there is no specific message, since it may be a valid request)
|Parameters required|The server expect at least a void JSON array (aka {\f1\fs20 []}) as parameters
|Unauthorized method|This method is not allowed with the current authenticated user group - see {\i @*Security@} above
|... instance id:? not found or deprecated|The supplied {\f1\fs20 "id":} parameter points to a wrong instance (in {\f1\fs20 sicPerSession / sicPerUser / sicPerGroup} mode)
|ExceptionClass: Exception Message (with {\f1\fs20 500 Internal Server Error})|An exception was raised during method execution
|%
:      Custom returned content
Note that even if the response format is a JSON object by default, and expected as such by our {\f1\fs20 TServiceContainerClient} implementation, there is a way of returning any content from a remote request. It may be used by @*AJAX@ or HTML applications to return any kind of data, i.e. not only JSON results, but pure text, HTML or even binary content. Our {\f1\fs20 TServiceFactoryClient} instance is also able to handle such requests, and will save client-server bandwidth when transmitting some BLOB data (since it won't serialized the content with {\f1\fs20 Base64} encoding).
In order to specify a custom format, you can use the following {\f1\fs20 @*TServiceCustomAnswer@ record} type as the {\f1\fs20 result} of an {\f1\fs20 interface function}:
!  TServiceCustomAnswer = record
!    Header: RawUTF8;
!    Content: RawByteString;
!  end;
The {\f1\fs20 Header} field shall be not null (i.e. not equal to ''), and contains the expected content type header (e.g. {\f1\fs20 TEXT_CONTENT_TYPE_HEADER} or {\f1\fs20 HTML_CONTENT_TYPE_HEADER}). Then the {\f1\fs20 Content} value will be transmitted back directly to the client, with no JSON @*serialization@. Of course, no {\f1\fs20 var} nor {\f1\fs20 out} parameter will be transmitted either (since there is no JSON result array any more).
In order to implement such method, you may define such an interface:
!  IComplexCalculator = interface(ICalculator)
!    ['{8D0F3839-056B-4488-A616-986CF8D4DEB7}']
!    function TestBlob(n: TComplexNumber): TServiceCustomAnswer;
!  end;
Which may be implemented for instance as such:
!function TServiceComplexCalculator.TestBlob(n: TComplexNumber): TServiceCustomAnswer;
!begin
!  Result.Header := TEXT_CONTENT_TYPE_HEADER;
!  Result.Content := FormatUTF8('%,%',[n.Real,n.Imaginary]);
!end;
This will return not a JSON object, but a plain TEXT content.
Regression tests will make the following process:
!  with CC.TestBlob(C3) do begin
!    Check(Header=TEXT_CONTENT_TYPE_HEADER);
!    Check(Content=FormatUTF8('%,%',[C3.Real,C3.Imaginary]));
!  end;
Note that since there is only one BLOB content returned, no {\f1\fs20 var} nor {\f1\fs20 out} parameters are allowed to be defined for this method. If this is the case, an exception will be raised during the {\f1\fs20 interface} registration step. But you can define any {\f1\fs20 const} parameter needed, to specify your request.
You may also be able to use this feature to implement custom UTF-8 HTML creation, setting the {\f1\fs20 Header} value to {\f1\fs20 HTML_CONTENT_TYPE_HEADER} constant, in conjunction with {\f1\fs20 rmREST} mode and URI-encoded parameters.
:   Hosting services
About @**hosting@, the easiest is to have your main {\f1\fs20 TSQLRestServer} class handling the service, in conjunction with other Client-Server process (like ORM). See @%%mORMotDesign1@ about this generic Client-Server architecture.
But you may find out some (good?) reasons which main induce another design:
- For better scalability, you should want to use a dedicated process (or even dedicated hardware) to split the database and the service process;
- For @*security@ reasons, you want to expose only services to your Internet clients, and would like to have a @*DMZ@ hosting only services, and a separate safe database and logic instance;
- Services are not the main part of your business, and you would like to enable or disable on need the exposed services, on demand;
- To implement an efficient solution for the most complex kind of application, as provided by @54@.
- Whatever your IT or managers want {\i mORMot} to.
:    Shared server
This is the easiest configuration: one HTTP server instance, which serves both ORM and Services. On practice, this is perfectly working and scalable.
\graph mORMotServices1 Service Hosting on mORMot - shared server
\Internet (VPN)\Local Network
node [shape=box];
\Local Network\HTTP server
subgraph cluster_0 {
"Client 1\n(Delphi)";
label="PC 1";
}
subgraph cluster_1 {
"Client 2\n(AJAX)";
label="PC 2";
}
subgraph cluster_2 {
\HTTP server\ORM
\HTTP server\Service
label="PC Server";
}
\Client 1¤(Delphi)\Local Network\JSON + REST¤over HTTP/1.1
\Client 2¤(AJAX)\Internet (VPN)\JSON + REST¤over HTTP/1.1
\
You can tune this solution, as such:
- Setting the group user rights properly - see @18@ - you can disable the remote ORM access from the Internet, for the AJAX Clients - but allow rich Delphi clients (like PC1) to access the ORM;
- You can have direct in-process access to the service interfaces from the ORM, and vice-versa: if your services and @*ORM@ are deeply inter-dependent, direct access will be the faster solution.
:    Two servers
In this configuration, two physical servers are available:
- A network DMZ is opened to serve only service content over the Internet, via "HTTP server 2";
- Then on the local network, "HTTP server 1" is used by both PC 1 and Services to access the ORM;
- Both "PC Client 1" and the ORM core are able to connect to Services via a dedicated "HTTP server 3".
\graph mORMotServices2 Service Hosting on mORMot - two servers
"Internet (VPN)";
"Local Network";
node [shape=box];
\Local Network\HTTP server 1
\Internet (VPN)\HTTP server 2
\Services\HTTP server 1
subgraph cluster_0 {
"Client 1\n(Delphi)";
label="PC 1";
}
subgraph cluster_1 {
"Client 2\n(AJAX)";
label="PC 2";
}
subgraph cluster_2 {
\HTTP server 2\Services
\HTTP server 3\Services
label="PC Server DMZ";
}
subgraph cluster_3 {
\HTTP server 1\ORM
\ORM\HTTP server 3
label="PC Server internal";
}
\Local Network\HTTP server 3
\Client 1¤(Delphi)\Local Network\JSON + REST¤over HTTP/1.1
\Client 2¤(AJAX)\Internet (VPN)\JSON + REST¤over HTTP/1.1
\
Of course, the database will be located on "PC Server internal", i.e. the one hosting the ORM, and the Services will be one regular client: so we may use @39@ on purpose to enhance performance. In order to access the remote ORM features, and provide a communication endpoint to the embedded services, a {\f1\fs20 @*TSQLRestServerRemoteDB@} kind of server class can be used.
:    Two instances on the same server
This is the most complex configuration.
In this case, only one physical server is deployed:
- A dedicated "HTTP server 2" instance will serve service content over the Internet (via a DMZ configuration of the associated network card);
- "PC Client 1" will access to the ORM via "HTTP server 1", or to services via "HTTP server 3";
- For performance reasons, since ORM and Services are on the same computer, using named pipes (or even local GDI messages) instead of slower HTTP-TCP/IP is a good idea: in such case, ORM will access services via "Named Pipe server 2", whereas Services will serve their content to the ORM via "Named Pipe server 1".
\graph mORMotServices3 Service Hosting on mORMot - one server, two instances
"Internet (VPN)";
"Local Network";
node [shape=box];
\Local Network\HTTP server 1
\Local Network\HTTP server 3
\Internet (VPN)\HTTP server 2
\Services\Named Pipe server 1
subgraph cluster_0 {
"Client 1\n(Delphi)";
label="PC 1";
}
subgraph cluster_1 {
"Client 2\n(AJAX)";
label="PC 2";
}
subgraph cluster_2 {
\Named Pipe server 1\ORM
\Named Pipe server 2\Services
\ORM\Named Pipe server 2
\HTTP server 2\Services
\HTTP server 3\Services
\HTTP server 1\ORM
label="PC Server";
}
\Client 1¤(Delphi)\Local Network\JSON + REST¤over HTTP/1.1
\Client 2¤(AJAX)\Internet (VPN)\JSON + REST¤over HTTP/1.1
\
Of course, you can make any combination of the protocols and servers, to tune hosting for a particular purpose. You can even create several ORM servers or Services servers (grouped per features family or per product), which will cooperate for better scaling and performance.
If you consider implementing a stand-alone application for hosting your services, and has therefore basic ORM needs (e.g. you may need only CRUD statements for handling authentication), you may use the lighter {\f1\fs20 TSQLRestServerFullMemory} kind of server instead of a full {\f1\fs20 TSQLRestServerDB}, which will embed a {\i SQLite3} database engine, perhaps not worth it in this case.
:   Comparison with WCF
Here is a short reference table of @**WCF@ / {\i mORMot} @*SOA@ features and implementation patterns.
|%30%35%35
|\b Feature|WCF|mORMot\b0
|Internal design|@*SOAP@|@*REST@ful
|Hosting|exe/service/ISS/WAS|in-process/exe/service
|Scalability/balancing|up to WAS|by dedicated hosting
|MetaData|WSDL|none (only code)
|Data contract|class|class/record
|ORM integration|separated|integrated in the model
|Service contract|{\f1\fs20 interface} + attributes|{\f1\fs20 interface} + shared Model
|Versioning|XML name-space|{\f1\fs20 interface} signature
|Message protocol|SOAP/custom|RESTful
|Messaging|single/duplex|stateless (REST)
|Sequence|attributes on methods|{\f1\fs20 interface} life time
|Transactional|fully transactional|on implementation side
|Instance life time|per call/per session/single|per call/per session/per user/ per group/single
|Configuration|{\f1\fs20 .config} file or code|code
|Operation|synchronous/asynchronous|synchronous (REST)
|Session|available (optional)|available (optional)
|Encryption|at Service level|at communication level
|Compression|at Service level|at communication level
|Serialization|XML/binary/JSON|JSON (customizable)
|Communication protocol|HTTP/HTTPS/TCP/pipe/MSMQ|HTTP/HTTPS/TCP/pipe/GDI/in-process
|HTTP/HTTPS server|{\f1\fs20 http.sys}|{\f1\fs20 http.sys}/native (winsock)
|Security|Attribute driven|User group driven
|Weight|Middle|Low
|Speed|Good|High
|Extensibility|verbose but complete|customizable
|Standard|De facto|KISS design (e.g. JSON, HTTP)
|Source code|Closed|Published
|License|Proprietary|Open
|Price|Depends|Free
|Support|Official + community|Community
|Runtime required|.Net framework (+ISS/WAS)|None (blank OS)
|%
We may be tempted to say that {\i mORMot} SOA architecture is almost complete, even for a young and {\i Open Source} project. Some features (like {\i per user} or {\i per group} instance life time, or GDI local communication) are even unique to {\i mORMot}.
Of course, WCF features its @**SOAP@-based architecture. But WCF also suffers from it: due to this ground-up message design, it will always endure its SOAP overweight, which is "Simple" only by name, not by reputation.
If you need to communicate with an external service provider, you can easily create a SOAP @*gateway@ from Delphi, as such:
- Import the WSDL (Web Service Definition Language) definition of a web service and turn it into a Delphi import unit;
- Publish the interface as a {\i mORMot} server-side implementation class.
Since SOAP features a lot of requirements, and expects some plumping according to its format (especially when services are provided from C# or Java), we choose to not re-invent the wheel this time, and rely on existing Delphi libraries (available within the Delphi IDE) for this purpose. If you need a cross-platform SOAP 1.1 compatible solution, or if you version of Delphi does not include SOAP process, you may take a look at @http://wiki.freepascal.org/Web_Service_Toolkit which is a web services package for FPC, Lazarus and Delphi.
But for service communication within the {\i mORMot} application domain, the RESTful / JSON approach gives much better performance and ease of use. You do not have to play with WSDL or unit wrappers, just share some {\f1\fs20 interface} definition between clients and servers.
The only missing feature of {\i mORMot} SOA is transactional process, which must be handled on server side, within the service implementation (e.g. with explicit commit or rollback).
An {\i @*Event Sourcing@} design has been added to the {\i mORMot} road map, in order to handle @*transaction@s on the SOA side, relying on ORM for its data persistence, but not depending on database transactional abilities. In fact, transactions should better be implemented at SOA level, as we do want transactions to be database agnostic ({\i SQLite3} has a limited per-connection transactional scheme, and we do not want to rely on the DB layer for this feature). {\i Event Sourcing} sounds to be a nice pattern to implement a strong and efficient transactional process in our framework - see @http://bliki.abdullin.com/event-sourcing/why
\page
:43Security and Testing
: Security
The framework tries to implement @**security@ at several levels:
- @*Atomic@ity of the {\i @*SQLite3@} database - see @60@;
- @15@ architecture to avoid most synchronization issues;
- @13@ associated to the Object pascal @*strong type@ syntax;
- {\i Per-table access right} functionalities built-in at lowest level of the framework;
- Build-in optional @*authentication@ mechanism, implementing both {\i per-user @*session@s} and individual REST {\i @*Query Authentication@} - used e.g. for service tuned execution policy, using the authentication groups.
:  Per-table access rights
A pointer to a {\f1\fs20 TSQLAccessRights} record, and its {\f1\fs20 GET / POST / PUT / DELETE} fields, is sent as parameter to the unique access point of the server class:
!function TSQLRestServer.URI(const url, method, SentData: RawUTF8;
!    out Resp, Head: RawUTF8; RestAccessRights: PSQLAccessRights): Int64Rec;
This will allow checking of access right for all @*CRUD@ operations, according to the table invoked. For instance, if the table {\f1\fs20 TSQLRecordPeople} has 2 as index in {\f1\fs20 TSQLModel.Tables[]}, any incoming {\f1\fs20 POST} command for {\f1\fs20 TSQLRecordPeople} will be allowed only if the 2nd bit in {\f1\fs20 RestAccessRights^.POST} field is set, as such:
!  if MethodUp='POST' then begin
!    if Table=nil then begin
!      (...)
!    end else
!    // here, Table<>nil and TableIndex in [0..MAX_SQLFIELDS-1]
!!    if not (TableIndex in RestAccessRights^.POST) then // check User
!!      result.Lo := 401 else // HTTP Unauthorized
!      (...)
Making access rights a parameter allows this method to be handled as pure stateless, @*thread-safe@ and @*session@-free, from the bottom-most level of the framework.
:19  SQL statements safety
In our RESTful implementation, the POST command with no table associated in the URI allows to execute any SQL statement directly.
This special command should be carefully tested before execution, since SQL misuses could lead into major security issues. A {\f1\fs20 AllowRemoteExecute: boolean} field has therefore been made available in the {\f1\fs20 TSQLAccessRights}  record to avoid such execution on any remote connection, if the SQL statement is not a SELECT, i.e. if it may affect the data content. By default, this field value is left to {\f1\fs20 false} in {\f1\fs20 SUPERVISOR_ACCESS_RIGHTS} constant for security reasons.
In the current implementation, the {\f1\fs20 SUPERVISOR_ACCESS_RIGHTS} constant is transmitted for all handled communication protocols (direct access, GDI messages, named pipe or HTTP).
Only direct access via {\f1\fs20 @*TSQLRestClientDB@} will use {\f1\fs20 FULL_ACCESS_RIGHTS}, i.e. will have {\f1\fs20 AllowRemoteExecute} parameter set to {\f1\fs20 true}.
By default, when no explicit authentication mechanism is enabled in the framework, most commands will be executed, following the {\f1\fs20 SUPERVISOR_ACCESS_RIGHTS} constant. But no remote call will be allowed of SQL statements with no SELECT inside with a generic POST command.
:18  Authentication
:   Principles
How to handle @*authentication@ in a @*REST@ful @*Client-Server@ architecture is a matter of debate.
Commonly, it can be achieved, in the @*SOA@ over @*HTTP@ world via:
- HTTP {\i basic auth} over @*HTTPS@;
- {\i Cookies} and @*session@ management;
- {\i @*Query Authentication@} with additional signature parameters.
We'll have to adapt, or even better mix those techniques, to match our framework architecture at best.
Each authentication scheme has its own PROs and CONs, depending on the purpose of your security policy and software architecture:
|%30%24%24%22
|\b Criteria|HTTPS {\i basic auth}|Cookies+Session|Query Auth.\b0
|Browser integration|Native|Native|Via @*JavaScript@
|User Interaction|Rude|Custom|Custom
|Web Service use\line (rough estimation)|95%|4%|1%
|Session handling|Yes|Yes|No
|Session managed by|Client|Server|N/A
|Password on Server|Yes|Yes/No|N/A
|Truly Stateless|Yes|No|Yes
|Truly RESTful|No|No|Yes
|HTTP-free|No|No|Yes
|%
:   HTTP basic auth over HTTPS
This first solution, based on the standard @**HTTPS@ protocol, is used by most web services. It's easy to implement, available by default on all browsers, but has some known draw-backs, like the awful authentication window displayed on the Browser, which will persist (there is no {\i LogOut}-like feature here), some server-side additional CPU consumption, and the fact that the user-name and password are transmitted (over HTTPS) into the Server (it should be more secure to let the password stay only on the client side, during keyboard entry, and be stored as secure hash on the Server).
The supplied {\f1\fs20 TSQLite3HttpClientWinHTTP} and {\f1\fs20 TSQLite3HttpClientWinINet} clients classes are able to connect using HTTPS, and the {\f1\fs20 THttpApiServer} server class can send compatible content.
:   Session via Cookies
To be honest, a @**session@ managed on the Server is not truly @*Stateless@. One possibility could be to maintain all data within the cookie content. And, by design, the cookie is handled on the Server side (Client in fact don’t even try to interpret this cookie data: it just hands it back to the server on each successive request). But this cookie data is application state data, so the client should manage it, not the server, in a pure Stateless world.
The cookie technique itself is HTTP-linked, so it's not truly RESTful, which should be protocol-independent. Since our framework does not provide only HTTP protocol, but offers other ways of transmission, Cookies were left at the baker's home.
:   Query Authentication
{\i @**Query Authentication@} consists in signing each RESTful request via some additional parameters on the URI. See @http://broadcast.oreilly.com/2009/12/principles-for-standardized-rest-authentication.html about this technique. It was defined as such in this article:
{\i All REST queries must be authenticated by signing the query parameters sorted in lower-case, alphabetical order using the private credential as the signing token. Signing should occur before URI encoding the query string.}
For instance, here is a generic URI sample from the link above:
$ GET /object?apiKey=Qwerty2010
should be transmitted as such:
$ GET /object?timestamp=1261496500&apiKey=Qwerty2010&signature=abcdef0123456789
The string being signed is "{\f1\fs20 /object?apikey=Qwerty2010&timestamp=1261496500}" and the signature is the {\i SHA256} hash of that string using the private component of the API key.
This technique is perhaps the more compatible with a Stateless architecture, and can also been implemented with a light @*session@ management.
Server-side data caching is always available. In our framework, we cache the responses at the SQL level, not at the URI level (thanks to our optimized implementation of {\f1\fs20 GetJSONObjectAsSQL}, the URI to SQL conversion is very fast). So adding this extra parameter doesn't break the cache mechanism.
:  Framework authentication
Even if, theoretically speaking, {\i @*Query Authentication@} sounds to be the better for implementing a truly @*REST@ful architecture, our framework tries to implement a @*Client-Server@ design.
In practice, we may consider two way of using it:
- With no authentication nor user right management (e.g. for local access of data, or framework use over a secured network);
- With per-user authentication and right management via defined {\i security groups}, and a per-query authentication.
According to RESTful principle, handling per-@*session@ data is not to be implemented in such an Architecture. A minimal "session-like" feature was introduced only to handle user authentication with very low overhead on both Client and Server side. The main technique used for our security is therefore {\i Query Authentication}, i.e. a per-URI signature.
If both {\f1\fs20 AuthGroup} and {\f1\fs20 AuthUser} are not available on the Server {\f1\fs20 @*TSQLModel@} (i.e. if the {\f1\fs20 aHandleUserAuthentication} parameter was set to {\f1\fs20 false} for the {\f1\fs20 TSQLRestServer. Create constructor}), no authentication is performed. All tables will be accessible by any client, as stated in @19@. As stated above, for security reasons, the ability to execute {\f1\fs20 INSERT / UPDATE / DELETE} SQL statement via a RESTful {\i POST} command is never allowed by default with remote connections: only {\f1\fs20 SELECT} can be executed via this {\i POST} verb.
On the Server side, a dedicated service, accessible via the {\f1\fs20 ModelRoot/Auth} URI is to be called to register an User, and create a session.
If authentication is enabled for the Client-Server process (i.e. if both {\f1\fs20 AuthGroup} and {\f1\fs20 AuthUser} are available in the Server {\f1\fs20 TSQLModel}, and the {\f1\fs20 aHandleUserAuthentication} parameter was set to {\f1\fs20 true} at the {\f1\fs20 TSQLRestServer} instance construction), the following security features will be added:
- Client {\i should} open a session to access to the Server, and provide a valid {\f1\fs20 UserName / Password} pair (see next paragraph);
- Each @*CRUD@ statement is checked against the authenticated User security group, via the {\f1\fs20 AccessRights} column and its {\f1\fs20 GET / POST / PUT / DELETE} per-table bit sets;
- Thanks to {\i Per-User} authentication, any SQL statement commands may be available via the RESTful {\i POST} verb for an user with its {\f1\fs20 AccessRights} group field containing {\f1\fs20 AllowRemoteExecute=true};
- Each REST request will expect an additional parameter, named {\f1\fs20 session_signature}, to every URL. Using the URI instead of {\i cookies} allows the signature process to work with all communication protocols, not only @*HTTP@.
:   Per-User authentication
On the Server side, two tables, defined by the {\f1\fs20 TSQLAuthGroup} and {\f1\fs20 TSQLAuthUser} classes will handle respectively per-group access rights, and user authentication.
Here is the layout of the {\f1\fs20 AuthGroup} table, as defined by the {\f1\fs20 TSQLAuthGroup} class type:
\graph DBAuthGroup AuthGroup Record Layout
rankdir=LR;
node [shape=Mrecord];
struct1 [label="ID : integer|AccessRights : RawUTF8|Ident : RawUTF8|SessionTimeout : integer"];
\
The {\f1\fs20 AccessRights} column is a textual CSV serialization of the {\f1\fs20 TSQLAccessRights} record content, as expected by the {\f1\fs20 TSQLRestServer.URI} method. Using a CSV serialization, instead of a binary serialization, will allow the change of the {\f1\fs20 MAX_SQLTABLES} constant value.
The {\f1\fs20 AuthUser} table, as defined by the {\f1\fs20 TSQLAuthUser} class type, is defined as such:
\graph DBAuthUser AuthUser Record Layout
rankdir=LR;
node [shape=Mrecord];
struct1 [label="ID : integer|Data : TSQLRawBlob|DisplayName : RawUTF8|<f0>GroupRights : TSQLAuthGroup|LogonName : RawUTF8|PasswordHashHexa : RawUTF8"];
struct2 [label="AuthGroup"];
struct1:f0 -> struct2;
\
Each user has therefore its own associated {\f1\fs20 AuthGroup} table, a name to be entered at login, a name to be displayed on screen or reports, and a SHA-256 hash of its registered password. A custom {\f1\fs20 Data} BLOB field is specified for your own application use, but not accessed by the framework.
By default, the following security groups are created on a void database:
|%20%16%16%16%16%16
|\b AuthGroup|POST SQL|Auth Read|Auth Write|Tables R|Tables W\b0
|Admin|Yes|Yes|Yes|Yes|Yes
|Supervisor|No|Yes|No|Yes|Yes
|User|No|No|No|Yes|Yes
|Guest|No|No|No|Yes|No
|%
Then the corresponding '{\i Admin}', '{\i Supervisor}' and '{\i User}' {\f1\fs20 AuthUser} accounts are created, with the default '{\i synopse}' password.
{\b You MUST override those default '{\i synopse}' passwords for each {\f1\fs20 AuthUser} record to a custom genuine value.}
'{\i Admin}' will be the only group able to execute remote not SELECT SQL statements for POST commands (i.e. to have {\f1\fs20 TSQLAccessRights. AllowRemoteExecute = true}) and modify the {\f1\fs20 Auth*} tables (i.e. {\f1\fs20 AuthUser} and {\f1\fs20 AuthGroup}) content.
Of course, you can change {\f1\fs20 AuthUser} and {\f1\fs20 AuthGroup} table content, to match your security requirements, and application specifications. You can specify a per-table @*CRUD@ access, via the {\f1\fs20 AccessRights} column, as we stated above, speaking about the {\f1\fs20 TSQLAccessRights} record layout.
This will implement both {\i Query Authentication} together with a group-defined {\i per-user right} management.
:   Session handling
A dedicated RESTful service, available from the {\f1\fs20 ModelRoot/Auth} URI, is to be used for user authentication, handling so called @**session@s.
Here are the typical steps to be followed in order to create a new user session:
- Client sends a {\f1\fs20 GET ModelRoot/auth?UserName=...} request to the remote server;
- Server answers with an hexadecimal {\i nonce} contents (valid for about 5 minutes), encoded as JSON result object;
- Client sends a {\f1\fs20 GET ModelRoot/auth?UserName=...&PassWord=...&ClientNonce=...} request to the remote server, in which {\f1\fs20 ClientNonce} is a random value used as Client {\i nonce}, and {\f1\fs20 PassWord} is computed from the log-on and password entered by the User, using both Server and Client {\i nonce} as salt;
- Server checks that the transmitted password is valid, i.e. that its matches the hashed password stored in its database and a time-valid Server {\i nonce} - if the value is not correct, authentication failed;
- On success, Server will create a new in-memory session (sessions are not stored in the database, for lighter and safer process) and returns the session number and a private key to be used during the session (encoded as JSON result object);
- On any further access to the Server, a {\f1\fs20 &session_signature=} parameter is added to the URL, and will be checked against the valid sessions in order to validate the request;
- When the Client is about to close (typically in {\f1\fs20 TSQLRestClientURI. Destroy}), the {\f1\fs20 GET ModelRoot/auth?UserName=...&Session=...} request is sent to the remote server, in order to explicitly close the corresponding session in the server memory (avoiding most {\i re-play} attacks);
- Each opened session has an internal {\i TimeOut} parameter (retrieved from the associated {\f1\fs20 TSQLAuthGroup} table content): after some time of inactivity, sessions are closed on the Server Side.
Note that sessions are used to manage safe cross-client @**transaction@s:
- When a transaction is initiated by a client, it will store the corresponding client Session ID, and use it to allow client-safe writing;
- Any further write to the DB (Add/Update/Delete) will be accessible only from this Session ID, until the transaction is released (via commit or rollback);
- If a transaction began and another client session try to write on the DB, it will wait until the current transaction is released - a timeout may occur if the server is not able to acquire the write status within some time;
- This global write locking is implemented in the {\f1\fs20 TSQLRest.AcquireWrite / ReleaseWrite} protected methods, and used on the Server-Side by {\f1\fs20 TSQLRestServer.URI};
- If the server do not handle Session/Authentication, transactions can be unsafe, in a multi-client concurrent architecture.
Therefore, for performance reasons in a multi-client environment, it's mandatory to release a transaction (via commit or rollback) as soon as possible.
:   Client interactivity
Note that with this design, it's up to the Client to react to an authentication error during any request, and ask again for the User pseudo and password at any time to create a new session. For multiple reasons (server restart, session timeout...) the session can be closed by the Server without previous notice.
In fact, the Client should just use create one instance of the {\f1\fs20 TSQLRestClientURI} classes as presented in @6@, then call the {\f1\fs20 SetUser} method as such:
!      Check(Client.SetUser('User','synopse')); // use default user
Then an event handled can be associated to the {\f1\fs20 TSQLRestClientURI. OnAuthentificationFailed} property, in order to ask the user to enter its login name and password:
!  TOnAuthentificationFailed = function(Retry: integer;
!    var aUserName, aPassword: string): boolean;
:   URI signature
{\i @**Query Authentication@} is handled at the Client side in {\f1\fs20 TSQLRestClientURI. SessionSign} method, by computing the {\f1\fs20 session_signature} parameter for a given URL.
In order to enhance security, the {\f1\fs20 session_signature} parameter will contain, encoded as 3 hexadecimal 32 bit cardinals:
- The @*Session@ ID (to retrieve the private key used for the signature);
- A Client Time Stamp (in 256 ms resolution) which must be greater or equal than the previous time stamp received;
- The URI signature, using the session private key, the user hashed password, and the supplied Client Time Stamp as source for its {\i crc32} hashing algorithm.
Such a classical 3 points signature will avoid most {\i man-in-the-middle} (MITM) or {\i re-play} attacks.
Here is a typical signature to access the {\f1\fs20 root} URL
$ root?session_signature=0000004C000F6BE365D8D454
In this case, {\f1\fs20 0000004C} is the Session ID, {\f1\fs20 000F6BE3} is the client time stamp (aka nonce), and {\f1\fs20 65D8D454} is the signature, checked by the following Delphi expression:
!     (crc32(crc32(fPrivateSaltHash,PTimeStamp,8),pointer(aURL),aURLlength)=aSignature);
A RESTful GET of the {\f1\fs20 TSQLRecordPeople} table with RowID=6 will have the following URI:
$ root/People/6?session_signature=0000004C000F6DD02E24541C
For better Server-side performance, the URI signature will use fast {\i crc32} hashing method, and not the more secure (but much slower) SHA-256. Since our security model is not officially validated as a standard method (there is no standard for per URI authentication of RESTful applications), the better security will be handled by encrypting the whole transmission channel, using standard @*HTTPS@ with certificates signed by a trusted CA, validated for both client and server side. The security involved by using {\i crc32} will be enough for most common use. Note that the password hashing and the session opening will use SHA-256, to enhance security with no performance penalty.
In our implementation, for better Server-side reaction, the {\f1\fs20 session_signature} parameter is appended at the end of the URI, and the URI parameters are not sorted alphabetically, as suggested by the reference article quoted above. This should not be a problem, either from a Delphi Client either from a @*AJAX@ / JavaScript client.
:   Authentication using AJAX
Some working @**JavaScript@ code has been published in our forum by a framework user (thanks, "RangerX"), which implements the authentication schema as detailed above. It uses {\f1\fs20 jQuery}, and HTML 5 {\f1\fs20 LocalStorage}, not cookies, for storing session information on the Client side.
See @http://synopse.info/forum/viewtopic.php?pid=2995#p2995
The current revision of the framework contains the code as expected by this JavaScript code - especially the results encoded as @2@ objects.
In the future, some "official" code will be available for such AJAX clients. It will probably rely on pure-pascal implementation using such an {\i Object-Pascal-to-JavaScript} compiler - it does definitively make sense to have Delphi-like code on the client side, not to break the @*ORM@ design. For instance, the Open Source {\f1\fs20 DWS} ({\i DelphiWebScript}) compiler matches our needs - see @http://delphitools.info/tag/javascript
:12 Testing
:25  Thread-safety
On the Server side, our Framework was designed to be @**thread-safe@.
In fact, the {\f1\fs20 TSQLRestServer.URI} method is expected to be thread-safe, e.g. from the {\f1\fs20 TSQLite3HttpServer. Request} method. Thanks to the @*REST@ful approach of our framework, this method is the only one which is expected to be thread-safe.
In order to achieve this thread-safety without sacrificing performance, the following rules were applied in {\f1\fs20 TSQLRestServer.URI}:
- Most of this methods's logic is to process the incoming parameters, so is thread-safe by design (e.g. {\f1\fs20 Model} and {\f1\fs20 RecordProps} access do not change during process);
- The {\i @*SQLite3@} engine access is protected at SQL/JSON @*cache@ level, via {\f1\fs20 DB.LockJSON()} calls in {\f1\fs20 @*TSQLRestServerDB@} methods;
- {\f1\fs20 TSQLRestServerStatic} main methods ({\f1\fs20 EngineList, EngineRetrieve, EngineAdd, EngineUpdate, EngineDelete, EngineRetrieveBlob, EngineUpdateBlob}) are thread-safe: e.g. {\f1\fs20 @*TSQLRestServerStaticInMemory@} uses a per-Table Critical Section;
- {\f1\fs20 TSQLRestServerCallBack} methods (i.e. @*published method@s of the inherited {\f1\fs20 TSQLRestServer} class) must be implemented to be thread-safe;
- A protected {\f1\fs20 fSessionCriticalSection} is used to protect shared {\f1\fs20 fSession[]} access between clients;
- Remote external tables - see @27@ - use thread-safe connections and statements when accessing the databases via SQL;
- Access to {\f1\fs20 fStats} was not made thread-safe, since this data is indicative only: a {\i mutex} was not used to protect this resource.
We tried to make the internal Critical Sections as short as possible, or relative to a table only (e.g. for {\f1\fs20 TSQLRestServerStaticInMemory}).
There is some kind of "giant lock" at the {\i SQLite3} engine level, so all requests process will be queued. This was not found to be a major issue, since the internal SQL/JSON cache implementation need such a global lock, and since most of the {\i SQLite3} resource use will consist in hard disk access, which gain to be queued.
From the Client-side, the REST core of the framework is expected to be Client-safe by design, therefore perfectly thread-safe: it's the benefit of the @*stateless@ architecture.
:  Automated testing
You know that @**test@ing is (almost) everything if you want to avoid regression problems in your application.
How can you be confident that any change made to your software code won't create any error in other part of the software?
Automated unit testing is a good candidate for avoiding any serious regression.
And even better, testing-driven coding can be encouraged:
- Write a void implementation of a feature, that is code the interface with no implementation;
- Write a test code;
- Launch the test - it must fail;
- Implement the feature;
- Launch the test - it must pass;
- Add some features, and repeat all previous tests every time you add a new feature.
It could sounds like a waste of time, but such coding improve your code quality a lot, and, at least, it help you write and optimize every implementation feature.
The framework has been implemented using this approach, and provide all the tools to write tests.
:   Involved classes in Unitary testing
The @!TSynTest,TSynTestCase,TSynTests!Lib\SynCommons.pas@ unit defines two classes (both inheriting from {\f1\fs20 TSynTest}), implementing a complete Unitary testing mechanism similar to {\i DUnit}, with less code overhead, and direct interface with the framework units and requirements (@*UTF-8@ ready, code compilation from Delphi 6 up to XE2, no external dependency).
The following diagram defines this class hierarchy:
\graph HierTSynTest TSynTest classes hierarchy
\TSynTests\TSynTest
\TSynTestCase\TSynTest
\
The main usable class types are:
- {\f1\fs20 TSynTestCase}, which is a class implementing a test case: individual tests are written in the published methods of this class;
- {\f1\fs20 TSynTests}, which is used to run a suit of test cases, as defined with the previous class.
In order to define tests, some {\f1\fs20 TSynTestCase} children must be defined, and will be launched by a {\f1\fs20 TSynTests} instance to perform all the tests. A text report is created on the current console, providing statistics and Pass/Fail.
:   First steps in testing
Here are the functions we want to test:
!function Add(A,B: double): Double; overload;
!begin
!  result := A+B;
!end;
!
!function Add(A,B: integer): integer; overload;
!begin
!  result := A+B;
!end;
!
!function Multiply(A,B: double): Double; overload;
!begin
!  result := A*B;
!end;
!
!function Multiply(A,B: integer): integer; overload;
!begin
!  result := A*B;
!end;
So we create three classes one for the whole test suit, one for testing addition, one for testing multiplication:
!type
!  TTestNumbersAdding = class(TSynTestCase)
!  published
!    procedure TestIntegerAdd;
!    procedure TestDoubleAdd;
!  end;
!
!  TTestNumbersMultiplying = class(TSynTestCase)
!  published
!    procedure TestIntegerMultiply;
!    procedure TestDoubleMultiply;
!  end;
!
!  TTestSuit = class(TSynTests)
!  published
!    procedure MyTestSuit;
!  end;
The trick is to create published methods, each containing some tests to process.
Here is how one of these test methods are implemented (I let you guess the others):
!procedure TTestNumbersAdding.TestDoubleAdd;
!var A,B: double;
!    i: integer;
!begin
!  for i := 1 to 1000 do
!  begin
!    A := Random;
!    B := Random;
!    CheckSame(A+B,Adding(A,B));
!  end;
!end;
The {\f1\fs20 CheckSame()} is necessary because of floating-point precision problem, we can't trust plain = operator (i.e. {\f1\fs20 Check(A+B=Adding(A,B))} will fail because of rounding problems).
And here is the test case implementation:
!procedure TTestSuit.MyTestSuit;
!begin
!  AddCase([TTestNumbersAdding,TTestNumbersMultiplying]);
!end;
And the main program (this {\f1\fs20 .dpr} is expected to be available as a console program):
!  with TTestSuit.Create do
!  try
!    ToConsole := @Output; // so we will see something on screen
!    Run;
!    readln;
!  finally
!    Free;
!  end;
Just run this program, and you'll get:
$   Suit
$  ------
$
$1. My test suit
$
$ 1.1. Numbers adding:
$  - Test integer add: 1000 assertions passed
$  - Test double add: 1000 assertions passed
$  Total failed: 0 / 2000  - Numbers adding PASSED
$
$ 1.2. Numbers multiplying:
$  - Test integer multiply: 1000 assertions passed
$  - Test double multiply: 1000 assertions passed
$  Total failed: 0 / 2000  - Numbers multiplying PASSED
$
$Generated with: Delphi 7 compiler
$
$Time elapsed for all tests: 1.96ms
$Tests performed at 23/07/2010 15:24:30
$
$Total assertions failed for all test suits:  0 / 4000
$
$! All tests passed successfully.
You can see that all text on screen was created by "UnCamelCasing" the method names (thanks to our good old @*Camel@), and that the test suit just follows the classes defined.
This test has been uploaded in the {\f1\fs20 SQLite3\\Sample\\07 - SynTest} folder of the Source Code Repository.
:   Implemented tests
The @SAD-DI-2.2.2@ defines all classes released with the framework source code, which covers all core aspects of the framework. Global testing coverage is good, excellent for core components (more than 8,000,000 individual checks are performed for revision 1.17), but there is still some User-Interface related tests to be written.
Before any release all unitary regression tests are performed with the following compilers:
- Delphi 6;
- Delphi 7, with and without our Enhanced Run Time Library;
- Delphi 2007;
- Delphi 2010 (and in some cases, Delphi 2009 - but we assume that if it works with Delphi 2010, it will work with Delphi 2009);
- Delphi XE2.
Then all sample source code (including the {\i Main Demo} and {\f1\fs20 @*SynDBExplorer@} sophisticated tools) are compiled, and user-level testing is performed.
:  Logging
The framework makes an extensive use of the logging features introduced with the {\f1\fs20 SynCommons} unit - see @16@.
In its current implementation, the framework is able to log on request:
- Any exceptions triggered during process via {\f1\fs20 sllException} and {\f1\fs20 sllExceptionOS} levels;
- Client and server @*REST@ful {\f1\fs20 URL} methods via {\f1\fs20 sllClient} and {\f1\fs20 sllServer} levels;
- @*SQL@ executed statements in the {\i @*SQLite3@} engine via the {\f1\fs20 sllSQL} level;
- @*JSON@ results when retrieved from the {\i SQLite3} engine via the {\f1\fs20 sllResult} level;
- Main errors triggered during process via {\f1\fs20 sllError} level;
- @*Security@ User authentication and @*session@ management via {\f1\fs20 sllUserAuth};
- Some additional low-level information via {\f1\fs20 sllDebug} and {\f1\fs20 sllInfo} levels.
Those levels are available via the {\f1\fs20 TSQLLog} class, inheriting from {\f1\fs20 TSynLog}, as defined in @!TSQLLog!Lib\SQLite3\SQLite3Commons.pas@.
Three main {\f1\fs20 TSynLogClass} global variables are defined in order to use the same {\f1\fs20 TSynLog} class for all logging available in the framework units. Since all layers are not common, several variables have been defined, as such:
- {\f1\fs20 SynDBLog} for all {\i @*SynDB@*} units, i.e. all generic database code;
- {\f1\fs20 SQLite3Log} for all {\i SQLite3*} units, i.e. all @*ORM@ related code;
- {\f1\fs20 SynSQLite3Log} for the {\f1\fs20 SynSQLite3} unit, which implements the {\i @*SQLite3@} engine itself.
For instance, if you execute the following statement at the beginning of {\f1\fs20 TestSQL3.dpr}, most regression @*test@s will produce some logging, and will create about 270 MB of log file content, if executed:
!  with TSQLLog.Family do begin
!    Level := LOG_VERBOSE;
!    HighResolutionTimeStamp := true;
!    TSynLogTestLog := TSQLLog;
!    SynDBLog := TSQLLog;
!    {$ifdef WITHLOG}
!    SQLite3Log := TSQLLog;
!    SynSQLite3Log := TSQLLog;
!    {$endif}
!  end;
Creating so much log content won't increase the processing time much. On a recent laptop, whole regression tests process will spent only 2 seconds to write the additional logging, which is the bottleneck of the hard disk writing.
If logging is turned off, there is no speed penalty noticeable.
\page
:44Source code
=[License]
: Availability
As a true {\i Open Source} project, all source code of the framework is available, and latest version can be retrieved from our online repository at @http://synopse.info/fossil
The source has been commented following the scheme used by our {\i SynProject} documentation tool. That is all interface definition of the units have special comments, which were extracted then incorporated into this @SAD@, in the following pages.
:  Obtaining the Source Code
Each official release of the framework is available in a dedicated {\f1\fs20 SynopseSQLite3.zip} archive from the official http://synopse.info web site, but you may want to use the latest version available.
You can obtain a {\f1\fs20 .zip} archive containing a snapshot of the latest version of the whole source code tree directly from this repository.
Follow these steps:
- Pointer your web browser at @http://synopse.info/fossil
- Click on the "{\i Login}" menu button.
- Log in as anonymous. The password is shown on screen. Just click on the "{\i Fill out captcha}" button then on the "{\i Login}" button. The reason for requiring this login is to prevent spiders from walking the entire website, downloading ZIP archives of every historical version, and thereby soaking up all our bandwidth.
- Click on the {\i Timeline} or {\i Leaves} link at the top of the page. Preferred way is {\i Leaves} which will give you the latest available version.
- Select a version of the source code you want to download: a version is identified by an hexadecimal link (e.g. {\f1\fs20 6b684fb2}). Note that you must successfully log in as "{\i anonymous}" in steps 1-3 above in order to see the link to the detailed version information.
- Finally, click on the "{\i Zip Archive}" link, available at the end of the "{\i Overview}" header, right ahead to the "{\i Other Links}" title. This link will build a {\f1\fs20 .zip} archive of the complete source code and download it to your browser.
:  Expected compilation platform
The framework source code tree will compile and is tested for the following platform:
- Delphi 6 up to Delphi XE2 compiler and IDE;
- For Windows 32 bit platform;
- GUI may be compiled optionally with third-party non Open-Source TMS Components, instead of default VCL components.
Some part of the library (e.g. {\f1\fs20 SynCommons.pas} or the @27@ units) are also compatible with Delphi 5.
Note that the framework is expected to create only 32 bit Windows applications yet (which will run without any issue on a @*64 bit@ Windows operating system). But 64 bit compilation and even cross-platform is on its way: 64 bit support will need to adapt some low-level WinAPI changes, and cross-platform will probably use the Delphi XE2 FireMonkey library for User Interface generation, or other tools more neutral, using @*JavaScript@ and @*AJAX@ - or both. But the framework source code implementation and design tried to be as cross-platform as possible, since the beginning. See @http://blog.synopse.info/post/2011/08/08/Our-mORMot-won-t-hibernate-this-winter%2C-thanks-to-FireMonkey
:  Note about sqlite3*.obj files
In order to maintain the source code repository in a decent size, we excluded the {\f1\fs20 sqlite3*.obj} storage in it, but provide the full source code of the {\i @*SQlite3@} engine in the corresponding {\f1\fs20 sqlite3.c} file, ready to be compiled with all conditional defined as expected by {\f1\fs20 SynSQlite3.pas}.
Therefore, {\f1\fs20 sqlite3.obj} and {\f1\fs20 sqlite3fts.obj} files are available as a separated download, from @http://synopse.info/files/sqlite3obj.7z Please download the latest compiled version of these {\f1\fs20 .obj} files from this link. You can also use the supplied {\f1\fs20 c.bat} file to compile from the original {\f1\fs20 sqlite3.c} file available in the repository, if you have the {\f1\fs20 bcc32} C command-line compiler installed.
The free version works and was used to create both {\f1\fs20 .obj} files, i.e. {\i C++Builder Compiler (bcc compiler) free download} - as available from {\i Embarcadero} web site. The upcoming 64 bit version will use the Microsoft Visual ++ compiler, since the {\i C++Builder} version is not existing yet.
:  Folder layout
As retrieved from our source code repository, you'll find the following file layout.
|%30%70
|\b Directory|Description\b0
|{\f1\fs20 /}|Root folder, containing common files
|{\f1\fs20 HtmlView/}|A fork of the freeware {\f1\fs20 THtmlView} component, used as a demo of the {\f1\fs20 SynPdf} unit - not finished, and not truly Unicode ready
|{\f1\fs20 LVCL/}|{\i Light VCL} replacement files for standard VCL (for Delphi 6-7 only)
|{\f1\fs20 RTL7/}|Enhanced RTL .dcu for Delphi 7 (not mandatory at all), and {\i FastMM4} memory manager to be used before Delphi 2006
|{\f1\fs20 SQLite3/}|Contains all @*ORM@ related files of the framework
|{\f1\fs20 SynProject/}|Source code of the {\i SynProject} tool, used to create e.g. this documentation
;|{\f1\fs20 zeos/}|A fork of the freeware {\i Zeos} library - not finished, and not truly Unicode ready
|%
In the {\i Root folder}, some common files are defined:
|%30%70
|\b File|Description\b0
|{\f1\fs20 CPort.*}|A fork of the freeware {\i ComPort} Library ver. 2.63
|{\f1\fs20 PasZip.pas}|ZIP/LZ77 Deflate/Inflate Compression in pure pascal
|{\f1\fs20 SynBigTable.pas}|class used to store huge amount of data with fast retrieval
|{\f1\fs20 SynBz.pas bunzipasm.inc}|fast BZ2 compression/decompression
|{\f1\fs20 SynBzPas.pas}|pascal implementation of BZ2 decompression
|{\f1\fs20 SynCommons.pas}|common functions used by most Synopse projects
|{\f1\fs20 SynCrtSock.pas}|classes implementing @*HTTP@/1.1 client and server protocol
|{\f1\fs20 SynCrypto.pas}|fast cryptographic routines (hashing and cypher)
|{\f1\fs20 SynDprUses.inc}|generic header included in the beginning of the uses clause of a .dpr source code
|{\f1\fs20 SynGdiPlus.pas}|GDI+ library API access with anti-aliasing drawing
|{\f1\fs20 SynLZ.pas}|@*SynLZ@ compression decompression unit - used by {\f1\fs20 SynCommons.pas}
|{\f1\fs20 SynLZO.pas}|LZO compression decompression unit
|{\f1\fs20 SynMemoEx.pas}|Synopse extended TMemo visual component (used e.g. in {\i SynProject})
|{\f1\fs20 SynPdf.pas}|@*PDF@ file generation unit
|{\f1\fs20 SynScaleMM.pas}|multi-thread friendly memory manager unit - not finished yet
|{\f1\fs20 SynSelfTests.pas}|automated @*test@s for common units of the Synopse Framework
|{\f1\fs20 SynSQLite3.pas}|{\i @*SQLite3@} embedded Database engine
|{\f1\fs20 SynTaskDialog.*}|implement TaskDialog window (native on Vista/Seven, emulated on XP)
|{\f1\fs20 SynWinSock.pas}|low level access to network Sockets for the Win32 platform
|{\f1\fs20 SynZip.pas deflate.obj trees.obj}|low-level access to ZLib compression, 1.2.5
|{\f1\fs20 SynZipFiles.pas}|high-level access to .zip archive file compression
|{\f1\fs20 Synopse.inc}|generic header to be included in all units to set some global conditional definitions
|{\f1\fs20 vista.*}|A resource file enabling theming under XP
|{\f1\fs20 vistaAdm.*}|A resource file enabling theming under XP and Administrator rights under Vista
|%
In the same {\i Root folder}, the external database-agnostic units are located:
|%30%70
|\b File|Description\b0
|{\f1\fs20 @*SynDB@}|abstract database direct access classes
|{\f1\fs20 SynOleDB}|fast @*OleDB@ direct access classes
|{\f1\fs20 SynDBODBC}|fast @*ODBC@ direct access classes
|{\f1\fs20 SynDBOracle}|{\i @*Oracle@} DB direct access classes (via OCI)
|{\f1\fs20 SynDBSQLite3}|{\i @*SQLite3@} direct access classes
|%
In the {\f1\fs20 SQlite3/} folder, the files defining the {\i Synopse ORM framework} (using mostly {\f1\fs20 SynCommons, SynLZ, SynSQLite3, SynGdiPlus, SynCrtSock, SynPdf, SynTaskDialog} and {\f1\fs20 SynZip} from the {\i Root folder}):
|%30%70
|\b File|Description\b0
|{\f1\fs20 Documentation/}|Sub folder containing the source of the Synopse documentation
|{\f1\fs20 Samples/}|Sub folders containing some sample code
|{\f1\fs20 SQLite3Commons.pas}|Main unit of the @*ORM@ framework
|{\f1\fs20 SQLite3.pas}|{\i SQLite3} kernel bridge between {\f1\fs20 SQLite3Commons.pas} and {\f1\fs20 SynSQLite3.pas}
|{\f1\fs20 *.bmp *.rc}|Resource files, compiled into {\f1\fs20 *.res} files
|{\f1\fs20 SQLite3FastCgiServer.pas}|FastCGI server - not fully tested
|{\f1\fs20 SQLite3HttpClient.pas}|HTTP/1.1 Client
|{\f1\fs20 SQLite3HttpServer.pas}|HTTP/1.1 Server
|{\f1\fs20 SQLite3Pages.pas}|Integrated Reporting engine
|{\f1\fs20 SQLite3Service.pas}|Stand-alone Service
|{\f1\fs20 SQLite3ToolBar.pas}|ORM ToolBar User Interface generation
|{\f1\fs20 SQLite3UI.*}|Grid to display Database content
|{\f1\fs20 SQLite3UIEdit.*}|Record edition dialog, used to edit record content on the screen
|{\f1\fs20 SQLite3UILogin.*}|some common User Interface functions and dialogs
|{\f1\fs20 SQLite3UIOptions.*}|General Options setting dialog, generated from code
|{\f1\fs20 SQLite3UIQuery.*}|Form handling queries to a User Interface Grid, using our ORM RTTI to define search parameters and algorithms
|{\f1\fs20 SQLite3i18n.pas}|internationalization (@*i18n@) routines and classes
|{\f1\fs20 TestSQL3.dpr}|Main unit @*test@ing program of the Synopse {\i mORMot} framework
|{\f1\fs20 TestSQL3Register.dpr}|Run as administrator for {\i TestSQL3} to use {\i http.sys} on Vista/Seven
|{\f1\fs20 c.bat sqlite3.c}|Source code of the {\i SQLite3} embedded Database engine
|%
: Installation
Just unzip the {\f1\fs20 .zip} archive, including all sub-folders, into a local directory of your computer (for instance, {\f1\fs20 D:\Dev\Lib}).
Then add the following paths to your Delphi IDE (in {\i Tools/Environment/Library} menu):
- {\i Library path}: {\f1\fs20 (...existing path...);D:\\Dev\\Lib;D:\\Dev\\Lib\\SQLite3}
- {\i Search path}: {\f1\fs20 (...existing path...);D:\\Dev\\Lib;D:\\Dev\\Lib\\SQLite3}
Open the {\f1\fs20 TestSQL3.dpr} program from the {\f1\fs20 SQLite3} sub-folder. You should be able to compile it and run all regression @*test@s on your computer.
Then open the {\f1\fs20 *.dpr} files, as available in the {\f1\fs20 SQLite3\\Samples} sub-folder. You should be able to compile all sample programs, including {\f1\fs20 SynFile.dpr} in the {\f1\fs20 MainDemo} folder.
\page
:45SynCommons unit
In the following next paragraphs, we'll comment some main features of the lowest-level of the framework, mainly located in @!Lib\SynCommons.pas@:
- Unicode and @*UTF-8@;
- {\f1\fs20 @*currency@} type;
- {\i @*dynamic array@} wrappers ({\f1\fs20 TDynArray} and {\f1\fs20 TDynArrayHashed});
- Enhanced @*log@ging.
:32 Unicode and UTF-8
Our {\i mORMot} Framework has 100% UNICODE compatibility, that is compilation under Delphi 2009/2010/XE/XE2. The code has been deeply rewritten and @*test@ed, in order to provide compatibility with the {\f1\fs20 String=UnicodeString} paradigm of these compilers.  But the code will also handle safely Unicode for older version, i.e. from Delphi 6 up to Delphi 2007.
Since our framework is natively @**UTF-8@ (this is the better character encoding for fast text - @*JSON@ - streaming/parsing and it is natively supported by the {\i SQLite3} engine), we had to establish a secure way our framework used strings, in order to handle all versions of Delphi (even pre-Unicode versions, especially the Delphi 7 version we like so much), and provide compatibility with the Free Pascal Compiler.
Some string types have been defined, and used in the code for best cross-compiler efficiency (avoiding most conversion between formats):
- {\f1\fs20 @*RawUTF8@} is used for every internal data usage, since both {\i @*SQLite3@} and JSON do expect UTF-8 encoding;
- {\f1\fs20 WinAnsiString} where {\i WinAnsi}-encoded {\f1\fs20 AnsiString} (code page 1252) are needed;
- Generic {\f1\fs20 string} for {\i @*i18n@} (e.g. in unit {\f1\fs20 SQLite3i18n}), i.e. text ready to be used within the VCL, as either {\f1\fs20 AnsiString} (for Delphi 2 to 2007) or {\f1\fs20 UnicodeString} (for Delphi 2009/2010/XE/XE2);
- {\f1\fs20 RawUnicode} in some technical places (e.g. direct Win32 *W() API call in Delphi 7) - note: this type is NOT compatible with Delphi 2009/2010/XE/XE2 {\f1\fs20 UnicodeString};
- {\f1\fs20 RawByteString} for byte storage (e.g. for {\f1\fs20 FileFromString()} function);
- {\f1\fs20 SynUnicode} is the fastest available Unicode {\i native} string type, depending on the compiler used (i.e. {\f1\fs20 WideString} before Delphi 2009, and {\f1\fs20 UnicodeString} since);
- Some special conversion functions to be used for Delphi 2009/2010/XE/XE2 {\f1 UnicodeString} (defined inside {\f1\fs20 \{$ifdef UNICODE\}...\{$endif\}} blocks);
- Never use {\f1\fs20 AnsiString} directly, but one of the types above.
Note that {\f1\fs20 RawUTF8} is the preferred {\f1\fs20 string} type to be used in our framework when defining textual properties in a {\f1\fs20 @*TSQLRecord@} and for all internal data processing. It's only when you're reaching the User Interface layer that you may convert explicitly the {\f1\fs20 RawUTF8} content into the generic VCL {\f1\fs20 string} type, using either the {\f1\fs20 Language. UTF8ToString} method (from {\f1\fs20 SQLite3i18n.pas} unit) or the following function from {\f1\fs20 SynCommons.pas}:
!/// convert any UTF-8 encoded String into a generic VCL Text
!// - it's prefered to use TLanguageFile.UTF8ToString() in SQLite3i18n,
!// which will handle full i18n of your application
!// - it will work as is with Delphi 2009/2010/XE/XE2 (direct unicode conversion)
!// - under older version of Delphi (no unicode), it will use the
!// current RTL codepage, as with WideString conversion (but without slow
!// WideString usage)
!function UTF8ToString(const Text: RawUTF8): string;
Of course, the {\f1\fs20 StringToUTF8} method or function are available to send back some text to the @*ORM@ layer.\line A lot of dedicated conversion functions (including to/from numerical values) are included in {\f1\fs20 SynCommons.pas}. Those were optimized for speed and multi-thread capabilities, and to avoid implicit conversions involving a temporary {\f1\fs20 string} variable.
Warning during the compilation process are not allowed, especially under Unicode version of Delphi (e.g. Delphi 2010): all string conversion from the types above are made explicitly in the framework's code, to avoid any unattended data loss.
:33 Currency handling
Faster and safer way of comparing two {\f1\fs20 @*currency@} values is certainly to map the variables to their internal {\f1\fs20 Int64} binary representation, as such:
!function CompCurrency(var A,B: currency): Int64;
!var A64: Int64 absolute A;
!    B64: Int64 absolute B;
!begin
!  result := A64-B64;
!end;
This will avoid any rounding error during comparison (working with *10000 integer values), and will be faster than the default implementation, which uses the FPU (or SSE2 under {\i x64} architecture) instructions.
You some direct {\f1\fs20 currency} handling in the {\f1\fs20 SynCommons.pas} unit. It will by-pass the FPU use, and is therefore very fast.
There are some functions using the {\f1\fs20 Int64} binary representation (accessible either as {\f1\fs20 PInt64(@aCurrencyVar)^} or the {\f1\fs20 absolute} syntax):
- {\f1\fs20 function Curr64ToString(Value: Int64): string;}
- {\f1\fs20 function StrToCurr64(P: PUTF8Char): Int64;}
- {\f1\fs20 function Curr64ToStr(Value: Int64): RawUTF8;}
- {\f1\fs20 function Curr64ToPChar(Value: Int64; Dest: PUTF8Char): PtrInt;}
- {\f1\fs20 function StrCurr64(P: PAnsiChar; const Value: Int64): PAnsiChar;}
Using those functions can be {\i much} faster for textual conversion than using the standard {\f1\fs20 FloatToText()} implementation. They are validated with provided regression tests.
Of course, in normal code, it is certainly not worth using the {\f1\fs20 Int64} binary representation of {\f1\fs20 currency}, but rely on the default compiler/RTL implementation. In all cases, having optimized functions was a need for both speed and accuracy of our ORM data processing, and also for @27@.
:48 Dynamic array wrapper
The {\f1\fs20 SynCommons} unit has been enhanced, since version 1.13:
- {\f1\fs20 BinToBase64} and {\f1\fs20 Base64ToBin} conversion functions;
- Low-level @*RTTI@ functions for handling record types: {\f1\fs20 RecordEquals, RecordSave, RecordSaveLength, RecordLoad};
- {\f1\fs20 TDynArray} and {\f1\fs20 TDynArrayHashed} objects, which are wrappers around any {\i @*dynamic array@}.
With {\f1\fs20 TDynArray}, you can access any {\i dynamic array} (like {\f1\fs20 TIntegerDynArray = array of integer}) using {\f1\fs20 TList}-like properties and methods, e.g. {\f1\fs20 Count, Add, Insert, Delete, Clear, IndexOf, Find, Sort} and some new methods like {\f1\fs20 LoadFromStream, SaveToStream, LoadFrom, SaveTo, Slice, Reverse,} and {\f1\fs20 AddArray}. It includes e.g. fast binary @*serialization@ of any {\i dynamic array}, even containing strings or records - a {\f1\fs20 CreateOrderedIndex} method is also available to create individual index according to the {\i dynamic array} content. You can also serialize the array content into @*JSON@, if you wish.
One benefit of {\i dynamic arrays} is that they are reference-counted, so they do not need any {\f1\fs20 Create/try..finally...Free} code, and are well handled by the Delphi compiler (access is optimized, and all array content will be allocated at once, therefore reducing the memory fragmentation and CPU cache slow-down).
They are no replacement to a {\f1\fs20 @*TCollection@} nor a {\f1\fs20 TList} (which are the standard and efficient way of storing class instances, and are also handled as @*published properties@ since revision 1.13 of the framework), but they are very handy way of having a list of content or a dictionary at hand, with no previous class nor properties definition.
You can look at them like Python's list, tuples (via records handling) and dictionaries (via {\f1\fs20 Find} method, especially with the dedicated {\f1\fs20 TDynArrayHashed} wrapper), in pure Delphi. Our new methods (about searching and serialization) allow most usage of those script-level structures in your Delphi code.
In order to handle {\i dynamic arrays} in our @*ORM@, some @*RTTI@-based structure were designed for this task. Since {\i dynamic array of records} should be necessary, some low-level fast access to the record content, using the common RTTI, has also been implemented (much faster than the "new" enhanced RTTI available since Delphi 2010).
:  TList-like properties
Here is how you can have method-driven access to the {\i dynamic array}:
!type
!   TGroup: array of integer;
!var
!   Group: TGroup;
!   GroupA: TDynArray;
!   i, v: integer;
!begin
!  GroupA.Init(TypeInfo(TGroup),Group); // associate GroupA with Group
!  for i := 0 to 1000 do
!  begin
!    v := i+1000; // need argument passed as a const variable
!    GroupA.Add(v);
!  end;
!  v := 1500;
!  if GroupA.IndexOf(v)<0 then // search by content
!    ShowMessage('Error: 1500 not found!');
!  for i := GroupA.Count-1 downto 0 do
!    if i and 3=0 then
!      GroupA.Delete(i); // delete integer at index i
!end;
This {\f1\fs20 TDynArray} wrapper will work also with array of string or array of records...
Records need only to be packed and have only not reference counted fields ({\f1\fs20 byte, integer, double}...) or {\f1\fs20 string} reference-counted fields (no {\f1\fs20 Variant} nor {\f1\fs20 Interface} within). But {\f1\fs20 TDynArray} is able to handle records within records, and even {\i dynamic arrays} within records.
Yes, you read well: it will handle a {\i dynamic array} of records, in which you can put some strings or whatever data you need.
The {\f1\fs20 IndexOf()} method will search by content. That is e.g. for an {\f1\fs20 array of record}, all record fields content (including {\f1\fs20 string} properties) must match.
Note that {\f1\fs20 TDynArray} is just a wrapper around an existing {\i dynamic array} variable. In the code above, {\f1\fs20 Add} and {\f1\fs20 Delete} methods are modifying the content of the {\f1\fs20 Group} variable. You can therefore initialize a {\f1\fs20 TDynArray} wrapper on need, to access more efficiently any native Delphi {\i dynamic array}. {\f1\fs20 TDynArray} doesn't contain any data: the elements are stored in the {\i dynamic array} variable, not in the {\f1\fs20 TDynArray} instance.
:  Enhanced features
Some methods were defined in the {\f1\fs20 TDynArray} record/object, which are not available in a plain {\f1\fs20 TList} - with those methods, we come closer to some native generics implementation:
- Now you can save and load a {\i dynamic array} content to or from a stream or a string (using {\f1\fs20 LoadFromStream/SaveToStream} or {\f1\fs20 LoadFrom/SaveTo} methods) - it will use a proprietary but very fast binary stream layout;
- And you can sort the {\i dynamic array} content by two means: either {\i in-place} (i.e. the array elements content is exchanged - use the {\f1\fs20 Sort} method in this case) or via an external integer {\i index look-up array} (using the {\f1\fs20 CreateOrderedIndex} method - in this case, you can have several orders to the same data);
- You can specify any custom comparison function, and there is a new {\f1\fs20 Find} method will can use fast binary search if available.
Here is how those new methods work:
!var
!  Test: RawByteString;
!...
!  Test := GroupA.SaveTo;
!  GroupA.Clear;
!  GroupA.LoadFrom(Test);
!  GroupA.Compare := SortDynArrayInteger;
!  GroupA.Sort;
!  for i := 1 to GroupA.Count-1 do
!    if Group[i]<Group[i-1] then
!      ShowMessage('Error: unsorted!');
!  v := 1500;
!  if GroupA.Find(v)<0 then // fast binary search
!    ShowMessage('Error: 1500 not found!');
Some unique methods like {\f1\fs20 Slice, Reverse} or {\f1\fs20 AddArray} are also available, and mimic well-known Python methods.
Still closer to the generic paradigm, working for Delphi 6 up to XE2, without the need of the slow enhanced RTTI...
:  Capacity handling via an external Count
One common speed issue with the default usage of {\f1\fs20 TDynArray} is that the internal memory buffer is reallocated when you change its length, just like a regular Delphi {\i dynamic array}.
That is, whenever you call {\f1\fs20 Add} or {\f1\fs20 Delete} methods, an internal call to {\f1\fs20 SetLength(DynArrayVariable)} is performed. This could be slow, because it always executes some extra code, including a call to {\f1\fs20 ReallocMem}.
In order not to suffer for this, you can define an external {\i Count} value, as an {\f1\fs20 Integer} variable.
In this case, the {\f1\fs20 Length(DynArrayVariable)} will be the memory capacity of the {\i dynamic array}, and the exact number of stored item will be available from this {\i Count} variable. A {\f1\fs20 Count} property is exposed by {\f1\fs20 TDynArray}, and will always reflect the number of items stored in the {\i dynamic array}. It will point either to the external {\f1\fs20 Count} variable, if defined; or it will reflect the {\f1\fs20 Length(DynArrayVariable)}, just as usual. A {\f1\fs20 Capacity} property is also exposed by {\f1\fs20 TDynArray}, and will reflect the capacity of the {\i dynamic array}: in case of an external {\i Count} variable, it will reflect {\f1\fs20 Length(DynArrayVariable)}.
As a result, adding or deleting items could be much faster.
!var
!   Group: TIntegerDynArray;
!   GroupA: TDynArray;
!   GroupCount, i, v: integer;
!begin
!  GroupA.Init(TypeInfo(TGroup),Group,@GroupCount);
!  GroupA.Capacity := 1023; // reserver memory
!  for i := 0 to 1000 do
!  begin
!    v := i+1000; // need argument passed as a const variable
!    GroupA.Add(v); // faster than with no external GroupCount variable
!  end;
!  Check(GroupA.Count=1001);
!  Check(GroupA.Capacity=1023);
!  Check(GroupA.Capacity=length(Group));
:  JSON serialization
:   TDynArray JSON features
The {\f1\fs20 TDynArray} wrapper features some native @*JSON@ @**serialization@ features: {\f1\fs20 TTextWriter. AddDynArrayJSON} and {\f1\fs20 TDynArray. LoadFromJSON} methods are available for @*UTF-8@ JSON serialization of {\i dynamic arrays}.
Most common kind of {\i dynamic arrays} ({\f1\fs20 array of byte, word, integer, cardinal, Int64, double, @*currency@, @*RawUTF8@, SynUnicode, WinAnsiString, string}) will be serialized as a valid JSON array, i.e. a list of valid JSON elements of the matching type (number, floating-point value or string).
Applications can supply a custom JSON serialization for any other dynamic array, via the {\f1\fs20 TTextWriter.@**RegisterCustomJSONSerializer@()} class method. Two callbacks are to be supplied for a dynamic array type information, in order to handle proper serialization and un-serialization of the JSON array.
Other not-known {\i dynamic arrays} (like any {\f1\fs20 array of packed record}) will be serialized as binary, then {\i Base64} encoded. This method will always work, but won't be easy to deal with from an AJAX client.
If you have any ideas of standard {\i dynamic arrays} which should be handled, feel free to post your proposal in the forum!
These methods were the purpose of adding in {\f1\fs20 SynCommons} both {\f1\fs20 BinToBase64} and {\f1\fs20 Base64ToBin} functions, very optimized for speed. In fact, we will use the {\i Base64} encoding to load or save any {\i dynamic array} of records from a {\f1\fs20 @*TSQLRecord@ published property}... using {\i @*SQLite3@} @*BLOB@ fields for storage, but {\i Base64} for JSON transmission (much more efficient than hexadecimal, and still JSON compatible).
This JSON serialization will indeed be used in our main @*ORM@ to support {\i dynamic arrays} as enhanced properties (stored as BLOB), and in the {\f1\fs20 interface}-based @*SOA@ architecture of the framework, for content transmission.
:53   Custom JSON serialization of dynamic arrays
As just stated, any {\i dynamic array} can be serialized using a custom JSON format, via the {\f1\fs20 TTextWriter.@*RegisterCustomJSONSerializer@()} class method. It will use the same method used for {\f1\fs20 @*record@} custom serialization - see @51@. In fact, if you register a {\i dynamic array} custom serializer, it will also be used for the associated internal {\f1\fs20 record}.
For instance, we would like to serialize a dynamic array of the following record:
!  TFV = packed record
!    Major, Minor, Release, Build: integer;
!    Main, Detailed: string;
!  end;
!  TFVs = array of TFV;
With the default serialization, such a dynamic array will be serialized as a {\i Base64} encoded binary buffer. This won't be easy to understand from an AJAX client, for instance.
In order to add a custom serialization for this kind of record, we need to implement the two needed callbacks. Our expected format will be a JSON array of all fields, i.e.:
! [1,2001,3001,4001,"1","1001"]
We may have used another layout, e.g. using {\f1\fs20 JSONEncode()} function and a JSON object layout, or any other valid JSON content.
Here comes the writer:
!class procedure TCollTstDynArray.FVWriter(const aWriter: TTextWriter; const aValue);
!var V: TFV absolute aValue;
!begin
!  aWriter.Add('[%,%,%,%,"%","%"]',
!    [V.Major,V.Minor,V.Release,V.Build,V.Main,V.Detailed],twJSONEscape);
!end;
This event will write one entry of the dynamic array, without the last ',' (which will be appended by {\f1\fs20 TTextWriter. AddDynArrayJSON}). In this method, {\f1\fs20 twJSONEscape} is used to escape the supplied {\f1\fs20 string} content as a valid JSON string (with double quotes and proper UTF-8 encoding).
Of course, the {\i Writer} is easier to code than the {\i Reader} itself:
!class function TCollTstDynArray.FVReader(P: PUTF8Char; var aValue;
!  out aValid: Boolean): PUTF8Char;
!var V: TFV absolute aValue;
!begin // '[1,2001,3001,4001,"1","1001"],[2,2002,3002,4002,"2","1002"],...'
!  aValid := false;
!  result := nil;
!  if (P=nil) or (P^<>'[') then
!    exit;
!  inc(P);
!  V.Major := GetNextItemCardinal(P);
!  V.Minor := GetNextItemCardinal(P);
!  V.Release := GetNextItemCardinal(P);
!  V.Build := GetNextItemCardinal(P);
!  V.Main := UTF8ToString(GetJSONField(P,P));
!  V.Detailed := UTF8ToString(GetJSONField(P,P));
!  if P=nil then
!    exit;
!  aValid := true;
!  result := P; // ',' or ']' for last item of array
!end;
The reader method shall return a pointer to the next separator of the JSON input buffer just after this item (either {\f1\fs20 ','} or {\f1\fs20 ']'}).
The registration process itself is as simple as:
!  TTextWriter.RegisterCustomJSONSerializer(TypeInfo(TFVs),
!    TCollTstDynArray.FVReader,TCollTstDynArray.FVWriter);
Then, from the user code point of view, this dynamic array handling won't change: once registered, the JSON serializers are used everywhere in the framework, as soon as this type is globally registered.
Here is a {\i Writer} method using a JSON object layout:
!class procedure TCollTstDynArray.FVWriter2(const aWriter: TTextWriter; const aValue);
!var V: TFV absolute aValue;
!begin
!  aWriter.AddJSONEscape(['Major',V.Major,'Minor',V.Minor,'Release',V.Release,
!    'Build',V.Build,'Main',V.Main,'Detailed',V.Detailed]);
!end;
This will create some JSON content as such:
# {"Major":1,"Minor":2001,"Release":3001,"Build":4001,"Main":"1","Detailed":"1001"}
Then the corresponding {\i Reader} callback could be written as:
!class function TCollTstDynArray.FVReader2(P: PUTF8Char; var aValue;
!  out aValid: Boolean): PUTF8Char;
!var V: TFV absolute aValue;
!    Values: TPUtf8CharDynArray;
!begin
!  aValid := false;
!  result := JSONDecode(P,['Major','Minor','Release','Build','Main','Detailed'],Values);
!  if result=nil then
!    exit; // result^ = ',' or ']' for last item of array
!  V.Major := GetInteger(Values[0]);
!  V.Minor := GetInteger(Values[1]);
!  V.Release := GetInteger(Values[2]);
!  V.Build := GetInteger(Values[3]);
!  V.Main := UTF8DecodeToString(Values[4],StrLen(Values[4]));
!  V.Detailed := UTF8DecodeToString(Values[5],StrLen(Values[5]));
!  aValid := true;
!end;
Most of the JSON decoding process is performed within the {\f1\fs20 JSONDecode()} function, which will let {\f1\fs20 Values[]} point to null-terminated un-escaped content within the {\f1\fs20 P^} buffer. In fact, such process will do only one memory allocation (for {\f1\fs20 Values[]}), and will therefore be very fast.
If you want to go back to the default binary + {\i Base64} encoding serialization, you may run the registering method as such:
!  TTextWriter.RegisterCustomJSONSerializer(TypeInfo(TFVs),nil,nil);
You can define now your custom JSON serializers, starting for the above code as reference.
Note that if the {\i record} corresponding to its item dynamic array has some associated RTTI (i.e. if it contains some reference-counted types, like any {\f1\fs20 string}), it will be serialized as JSON during the {\i mORMot} Service process, just as stated with @51@.
:  Daily use
The {\f1\fs20 TTestLowLevelCommon._TDynArray} and {\f1\fs20 _TDynArrayHashed} methods implement the automated unitary tests associated with these wrappers.
You'll find out there samples of {\i dynamic array} handling and more advanced features, with various kind of data (from plain {\f1\fs20 TIntegeryDynArray} to records within records).
The {\f1\fs20 TDynArrayHashed} wrapper allow implementation of a dictionary using a {\i dynamic array} of record. For instance, the @*prepared@ statement cache is handling by the following code in @!Lib\SynSQLite3.pas@:
!  TSQLStatementCache = record
!    StatementSQL: RawUTF8;
!    Statement: TSQLRequest;
!  end;
!  TSQLStatementCacheDynArray = array of TSQLStatementCache;
!
!  TSQLStatementCached = object
!!    Cache: TSQLStatementCacheDynArray;
!!    Count: integer;
!!    Caches: TDynArrayHashed;
!    DB: TSQLite3DB;
!    procedure Init(aDB: TSQLite3DB);
!    function Prepare(const GenericSQL: RaWUTF8): PSQLRequest;
!    procedure ReleaseAllDBStatements;
!  end;
Those definitions will prepare a {\i dynamic array} storing a {\f1\fs20 TSQLRequest} and {\i SQL statement} association, with an external {\f1\fs20 Count} variable, for better speed.
It will be used as such in {\f1\fs20 TSQLRestServerDB}:
!constructor TSQLRestServerDB.Create(aModel: TSQLModel; aDB: TSQLDataBase);
!begin
!  fStatementCache.Init(aDB);
! (...)
The wrapper will be initialized in the object constructor:
!procedure TSQLStatementCached.Init(aDB: TSQLite3DB);
!begin
!!  Caches.Init(TypeInfo(TSQLStatementCacheDynArray),Cache,nil,nil,nil,@Count);
!  DB := aDB;
!end;
The {\f1\fs20 TDynArrayHashed.Init} method will recognize that the first {\f1\fs20 TSQLStatementCache} field is a {\f1\fs20 @*RawUTF8@}, so will set by default an {\f1\fs20 AnsiString} hashing of this first field (we could specify a custom hash function or content hashing by overriding the default {\f1\fs20 nil} parameters to some custom functions).
So we can specify directly a {\f1\fs20 GenericSQL} variable as the first parameter of {\f1\fs20 FindHashedForAdding}, since this method will only access to the first field {\f1\fs20 RawUTF8} content, and won't handle the whole record content. In fact, the {\f1\fs20 FindHashedForAdding} method will be used to make all the hashing, search, and new item adding if necessary - just in one step. Note that this method only prepare for adding, and code needs to explicitly set the {\f1\fs20 StatementSQL} content in case of an item creation:
!function TSQLStatementCached.Prepare(const GenericSQL: RaWUTF8): PSQLRequest;
!var added: boolean;
!begin
!!  with Cache[Caches.FindHashedForAdding(GenericSQL,added)] do begin
!    if added then begin
!!      StatementSQL := GenericSQL; // need explicit set the content
!      Statement.Prepare(DB,GenericSQL);
!    end else begin
!      Statement.Reset;
!      Statement.BindReset;
!    end;
!    result := @Statement;
!  end;
!end;
The latest method of {\f1\fs20 TSQLStatementCached} will just loop for each statement, and close them: you can note that this code uses the dynamic array just as usual:
!procedure TSQLStatementCached.ReleaseAllDBStatements;
!var i: integer;
!begin
!  for i := 0 to Count-1 do
!    Cache[i].Statement.Close; // close prepared statement
!  Caches.Clear; // same as SetLength(Cache,0) + Count := 0
!end;
The resulting code is definitively quick to execute, and easy to read/maintain.
:16 Enhanced logging
A new @**log@ging mechanism has been introduced with revision 1.13 of the framework. It includes stack trace exception and such, just like {\i MadExcept}, using {\f1\fs20 .map} file content to retrieve debugging information from the source code.
:  Using logging
It's now used by the unit @*test@ing classes, so that any failure will create an entry in the log with the source line, and stack trace:
$C:\Dev\lib\SQLite3\exe\TestSQL3.exe 0.0.0.0 (2011-04-13)
$Host=Laptop User=MyName CPU=2*0-15-1027 OS=2.3=5.1.2600 Wow64=0 Freq=3579545
$TSynLogTest 1.13 2011-04-13 05:40:25
$
$20110413 05402559 fail  TTestLowLevelCommon(00B31D70) Low level common: TDynArray "" stack trace 0002FE0B SynCommons.TDynArray.Init (15148) 00036736 SynCommons.Test64K (18206) 0003682F SynCommons.TTestLowLevelCommon._TDynArray (18214) 000E9C94 TestSQL3 (163)
The difference between a test suit without logging ({\f1\fs20 TSynTests}) and a test suit with logging ({\f1\fs20 TSynTestsLogged}) is only this overridden method:
!procedure TSynTestsLogged.Failed(const msg: string; aTest: TSynTestCase);
!begin
!  inherited;
!  with TestCase[fCurrentMethod] do
!    fLogFile.Log(sllFail,'%: % "%"',
!      [Ident,TestName[fCurrentMethodIndex],msg],aTest);
!end;
The logging mechanism can be used to trace recursive calls. It can use an interface-based mechanism to log when you enter and leave any method:
!procedure TMyDB.SQLExecute(const SQL: RawUTF8);
!var ILog: ISynLog;
!begin
!  ILog := TSynLogDB.Enter(self,'SQLExecute');
!  // do some stuff
!  ILog.Log(sllInfo,'SQL=%',[SQL]);
!end; // when you leave the method, it will write the corresponding event to the log
It will be logged as such:
$20110325 19325801  +    MyDBUnit.TMyDB(004E11F4).SQLExecute
$20110325 19325801 info   SQL=SELECT * FROM Table;
$20110325 19325801  -    MyDBUnit.TMyDB(004E11F4).SQLExecute
Note that by default you have human-readable {\i time and date} written to the log, but it's also possible to replace this timing with {\i high-resolution timestamps}. With this, you'll be able to profile your application with data coming from the customer side, on its real computer. Via the {\f1\fs20 Enter} method (and its {\i auto-Leave} feature), you have all information needed for this.
:  Including symbol definitions
In the above logging content, the method name is set in the code (as {\f1\fs20 'SQLExecute'}). But if the logger class is able to find a {\f1\fs20 .map} file associated to the {\f1\fs20 .exe}, the logging mechanism is able to read this symbol information, and write the exact line number of the event.
In the following log entries, you'll see both high-resolution time stamp, and the entering and leaving of a {\f1\fs20 TTestCompression.TestLog} method traced with no additional code (with accurate line numbers, extracted from the {\f1\fs20 .map} content):
$0000000000000B56  +    TTestCompression(00AB3570).000E6C79 SynSelfTests.TTestCompression.TestLog (376)
$0000000000001785  -    TTestCompression(00AB3570).000E6D09 SynSelfTests.TTestCompression.TestLog (385)
There is already a dedicated {\f1\fs20 TSynLogFile} class able to read the {\f1\fs20 .log} file, and recognize its content.
The first time the {\f1\fs20 .map} file is read, a {\f1\fs20 .mab} file is created, and will contain all symbol information needed. You can send the {\f1\fs20 .mab} file with the {\f1\fs20 .exe} to your client, or even embed its content to the {\f1\fs20 .exe} (see the {\f1\fs20 Map2Mab.dpr} sample file located in the {\f1\fs20 Samples\\11 - Exception logging\\} folder).
This {\f1\fs20 .mab} file is very optimized: for instance, a {\f1\fs20 .map} of 927,984 bytes compresses into a 71,943 {\f1\fs20 .mab} file.
You have several debugging levels available. And even 4 custom types. It's worth saying that the logging level is a SET, and not an @*enumerated@: that is, you can select several kind of logging information to be logged at once, on request:
!  TSynLogInfo = (
!    sllNone, sllInfo, sllDebug, sllTrace, sllWarning, sllError,
!    sllEnter, sllLeave,
!    sllLastError, sllException, sllExceptionOS, sllMemory, sllStackTrace,
!    sllFail, sllSQL, sllCache, sllResult, sllDB, sllHTTP, sllClient, sllServer,
!    sllServiceCall, sllServiceReturn, sllUserAuth,
!    sllCustom1, sllCustom2, sllCustom3, sllCustom4);
!
!  /// used to define a logging level
!  // - i.e. a combination of none or several logging event
!  // - e.g. use LOG_VERBOSE constant to log all events
!  TSynLogInfos = set of TSynLogInfo;
Here are the purpose of each logging level:
- {\f1\fs20 sllInfo} will log general information events;
- {\f1\fs20 sllDebug} will log detailed debugging information;
- {\f1\fs20 sllTrace} will log low-level step by step debugging information;
- {\f1\fs20 sllWarning} will log unexpected values (not an error);
- {\f1\fs20 sllError} will log errors;
- {\f1\fs20 sllEnter} will log every method start;
- {\f1\fs20 sllLeave} will log every method quit;
- {\f1\fs20 sllLastError} will log the {\f1\fs20 GetLastError} OS message;
- {\f1\fs20 sllException} will log all exception raised - available since Windows XP;
- {\f1\fs20 sllExceptionOS} will log all OS low-level exceptions ({\f1\fs20 EDivByZero, ERangeError, EAccessViolation}...);
- {\f1\fs20 sllMemory} will log memory statistics;
- {\f1\fs20 sllStackTrace} will log caller's stack trace (it's by default part of {\f1\fs20 TSynLogFamily. LevelStackTrace} like {\f1\fs20 sllError, sllException, sllExceptionOS, sllLastError} and {\f1\fs20 sllFail});
- {\f1\fs20 sllFail} was defined for {\f1\fs20 TSynTestsLogged. Failed} method, and can be used to log some customer-side assertions (may be notifications, not errors);
- {\f1\fs20 sllSQL} is dedicated to trace the SQL statements;
- {\f1\fs20 sllCache} should be used to trace any internal caching mechanism (it's used for instance by our SQL statement caching);
- {\f1\fs20 sllResult} could trace the SQL results, JSON encoded;
- {\f1\fs20 sllDB} is dedicated to trace low-level database engine features;
- {\f1\fs20 sllHTTP} could be used to trace HTTP process;
- {\f1\fs20 sllClient/sllServer} could be used to trace some Client or Server process;
- {\f1\fs20 sllServiceCall/sllServiceReturn} to trace some remote service or library;
- {\f1\fs20 sllUserAuth} to trace user authentication (e.g. for individual requests);
- {\f1\fs20 sllCustom*} items can be used for any purpose by your programs.
:  Exception handling
Of course, this @*log@ging mechanism is able to intercept the raise of exceptions, including the worse (e.g. {\f1\fs20 EAccessViolation}), to be logged automatically in the log file, as such:
$000000000000090B EXCOS EAccessViolation (C0000005) at 000E9C7A SynSelfTests.Proc1 (785)  stack trace 000E9D51 SynSelfTests.Proc2 (801) 000E9CC1 SynSelfTests.Proc1 (790) 000E9D51 SynSelfTests.Proc2 (801) 000E9CC1 SynSelfTests.Proc1 (790) 000E9D51 SynSelfTests.Proc2 (801) 000E9CC1 SynSelfTests.Proc1 (790) 000E9D51 SynSelfTests.Proc2 (801) 000E9CC1 SynSelfTests.Proc1 (790) 000E9D51 SynSelfTests.Proc2 (801) 000E9CC1 SynSelfTests.Proc1 (790) 000E9E2E SynSelfTests.TestsLog (818) 000EA0FB SynSelfTests (853) 00003BF4 System.InitUnits 00003C5B System.@StartExe 000064AB SysInit.@InitExe 000EA3EC TestSQL3 (153)
The {\f1\fs20 TSynLogInfo} logging level makes a difference between high-level Delphi exceptions ({\f1\fs20 sllException}) and lowest-level OS exceptions ({\f1\fs20 sllExceptionOS}) like {\f1\fs20 EAccessViolation}.
:   Intercepting exceptions
In order to let our {\f1\fs20 TSynLog} logging class intercept all exceptions, we use the low-level global {\f1\fs20 RtlUnwindProc} pointer, defined in {\f1\fs20 System.pas}.
Alas, under Delphi 5, this global {\f1\fs20 RtlUnwindProc} variable is not existing. The code calls directly the {\f1\fs20 RtlUnWind} Windows API function, with no hope of custom interception.
Two solutions could be envisaged:
- Modify the {\f1\fs20 Sytem.pas} source code, adding the new {\f1\fs20 RtlUnwindProc} variable, just like Delphi 7;
- Patch the assembler code, directly in the process memory.
The first solution is simple. Even if compiling {\f1\fs20 System.pas} is a bit more difficult than compiling other units, we already made that for our {\i Enhanced RTL units}. But you'll have to change the whole build chain in order to use your custom {\f1\fs20 System.dcu} instead of the default one. And some third-party units (only available in {\f1\fs20 .dcu} form) may not like the fact that the {\i System.pas} interface changed...
So we used the second solution: change the assembler code in the running process memory, to let call our {\f1\fs20 RtlUnwindProc} variable instead of the Windows API.
:   One patch to rule them all
The first feature we have to do is to allow on-the-fly change of the assembler code of a process.
In fact, we already use this in order to provide class-level variables, as stated by @SDD-DI-2.1.3@.
We have got the {\f1\fs20 PatchCodePtrUInt} function at hand to change the address of each a {\f1\fs20 RtlUnWind} call.
We'll first define the missing global variable, available since Delphi 6, for the Delphi 5 compiler:
!{$ifdef DELPHI5OROLDER}
!// Delphi 5 doesn't define the needed RTLUnwindProc variable :(
!// so we will patch the System.pas RTL in-place
!var
!  RTLUnwindProc: Pointer;
The {\f1\fs20 RtlUnwind} API call we have to hook is defined as such in {\f1\fs20 System.pas}:
!procedure RtlUnwind; external kernel name 'RtlUnwind';
$ 0040115C FF255CC14100     jmp dword ptr [$0041c15c]
The {\f1\fs20 $0041c15c} is a pointer to the address of {\f1\fs20 RtlUnWind} in {\f1\fs20 kernel32.dll}, as retrieved during linking of this library to the main executable process.
The patch will consist in changing this asm call into this one:
$ 0040115C FF25????????     jmp dword ptr [RTLUnwindProc]
Where {\f1\fs20 ????????} is a pointer to the global {\f1\fs20 RTLUnwindProc} variable.
The problem is that we do not have any access to this {\f1\fs20 procedure RtlUnwind} declaration, since it was declared only in the {\f1\fs20 implementation} part of the {\f1\fs20 System.pas} unit. So its address has been lost during the linking process.
So we will have to retrieve it from the code which in fact calls this external API, i.e. from this assembler content:
!procedure       _HandleAnyException;
!asm
$    (...)
$    004038B6 52               push edx  // Save exception object
$    004038B7 51               push ecx  // Save exception address
$    004038B8 8B542428         mov edx,[esp+$28]
$    004038BC 83480402         or dword ptr [eax+$04],$02
$    004038C0 56               push esi  // Save handler entry
$    004038C1 6A00             push $00
$    004038C3 50               push eax
$    004038C4 68CF384000       push $004038cf  // @@returnAddress
$    004038C9 52               push edx
$    004038CA E88DD8FFFF       call RtlUnwind
So we will retrieve the {\f1\fs20 RtlUnwind} address from this very last line.
The {\f1\fs20 E8} byte is in fact the {\i opcode} for the asm {\f1\fs20 call} instruction. Then the called function is stored as an {\i integer} offset, starting from the current pointing value.
The {\f1\fs20 E8 8D D8 FF FF} byte sequence is executed as "{\i call the function available at the current execution address, plus {\f1\fs20 integer($ffffd88d)}}". As you may have guessed, {\f1\fs20 $004038CA+$ffffd88d+5} points to the {\f1\fs20 RtlUnwind} definition.
So here is the main function of this patching:
!procedure Patch(P: PAnsiChar);
!var i: Integer;
!    addr: PAnsiChar;
!begin
!  for i := 0 to 31 do
!    if (PCardinal(P)^=$6850006a) and  // push 0; push eax; push @@returnAddress
!       (PWord(P+8)^=$E852) then begin // push edx; call RtlUnwind
!      inc(P,10); // go to call RtlUnwind address
!      if PInteger(P)^<0 then begin
!        addr := P+4+PInteger(P)^;
!        if PWord(addr)^=$25FF then begin // jmp dword ptr []
!          PatchCodePtrUInt(Pointer(addr+2),cardinal(@RTLUnwindProc));
!          exit;
!        end;
!      end;
!    end else
!    inc(P);
!end;
We will cal this {\f1\fs20 Patch} subroutine from the following code:
!procedure PatchCallRtlUnWind;
!asm
!  mov eax,offset System.@HandleAnyException+200
!  call Patch
!end;
You can note that we need to retrieve the {\f1\fs20 _HandleAnyException} address from asm code. In fact, the compiler does not let access from plain pascal code to the functions of {\f1\fs20 System.pas} having a name beginning with an underscore.
Then the following lines:
!  for i := 0 to 31 do
!    if (PCardinal(P)^=$6850006a) and  // push 0; push eax; push @@returnAddress
!       (PWord(P+8)^=$E852) then begin // push edx; call RtlUnwind
will look for the expected opcode asm pattern in {\f1\fs20 _HandleAnyException} routine.
Then we will compute the position of the {\f1\fs20 jmp dword ptr []} call, via this line:
!        addr := P+4+PInteger(P)^;
After checking that this is indeed a {\f1\fs20 jmp dword ptr []} instruction (expected opcodes are {\f1\fs20 FF 25}), we will simply patch the absolute address with our {\f1\fs20 RTLUnwindProc} procedure variable.
With this code, each call to {\f1\fs20 RtlUnwind} in {\f1\fs20 System.pas} will indeed call the function set by {\f1\fs20 RTLUnwindProc}.
In our case, it will launch the following procedure:
!procedure SynRtlUnwind(TargetFrame, TargetIp: pointer;
!  ExceptionRecord: PExceptionRecord; ReturnValue: Pointer); stdcall;
!asm
!  pushad
!  cmp  byte ptr SynLogExceptionEnabled,0
!  jz   @oldproc
!  mov  eax,TargetFrame
!  mov  edx,ExceptionRecord
!  call LogExcept
!@oldproc:
!  popad
!  pop ebp // hidden push ebp at asm level
!{$ifdef DELPHI5OROLDER}
!  jmp RtlUnwind
!{$else}
!  jmp oldUnWindProc
!{$endif}
!end;
This code will therefore:
- Save the current register context via {\f1\fs20 pushad / popad} opcodes pair;
- Check if {\f1\fs20 TSynLog} should intercept exceptions (i.e. if the global {\f1\fs20 SynLogExceptionEnabled} boolean is true);
- Call our logging function {\f1\fs20 LogExcept};
- Call the default Windows {\f1\fs20 RtlUnwind} API, as expected by the Operating System.
:  Serialization
{\i @*dynamic array@s} can also be serialized as @*JSON@ in the log on request, via the default {\f1\fs20 TSynLog} class, as defined in {\f1\fs20 SynCommons} unit - see @48@.
The {\f1\fs20 TSQLLog} class (using the enhanced @*RTTI@ methods defined in {\f1\fs20 SQLite3Commons} unit) is even able to serialize {\f1\fs20 @*TSQLRecord@, @*TPersistent@, TList} and {\f1\fs20 @*TCollection@} instances as JSON, or any other class instance, after call to {\f1\fs20 TJSONSerializer. @*RegisterCustomSerializer@}.
For instance, the following code:
!procedure TestPeopleProc;
!var People: TSQLRecordPeople;
!    Log: ISynLog;
!begin
!  Log := TSQLLog.Enter;
!  People := TSQLRecordPeople.Create;
!  try
!    People.ID := 16;
!    People.FirstName := 'Louis';
!    People.LastName := 'Croivébaton';
!    People.YearOfBirth := 1754;
!    People.YearOfDeath := 1793;
!    Log.Log(sllInfo,People);
!  finally
!    People.Free;
!  end;
!end;
will result in the following log content:
$0000000000001172  +    000E9F67 SynSelfTests.TestPeopleProc (784)
$000000000000171B info      {"TSQLRecordPeople(00AB92E0)":{"ID":16,"FirstName":"Louis","LastName":"Croivébaton","Data":"","YearOfBirth":1754,"YearOfDeath":1793}}
$0000000000001731  -    000EA005 SynSelfTests.TestPeopleProc (794)
For instance, if you add to your program:
!uses
!  SynCommons;
!(...)
!  TSynLog.Family.Level := [sllExceptionOS];
all OS exceptions (excluding pure Delphi exception like {\f1\fs20 EConvertError} and such) will be logged to a separate log file.
!TSynLog.Family.Level := [sllException,sllExceptionOS];
will trace also Delphi exceptions, for instance.
:  Family matters
You can have several @*log@ files per process, and even a per-thread log file, if needed (it could be sometimes handy, for instance on a server running the same logic in parallel in several threads).
The logging settings are made at the logging class level. Each logging class (inheriting from {\f1\fs20 TSynLog}) has its own {\f1\fs20 TSynLogFamily} instance, which is to be used to customize the logging class level. Then you can have several instances of the individual {\f1\fs20 TSynLog} classes, each class sharing the settings of the {\f1\fs20 TSynLogFamily}.
You can therefore initialize the "family" settings before using logging, like in this code which will force to log all levels ({\f1\fs20 LOG_VERBOSE}), and create a per-thread log file, and write the {\f1\fs20 .log} content not in the {\f1\fs20 .exe} folder, but in a custom directory:
! with TSynLogDB.Family do
! begin
!   Level := LOG_VERBOSE;
!   PerThreadLog := true;
!   DestinationPath := 'C:\Logs';
! end;
:  Automated log archival
Log archives can be created with the following settings:
! with TSynLogDB.Family do
! begin
!  (...)
!  OnArchive := EventArchiveZip;
!  ArchivePath := '\\Remote\WKS2302\Archive\Logs'; // or any path
! end;
The {\f1\fs20 ArchivePath} property can be set to several functions, taking a timeout delay from the {\f1\fs20 ArchiveAfterDays} property value:
- {\f1\fs20 nil} is the default value, and won't do anything: the {\f1\fs20 .log} will remain on disk until they will be deleted by hand;
- {\f1\fs20 EventArchiveDelete} in order to delete deprecated {\f1\fs20 .log} files;
- {\f1\fs20 EventArchiveSynLZ} to compress the {\f1\fs20 .log} file into a proprietary {\i SynLZ} format: resulting file name will be located in {\f1\fs20 ArchivePath\\log\\YYYYMM\\*.log.synlz}, and the command-line {\f1\fs20 UnSynLz.exe} tool (calling {\f1\fs20 FileUnSynLZ} function of {\f1\fs20 SynCommons} unit) can be used to uncompress it in to plain {\f1\fs20 .log} file;
- {\f1\fs20 SynZip.EventArchiveZip} will archive the {\f1\fs20 .log} files in {\f1\fs20 ArchivePath\\log\\YYYYMM.zip} files, grouping every .
{\i SynLZ} files are less compressed, but created much faster than {\f1\fs20 .zip} files. However, {\f1\fs20 .zip} files are more standard, and on a regular application, compression speed won't be an issue for the application.
:  Log Viewer
Since the log files tend to be huge (for instance, if you set the logging for our unitary tests, the 7,000,000 unitary tests create a 320 MB log file), a log viewer was definitively in need.
The log-viewer application is available as source code in the "{\i Samples}" folder, in the "{\i 11 - Exception logging}" sub-folder.
:   Open log files
You can run it with a specified log file on the command line, or use the "{\i Open}" button to browse for a file. That is, you can associate this tool with your {\f1\fs20 .log} files, for instance, and you'll open it just by double-clicking on such files.
Note that if the file is not in our {\f1\fs20 TSynLog} format, it will still be opened as plain text. You'll be able to browse its content and search within, but all the nice features of our logging won't be available, of course.
It's worth saying that the viewer was designed to be {\i fast}.\line In fact, it takes no time to open any log file. For instance, a 320 MB log file is opened in less than one second on my laptop. Under Windows Seven, it takes more time to display the "Open file" dialog window than reading and indexing the 320 MB content.\line It uses internally memory mapped files and optimized data structures to access to the data as fast as possible - see {\f1\fs20 TSynLogFile} class.
:   Log browser
The screen is divided into three main spaces:
- On the left side, the panel of commands;
- On the right side, the log events list;
- On the middle, an optional list of method calls (not shown by default).
The command panel allows to {\i Open} a {\f1\fs20 .log} file, see the global {\i Stats} about its content (customer-side hardware and software running configuration, general numbers about the log), and even ask for a source code line number and unit name from an hexadecimal address available in the log, by browsing for the corresponding {\f1\fs20 .map} file (could be handy if you did not deliver the {\f1\fs20 .map} content within your main executable - which you should have to, IMHO).
Just below the "{\i Open}" button, there is an edit field available, with a ? button. Enter any text within this edit field, and it will be searched within the log events list. Search is case-insensitive, and was designed to be fast. Clicking on the ? button (or pressing the {\f1\fs20 F3} key) allows to repeat the last search.
In the very same left panel, you can see all existing events, with its own color and an associated check-box. Note that only events really encountered in the {\f1\fs20 .log} file appear in this list, so its content will change between log files. By selecting / un-selecting a check-box, the corresponding events will be instantaneously displayed / or not on the right side list of events. You can click on the {\i Filter} button (or right click on the events check-box list) to select a predefined set of events.
The right colored event list follows the events appended to the log, by time order. When you click on an event, its full line content is displayed at the bottom on the screen, in a memo.
Having all @*SQL@ and @*Client-Server@ events traced in the log is definitively a huge benefit for customer support and bug tracking.
:   Customer-side profiler
One distinctive feature of the {\f1\fs20 TSynLog} logging class is that it is able to map methods or functions entering/leaving (using the {\f1\fs20 Enter} method), and trace this into the logs. The corresponding timing is also written within the "{\i Leave}" event, and allows application profiling from the customer side. Most of the time, profiling an application is done during the testing, with a test environment and database. But this is not, and will never reproduce the exact nature of the customer use: for instance, hardware is not the same (network, memory, CPU), nor the software (Operating System version, [anti-]virus installed)... By enabling customer-side method profiling, the log will contain all relevant information. Those events are named "{\i Enter}" / "{\i Leave}" in the command panel check-box list, and written as + and - in the right-sided event list.
The "{\i Methods profiler}" options allow to display the middle optional method calls list. Several sort order are available: by name (alphabetical sort), by occurrence (in running order, i.e. in the same order than in the event log), by time (the full time corresponding to this method, i.e. the time written within the "{\i Leave}" event), and by proper time (i.e. excluding all time spent in the nested methods).
The "{\i Merge method calls}" check-box allows to regroup all identical method calls, according to their name. In fact, most methods are not called once, but multiple time. And this is the accumulated time spent in the method which is the main argument for code profiling.
I'm quite sure that the first time you'll use this profiling feature on a huge existing application, you'll find out some bottlenecks you would have never thought about before.

[SAD-SynFile]
SourcePath=Lib\SQLite3\Samples\MainDemo
IncludePath=Lib;Lib\SQLite3;Lib\SQLite3\Samples\MainDemo
SourceFile=SynFile.dpr
Version=1.17
DisplayName=Main SynFile Demo

:50SynFile application
This sample application is a simple database tool which stores text content and files into the database, in both clear and "safe" manner. Safe records are stored using {\i AES-256/SHA-256} encryption. There is an {\i Audit Trail} table for tracking the changes made to the database.
This document will follow the application architecture and implementation, in order to introduce the reader to some main aspects of the Framework:
- General architecture - see @7@;
- Database design - see @13@;
- User Interface generation.
We hope this part of the @SAD@ would be able to be a reliable guideline for using our framework for your own projects.
:General architecture
According to the Multi-tier architecture, some units will define the three layers of the {\i SynFile} application:
{\b Database Model}
First, the database tables are defined as regular Delphi classes, like a true @*ORM@ framework. Classes are translated to database tables. @*Published properties@ of these classes are translated to table fields. No external configuration files to write - only Delphi code. Nice and easy. See @!TSQLFile,TSQLMemo,TSQLData,TSQLSafeMemo,TSQLSafeData,TSQLAuditTrail!Lib\SQLite3\Samples\MainDemo\FileTables.pas@ unit.
This unit is shared by both client and server sides, with a shared data model, i.e. a {\f1\fs20 @*TSQLModel@} class instance, describing all ORM tables/classes.
It contains also internal event descriptions, and actions, which will be used to describe the software UI.
{\b Business Logic}
The {\i server side} is defined in a dedicated class, which implements an automated Audit Trail, and a @*service@ named "Event" to easily populate the Audit Trail from the Client side. See @!TFileServer!Lib\SQLite3\Samples\MainDemo\FileServer.pas@ unit.
The {\i client side} is defined in another class, which is able to communicate with the server, and fill/update/delete/add the database content playing with classes instances. It's also used to call the Audit Trail related service, and create the reports. See @!TFileClient!Lib\SQLite3\Samples\MainDemo\FileClient.pas@ unit.
You'll see that @*BLOB@ fields are handled just like other fields, even if they use their own @*REST@ful GET/PUT dedicated URI (they are not @*JSON@ encoded, but transmitted as raw data, to save bandwidth and maintain the RESTful model). The framework handles it for you, thanks to its ORM orientation, and the {\f1\fs20 ForceBlobTransfert := true} line in {\f1\fs20 TFileClient. Create} method.
{\b Presentation Layer}
The main form of the Client is void, if you open its {\f1\fs20 FileMain.dfm} file. All the User Interface is created by the framework, dynamically from the database model and some constant values and enumeration types (thanks to Delphi @*RTTI@) as defined in @!TFileRibbonTabParameters.Actions!Lib\SQLite3\Samples\MainDemo\FileTables.pas@ unit (the first one, which defines also the classes/tables).
It's main method is {\f1\fs20 TMainForm.ActionClick}, which will handle the actions, triggered when a button is pressed.
The reports use {\i GDI+} for anti-aliased drawing, can be zoomed and saved as @*pdf@ or text files.
The last @!TEditForm!Lib\SQLite3\Samples\MainDemo\FileEdit.pas@ unit is just the form used for editing the data. It also performs the encryption of "safe memo" and "safe data" records, using our @!TAESFull.EncodeDecode!Lib\SynCrypto.pas@ unit.
You'll discover how the @*ORM@ plays its role here: you change the data, just like changing any class instance properties.
It also uses our @!SaveAsRawByteString!Lib\SynGdiPlus.pas@ unit to create thumbnails of any picture ({\f1\fs20 emf+jpg+tif+gif+bmp}) of data inserted in the database, and add a @*BLOB@ data field containing these thumbnails.
:Database design
The @!TSQLFile,TSQLMemo,TSQLData,TSQLSafeMemo,TSQLSafeData,TSQLAuditTrail!Lib\SQLite3\Samples\MainDemo\FileTables.pas@ unit is implementing all {\f1\fs20 @*TSQLRecord@} child classes, able to create the database tables, using the @*ORM@ aspect of the framework - see @13@. The following class hierarchy was designed:
\graph HierSynFileRecord SynFile TSQLRecord classes hierarchy
\TSQLAuditTrail\TSQLRecord
\TSQLSafeMemo\TSQLData
\TSQLData\TSQLFile
\TSQLSafeData\TSQLData
\TSQLMemo\TSQLFile
\TSQLFile\TSQLRecordSigned
\TSQLRecordSigned\TSQLRecord
\
Most common @*published properties@ (i.e. {\f1\fs20 Name, Created, Modified, Picture, KeyWords}) are taken from the {\f1\fs20 TSQLFile} abstract parent class. It's called "{\i abstract}", not in the current Delphi @*OOP@ terms, but as a class with no "real" database table associated. It was used to defined the properties only once, without the need of writing the private variables nor the getter/setter for children classes. Only {\f1\fs20 TSQLAuditTrail} won't inherit from this parent class, because it's purpose is not to contain data, but just some information.
The database itself will define {\f1\fs20 TSQLAuditTrail, TSQLMemo, TSQLData, TSQLSafeMemo}, and {\f1\fs20 TSQLSafeData} classes. They will be stored as {\i AuditTrail, Memo, Data, SafeMemo} and {\i SafeData} tables in the {\i SQlite3} database (the table names are extract from the class name, trimming the left '{\f1\fs20 TSQL}' characters).
Here is this common ancestor type declaration:
!  TSQLFile = class(TSQLRecordSigned)
!  public
!    fName: RawUTF8;
!    fModified: TTimeLog;
!    fCreated: TTimeLog;
!    fPicture: TSQLRawBlob;
!    fKeyWords: RawUTF8;
!  published
!    property Name: RawUTF8 read fName write fName;
!    property Created: TTimeLog read fCreated write fCreated;
!    property Modified: TTimeLog read fModified write fModified;
!    property Picture: TSQLRawBlob read fPicture write fPicture;
!    property KeyWords: RawUTF8 read fKeyWords write fKeyWords;
!  end;
Sounds like a regular Delphi class, doesn't it? The only fact to be noticed is that it does not inherit from a {\f1\fs20 @*TPersistent@} class, but from a {\f1\fs20 @*TSQLRecord@} class, which is the parent object type to be used for our ORM. The {\f1\fs20 TSQLRecordSigned} class type just defines some {\f1\fs20 Signature} and {\f1\fs20 SignatureTime} additional properties, which will be used here for handling digital signing of records.
Here follows the Delphi code written, and each corresponding database field layout of each registered class:
!  TSQLMemo = class(TSQLFile)
!  public
!    fContent: RawUTF8;
!  published
!    property Content: RawUTF8 read fContent write fContent;
!  end;
\graph DBMemo Memo Record Layout
rankdir=LR;
node [shape=Mrecord];
struct1 [label="ID : integer|Content : RawUTF8|Created : TTimeLog|KeyWords : RawUTF8|Modified : TTimeLog|Name : RawUTF8|Picture : TSQLRawBlob|Signature : RawUTF8|SignatureTime: TTimeLog"];
\
!  TSQLData = class(TSQLFile)
!  public
!    fData: TSQLRawBlob;
!  published
!    property Data: TSQLRawBlob read fData write fData;
!  end;
\graph DBData Data Record Layout
rankdir=LR;
node [shape=Mrecord];
struct1 [label="ID : integer|Data : TSQLRawBlob|Created : TTimeLog|KeyWords : RawUTF8|Modified : TTimeLog|Name : RawUTF8|Picture : TSQLRawBlob|Signature : RawUTF8|SignatureTime: TTimeLog"];
\
!  TSQLSafeMemo = class(TSQLData);
\graph DBSafeMemo SafeMemo Record Layout
rankdir=LR;
node [shape=Mrecord];
struct1 [label="ID : integer|Data : TSQLRawBlob|Created : TTimeLog|KeyWords : RawUTF8|Modified : TTimeLog|Name : RawUTF8|Picture : TSQLRawBlob|Signature : RawUTF8|SignatureTime: TTimeLog"];
\
!  TSQLSafeData = class(TSQLData);
\graph DBSafeData SafeData Record Layout
rankdir=LR;
node [shape=Mrecord];
struct1 [label="ID : integer|Data : TSQLRawBlob|Created : TTimeLog|KeyWords : RawUTF8|Modified : TTimeLog|Name : RawUTF8|Picture : TSQLRawBlob|Signature : RawUTF8|SignatureTime: TTimeLog"];
\
You can see that {\f1\fs20 TSQLSafeMemo} and {\f1\fs20 TSQLSafeData} are just a direct sub-class of {\f1\fs20 TSQLData} to create "{\i SafeMemo}" and "{\i SafeData}" tables with the exact same fields as the "{\i Data}" table. Since they were declared as {\f1\fs20 class(TSQLData)}, they are some new class type,
Then the latest class is not inheriting from {\f1\fs20 TSQLFile}, because it does not contain any user data, and is used only as a log of all actions performed using {\i SynFile}:
!  TSQLAuditTrail = class(TSQLRecord)
!  protected
!    fStatusMessage: RawUTF8;
!    fStatus: TFileEvent;
!    fAssociatedRecord: TRecordReference;
!    fTime: TTimeLog;
!  published
!    property Time: TTimeLog read fTime write fTime;
!    property Status: TFileEvent read fStatus write fStatus;
!    property StatusMessage: RawUTF8 read fStatusMessage write fStatusMessage;
!    property AssociatedRecord: TRecordReference read fAssociatedRecord write fAssociatedRecord;
!  end;
\graph DBAuditTrail AuditTrail Record Layout
rankdir=LR;
node [shape=Mrecord];
struct1 [label="ID : integer|AssociatedRecord : TRecordReference|Status : TFileEvent|StatusMessage : RawUTF8|Time : TTimeLog"];
\
The {\f1\fs20 AssociatedRecord} property was defined as {\f1\fs20 TRecordReference}. This special type (mapped as an INTEGER field in the database) is able to define a "one to many" relationship with ANY other record of the database model.
- If you want to create a "one to many" relationship with a particular table, you should define a property with the corresponding {\f1\fs20 @*TSQLRecord@} sub-type (for instance, if you want to link to a particular {\i SafeData} row, define the property as {\f1\fs20 AssociatedData: TSQLSafeData;}) - in this case, this will create an INTEGER field in the database, holding the {\i RowID} value of the associated record (and this field content will be filled with {\f1\fs20 pointer(RowID)} and not with a real {\f1\fs20 TSQLSafeData} instance).
- Using a {\f1\fs20 TRecordReference} type will not link to a particular table, but any table of the database model: it will store in its associated INTEGER database field not only the {\i RowID} of the record, but also the table index as registered at {\f1\fs20 @*TSQLModel@} creation. In order to access this {\f1\fs20 AssociatedRecord} property content, you could use either {\f1\fs20 @*TSQLRest@. Retrieve(AssociatedRecord)} to get the corresponding record instance, or typecast it to {\f1\fs20 RecordRef(AssociatedRecord)} to easily retrieve or set the associated table and {\i RowID}. You could also use the {\f1\fs20 TSQLRecord. RecordReference(Model)} method in order to get the value corresponding to an existing {\f1\fs20 TSQLRecord} instance.
According to the @*MVC@ model - see @10@ - the framework expect a common database model to be shared between client and server. A common function has been defined in the @!CreateFileModel!Lib\SQLite3\Samples\MainDemo\FileTables.pas@ unit, as such:
!function CreateFileModel(Owner: TSQLRest): TSQLModel;
We'll see later its implementation. Just note for the moment that it will register the {\f1\fs20 TSQLAuditTrail, TSQLMemo, TSQLData, TSQLSafeMemo}, and {\f1\fs20 TSQLSafeData} classes as part of the database model. The order of the registration of those classes will be used for the {\f1\fs20 AssociatedRecord: TRecordReference} field of {\f1\fs20 TSQLAuditTrail} - e.g. a {\f1\fs20 TSQLMemo} record will be identified with a table index of 1 in the {\f1\fs20 RecordReference} encoded value. So it's mandatory to NOT change this order in any future modification of the database schema, without providing any explicit database content conversion mechanism.
Note that all above graphs were created directly from the {\i SynProject}, which is able to create custom graphs from the application source code it parsed.
:31User Interface generation
You could of course design your own User Interface without our framework. That is, this is perfectly feasible to use only the @*ORM@ part of it. For instance, it should be needed to develop @*AJAX@ applications using its @*REST@ful model - see @9@ - since such a feature is not yet integrated to our provided source code.
But for producing easily applications, the framework provides a mechanism based on both ORM description and @*RTTI@ compiler-generated information in order to create most User Interface by code.
It is able to generated a Ribbon-based application, in which each table is available via a Ribbon tab, and some actions performed to it.
So the framework would need to know:
- Which tables must be displayed;
- Which actions should be associated with each table;
- How the User Interface should be customized (e.g. hint texts, grid layout on screen, reporting etc...);
- How generic automated edition, using the @!TRecordEditForm!Lib\SQLite3\SQLite3UIEdit.pas@ unit, is to be generated.
To this list could be added an integrated event feature, which can be linked to actions and custom status, to provide a centralized handling of user-level @*log@ing (as used e.g. in the {\i SynFile} {\f1\fs20 TSQLAuditTrail} table) - please do not make confusion between this user-level logging and technical-level logging using {\f1\fs20 TSynLog} and {\f1\fs20 TSQLLog} classes and "families" - see @16@.
: Rendering
The current implementation of the framework User Interface generation handles two kind of rendering:
- Native VCL components;
- Proprietary TMS components.
You can select which set of components are used, by defining - globally to your project (i.e. in the {\i Project/Options/Conditionals} menu) - the {\f1\fs20 USETMSPACK} conditional. If it is not set (which is by default), it will use VCL components.
The native VCL components will use native Windows API components. So the look and feel of the application will vary depending on the Windows version it is running on. For instance, the resulting screen will be diverse if the application is run under Windows 2000, XP, Vista and Seven. The "ribbon" as generated with VCL components has most functionalities than the Office 2007/2010 ribbon, but will have a very diverse layout.
The TMS components will have the same rendering whatever the Windows it's running on, and will display a "ribbon" very close to the official Office 2007/2010 version.
Here are some PROs and CONs about both solutions:
|%35%30%30
|\b Criteria|VCL|TMS\b0
|Rendering|Basic|Sophisticated
|OS version|Variant|Constant
|Ribbon look|Unusual|Office-like
|Preview button & Shortcuts|None by default|Available
|Extra Price|None|High
|GPL ready|Yes|No
|Office UI Licensing|N/A|Required
|EXE size|Smaller|Bigger
|%
It's worth saying that the choice of one or other component set could be changed on request. If you use the generic components as defined in {\f1\fs20 SQLite3ToolBar} (i.e. the {\f1\fs20 TSynForm, TSynToolBar, TSynToolButton, TSynPopupMenu, TSynPage, TSynPager, TSynBodyPager} and {\f1\fs20 TSynBodyPage} classes) and {\f1\fs20 SynTaskDialog} (for {\f1\fs20 TSynButton}) in your own code, the {\f1\fs20 USETMSPACK} conditional will do all the magic for you.
The {\i Office UI licensing program} was designed by {\i Microsoft} for software developers who wish to implement the Office UI as a software component and/or incorporate the Office UI into their own applications. If you use TMS ribbon, it will require acceptance of the Office UI License terms as defined at @http://msdn.microsoft.com/en-us/office/aa973809.aspx
If you want to design your user interface using a Office 2007/2010 ribbon look, please take a look at those official guidelines: @http://msdn.microsoft.com/en-us/library/cc872782.aspx
Here is the screen content, using the TMS components:
%synfiletms.png
And here is the same application compiled using only VCL components, available from Delphi 6 up to XE2:
%synfilevcl.png
We did not use yet the Ribbon component as was introduced in Delphi 2009. Its action-driven design won't make it easy to interface with the event-driven design of our User Interface handling, and we have to confess that this component has rather bad reputation (at least in the Delphi 2009 version). Feel free to adapt our Open Source code to use it - we'll be very pleased to release a new version supporting it, but we don't have time nor necessity to do it by ourself.
: Enumeration types
A list of available actions should be defined, as an enumeration type:
!  TFileAction = (
!    faNoAction, faMark, faUnmarkAll, faQuery, faRefresh, faCreate,
!    faEdit, faCopy, faExport, faImport, faDelete, faSign, faPrintPreview,
!    faExtract, faSettings );
Thanks to the Delphi @*RTTI@, and "{\i Un @*Camel@ Casing}", the following list will generate a set of available buttons on the User Interface, named "Mark", "Unmark all", "Query", "Refresh", "Create", "Edit", "Copy", "Export", "Import", "Delete", "Sign", "Print preview", "Extract" and "Settings". Thanks to the @!TLanguageFile.Translate!Lib\SQLite3\SQLite3i18n.pas@ unit (responsible of application @*i18n@) and the {\f1\fs20 TLanguageFile. Translate} method, it could be translated on-the-fly from English into the current desired language, before display on screen or report creation.
See both above screen-shots to guess how the button captions match the enumeration names - i.e. @%synfilevcl.png@ and @%synfilevcl.png@.
A list of events, as used for the {\f1\fs20 TSQLAuditTrail} table, was also defined. Some events reflect the change made to the database rows (like {\f1\fs20 feRecordModified}), or generic application status (like {\f1\fs20 feServerStarted}):
!  TFileEvent = (
!    feUnknownState, feServerStarted, feServerShutdown,
!    feRecordCreated, feRecordModified, feRecordDeleted,
!    feRecordDigitallySigned, feRecordImported, feRecordExported );
In the grid and the reports, @*RTTI@ and "{\i uncamelcasing}" will be used to display this list as regular text, like "{\i Record digitally signed}", and translated to the current language, if necessary.
: ORM Registration
The User Interface generation will be made by creating an array of objects inheriting from the {\f1\fs20 TSQLRibbonTabParameters} type.
Firstly, a custom object type is defined, associating
!  TFileRibbonTabParameters = object(TSQLRibbonTabParameters)
!    /// the SynFile actions
!    Actions: TFileActions;
!  end;
Then a constant array of such objects is defined:
!const
!FileTabs: array[0..4] of TFileRibbonTabParameters = (
!(Table: TSQLAuditTrail;
! Select: 'Time,Status,StatusMessage'; Group: GROUP_MAIN;
! FieldWidth: 'gIZ'; ShowID: true; ReverseOrder: true; Layout: llClient;
! Actions: [faDelete,faMark,faUnmarkAll,faQuery,faRefresh,faPrintPreview,faSettings]),
!(Table: TSQLMemo;
! Select: DEF_SELECT; Group: GROUP_CLEAR; FieldWidth: 'IddId'; Actions: DEF_ACTIONS),
!(Table: TSQLData;
! Select: DEF_SELECT; Group: GROUP_CLEAR; FieldWidth: 'IddId'; Actions: DEF_ACTIONS_DATA),
!(Table: TSQLSafeMemo;
! Select: DEF_SELECT; Group: GROUP_SAFE; FieldWidth: 'IddId';  Actions: DEF_ACTIONS),
!(Table: TSQLSafeData;
! Select: DEF_SELECT; Group: GROUP_SAFE; FieldWidth: 'IddId';  Actions: DEF_ACTIONS_DATA));
The {\f1\fs20 Table} property will map the @*ORM@ class to the User Interface ribbon tab. A custom CSV list of fields should be set to detail which database columns must be displayed on the grids and the reports, in the {\f1\fs20 Select} property. Each ribbon tab could contain one or more {\f1\fs20 @*TSQLRecord@} table: the {\f1\fs20 Group} property is set to identify on which ribbon group it should be shown. The grid column widths are defined as a {\f1\fs20 FieldWidth} string in which each displayed field length mean is set with one char per field (A=first {\f1\fs20 Select} column,Z=26th column) - lowercase character will center the field data. For each table, the available actions are also set, and will be used to create the possible buttons to be shown on the ribbon toolbars (enabling or disabling a button is to be done at runtime).
Note that this array definition uses some previously defined individual constants (like {\f1\fs20 DEF_SELECT}, {\f1\fs20 DEF_ACTIONS_DATA} or {\f1\fs20 GROUP_SAFE}. This is a good practice, and could make code maintenance easier later on.
: Report generation
The following {\f1\fs20 CreateReport} method is overridden in @!TFileRibbon.CreateReport!Lib\SQLite3\Samples\MainDemo\FileClient.pas@:
!  /// class used to create the User interface
!  TFileRibbon = class(TSQLRibbon)
!  public
!    /// overriden method used customize the report content
!    procedure CreateReport(aTable: TSQLRecordClass; aID: integer; aReport: TGDIPages;
!      AlreadyBegan: boolean=false); override;
!  end;
The reporting engine in the framework is implemented via the {\f1\fs20 TGDIPages} class, defined in the @!TGDIPages!Lib\SQLite3\SQLite3Pages.pas@:
- Data is drawn in memory, they displayed or printed as desired;
- High-level reporting methods are available (implementing tables, columns, titles and such), but you can have access to a {\f1\fs20 TCanvas} property which allows any possible content generation via standard VCL methods;
- Allow preview (with anti-aliased drawing via GDI+) and printing;
- Direct export as {\f1\fs20 .txt} or {\f1\fs20 .@*pdf@} file;
- Handle bookmark, outlines and links inside the document.
By default, the {\f1\fs20 CreateReport} method of {\f1\fs20 TSQLRibbon} will write all editable fields value to the content.
The method is overridden by the following code:
!procedure TFileRibbon.CreateReport(aTable: TSQLRecordClass; aID: integer; aReport: TGDIPages;
!  AlreadyBegan: boolean=false);
!var Rec: TSQLFile;
!    Pic: TBitmap;
!    s: string;
!    PC: PChar;
!    P: TSQLRibbonTab;
!begin
!  with aReport do
!  begin
!    // initialize report
!    Clear;
!    BeginDoc;
!    Font.Size := 10;
!    if not aTable.InheritsFrom(TSQLFile) then
!      P := nil else
!      P := GetActivePage;
!    if (P=nil) or (P.CurrentRecord.ID<>aID) or (P.Table<>aTable) then
!    begin
!      inherited; // default handler
!      exit;
!    end;
!    Rec := TSQLFile(P.CurrentRecord);
!    Caption := U2S(Rec.fName);
The report is cleared, and {\f1\fs20 BeginDoc} method is called to start creating the internal canvas and band positioning. The font size is set, and parameters are checked against expected values. Then the current viewed record is retrieved from {\f1\fs20 GetActivePage. CurentRecord}, and the report caption is set via the record {\f1\fs20 Name} field.
!    // prepare page footer
!    SaveLayout;
!    Font.Size := 9;
!    AddPagesToFooterAt(sPageN,LeftMargin);
!    TextAlign := taRight;
!    AddTextToFooterAt('SynFile  http://synopse.info - '+Caption,RightMarginPos);
!    RestoreSavedLayout;
Page footer are set by using two methods:
- {\f1\fs20 AddPagesToFooterAt} to add the current page number at a given position (here the left margin);
- {\f1\fs20 AddTextToFooterAt} to add some custom text at a given position (here the right margin, after having changed the text alignment into right-aligned).
Note that {\f1\fs20 SaveLayout/RestoreSavedLayout} methods are used to modify temporary the current font and paragraph settings for printing the footer, then restore the default settings.
!    // write global header at the beginning of the report
!    DrawTitle(P.Table.CaptionName+' : '+Caption,true);
!    NewHalfLine;
!    AddColumns([6,40]);
!    SetColumnBold(0);
!    if Rec.SignatureTime<>0 then
!    begin
!      PC := Pointer(Format(sSignedN,[Rec.SignedBy,Iso2S(Rec.SignatureTime)]));
!      DrawTextAcrossColsFromCSV(PC,$C0C0FF);
!    end;
!    if Rec.fCreated<>0 then
!      DrawTextAcrossCols([sCreated,Iso2S(Rec.fCreated)]);
!    if Rec.fModified<>0 then
!      DrawTextAcrossCols([sModified,Iso2S(Rec.fModified)]);
!    if Rec.fKeyWords='' then
!      s := sNone else
!    begin
!      s := U2S(Rec.fKeyWords);
!      ExportPDFKeywords := s;
!    end;
!    DrawTextAcrossCols([sKeyWords,s]);
!    NewLine;
!    Pic := LoadFromRawByteString(Rec.fPicture);
!    if Pic<>nil then
!    try
!      DrawBMP(Pic,0,Pic.Width div 3);
!    finally
!      Pic.Free;
!    end;
Report header is written using the following methods:
- {\f1\fs20 DrawTitle} to add a title to the report, with a black line below it (second parameter to {\f1\fs20 true}) - this title will be added to the report global outline, and will be exported as such in {\f1\fs20 .pdf} on request;
- {\f1\fs20 NewHalfLine} and {\f1\fs20 NewLine} will leave some vertical gap between two paragraphs;
- {\f1\fs20 AddColumns}, with parameters set as percentages, will initialize a table with the first column content defined as bold ({\f1\fs20 SetColumnBold(0)});
- {\f1\fs20 DrawTextAcrossCols} and {\f1\fs20 DrawTextAcrossColsFromCSV} will fill a table row according to the text specified, one string per column;
- {\f1\fs20 DrawBMP} will draw a bitmap to the report, which content is loaded using the generic {\f1\fs20 LoadFromRawByteString} function implemented in @!Lib\SynGdiPlus.pas@;
- {\f1\fs20 U2S} and {\f1\fs20 Iso2S} function, as defined in @!Iso2S,U2S!Lib\SQLite3\SQLite3i18n.pas@, are used for conversion of some text or {\f1\fs20 @*TTimeLog@} into a text formated with the current language settings (@*i18n@).
!    // write report content
!    DrawTitle(sContent,true);
!    SaveLayout;
!    Font.Name := 'Courier New';
!    if Rec.InheritsFrom(TSQLSafeMemo) then
!      DrawText(sSafeMemoContent) else
!    if Rec.InheritsFrom(TSQLMemo) then
!      DrawTextU(TSQLMemo(Rec).Content) else
!    if Rec.InheritsFrom(TSQLData) then
!    with TSQLData(Rec) do
!    begin
!      DrawTextU(Rec.fName);
!      s := PictureName(TSynPicture.IsPicture(TFileName(Rec.fName)));
!      if s<>'' then
!        s := format(sPictureN,[s]) else
!        if not Rec.InheritsFrom(TSQLSafeData) then
!          s := U2S(GetMimeContentType(Pointer(Data),Length(Data),TFileName(Rec.fName)));
!      if s<>'' then
!        DrawTextFmt(sContentTypeN,[s]);
!      DrawTextFmt(sSizeN,[U2S(KB(Length(Data)))]);
!      NewHalfLine;
!      DrawText(sDataContent);
!    end;
!    RestoreSavedLayout;
Then the report content is appended, according to the record class type:
- {\f1\fs20 DrawText}, {\f1\fs20 DrawTextU} and {\f1\fs20 DrawTextFmt} are able to add a paragraph of text to the report, with the current alignment - in this case, the font is set to '{\i Courier New}' so that it will be displayed with fixed width;
- {\f1\fs20 GetMimeContentType} is used to retrieve the exact type of the data stored in this record.
!    // set custom report parameters
!    ExportPDFApplication := 'SynFile  http://synopse.info';
!    ExportPDFForceJPEGCompression := 80;
!  end;
!end;
Those {\f1\fs20 ExportPDFApplication} and {\f1\fs20 ExportPDFForceJPEGCompression} properties (together with the {\f1\fs20 ExportPDFKeywords} are able to customize how the report will be exported into a {\f1\fs20 .pdf} file. In our case, we want to notify that {\i SynFile} generated those files, and that the header bitmap should be compressed as JPEG before writing to the file (in order to produce a small sized {\f1\fs20 .pdf}).
You perhaps did notice that textual constant were defined as {\f1\fs20 @*resourcestring@}, as such:
!resourcestring
!  sCreated = 'Created';
!  sModified = 'Modified';
!  sKeyWords = 'KeyWords';
!  sContent = 'Content';
!  sNone = 'None';
!  sPageN = 'Page %d / %d';
!  sSizeN = 'Size: %s';
!  sContentTypeN = 'Content Type: %s';
!  sSafeMemoContent = 'This memo is password protected.'#13+
!    'Please click on the "Edit" button to show its content.';
!  sDataContent = 'Please click on the "Extract" button to get its content.';
!  sSignedN = 'Signed,By %s on %s';
!  sPictureN = '%s Picture';
The @!Lib\SQLite3\SQLite3i18n.pas@ unit is able to parse all those {\f1\fs20 resourcestring} from a running executable, via its {\f1\fs20 ExtractAllResources} function, and create a reference text file to be translated into any handled language.
Creating a report from code does make sense in an ORM. Since we have most useful data at hand as Delphi classes, code can be shared among all kind of reports, and a few lines of code is able to produce complex reports, with enhanced rendering, unified layout, direct internationalization and export capabilities.
: Application i18n and L10n
In computing, internationalization and localization (also spelled internationalisation and localisation) are means of adapting computer software to different languages, regional differences and technical requirements of a target market:
- {\i Internationalization} (@**i18n@) is the process of designing a software application so that it can be adapted to various languages;
- {\i Localization} (@**L10n@) is the process of adapting internationalized software for a specific region or language by adding locale-specific components and translating text, e.g. for dates display.
Our framework handles both features, via the @!Lib\SQLite3\SQLite3i18n.pas@ unit. We just saw above how {\f1\fs20 @*resourcestring@} defined in the source code are retrieved from the executable and can be translated on the fly. The unit extends this to visual forms, and even captions generated from @*RTTI@ - see @5@.
The unit expects all textual content (both {\f1\fs20 resourcestring} and RTTI derived captions) to be correct English text. A list of all used textual elements will be retrieved then hashed into an unique numerical value. When a specific locale is set for the application, the unit will search for a {\f1\fs20 @*.msg@} text file in the executable folder matching the expected locale definition. For instance, it will search for {\f1\fs20 FR.msg} for translation into French.
In order to translate all the user interface, a corresponding {\f1\fs20 .msg} file is to be supplied in the executable folder. Neither the source code, nor the executable is to be rebuild to add a new language. And since this file is indeed a plain textual file, even a non developer (e.g. an end-user) is able to add a new language, starting from another {\f1\fs20 .msg}.
:  Creating the reference file
In order to begin a translation task, the {\f1\fs20 SQlite3i18n.pas} unit is able to extract all textual resource from the executable, and create a reference text file, containing all English sentences and words to be translated, associated with their numerical hash value.
It will in fact:
- Extract all {\f1\fs20 resourcestring} text;
- Extract all captions generated from RTTI (e.g. from enumerations or class properties names);
- Extract all embedded {\f1\fs20 dfm} resources, and create per-form sections, allowing a custom translation of displayed captions or hints.
This creation step needs a compilation of the executable with the {\f1\fs20 EXTRACTALLRESOURCES} conditional defined, {\i globally} to the whole application (a full {\i rebuild} is necessary after having added or suppressed this conditional from the {\i Project / Options / Folders-Conditionals} IDE field).
Then the {\f1\fs20 ExtractAllResources} global procedure is to be called somewhere in the code.
For instance, here is how this is implemented in @!TMainForm.FormShow!Lib\SQLite3\Samples\MainDemo\FileMain.pas@, for the framework main demo:
!procedure TMainForm.FormShow(Sender: TObject);
!begin
!!{$ifdef EXTRACTALLRESOURCES}
!!  ExtractAllResources(
!    // first, all enumerations to be translated
!!    [TypeInfo(TFileEvent),TypeInfo(TFileAction),TypeInfo(TPreviewAction)],
!    // then some class instances (including the TSQLModel will handle all TSQLRecord)
!!    [Client.Model],
!    // some custom classes or captions
!    [],[]);
!!  Close;
!{$else}
!  //i18nLanguageToRegistry(lngFrench);
!{$endif}
!  Ribbon.ToolBar.ActivePageIndex := 1;
!end;
The {\f1\fs20 TFileEvent} and {\f1\fs20 TFileAction} enumerations RTTI information is supplied, together with the current {\f1\fs20 TSQLModel} instance. All {\f1\fs20 TSQLRecord} classes (and therefore properties) will be scanned, and all needed English caption text will be extracted.
The {\f1\fs20 Close} method is then called, since we don't want to use the application itself, but only extract all resources from the executable.
Running once the executable will create a {\f1\fs20 SynFile.messages} text file in the {\f1\fs20 SynFile.exe} folder, containing all English text:
$[TEditForm]
$Name.EditLabel.Caption=_2817614158   Name
$KeyWords.EditLabel.Caption=_3731019706   KeyWords
$
$[TLoginForm]
$Label1.Caption=_1741937413   &User name:
$Label2.Caption=_4235002365   &Password:
$
$[TMainForm]
$Caption=_16479868    Synopse SQLite3 Framework demo - SynFile
$
$[Messages]
$2784453965=Memo
$2751226180=Data
$744738530=Safe memo
$895337940=Safe data
$2817614158=Name
$1741937413=&User name:
$4235002365=&Password:
$16479868= Synopse SQLite3 Framework demo - SynFile
$940170664=Content
$3153227598=None
$3708724895=Page %d / %d
$2767358349=Size: %s
$4281038646=Content Type: %s
$2584741026=This memo is password protected.|Please click on the "Edit" button to show its content.
$3011148197=Please click on the "Extract" button to get its content.
$388288630=Signed,By %s on %s
$ (...)
The main section of this text file is named {\f1\fs20 [Messages]}. In fact, it contains all English extracted texts, as {\f1\fs20 NumericalKey=EnglishText} pairs. Note this will reflect the exact content of {\f1\fs20 resourcestring} or RTTI captions, including formating characters (like {\f1\fs20 %d}), and replacing line feeds ({\f1\fs20 #13}) by the special {\f1\fs20 |} character (a line feed is not expected on a one-line-per-pair file layout). Some other text lines are separated by a comma. This is usual for instance for hint values, as expected by the code.
As requested, each application form has its own section (e.g. {\f1\fs20 [TEditForm]}, {\f1\fs20 [TMainForm]}), proposing some default translation, specified by a numerical key (for instance {\f1\fs20 Label1.Caption} will use the text identified by 1741937413 in the {\f1\fs20 [Messages]} section). The underline character before the numerical key is used to refers to this value. Note that if no {\f1\fs20 _NumericalKey} is specified, a plain text can be specified, in order to reflect a specific use of the generic text on the screen.
:  Adding a new language
In order to translate the whole application into French, the following {\f1\fs20 SynFile.FR} file could be made available in the {\f1\fs20 SynFile.exe} folder:
$[Messages]
$2784453965=Texte
$2751226180=Données
$744738530=Texte sécurisé
$895337940=Données sécurisées
$2817614158=Nom
$1741937413=&Nom utilisateur:
$4235002365=&Mot de passe:
$16479868= Synopse mORMot Framework demo - SynFile
$940170664=Contenu
$3153227598=Vide
$3708724895=Page %d / %d
$2767358349=Taille: %s                                                        4281038646=Type de contenu: %s                                               2584741026=Le contenu de ce memo est protégé par un mot de passe.|Choisissez "Editer" pour le visualiser.
$3011148197=Choisissez "Extraire" pour enregistrer le contenu.
$388288630=Signé,Par %s le %s
$ (....)
Since no form-level custom captions have been defined in this {\f1\fs20 SynFile.FR} file, the default numerical values will be used. In our case, {\f1\fs20 Name.EditLabel.Caption} will be displayed using the text specified by 2817614158, i.e. {\f1\fs20 'Nom'}.
Note that the special characters {\f1\fs20 %s %d , |} markup was preserved: only the plain English text has been translated to the corresponding French.
:  Language selection
User Interface language can be specified at execution.
By default, it will use the registry to set the language. It will need an application restart, but it will also allow easier translation of all forms, using a low-level hook of the {\f1\fs20 TForm.Create} constructor.
For instance, if you set in @!TMainForm.FormShow!Lib\SQLite3\Samples\MainDemo\FileMain.pas@, for the framework main demo:
!procedure TMainForm.FormShow(Sender: TObject);
!begin
!  (...)
!!  i18nLanguageToRegistry(lngFrench);
!  Ribbon.ToolBar.ActivePageIndex := 1;
!end;
Above code will set the main application language as French. At next startup, the content of a supplied {\f1\fs20 SynFileFR.msg} file will be used to translate all screen layout, including all RTTI-generated captions.
Of course, for a final application, you'll need to change the language by a common setting. See {\f1\fs20 i18nAddLanguageItems, i18nAddLanguageMenu} and {\f1\fs20 i18nAddLanguageCombo} functions and procedures to create your own language selection dialog, using a menu or a combo box, for instance.
:  Localization
Take a look at the {\f1\fs20 TLanguageFile} class. After the main language has been set, you can use the global {\f1\fs20 Language} instance in order to localize your application layout.
The {\f1\fs20 SQlite3i18n} unit will register itself to some methods of {\f1\fs20 SQlite3Commons.pas}, in order to translate the RTTI-level text into the current selected language. See for instance {\f1\fs20 i18nDateText}.

[SAD-Ajax]
SourcePath=
IncludePath=
SourceFile=
Version=1.17
DisplayName=Ajax clients for mORMot

:The Smart Project
Did you hear from the great {\i @**Smart@} project?
It is an IDE and some source runtime able to develop and compile an Object-Pascal project into a {\i @*HTML 5@ / @*CSS 3@ / @*JavaScript@} embedded application. It does target AJAX Mobile application (i.e. {\i Android} and {\i iPhone/iPad} apps running {\i Web-Kit}) creation. You'll get an unique {\f1\fs20 .html} file containing the whole client-side application: it won't need any server side implementation. Using a third-party tool like {\i PhoneGap}, you'd be able to supply your customers with true native applications, running without any network, and accessing the full power of any modern Smart Phone.
{\i Smart} is a great candidate for implementing rich client-side AJAX applications, to work with our client-server {\i mORMot} framework.
:Introduction to Smart coding
In order to interface {\i Smart} code with {\i mORMot}, we started implementing some low-level code to work with our @*REST@ful authentication scheme.
So we'll need to implement some Smart dedicated Open Source code implementing {\i crc32} and {\i SHA-256} hashing.
: First steps
It is very easy to work with Smart.
Open the IDE, create a new project, select for instance a "Console" template, then code your object pascal units, just as usual.
The only missing feature is a debugger integrated into the IDE. You'll have to debug your code from the {\i JavaScript} side (using the debugging tools featured in {\i Chrome}, for instance).
You have at hand the full power and readability of the great object pascal implementation of {\i Delphi Web Script}. See @http://code.google.com/p/dwscript about this great Open Source project.
No more type-less {\i JavaScript} coding, no more pseudo classes, no more endless nested delegates... Readable code, modern object oriented language (including strong typing, interfaces and inheritance), with a lot of existing and proven code base from a happy-sharing community. Just plain Object Pascal code, with the full power of the HTML 5 platform at hand.
It does not need external JavaScript library (like {\i jQuery} or such), but you can use any of those libraries, if needed.
: Conversion rules
The {\i Delphi Web Script} implementation is very close to the {\i Delphi} / FPC implementation. It features a very modern object pascal syntax. But it will run with {\i JavaScript} as its runtime engine: you can see {\i JavaScript} as the replacement of {\i assembler} code for a regular {\i Delphi}/FPC compiler - and you can in fact write directly {\i JavaScript} code in the middle of a {\i Smart} unit using the {\f1\fs20 asm .. end} keywords.
As a consequence, when implementing low-level algorithms like {\i crc32} or {\i SHA-256} with Smart, some genuineness is to be taken in account:
- There is no low level binary types like {\f1\fs20 byte, integer, cardinal, Int64, UInt64} which are all mapped as one {\f1\fs20 integer} type (since hashing use binary representation of the data, we must take care of this);
- You can't play with memory buffers directly in the language (this is a managed code, without any pointer nor memory allocation) - so we'll use {\f1\fs20 string} to handle memory buffers;
- You have to handle all code endianess by hand (by definition, {\i JavaScript} is endian-agnostic);
- There is no {\f1\fs20 char} type, just {\f1\fs20 string}.
But most high-level code could be shared between a {\i Delphi} application and a {\i Smart} application. For instance, when used within our {\i mORMot} framework, you may share the @*ORM@ definitions (via {\f1\fs20 class}) or the @*SOA@ contract definitions (via {\f1\fs20 @*interface@}), and most of your business logic.
: CRC32 computation
So let's start with {\i @*crc32@} algorithm.
Here is the main computation code:
!var
!  crc32Tab: array [0..255] of integer;
!
!function crc32(aCRC32: integer; const data: string): integer;
!var i: integer;
!begin
!  result := (not aCRC32) shr 0;
!  for i := 1 to length(data) do
!    result := crc32Tab[(result xor ord(data[i])) and $ff] xor (result shr 8);
!  result := (not result) shr 0;
!end;
This is a standard implementation pattern, except for three remarks:
- We added {\f1\fs20 ... shr 0} in order to ensure that an {\f1\fs20 integer} variable will be maintained as an {\f1\fs20 UInt32} variable. Since {\i crc32} is a 32 bit hashing algorithm, we need to ensure that we'll only have positive values;
- No {\f1\fs20 byte} type is available here: so we'll explicitly call {\f1\fs20 ... and $ff} in order to truncate the {\f1\fs20 integer} value into its 8 bit content;
- Since the {\f1\fs20 string} type is used for data manipulation, we use {\f1\fs20 ord(data[i])} to retrieve each {\f1\fs20 byte} (or {\f1\fs20 char}) of the supplied text.
This implementation of the {\i crc32} algorithm expect a pre-computed table to be available. All {\i JavaScript} implementation of this algorithm (at least, all that I was able to found on Internet) use a fixed constant array. Since we are not afraid to write code any more in our AJAX application, and since it may help saving bandwidth and application size, we'll compute our own {\f1\fs20 crc32Tab[]} array content with the following code:
!procedure InitCrc32Tab;
!var i,n,crc: integer;
!begin // this code generates a 1KB table
!  for i := 0 to 255 do begin
!    crc := i;
!    for n := 1 to 8 do
!      if (crc and 1)<>0 then
!        // $edb88320 from polynomial p=(0,1,2,4,5,7,8,10,11,12,16,22,23,26)
!        crc := ((crc shr 1) xor $edb88320) shr 0 else
!        crc := crc shr 1;
!    CRC32Tab[i] := crc;
!  end;
!end;
Then we are able to use this code as such, for instance in a {\i Smart} console application:
!procedure TApplication.PopulateConsole;
!var i: integer;
!begin
!  InitCrc32Tab;
!  console.Writeln(IntToHex(crc32(0,'TestCRC32'),8));
!end;
The {\f1\fs20 InitCrc32Tab} shall be called only once, at application startup. Its execution is immediate. It won't delay your application display.
: SHA-256
The well-known @*SHA-256@ algorithm is a proven way of creating an unique identifier from any data input. You can use it for instance to sign any content, or store efficiently a password. It is mathematically proven to be impossible to find out the input data from its hashed reduction (at least for the next decade of computer power). And it has a very low potential of "collision" (i.e. two diverse data having the same resulting hash). It is "Top Secret" enabled - U.S. National Institute of Standards and Technology says, "Federal agencies must use the SHA-2 family of hash functions for applications  that require collision resistance after 2010". This is the hashing pattern used within {\i mORMot}.
But it is also more complex than the {\i crc32} algorithm. See @http://en.wikipedia.org/wiki/SHA-2
You have an optimized implementation in the {\f1\fs20 SynCrypto} unit, with tuned {\i x86} assembler code, and provided regression tests. We'll implement a pure Object Pascal version, compatible with the {\i Smart / Delphi Web Script (DWS)} compiler.
First of all, we'll define a {\f1\fs20 record} type. We may have used a {\f1\fs20 class}, but since we have an extended {\f1\fs20 record} type at hand with {\i DWS} (including properties and methods), we will stay to it.
!type
!  TSHA256Buffer = array[0..63] of integer;
!  TSHAHash  = record
!    A,B,C,D,E,F,G,H: integer;
!  end;
!  TSHA256 = record
!  private
!    // Working hash
!    Hash: TSHAHash;
!    // 64bit msg length
!    MLen: integer;
!    // Block buffer
!    Buffer: TSHA256Buffer;
!    // Index in buffer
!    Index: integer;
!    // used by Update and Finalize
!    procedure Compress;
!  public
!    /// initialize SHA256 context for hashing
!    procedure Init;
!    /// update the SHA256 context with some data
!    procedure Update(const Data: string);
!    /// finalize and compute the resulting SHA256 hash Digest of all data
!    // affected to Update() method
!    // - returns the data as Hexadecimal
!    function Finalize: string;
!    /// compute SHA256 hexa digest of a given text
!    class function Compute(Data: string): string;
!  end;
The main {\f1\fs20 class function} can be used as such:
!  console.WriteLn(TSHA256.Compute('abc'));
And it will be implemented as:
!class function TSHA256.Compute(Data: string): string;
!var SHA: TSHA256;
!begin
!  SHA.Init;
!  SHA.Update(Data);
!  result := SHA.Finalize;
!end;
The initialization will be done with this method:
!procedure TSHA256.Init;
!begin
!  Hash.A := $6a09e667;
!  Hash.B := $bb67ae85;
!  Hash.C := $3c6ef372;
!  Hash.D := $a54ff53a;
!  Hash.E := $510e527f;
!  Hash.F := $9b05688c;
!  Hash.G := $1f83d9ab;
!  Hash.H := $5be0cd19;
!end;
Note that the {\i DWS} compiler will initialize all record content to zero by default. The corresponding {\i JavaScript} code will be emitted. So only {\f1\fs20 Hash.?} values are to be set explicitly: {\f1\fs20 Index} and {\f1\fs20 MLen} properties are already set to 0.
Then the following method will update the current hash with some supplied data:
!procedure TSHA256.Update(const Data: string);
!var Len, aLen, i: integer;
!    DataNdx: integer = 1;
!begin
!  Len := length(Data);
!  inc(MLen,Len shl 3);
!  while Len>0 do begin
!    aLen := 64-Index;
!    if aLen<=Len then begin
!      for i := 0 to aLen-1 do
!        Buffer[Index+i] := ord(Data[DataNdx+i]) and $ff;
!      dec(Len,aLen);
!      inc(DataNdx,aLen);
!!      Compress;
!      Index := 0;
!    end else begin
!      for i := 0 to Len-1 do
!        Buffer[Index+i] := ord(Data[DataNdx+i]) and $ff;
!      inc(Index,Len);
!      break;
!    end;
!  end;
!end;
The internal {\f1\fs20 Buffer[]}, which is expected to contain up to 64 bytes, is filled with the provided data, then the global {\f1\fs20 MLen} is refreshed, and the {\f1\fs20 Compress} method will do the proper hashing, when the 64 bytes buffer is full. The {\f1\fs20 Index} variable is used to track the number of bytes available in the internal {\f1\fs20 Buffer[]} array.
The {\f1\fs20 Finalize} method will compute the latest block, including the global length to the incoming data (with padding if needed), then will return the data as an hexadecimal string:
!function TSHA256.Finalize: string;
!var i: integer;
!begin
!  // Message padding
!  // 1. append bit '1' after Buffer
!  Buffer[Index]:= $80;
!  for i := Index+1 to 63 do
!    Buffer[i] := 0;
!  // 2. Compress if more than 448 bits, (no room for 64 bit length)
!  if Index>=56 then begin
!    Compress;
!    for i := 0 to 59 do
!      Buffer[i] := 0;
!  end;
!  // Write 64 bit Buffer length into the last bits of the last block
!  // (in big endian format) and do a final compress
!  Buffer[60] := (MLen and $ff000000)shr 24;
!  Buffer[61] := (MLen and $ff0000)shr 16;
!  Buffer[62] := (MLen and $ff00)shr 8;
!  Buffer[63] := MLen and $ff;
!  Compress;
!  // Hash -> Digest to big endian format
!  result := LowerCase(IntToHex(Hash.A,8)+IntToHex(Hash.B,8)+IntToHex(Hash.C,8)+
!    IntToHex(Hash.D,8)+IntToHex(Hash.E,8)+IntToHex(Hash.F,8)+IntToHex(Hash.G,8));
!  // Clear Data
!  Init;
!end;
Some endianess tricks are used in the above code. But it remains easy to follow and maintain.
The main hash computation is performed in the {\f1\fs20 Compress} method, as such:
!const
!  K: TSHA256Buffer = ([
!   $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1,
!   $923f82a4, $ab1c5ed5, $d807aa98, $12835b01, $243185be, $550c7dc3,
!   $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174, $e49b69c1, $efbe4786,
!   $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
!   $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147,
!   $06ca6351, $14292967, $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13,
!   $650a7354, $766a0abb, $81c2c92e, $92722c85, $a2bfe8a1, $a81a664b,
!   $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
!   $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a,
!   $5b9cca4f, $682e6ff3, $748f82ee, $78a5636f, $84c87814, $8cc70208,
!   $90befffa, $a4506ceb, $bef9a3f7, $c67178f2]);
!
!procedure TSHA256.Compress;
!var W: TSHA256Buffer;
!    H: TSHAHash = Hash;
!    i, t1, t2: integer;
!begin
!  for i := 0 to 15 do
!    W[i]:= (((Buffer[i*4] shl 24)shr 0)or(Buffer[i*4+1] shl 16)or
!           (Buffer[i*4+2] shl 8)or Buffer[i*4+3]) shr 0;
!  for i := 16 to 63 do
!    W[i] := ((((W[i-2]shr 17)or(W[i-2]shl 15))xor((W[i-2]shr 19)or(W[i-2]shl 13))
!      xor (W[i-2]shr 10))+W[i-7]+(((W[i-15]shr 7)or(W[i-15]shl 25))
!      xor ((W[i-15]shr 18)or(W[i-15]shl 14))xor(W[i-15]shr 3))+W[i-16])shr 0;
!  for i := 0 to high(W) do begin
!    t1 := (H.H+(((H.E shr 6)or(H.E shl 26))xor((H.E shr 11)or(H.E shl 21))xor
!      ((H.E shr 25)or(H.E shl 7)))+((H.E and H.F)xor(not H.E and H.G))+K[i]+W[i])shr 0;
!    t2 := ((((H.A shr 2)or(H.A shl 30))xor((H.A shr 13)or(H.A shl 19))xor
!      ((H.A shr 22)xor(H.A shl 10)))+((H.A and H.B)xor(H.A and H.C)xor(H.B and H.C))) shr 0;
!    H.H := H.G; H.G := H.F; H.F := H.E; H.E := (H.D+t1)shr 0;
!    H.D := H.C; H.C := H.B; H.B := H.A; H.A := (t1+t2)shr 0;
!  end;
!  Hash.A := (Hash.A+H.A)shr 0;
!  Hash.B := (Hash.B+H.B)shr 0;
!  Hash.C := (Hash.C+H.C)shr 0;
!  Hash.D := (Hash.D+H.D)shr 0;
!  Hash.E := (Hash.E+H.E)shr 0;
!  Hash.F := (Hash.F+H.F)shr 0;
!  Hash.G := (Hash.G+H.G)shr 0;
!  Hash.H := (Hash.H+H.H)shr 0;
!end;
A {\f1\fs20 K: TSHA256Buffer} constant table is used. Note the non standard definition of {\i DWS} for a {\f1\fs20 const array}: it will use {\f1\fs20 .. = ([...]);} instead of {\f1\fs20 .. = ();} as in classical {\i Object Pascal}.
The hash is computed from an internal {\f1\fs20 W[]} array, which is filled with the binary representation of the supplied {\f1\fs20 Buffer[]} bytes. That is, {\f1\fs20 Buffer: array[0..63] of byte} is first un-serialized in {\f1\fs20 W: array[0..15] of cardinal}.
Then the SHA-256 algorithm is performed in its most simple rolled version. An un-rolled version is not mandatory here, in our managed {\i JavaScript} runtime environment.
The only non obvious part of the above code is the use of {\f1\fs20 ... shr 0} to enforce only positive 32 bit integers (aka {\f1\fs20 cardinal}) are used during the computation.

[SDD]
Owner=SRS
Order=SRS
; Owner: [SDD-*] -> * reference; Order=SRS -> [SDD-*] have no sub items
Name=Software Design Document
Purpose=Summarize the software DI implementation for QA review
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=1.17
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
; [SDD-*] sections contain details for each SRS, [SDD-SER-03] or [SDD-DI-4.10.6] e.g.
; [SDD-*] are displayed as they appear in the [SRS-*] sections
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose
WriteTableOfContent=Yes
; Write global Table Of Contents at the end of the file
; Write global Table Of Contents at the end of the file
TableOfContentsAtTheBeginning=Yes
; if the Table of Contents must be at the beginning (default=No=at the end of the file)
DocumentIndex=Pictures,Source,Index

:Introduction
: Documentation overview
The whole Software documentation process follows the typical steps of this diagram:
%%FMEADI
: Purpose
This @SDD@ applies to the release of the {\i Synopse mORMot Framework} library.
It summarizes the software implementation of each design input as specified by the @DI@.
This document is divided into the main parts of the Software implementation:
\LayoutPage
Inside this sections, source code or User Interface modifications are detailed for every @SRS@ item.
: Responsibilities
- Synopse will try to correct any identified issue;
- The Open Source community will create tickets in a public Tracker web site located at @http://synopse.info/fossil ;
- Synopse work on the framework is distributed without any warranty, according to the chosen license terms;
- This documentation is released under the GPL (GNU General Public License) terms, without any warranty of any kind.
=[GPL]

[SDD-DI-2.1.1]
; SRS-DI-2.1.1 - The framework must be Client-Server oriented
:Implementation
The @*Client-Server@ aspect of the framework is implemented in the @!TSQLRecord,TSQLRest,TSQLRestServer,TSQLRestClientURI,TSQLTableJSON!Lib\SQLite3\SQLite3Commons.pas@ unit, with the {\f1\fs20 TSQLRestServer} and {\f1\fs20 TSQLRestClientURI} classes.
Both classes inherit from a generic {\f1\fs20 TSQLRest} class, which implements some generic database access methods and properties (through @*ORM@ model for objects descending from {\f1\fs20 TSQLRecord} or table-based query using {\f1\fs20 TSQLTableJSON}).

[SDD-DI-2.1.1.1]
; SRS-DI-2.1.1.1 - A RESTful mechanism must be implemented
:Implementation
The @*REST@ful mechanism is implemented using the {\f1\fs20 URI} method of both {\f1\fs20 TSQLRestServer} and {\f1\fs20 TSQLRestClientURI} classes, as defined in the @!TSQLRest,TSQLRestServer,TSQLRestClientURI!Lib\SQLite3\SQLite3Commons.pas@ unit.
: Server-Side
In the {\f1\fs20 TSQLRestServer} class, the {\f1\fs20 URI} method is defined as {\f1\fs20 public}, and must implement the actual database query or update, according to the REST request:
!function TSQLRestServer.URI(const url, method: RawUTF8; const SentData: RawUTF8;
!      out Resp, Head: RawUTF8; const RestAccessRights: TSQLAccessRights): Int64Rec;
The purpose of this method is to:
- Return internal database state count (used for caching);
- Retrieve URI expecting the RESTful {\f1\fs20 'ModelRoot[/TableName[/ID[/BlobFieldName]]]'} format;
- Call appropriate database commands, by using the protected {\f1\fs20 EngineList EngineRetrieve EngineAdd EngineUpdate EngineDelete EngineRetrieveBlob EngineUpdateBlob} methods.
The {\f1\fs20 TSQLRestServer} class itself doesn't implement these database command methods: they are all defined as {\f1\fs20 virtual; abstract;}. Children classes must override these virtual methods, and implement them using the corresponding database engine.
: Client-Side
In the {\f1\fs20 TSQLRestClientURI} class, the {\f1\fs20 URI} method is defined as {\f1\fs20 protected} and as {\f1\fs20 virtual; abstract;}. Children classes must override this method, and implement the remote database query or update, according to the REST request, and its internal protocol.

[SDD-DI-2.1.1.2]
; SRS-DI-2.1.1.2 - Commmunication should be available directly in the same process memory, or remotly using Named Pipes, Windows messages or HTTP/1.1 protocols

[SDD-DI-2.1.1.2.1]
; SRS-DI-2.1.1.2.1 - Client-Server Direct communication inside the same process
:Implementation
The in-process communication is implemented by using a global function, named {\f1\fs20 URIRequest} and defined in @!TSQLRestServer,URIRequest,USEFASTMM4ALLOC,TSQLRestClientURIDll.Create!Lib\SQLite3\SQLite3Commons.pas@:
!function URIRequest(url, method, SendData: PUTF8Char; Resp, Head: PPUTF8Char): Int64Rec; cdecl;
: Server-Side
This function can be exported from a DLL to remotely access to a {\f1\fs20 TSQLRestServer}, or used in the same process:
- Use {\f1\fs20 TSQLRestServer.ExportServer} to assign a server to this function;
- Return {\i 501 NOT IMPLEMENTED} error if no {\f1\fs20 TSQLRestServer.ExportServer} has been assigned yet;
- Memory for {\f1\fs20 Resp} and {\f1\fs20 Head} parameters are allocated with {\f1\fs20 GlobalAlloc()} Win32 API function: client must release this pointers with {\f1\fs20 GlobalFree()} after having retrieved their content - you can force using the Delphi heap (and {\f1\fs20 GetMem} function which is much faster than {\f1\fs20 GlobalAlloc}) by setting the {\f1\fs20 USEFASTMM4ALLOC} variable to TRUE: in this case, client must release this pointers with {\f1\fs20 Freemem()}.
: Client-Side
The Client should simply use a {\f1\fs20 TSQLRestClientURIDll} instance to access to an exported {\f1\fs20 URIRequest()} function.

[SDD-DI-2.1.1.2.2]
; SRS-DI-2.1.1.2.2 - Client-Server Named Pipe communication
:Implementation
: Server-Side
The communication is implemented by using the {\f1\fs20 TSQLRestServer} class, defined in @!TSQLRestServer.ExportServerNamedPipe,TSQLRestClientURINamedPipe.Create,TSQLRestClientURI!Lib\SQLite3\SQLite3Commons.pas@.
This class implements a server over Named Pipe communication, when its {\f1\fs20 ExportServerNamedPipe} method is called.
: Client-Side
A dedicated {\f1\fs20 TSQLRestClientURINamedPipe} class has been defined. It inherits from {\f1\fs20 TSQLRestClientURI}, and override its {\f1\fs20 URI} protected method so that it communicates using a specified Named Pipe.

[SDD-DI-2.1.1.2.3]
; SRS-DI-2.1.1.2.3 - Client-Server Windows Messages communication
:Implementation
Communication using Win32 GDI messages is very handy and efficient on the same computer. It's also perfectly safe, because, by design, it can't be access remotely. Performances for small messages is also excellent. Named pipe could be faster only when bigger messages are transmitted.
: Server-Side
The communication is implemented by using the {\f1\fs20 TSQLRestServer} class, defined in @!TSQLRestClientURI,TSQLRestServer.ExportServerMessage,TSQLRestClientURIMessage.Create!Lib\SQLite3\SQLite3Commons.pas@.
This class implements a server over Win32 GDI messages communication, when its {\f1\fs20 ExportServerMessage} method is called.
: Client-Side
A dedicated {\f1\fs20 TSQLRestClientURIMessage} class has been defined. It inherits from {\f1\fs20 TSQLRestClientURI}, and override its {\f1\fs20 URI} protected method so that it communicates using Win32 GDI messages.

[SDD-DI-2.1.1.2.4]
; SRS-DI-2.1.1.2.4 - Client-Server HTTP/1.1 protocol communication
:Implementation
: Server-Side
The communication is not implemented directly in the {\f1\fs20 TSQLRestServer} class, defined in @!TSQLRestClientURI,TSQLRestServer.URI!Lib\SQLite3\SQLite3Commons.pas@, but by a dedicated {\f1\fs20 TSQLite3HttpServer} class defined in @!TSQLite3HttpServer.Create,TSQLite3HttpServer.DBServer,TSQLite3HttpServer.AddServer!Lib\SQLite3\SQLite3HttpServer.pas@.
This class will instantiate a {\f1\fs20 THttpServerGeneric} instance, defined in @!THttpServer.Create,THttpApiServer.Create,THttpServerGeneric.Request,THttpServerGeneric.OnRequest!Lib\SynCrtSock.pas@, which implements a HTTP/1.1 server over TCP/IP communication.
This server is implemented either:
- Via {\f1\fs20 THttpApiServer} for using the fast kernel-mode http.sys server;
- Via {\f1\fs20 THttpServer}, which is an optimized pure Delphi HTTP/1.1 compliant server, using {\i Thread pool} to reduce resources, and provide best possible performance in user land.
You can register several {\f1\fs20 TSQLRestServer} instance to the same HTTP server, via its {\f1\fs20 AddServer} method.  Each {\f1\fs20 TSQLRestServer} class must have an unique {\f1\fs20 Model.Root} value, to identify which instance must handle a particular request from its URI root string.
A dedicated property, named {\f1\fs20 DBServer}, is an array to all registered {\f1\fs20 TSQLRestServer} instances, which are used to process any request, and answer to it by using the corresponding {\f1\fs20 URI} method - via the {\f1\fs20 OnRequest} standard event prototype.
: Client-Side
A dedicated {\f1\fs20 TSQLite3HttpClient} class has been defined in @!TSQLite3HttpClient.Create!Lib\SQLite3\SQLite3HttpClient.pas@. It inherits from {\f1\fs20 TSQLRestClientURI}, and override its {\f1\fs20 URI} protected method so that it communicates using HTTP/1.1 protocol over TCP/IP, according to the supplied HTTP address name.
By default, {\f1\fs20 TSQLite3HttpClient} maps to a {\f1\fs20 TSQLite3HttpClientWinHTTP} class, which was found out to perform well on most configurations and networks (whereas {\f1\fs20 TSQLite3HttpClientWinSock} should be a bit faster on a local computer).

[SDD-DI-2.1.2]
; SRS-DI-2.1.2 - UTF-8 JSON format must be used to communicate
:Implementation
The JSON parsing and producing is implemented in the @!TTextWriter.Create,TTextWriter.AddJSONEscape,IsJSONString,JSONDecode,JSONEncode,JSONEncodeArray,GetJSONField,JSON_CONTENT_TYPE!Lib\SynCommons.pas@ and @!TSQLTable.GetJSONValues,TSQLTableJSON.Create,TSQLTableJSON.UpdateFrom,TJSONWriter.Create,TSQLRecord.CreateJSONWriter,TSQLRecord.GetJSONValues,GetJSONObjectAsSQL,UnJSONFirstField!Lib\SQLite3\SQLite3Commons.pas@ units.
The JSON encoding and decoding is handled at diverse levels:
- With some JSON-dedicated functions and classes;
- At the database record level;
- At the database request table level.
: JSON-dedicated functions and classes
The main class for producing JSON content is {\f1\fs20 TJSONWriter}. This class is a simple writer to a Stream, specialized for the JSON format. Since it makes
use of an internal buffer, and avoid most temporary {\f1\fs20 string} allocation ({\i e.g.} using the stack instead of a temporary {\f1\fs20 string} via {\f1\fs20 IntToStr()} when converting a numerical value to text), it is much faster than a string append (standard Delphi {\f1\fs20 string := string+string} clauses) to produce its content. In particular, its {\f1\fs20 AddJSONEscape} method will handle JSON content escape, according to the official JSON RFC - see @http://www.ietf.org/rfc/rfc4627.txt paragraph 2.5, directly into the destination buffer. It was also designed to scales well on multi-core sytems.
Some JSON-dedicated function are also available:
- {\f1\fs20 GetJSONObjectAsSQL} decodes a JSON fields object into an UTF-8 encoded SQL-ready statement;
- {\f1\fs20 IsJSONString} returns TRUE if the supplied content must be encoded as a JSON string according to the JSON encoding schema, i.e. if it's some null/false/true content or any pure numerical data (integer or floating point);
- {\f1\fs20 UnJSONFirstField} can be used to retrieve the FIRST field value of the FIRST row, from a JSON content: it may be useful to get an ID without converting the whole JSON content into a {\f1\fs20 TSQLTableJSON};
- {\f1\fs20 JSONEncode} and {\f1\fs20 JSONDecode} functions are available to directly encode or decode some UTF-8 JSON content (used in the remote @*Service@ implementation, for instance).
: Database record level
The {\f1\fs20 TJSONWriter} class (based on {\f1\fs20 TTextWriter}) is used by the {\f1\fs20 GetJSONValues} method of the {\f1\fs20 TSQLRecord} class to get all the data of a database record as JSON content.
Here is an extract of the main loop of this method:
!procedure TSQLTable.GetJSONValues(JSON: TStream; Expand: boolean;
!  RowFirst: integer=0; RowLast: integer=0);
!  (...)
!    for R := RowFirst to RowLast do
!    begin
!      if Expand then
!        W.Add('{');
!      for F := 0 to FieldCount-1 do
!      begin
!        if Expand then
!          W.AddString(W.ColNames[F]); // '"'+ColNames[]+'":'
!        if Assigned(QueryTables) then
!        if IsJSONString(U^) then
!        begin
!          W.Add('"');
!          W.AddJSONEscape(U^,0);
!          W.Add('"');
!        end else
!          W.AddNoJSONEscape(U^,0);
!        W.Add(',');
!        inc(U); // points to next value
!      end;
!      W.CancelLastComma; // cancel last ','
!      if Expand then
!        W.Add('}');
!      W.Add(',');
!    end;
!  (...)
: Database request table level
Most high-level Client-sided list request methods returns a {\f1\fs20 TSQLTableJSON} instance as a result. This {\f1\fs20 TSQLTableJSON} class has been created from a pure JSON content, retrieved from the Server using on of the protocols defined in @SRS-DI-2.1.1.2@.
Its {\f1\fs20 Create} constructor method call its internal {\f1\fs20 protected} method named {\f1\fs20 FillFrom()}, which make the JSON conversion into pure UTF-8 text fields, as expected by the {\f1\fs20 TSQLTable} class and its various {\f1\fs20 Get*()} methods. The {\f1\fs20 FillFrom()} method implements a very fast parsing of the supplied JSON content, then un-escape its content according to the JSON RFC quoted above.
: Fast JSON parsing
When it deals with parsing some (textual) content, two directions are usually envisaged. In the XML world, you have usually to make a choice between:
- A DOM parser, which creates an in-memory tree structure of objects mapping the XML nodes;
- A SAX parser, which reads the XML content, then call pre-defined {\i events} for each XML content element.
In fact, DOM parsers use internally a SAX parser to read the XML content. Therefore, with the overhead of object creation and their property initialization, DOM parsers are typically three to five times slower than SAX. But, DOM parsers are much more powerful for handling the data: as soon as it's mapped in native objects, code can access with no time to any given node, whereas a SAX-based access will have to read again the whole XML content.
Most JSON parser available in Delphi use a DOM-like approach. For instance, the {\i DBXJSON} unit included since Delphi 2010 or the {\i SuperObject} library create a class instance mapping each JSON node.
In a JSON-based Client-Server ORM like ours, profiling shows that a lot of time is spent in JSON parsing, on both Client and Server side. Therefore, we tried to optimize this part of the library.
In order to achieve best speed, we try to use a mixed approach:
- All the necessary conversion (e.g. un-escape text) is made in-memory, from and within the JSON buffer, to avoid memory allocation;
- The parser returns {\i pointers} to the converted elements (just like the {\i vtd-xml} library).
In practice, here is how it is implemented:
- A private copy of the source JSON data is made internally (so that the Client-Side method used to retrieve this data can safely free all allocated memory);
- The source JSON data is parsed, and replaced by the UTF-8 text un-escaped content, in the same internal buffer (for example, strings are un-escaped and #0 are added at the end of any field value; and numerical values remains text-encoded in place, and will be extracted into {\f1\fs20 Int64} or {\f1\fs20 double} only if needed);
- Since data is replaced in-memory (JSON data is a bit more verbose than pure UTF-8 text so we have enough space), no memory allocation is performed during the parsing: the whole process is very fast, not noticeably slower than a SAX approach;
- This very profiled code (using pointers and tuned code) results in a very fast parsing and conversion.
This parsing "magic" is done in the {\f1\fs20 GetJSONField} function, as defined in the @!GetJSONField!Lib\SynCommons.pas@ unit:
!/// decode a JSON field in an UTF-8 encoded buffer (used in TSQLTableJSON.Create)
!// - this function decodes in the P^ buffer memory itself (no memory allocation
!// or copy), for faster process - so take care that it's an unique string
!// - PDest points to the next field to be decoded, or nil on any unexpected end
!// - null is decoded as nil
!// - '"strings"' are decoded as 'strings'
!// - strings are JSON unescaped (and \u0123 is converted to UTF-8 chars)
!// - any integer value is left as its ascii representation
!// - wasString is set to true if the JSON value was a "string"
!// - works for both field names or values (e.g. '"FieldName":' or 'Value,')
!// - EndOfObject (if not nil) is set to the JSON value char (',' ':' or '}' e.g.)
!function GetJSONField(P: PUTF8Char; out PDest: PUTF8Char;
!  wasString: PBoolean=nil; EndOfObject: PUTF8Char=nil): PUTF8Char;
This function allows to iterate throughout the whole JSON buffer content, retrieving values or property names, and checking {\f1\fs20 EndOfObject} returning value to handle the JSON structure.
This in-place parsing of textual content is one of the main reason why we used UTF-8 (via {\f1\fs20 RawUTF8}) as the common string type in our framework, and not the generic {\f1\fs20 string} type, which would have introduced a memory allocation and a char-set conversion.
For instance, here is how JSON content is converted into SQL, as fast as possible:
!function GetJSONObjectAsSQL(var P: PUTF8Char; const Fields: TRawUTF8DynArray;
!  Update, InlinedParams: boolean): RawUTF8;
! (...)
!    // get "COL1"="VAL1" pairs, stopping at '}' or ']'
!    FieldsCount := 0;
!    repeat
!!      FU := GetJSONField(P,P);
!      inc(Len,length(FU));
!      if P=nil then break;
!      Fields2[FieldsCount] := FU;
!!      Values[FieldsCount] := GetValue; // update EndOfObject
!      inc(FieldsCount);
!    until EndOfObject in [#0,'}',']'];
!    Return(@Fields2,@Values,InlinedParams);
!  (...)
And the sub-function {\f1\fs20 GetValue} makes use of {\f1\fs20 GetJSONField} also:
!function GetValue: RawUTF8;
!var wasString: boolean;
!    res: PUTF8Char;
!begin
!  res := P;
!  if (PInteger(res)^ and $DFDFDFDF=NULL_DF) and (res[4] in [#0,',','}',']'])  then
!    /// GetJSONField('null') returns '' -> check here to make a diff with '""'
!    result := 'null' else begin
!    // any JSON string or number or 'false'/'true' in P:
!!    res := GetJSONField(res,P,@wasString,@EndOfObject);
!    if wasString then
!      if not InlinedParams and
!         (PInteger(res)^ and $00ffffff=JSON_BASE64_MAGIC) then
!        // \\uFFF0base64encodedbinary -> 'X''hexaencodedbinary'''
!        // if not inlined, it can be used directly in INSERT/UPDATE statements
!        result := Base64MagicToBlob(res+3) else
!        { escape SQL strings, cf. the official SQLite3 documentation }
!        result := QuotedStr(pointer(res),'''') else
!      result := res;
!  end;
!  Inc(Len,length(result));
!end;
This code will create a string for each key/value in {\f1\fs20 Fields2[]} and {\f1\fs20 Values[]} arrays, but only once, with the definitive value (even single quote escape and BLOB un-serialize from Base-64 encoding are performed directly from the JSON buffer).

[SDD-DI-2.1.3]
; SRS-DI-2.1.3 - The framework must use an innovative ORM (Object-relational mapping) approach, based on classes RTTI (Runtime Type Information)
:Implementation
Some Delphi @*RTTI@ (Runtime Type Information) objects and classes are implemented in the @!TClassProp,TClassType,TEnumType,TTypeInfo,TSQLRecord.ClassProp,TSQLRecord.GetJSONValues,TPropInfo.GetValue,TPropInfo.SetValue,TSQLRecordProperties!Lib\SQLite3\SQLite3Commons.pas@ unit. The {\i Synopse mORMot Framework} uses this custom functions and objects in order to access to the Delphi @*RTTI@.
The generic functions supplied by the standard {\f1\fs20 TypInfo.pas} unit where not found to be easy to use: there are some record types from one hand, which details the internal @*RTTI@ memory layout generated by the compiler, and there are some functions on the other hand. So the framework unified both RTTI memory layout and methods by defining some {\f1\fs20 object} types (i.e. not Delphi classes, but raw objects which can map directly the RTTI memory layout via a {\f1\fs20 pointer}) with some methods dedicated for RTTI handling and @*ORM@. These {\f1\fs20 object} types are {\f1\fs20 TClassProp, TClassType, TEnumType, TTypeInfo} and {\f1\fs20 TPropInfo}.
Since this ORM is the core of the framework, the code of most of these objects has been tuned for performance: quit all of the methods have two versions in the framework, one in pure pascal code (easy to maintain and understand, and @*64 bit@ compatible), and one in optimized i386 assembler.
As a result, ORM code based on RTTI is fairly easy to use. See for example who a database field index is retrieved for a {\f1\fs20 TSQLRecord} class:
!function ClassFieldIndex(ClassType: TClass; const PropName: shortstring): integer;
!var P: PPropInfo;
!    CP: PClassProp;
!begin
!  if ClassType<>nil then
!  begin
!    CP := InternalClassProp(ClassType);
!    if CP<>nil then
!    begin
!      P := @CP^.PropList;
!      for result := 0 to CP^.PropCount-1 do
!        if IdemPropName(P^.Name,PropName) then
!          exit else
!          P := P^.Next;
!    end;
!  end;
!  result := -1;
!end;
Internally, the {\f1\fs20 TSQLRecord} will cache some of this RTTI derived data into an internal {\f1\fs20 TSQLRecordProperties} instance, global for the whole process. For instance, the method used to retrieve a field index from its property name is the following:
!function TSQLRecordProperties.FieldIndex(const PropName: shortstring): integer;
!begin
!  if self<>nil then
!  for result := 0 to high(Fields) do
!    if IdemPropName(Fields[result]^.Name,PropName) then
!      exit;
!  result := -1;
!end;
And will be available from {\f1\fs20 TSQLRecord.RecordProps.FieldIndex}.
:TSQLRecord table properties
: Per-class variable needed
For our ORM, we needed a {\i class variable} to be available for each {\f1\fs20 TSQLRecord} class type. This variable is used to store the properties of this class type, i.e. the database Table properties (e.g. table and column names and types) associated with a particular {\f1\fs20 TSQLRecord} class, from which all our ORM objects inherit.
The {\f1\fs20 class var} statement was not enough for us:
- It's not available on earlier Delphi versions, and we try to have our framework work with Delphi 6-7;
- This {\f1\fs20 class var} instance will be shared by all classes inheriting from the class where it is defined - and we need ONE instance PER class type, not ONE instance for ALL
We need to find another way to implement this {\i class variable}. An unused VMT slot in the class type description was identified, then each class definition was patched in the process memory to contain our class variable.
: Patching a running process code
The first feature we have to do is to allow on-the-fly change of the assembler code of a process.
When an executable is mapped in RAM, the memory page corresponding to the process code is marked as {\i Read Only}, in order to avoid any security attack from the outside. Only the current process can patch its own code.
We'll need to override a {\f1\fs20 pointer} value in the code memory. The following function, defined in {\f1\fs20 SynCommons.pas} will handle it:
!procedure PatchCodePtrUInt(Code: PPtrUInt; Value: PtrUInt);
!var RestoreProtection, Ignore: DWORD;
!begin
!  if VirtualProtect(Code, SizeOf(Code^), PAGE_EXECUTE_READWRITE, RestoreProtection) then
!  begin
!    Code^ := Value;
!    VirtualProtect(Code, SizeOf(Code^), RestoreProtection, Ignore);
!    FlushInstructionCache(GetCurrentProcess, Code, SizeOf(Code^));
!  end;
!end;
The {\f1\fs20 VirtualProtect} low-level Windows API is called to force the corresponding memory to be written (via the {\f1\fs20 PAGE_EXECUTE_READWRITE} flag), then modify the corresponding {\f1\fs20 pointer} value, then the original memory page protection setting (should be {\f1\fs20 PAGE_EXECUTE_READ}) is restored.
According to the MSDN documentation, we'd need to flush the CPU operation cache in order to force the modified code to be read on next access.
: Per-class variable in the VMT
The VMT is the {\i Virtual-Method Table}, i.e. a Table which defines every Delphi {\f1\fs20 class}. In fact, every Delphi {\f1\fs20 class} is defined internally by its VMT, contains a list of pointers to the {\f1\fs20 class}’s {\f1\fs20 virtual} methods. This VMT also contains non-method values, which are class-specific information at negative offsets:
|%30%10%60
|\b Name|Offset|Description\b0
|{\f1\fs20 vmtSelfPtr}|–76|points back to the beginning of the table
|{\f1\fs20 vmtIntfTable}|–72|{\f1\fs20 TObject.GetInterfaceTable} method value
|{\f1\fs20 vmtAutoTable}|–68|class’s automation table (deprecated)
|{\f1\fs20 vmtInitTable}|–64|reference-counted fields type information
|{\f1\fs20 vmtTypeInfo}|–60|the associated RTTI type information
|{\f1\fs20 vmtFieldTable}|–56|field addresses
|{\f1\fs20 vmtMethodTable}|–52|method names
|{\f1\fs20 vmtDynamicTable}|–48|{\f1\fs20 dynamic} methods table
|{\f1\fs20 vmtClassName}|–44|{\f1\fs20 PShortString} of the class name
|{\f1\fs20 vmtInstanceSize}|–40|bytes needed by one class Instance
|{\f1\fs20 vmtParent}|–36|parent VMT
|%
We'll implement the low-level trick as detailed in this reference article available at @http://hallvards.blogspot.com/2007/05/hack17-virtual-class-variables-part-ii.html in order to use the {\f1\fs20 vmtAutoTable} deprecated entry in the VMT. This entry was used in Delphi 2 only for implementing {\i Automation}. Later version of Delphi (our goal) won't use it any more. But the slot is still here, ready for being used by the framework.
We'll therefore be able to store a pointer to the {\f1\fs20 TSQLRecordProperties} instance corresponding to a {\f1\fs20 TSQLRecord} class, which will be retrieved as such:
!class function TSQLRecord.RecordProps: TSQLRecordProperties;
!begin
!  if Self<>nil then begin
!    result := PPointer(PtrInt(Self)+vmtAutoTable)^;
!    if result=nil then
!      result := PropsCreate(self);
!  end else
!    result := nil;
!end;
Since this method is called a lot of time by our ORM, there is an asm-optimized version of the pascal code above:
!class function TSQLRecord.RecordProps: TSQLRecordProperties;
!asm
!  or eax,eax
!  jz @null
!  mov edx,[eax+vmtAutoTable]
!  or edx,edx
!  jz PropsCreate
!  mov eax,edx
!@null:
!end;
Most of the time, this method will be executed very quickly. In fact, the {\f1\fs20 PropsCreate} global function is called only once, i.e. the first time this {\f1\fs20 RecordProps} method is called.
The {\f1\fs20 TSQLRecordProperties} instance is therefore created within this function:
!function PropsCreate(aTable: TSQLRecordClass): TSQLRecordProperties;
!begin // private sub function makes the code faster in most case
!  if not aTable.InheritsFrom(TSQLRecord) then
!    // invalid call
!    result := nil else begin
!    // create the properties information from RTTI
!    result := TSQLRecordProperties.Create(aTable);
!    // store the TSQLRecordProperties instance into AutoTable unused VMT entry
!    PatchCodePtrUInt(pointer(PtrInt(aTable)+vmtAutoTable),PtrUInt(result));
!    // register to the internal garbage collection (avoid memory leak)
!    GarbageCollector.Add(result);
!  end;
!end;
The {\f1\fs20 GarbageCollector} is a global {\f1\fs20 TObjectList}, which is used to store some global instances, living the whole process time, just like our {\f1\fs20 TSQLRecordProperties} values.
A per-class {\f1\fs20 TSQLRecordProperties} was made therefore available for each kind of {\f1\fs20 TSQLRecord} class.
Even most sophisticated methods of the @*ORM@ (like {\f1\fs20 TSQLRecord. GetJSONValues}) make use of these low-level {\f1\fs20 object} types. In most cases, the {\f1\fs20 GetValue} and {\f1\fs20 SetValue} methods of the {\f1\fs20 TPropInfo object} are used to convert any field value stored inside the current {\f1\fs20 TSQLRecord} instance in or from UTF-8 encoded text.

[SDD-DI-2.2.1]
; SRS-DI-2.2.1 - The {\i SQLite3} engine must be embedded to the framework
:Implementation
It's worth noting that the {\i Synopse SQLite3 database engine}, whatever its name states, is not bound to {\i SQLite3} (you can use another database engine for data storage, for example we provide a {\f1\fs20 TSQLRestServerStaticInMemory} class which implements a fast but limited in-memory database engine). Therefore, the {\i SQLite3} engine itself is not implemented in the @!TSQLRestServer!Lib\SQLite3\SQLite3Commons.pas@ unit, but in dedicated units.
The {\i SQLite3} engine is accessed at two levels:
- A low-level direct access to the {\i SQLite3} library, implemented in @!TSQLRequest.Execute,TSQLDataBase,TSQLTableDB.Create!Lib\SynSQLite3.pas@;
- A high-level access, implementing a Client-Side or Server-Side native {\f1\fs20 TSQLRest} descendant using the {\i SQLite3} library for its data persistence, in @!TSQLRestServerDB,TSQLRestClientDB!Lib\SQLite3\SQLite3.pas@.
: Low-Level access to the library
:  Compilation of the SQLite3 engine
First of all, the original source code of the library, which is retrieved from the official {\i SQLite3} web site in the form of the optimized Amalgamation file - see @http://www.sqlite.org/amalgamation.html - is compiled using the free Borland C++ command-line compiler.
&Here are the defines used for this compilation:
&//#define SQLITE_ENABLE_FTS3
&//  this unit is FTS3-ready, but not compiled with it by default
&//  if you don't use FTS3, dont define this conditional: you'll spare 50KB of code
&//  this conditional is defined at compile time, in order to create sqlite3fts3.obj
&#define SQLITE_DEFAULT_MEMSTATUS 0
&//  don't need any debug here
&#define SQLITE_THREADSAFE 2
&//  assuming multi-thread safety is made by caller - in our framework, there is
&// only one thread using the database connection at the same time, but there could
&// be multiple database connection at the same time (previous was 0 could be unsafe)
&#define SQLITE_OMIT_SHARED_CACHE 1
&// no need of shared cache in a threadsafe calling model
&#define SQLITE_OMIT_AUTOINIT 1
&//  sqlite3_initialize() is done in initialization section below -> no AUTOINIT
&#define SQLITE_OMIT_DEPRECATED 1
&//  spare some code size
&#define SQLITE_OMIT_TRACE 1
&// we don't need sqlite3_profile() and sqlite3_trace() interfaces
&#define SQLITE_OMIT_LOAD_EXTENSION 1
&// we don't need extension in an embedded engine
&#define SQLITE_OMIT_COMPILEOPTION_DIAGS 1
&// we don't need Compilation Options Diagnostics in our embedded engine
&#define SQLITE_OMIT_PROGRESS_CALLBACK 1
&// we don't need sqlite3_progress_handler() API function
&#define SQLITE_ENABLE_RTREE 1
&// the RTREE extension is now (from v.1.8/3.7) compiled into the engine
&//#define SQLITE_OMIT_LOOKASIDE
&// since we use FastMM4, LookAside is not needed but seems mandatory in c source
The only code modification made to the official {\i SQLite3} engine source code is to make {\f1\fs20 winRead} and {\f1\fs20 winWrite} function external, which will be coded in pure pascal code, in order to implement our on-the-fly encryption of the database file:
&extern int winRead(
&  sqlite3_file *id,          /* File to read from */
&  void *pBuf,                /* Write content into this buffer */
&  int amt,                   /* Number of bytes to read */
&  sqlite3_int64 offset       /* Begin reading at this offset */
&);
&extern int winWrite(
&  sqlite3_file *id,         /* File to write into */
&  const void *pBuf,         /* The bytes to be written */
&  int amt,                  /* Number of bytes to write */
&  sqlite3_int64 offset      /* Offset into the file to begin writing at */
&);
Two {\f1\fs20 .obj} files are created, named {\f1\fs20 sqlite3.obj} and {\f1\fs20 sqlite3fts3.obj}, using the following batch command (named {\f1\fs20 c.bat} in the source code repository):
$\\dev\\bcc\\bin\\bcc32 -6 -O2 -c -d -DSQLITE_ENABLE_FTS3 -u- sqlite3.c
$copy sqlite3.obj sqlite3fts3.obj
$\\dev\\bcc\\bin\\bcc32 -6 -O2 -c -d -u- sqlite3.c
The {\f1\fs20 sqlite3.obj} file won't include FTS3/FTS4, whereas {\f1\fs20 sqlite3fts3.obj} will include the FTS3/FTS4 module: the code size is a bit bigger. The {\f1\fs20 INCLUDE_FTS3} conditional must be defined for the whole application, to embed this module, as stated by the following code extracted from {\f1\fs20 SynSQLite3.pas}:
!{$ifdef INCLUDE_FTS3}
!{$L sqlite3fts3.obj}   // link SQlite3 database engine with FTS3
!{$else}
!{$L sqlite3.obj}       // link SQlite3 database engine
!{$endif}
Some low-level functions, necessary for linking to the {\i Borland C++} generated {\f1\fs20 .obj} files, are coded in asm. These runtime functions will call Delphi equivalences, which are indeed close from the {\f1\fs20 BCC32} need - see for instance {\f1\fs20 _ftol() _ftoul() malloc() free() memset() memmove() atol() _lldiv() strlen()} and such. Even higher-level functions - like {\f1\fs20 localtime()} or {\f1\fs20 qsort()} - are coded in pure Delphi code.
:  SQLite3 API access
Some types are defined in @!TSQLite3Blob,TSQLite3DB,TSQLite3FunctionContext,TSQLite3Statement,TSQLite3Value,TSQLite3ValueArray!Lib\SynSQLite3.pas@ to map the types used by {\i SQLite3}: {\f1\fs20 TSQLite3DB, TSQLite3Statement, TSQLite3Blob, TSQLite3Value, TSQLite3FunctionContext}, which are mapped to a {\f1\fs20 PtrUInt}, i.e. an unsigned integer matching the current pointer size. This is the {\i handle} type exposed by the {\i SQLite} API.
Then most C-language interface to {\i SQLite} has been converted into pure Delphi external {\f1\fs20 function} or {\f1\fs20 procedure} calls (see @http://www.sqlite.org/c3ref/intro.html for a complete reference). The conversion rule was to match the API name (all {\f1\fs20 sqlite3_*} identifiers), then provide the most Delphi-standard access to the parameters: for instance, we use standard Integer/Int64/PUTF8Char types, or a {\f1\fs20 var} declaration instead of a C pointer.
: High level access
Some Delphi classes are introduced to manage all calls and statements to C-language interface to {\i SQLite}, mapping all {\f1\fs20 sqlite3_*} functions and methods to object-oriented methods.
The @!TSQLTableDB,TSQLRequest,TSQLDataBase,TSQLBlobStream,ESQLException!Lib\SynSQLite3.pas@ unit defines the following classes:
- {\f1\fs20 ESQLException} is a custom {\i SQLite3} dedicated Exception type;
- {\f1\fs20 TSQLDataBase} is a simple wrapper for direct {\i SQLite3} database manipulation;
- {\f1\fs20 TSQLRequest} encapsulates a {\i SQLite3} request;
- {\f1\fs20 TSQLTableDB} executes a @*SQL@ statement in the local {\f1\fs20 SQLite3} database engine, and get result in memory, as JSON content;
- {\f1\fs20 TSQLBlobStream} is available to access to a {\i SQLite3} BLOB Stream.
Those database access types are then used by the following Client-Server @*REST@ful classes, to implement {\i SQLite3} storage for persistence of our @*ORM@ (the so called objects hibernation) in @!TSQLRestClientDB,TSQLRestServerDB!Lib\SQLite3\SQLite3.pas@:
- {\f1\fs20 TSQLRestClientDB} implements a REST client with direct access to a {\i SQLite3} database, that is without the Client-Server aspect of the framework;
- {\f1\fs20 TSQLRestServerDB} can be used to implement a REST server using {\i SQLite3} as its storage engine.

[SDD-DI-2.2.2]
; SRS-DI-2.2.2 - The framework libraries, including all its {\i SQLite3} related features, must be tested using Unitary testing
:Implementation
Some @*test@s classes have been developed, which methods cover most aspect of the framework:
\graph HierTSynTestCase TSynTestCase classes hierarchy
\TTestSynopsePDF\TSynTestCase
\TTestMemoryBased\TTestSQLite3Engine
\TTestFileBasedWAL\TTestFileBased
\TTestFileBased\TTestSQLite3Engine
\TTestSQLite3Engine\TSynTestCase
\TTestLowLevelTypes\TSynTestCase
\TTestLowLevelCommon\TSynTestCase
\TTestCryptographicRoutines\TSynTestCase
\TTestCompression\TSynTestCase
\TTestClientServerAccess\TSynTestCase
\TTestBigTable\TSynTestCase
\TTestBasicClasses\TSynTestCase
\TSynTestCase\TSynTest
rankdir=LR;
\
Those classes are implemented in @!TTestLowLevelCommon!Lib\SynCommons.pas@, @!TTestLowLevelTypes,TTestBasicClasses!Lib\SQLite3\SQLite3Commons.pas@, @!TTestSQLite3Engine,TTestFileBased,TTestMemoryBased,TTestFileBasedWAL!Lib\SQLite3\SQLite3.pas@ and @!TTestClientServerAccess!Lib\SQLite3\SQLite3HttpServer.pas@ units.

[SDD-DI-2.2.3]
; SRS-DI-2.2.3 - The framework shall be able to access any external database, via OleDB or direct access for Oracle (OCI) or SQLite3 (for external database files)
:SynDB classes
The @SAD@ document detailed the architecture, and main implementation part of the database-agnostic features of the framework.
:Faster late binding
For both our {\i SynDB} and {\i SynBigTable} units, we allow {\i late-binding} of data row values, using a variant and direct named access of properties. It's a very convenient way of accessing result rows values.
: Speed issue
But, in practice, this approach is slow. It uses the internal mechanism used for {\i Ole Automation}, here to access column content as if column names where native object properties. There is plenty of space for speed improvement here.
So, how does the variant type used by {\i Ole Automation} and our custom variant types (i.e. {\f1\fs20 TSynTableVariantType} or {\f1\fs20 TSQLDBRowVariantType}) handle their properties access?
Behind the scene, the Delphi compiler calls the {\f1\fs20 DispInvoke} function, as defined in the {\i Variant.pas} unit.
The default implementation of this {\f1\fs20 DispInvoke} is some kind of slow:
- It uses a {\f1\fs20 TMultiReadExclusiveWriteSynchronizer} under Delphi 6, which is a bit over-sized for its purpose: since Delphi 7, it uses a lighter critical section;
- It makes use of {\f1\fs20 WideString} for string handling (not at all the better for speed), and tends to define a lot of temporary string variables;
- For the getter method, it always makes a temporary local copy during process, which is not useful for our classes.
: Fast and furious
So we rewrite the {\f1\fs20 DispInvoke} function with some enhancements in mind:
- Will behave exactly the same for other kind of variants, in order to avoid any compatibility regression, especially with {\i Ole Automation};
- Will quick intercept our custom variant types (as registered via the global {\f1\fs20 SynRegisterCustomVariantType} function), and handle those with less overhead: no critical section nor temporary {\f1\fs20 WideString} allocations are used.
: Implementation
Here is the resulting code, from our {\i SynCommons} unit:
!procedure SynVarDispProc(Result: PVarData; const Instance: TVarData;
!      CallDesc: PCallDesc; Params: Pointer); cdecl;
!const DO_PROP = 1; GET_PROP = 2; SET_PROP = 4;
!var i: integer;
!    Value: TVarData;
!    Handler: TCustomVariantType;
!begin
!  if Instance.VType=varByRef or varVariant then // handle By Ref variants
!    SynVarDispProc(Result,PVarData(Instance.VPointer)^,CallDesc,Params) else begin
!    if Result<>nil then
!      VarClear(Variant(Result^));
!    case Instance.VType of
!    varDispatch, varDispatch or varByRef,
!    varUnknown, varUnknown or varByRef, varAny:
!       // process Ole Automation variants
!        if Assigned(VarDispProc) then
!          VarDispProc(pointer(Result),Variant(Instance),CallDesc,@Params);
!    else begin
!      // first we check for our own TSynInvokeableVariantType types
!      if SynVariantTypes<>nil then
!      for i := 0 to SynVariantTypes.Count-1 do
!        with TSynInvokeableVariantType(SynVariantTypes.List[i]) do
!        if VarType=TVarData(Instance).VType then
!        case CallDesc^.CallType of
!        GET_PROP, DO_PROP: if (Result<>nil) and (CallDesc^.ArgCount=0) then begin
!          IntGet(Result^,Instance,@CallDesc^.ArgTypes[0]);
!          exit;
!        end;
!        SET_PROP: if (Result=nil) and (CallDesc^.ArgCount=1) then begin
!          ParseParamPointer(@Params,CallDesc^.ArgTypes[0],Value);
!          IntSet(Instance,Value,@CallDesc^.ArgTypes[1]);
!          exit;
!        end;
!        end;
!      // here we call the default code handling custom types
!      if FindCustomVariantType(Instance.VType,Handler) then
!        TSynTableVariantType(Handler).DispInvoke(
!          {$ifdef DELPHI6OROLDER}Result^{$else}Result{$endif},
!          Instance,CallDesc,@Params)
!      else raise EInvalidOp.Create('Invalid variant invoke');
!    end;
!    end;
!  end;
!end;
Our custom variant types have two new virtual protected methods, named {\f1\fs20 IntGet/IntSet}, which are the getter and setter of the properties. They will to the property process, e.g. for our {\i OleDB} column retrieval:
!procedure TSQLDBRowVariantType.IntGet(var Dest: TVarData;
!  const V: TVarData; Name: PAnsiChar);
!var Rows: TSQLDBStatement;
!begin
!  Rows := TSQLDBStatement(TVarData(V).VPointer);
!  if Rows=nil then
!    EOleDBException.Create('Invalid SQLDBRowVariant call');
!  Rows.ColumnToVariant(Rows.ColumnIndex(RawByteString(Name)),Variant(Dest));
!end;
As you can see, the returned variant content is computed with the following method:
!function TOleDBStatement.ColumnToVariant(Col: integer;
!  var Value: Variant): TSQLDBFieldType;
!const FIELDTYPE2VARTYPE: array[TSQLDBFieldType] of Word = (
!  varEmpty, varNull, varInt64, varDouble, varCurrency, varDate,
!  {$ifdef UNICODE}varUString{$else}varOleStr{$endif}, varString);
!var C: PSQLDBColumnProperty;
!    V: PColumnValue;
!    P: pointer;
!    Val: TVarData absolute Value;
!begin
!  V := GetCol(Col,C);
!  if V=nil then
!    result := ftNull else
!    result := C^.ColumnType;
!  VarClear(Value);
!  Val.VType := FIELDTYPE2VARTYPE[result];
!  case result of
!    ftInt64, ftDouble, ftCurrency, ftDate:
!      Val.VInt64 := V^.Int64; // copy 64 bit content
!    ftUTF8: begin
!      Val.VPointer := nil;
!      if C^.ColumnValueInlined then
!        P := @V^.VData else
!        P := V^.VAnsiChar;
!      SetString(SynUnicode(Val.VPointer),PWideChar(P),V^.Length shr 1);
!    end;
!    ftBlob: begin
!      Val.VPointer := nil;
!      if C^.ColumnValueInlined then
!        P := @V^.VData else
!        P := V^.VAnsiChar;
!      SetString(RawByteString(Val.VPointer),PAnsiChar(P),V^.Length);
!    end;
!    end;
!end;
This above method will create the variant content without any temporary variant or string. It will return TEXT ({\f1\fs20 ftUTF8}) column as {\f1\fs20 SynUnicode}, i.e. into a generic {\f1\fs20 WideString} variant for pre-Unicode version of Delphi, and a generic {\f1\fs20 UnicodeString} (={\f1\fs20 string}) since Delphi 2009. By using the fastest available native Unicode {\f1\fs20 string} type, you will never loose any Unicode data during char-set conversion.
: Hacking the VCL
In order to enable this speed-up, we'll need to change each call to {\f1\fs20 DispInvoke} into a call to our custom {\f1\fs20 SynVarDispProc} function.
With Delphi 6, we can do that by using {\f1\fs20 GetVariantManager   /SetVariantManager} functions, and the following code:
!    GetVariantManager(VarMgr);
!    VarMgr.DispInvoke := @SynVarDispProc;
!    SetVariantManager(VarMgr);
But since Delphi 7, the {\f1\fs20 DispInvoke} function is hard-coded by the compiler into the generated asm code. If the {\i Variants} unit is used in the project, any late-binding variant process will directly call the {\f1\fs20 _DispInvoke} private function of {\f1\fs20 Variants.pas}.
First of all, we'll have to retrieve the address of this {\f1\fs20 _DispInvoke}. We just can't use {\f1\fs20 _DispInvoke} or {\f1\fs20 DispInvoke} symbol, which is not exported by the Delphi linker... But this symbol is available from asm!
So we will first define a pseudo-function which is never called, but will be compiled to provide a pointer to this {\f1\fs20 _DispInvoke} function:
!procedure VariantsDispInvoke;
!asm
!  call Variants.@DispInvoke;
!end;
Then we'll compute the corresponding address via this low-level function, the asm {\f1\fs20 call} opcode being {\f1\fs20 $E8}, followed by the relative address of the sub-routine:
!function GetAddressFromCall(AStub: Pointer): Pointer;
!begin
!  if AStub=nil then
!    result := AStub else
!  if PBYTE(AStub)^ = $E8 then begin
!    Inc(PtrInt(AStub));
!!    Result := Pointer(PtrInt(AStub)+SizeOf(integer)+PInteger(AStub)^);
!  end else
!    Result := nil;
!end;
And we'll patch this address to redirect to our own function:
! RedirectCode(GetAddressFromCall(@VariantsDispInvoke),@SynVarDispProc);
The resulting low-level asm will just look like this at the call level:
$!TestOleDB.dpr.28: assert(Copy(Customer.AccountNumber,1,8)='AW000001');
$00431124 8D45D8           lea eax,[ebp-$28]
$00431127 50               push eax
$00431128 6828124300       push $00431228
$0043112D 8D45E8           lea eax,[ebp-$18]
$00431130 50               push eax
$00431131 8D45C4           lea eax,[ebp-$3c]
$00431134 50               push eax
$!00431135 E86ED1FDFF       call @DispInvoke
It will therefore call the following hacked function:
$0040E2A8 E9B3410100       jmp SynVarDispProc
$0040E2AD E853568B5D       call +$5d8b5653
$... (previous function content, never executed)
That is, it will jump ({\f1\fs20 jmp}) to our very own {\f1\fs20 SynVarDispProc}, just as expected.
In fact, the resulting code is very close to a direct {\f1\fs20 ISQLDBRows.Column['AccountNumber']} call. Using {\i late-binding} can be both fast on the execution side, and easier on the code side.

[SDD-DI-2.3]
:SynFile main Demo
The @SAD-SynFile@ section of the associated @SAD@ has already detailed the architecture and the code used to produce a full featured application, including the User Interface generation.
Please refer to these pages for sample code and general explanation about this feature of the framework.

[SDD-DI-2.3.1.1]
; SRS-DI-2.3.1.1 - Database Grid Display, providing data in the Client Application
:Implementation
A standard {\f1\fs20 TDrawGrid} can be associated to a {\f1\fs20 TSQLTable} instance by using a {\f1\fs20 TSQLTableToGrid} object, as defined in the @!TSQLTableToGrid.Create!Lib\SQLite3\SQLite3UI.pas@:
- Just call {\f1\fs20 TSQLTableToGrid.Create(Grid,Table)} to initiate the association;
- The Table will be released when no longer necessary;
- Any former association by {\f1\fs20 TSQLTableToGrid.Create()} will be overridden;
- Handle Unicode, auto column size, field sort, incremental key lookup, optional hide ID;
- {\i Ctrl + click} on a cell to display its full Unicode content.
For instance, here is how the @!TSQLLister.Create!Lib\SQLite3\SQLite3ToolBar.pas@ unit creates a grid for every {\f1\fs20 TSQLRecord} class it refers to:
!constructor TSQLLister.Create(aOwner: TComponent; aClient: TSQLRestClientURI;
!  (...)
!!  fTableToGrid := TSQLTableToGrid.From(fGrid);
!  if fTableToGrid=nil then begin
!    // this Grid has no associated TSQLTableToGrid -> create default one
!    if fClient.InheritsFrom(TSQLRestClientURI) then
!      C := TSQLRestClientURI(fClient) else
!      C := nil;
!!    fTableToGrid := TSQLTableToGrid.Create(fGrid,aTable,C);
!    if aIDColumnHide then
!!      fTableToGrid.IDColumnHide;
!  end;
!  fTableToGrid.OnRightClickCell := OnRightClickCell;
!  TableToGrid.OnValueText := aOnValueText;
!  fGrid.DefaultDrawing := false; // we force full redraw
!  TableToGrid.OnDrawCellBackground := OnDrawCellBackground;
!  TableToGrid.OnSelectCell := OnSelectCell;
!  (...)
All the process will be done in an automated manner, using the methods of the {\f1\fs20 TDrawGrid} component.
The current implementation is very fast, since the data is taken directly from the {\f1\fs20 TSQLTable} content. A grid with more than 200,000 rows is displayed with no delay. All content is converted into pure text, according to the @*RTTI@ information associated with the {\f1\fs20 TSQLTable} columns. If it was created as a {\f1\fs20 TSQLTableJSON}, from an @*ORM@ call of the framework, it will contain the RTTI information for each column. For instance, time and date will be displayed with the current internationalization settings, from either @*ISO 8601@ encoded text (for {\f1\fs20 @*TDateTime@} published property) or our optimized {\f1\fs20 Int64} format (for {\f1\fs20 @*TTimeLog@ / @*TModTime@ / @*TCreateTime@} published property).

[SDD-DI-2.3.1.2]
; SRS-DI-2.3.1.2 - Toolbar creation from code, using RTTI
:Implementation
: Rendering
The current implementation of the framework User Interface generation handles two kind of rendering:
- Native VCL components;
- Proprietary TMS components.
You can select which set of components are used, by defining - globally to your project (i.e. in the {\i Project/Options/Conditionals} menu) - the {\f1\fs20 USETMSPACK} conditional. If it is not set (which is by default), it will use VCL components.
: Ribbon-like toolbars
As stated by the @SAD-SynFile@ section of the associated @SAD@, ribbon-like toolbars can be generated by using the {\f1\fs20 TSQLRibbon} class, as defined in @!TSQLRibbon.Create,TSQLRibbonTab,TSQLLister,TSQLCustomToolBar.Init!Lib\SQLite3\SQLite3ToolBar.pas@.
This class will use one {\f1\fs20 TSQLRibbonTab} instance per {\f1\fs20 TSQLRecord} class type it handles, displayed on its own ribbon page, with an associated {\f1\fs20 TDrawGrid} instance and a {\f1\fs20 TGDIPages} report, via a corresponding {\f1\fs20 TSQLLister} instance. Parameters provided from code to the {\f1\fs20 TSQLRibbon. Create} method can customize the toolbar content on purpose. Actions will be provided as an enumeration type, and button captions will be extracted by {\i Un @*Camel@ Casing} of each @*enumerate@d value, using @*RTTI@.
: Stand-alone toolbars
A {\f1\fs20 TSQLCustomToolBar} object can be used to create some generic toolbars, with just some icons and actions on screen, with no reference to any associated {\f1\fs20 TSQLRecord} class. See for instance this sample code:
!procedure TMainLogView.FormCreate(Sender: TObject);
!begin
!  FToolBar.Init(self,TypeInfo(TLogViewAction),ActionClick,ImageList,'');
!  FToolBar.AddToolBar('Test')
!end;
The above lines will create a panel on the owner form, with a toolbar containing one button per each {\f1\fs20 TLogViewAction} element. Icons will be taken from the supplied {\f1\fs20 ImageList} component, and the {\f1\fs20 ActionClick} event handler will be called when a button is pressed.

[SDD-DI-2.3.1.3]
; SRS-DI-2.3.1.3 - Internationalization (i18n) of the whole User Interface
:Implementation
The @!TLanguage,TLanguageFile.Create,S2U,U2S,TLanguageFile.StringToUTF8,TLanguageFile.TimeToText,TLanguageFile.DateToText,TLanguageFile.DateTimeToText,TLanguageFile.UTF8ToString,TLanguageFile.Translate,_!Lib\SQLite3\SQLite3i18n.pas@ unit is able to handle both Internationalization (i18n) and Localization (L10n).
The {\f1\fs20 TLanguageFile} class is able to retrieve a custom list of text, and use it for all {\f1\fs20 resourcestring} and screen captions. The global {\f1\fs20 _()} function, or the {\f1\fs20 Translate} method of the {\f1\fs20 TLanguageFile} class can be used to translate any English text into the corresponding language.
The generic {\f1\fs20 string} type is used when some text is to be displayed on screen. Dedicated {\f1\fs20 U2S} and {\f1\fs20 S2U} functions, or even better the {\f1\fs20 UTF8ToString} and {\f1\fs20 StringToUTF8} methods of a {\f1\fs20 TLanguageFile} instance can be used for proper conversion.
Localization is performed via some dedicated methods of the {\f1\fs20 TLanguageFile} class, like {\f1\fs20 DateToText, DateTimeToText, TimeToText}.

[SDD-DI-2.3.2]
; SRS-DI-2.3.2 - A reporting feature, with full preview and export as PDF or TXT files, must be integrated
:Implementation
The @!TGDIPages!Lib\SQLite3\SQLite3Pages.pas@ unit implements a reporting component named {\f1\fs20 TGDIPages}, with full preview and {\f1\fs20 txt/pdf} export.
Anti-aliased drawing is using the @!TGDIPlus.DrawAntiAliased!Lib\SynGdiPlus.pas@ unit, and the {\f1\fs20 TGDIPlus. DrawAntiAliased} method.
The pdf export itself is implemented via the @!TPdfDocument,TPdfCanvas.RenderMetaFile!Lib\SynPdf.pas@ unit, via a {\f1\fs20 TPdfDocument} component: every page content (in fact, a {\f1\fs20 TMetaFile} instance) is rendered via the {\f1\fs20 TPdfCanvas. RenderMetaFile} method.

[VV]
Owner=SRS
Order=SRS
DisplayName=V&V Plan
DocName=V&V Plan
Name=Software Validation and Verification Plan
Purpose=Define the testing required for the updates to the mORMot Framework software along with the testing responsibilities
; just the message for 'Goal:' in the Software Verification Plan section
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=1.8
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose
WriteTableOfContent=Yes
WriteSummaryOf=Test
; WriteSummaryOf=Test -> VV document will have 2 parts: [VV] body as introduction, then a list of all documents divided by [Test-*].Description=.., with [Test].Goal,[Test].DocName, and associated [SRS-*] (Owner=SRS) sections

:Introduction
: Purpose
This @VV@ applies to the upgrade software for the mORMot Framework, implementing the Software part of the @DI@.
Its activities cover the modifications to the software as described in the @SRS@.
: Scope
The software supplied with the instrument is divided in the following parts, as specified by the @SAD@:
...
: Risks and Contingencies
Modifications to the software are required to resolve defects or add additional approved functionality. For each new version of software the risks must be assessed if the complete V&V plan is not followed. At a minimum the following must be documented: description of change, potential impact on software, minimal testing of software that must be performed and a summary of the testing. For additional risks see the @Risk@.
: Approach
The overall approach to testing will be functionality (black box) testing. Some test will be made with specific tools (software or debugger). The comprehensiveness of the testing will be evaluated by the completion of the test summary where the features are listed.
Any software defects will be reported to the developer and added to the bug tracking database. Defects will be reviewed and resolved based on the severity, occurrence and customer impact assigned to each defect.
: Item Pass/Fail criteria
{\i Pass/Fail} criteria will be determined based on the software requirements definition.
: Test results
Test results will be documented on the test protocol summary sheet. Any defects reported during testing will also be referenced on the test protocol summary.
: Test reports
Test Reports will be compiled from the Test Plan, Procedures and Test Results.  The purposes of the Test Reports are to summarize the test protocols and results and draw a conclusion regarding the validation of the {\i Synopse mORMot Framework} to meet its design goals.
The Test Reports will contain the following information:
- Software Version Tested;
- Summary of test results;
- Summary of observations not included in this V&V plan;
- Recommendations;
- List of the features to be tested showing the test procedure used to test each specification, version tested and the pass/fail determination;
- List of each test procedure showing the date of testing, tester, pass/fail determination and reference to any defect observed in testing;
- Software Problem Reports. List all defects reported during testing with the priority and open/closed status.
: Software Verification Plan
The Test Reports are divided into several document files, as listed in the {\i Software Verification Plan} below.
The {\i Software Verification Plan} layout follows the main sections of the @DI@:
\LayoutPage
: Responsibilities
This document is intended to be reviewed by QA team.
It is the responsibility of the Software V&V person or team to:
- Follow the software V&V protocols prepared in support of this V&V plan and to document the results;
- Complete all parts of a protocol in which a selection, such as Pass or Fail is requested or explain why a determination could not be made;
- Generate a V&V report noting compliance and deviance of the measured or observed from the expected and to submit it for review.
It is the responsibility of all Software Managers, Project Manager, or their designee to review and approve the software V&V plan and the software report.
\page
:Software Verification Plan

[Test]
Owner=SRS
Order=SRS
; Owner: [Test-*] -> * reference; Order=Test -> [Test-*] have sub items
Name=Test protocols
ItemName=Test protocol
DocName=Test
Purpose=Describe all Test protocols with specific pass/fail criteria
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=1.8
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
; [Test-*] sections contain details for each SRS, [Test-SER-03] or [Test-DI-4.7.6] e.g.
; [Test-*] are displayed as they appear in the [SRS-*] sections
; all individual Test documents can be created with the [Tests] section
; a global 'Test protocols.doc' can also be created, containing all Test reports in one big file, with corresponding options
;WriteRisk=Yes
; global 'Test protocols.doc' will contain corresponding Risk assessment
BodyIsTest=Yes
; so any [Test-*] will have a special format: | at the beginning of the line, like |Actions[|Expected Results[|Observations]] - same as a table, but with no |% before and after, and possibly missing |
; -> text between | has to use \line to between paragraph/lines
; -> a line with only | is a separator between tests: a new table will be printed
; -> if any value is entered in 'Expected Results', a Pass/Fail message will be added in Validation
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose

:Introduction
This @Test@ regroups all the {\i Test protocols} in an unique document. It may be convenient to have all the test procedures in the same file, for review purpose, e.g.
Every @SRS@ item is listed with an abstract of its implementation, then its specific protocol is written, following the main sections the @DI@:
\LayoutPage
;The numerical Risk evaluation, as stated in @Risk@, is written again for every Design Input item, according to the {\i Risk Assessment Scale} table shown on page 2.

[Tests]
Owner=SRS
Order=SRS
Name=Test report
Purpose=Create all Tests protocols documents
; these Purpose= value will be used in the menu item hint
DocByDescription=Test
; -> individual documents divided by [Test-*].Description=.. can be created, with layout in [Tests]
; -> a last page is added, named 'Summary Sheet', taking executable versions and name in [SAD-*].Source= (SAD is TProject.ParseDoc)
SubDocFrontPage=TestDetails,RevisionDetails,AddPurpose
; additionnal front page values are taken from [Test-*].Requirements=.. and [Test-*].Notes=..
PreparedBy=Arnaud Bouchez
; // may be overwritten in [Test-*]

; body of this section is the last page summary sheet
{\ul{\b START TIME:}}    {\f1 __________________    }{\ul{\b END TIME:}}    {\f1 __________________}\line
{\ul{\b RELEVANT TESTING INFORMATION}}\line{\f1 _______________________________________________________________\line _______________________________________________________________\line _______________________________________________________________}\line
{\ul{\b DEVIATIONS FROM PROCEDURE}}\line {\f1 _______________________________________________________________\line _______________________________________________________________\line _______________________________________________________________}\line
{\ul{\b ATTACHMENTS LISTING}}
\par
\par
\par
{\b [  ]  MEETS SPECIFICATIONS}
{\b [  ]  DOES NOT MEET SPECIFICATIONS}\line
{\b PROCEDURE PERFORMED BY:\line{\f1  _____________________________________}}
{\b DATE:}{\f1  _____________}\line
{\b REVIEW AND APPROVAL BY:\line{\f1  _____________________________________}}
{\b DATE:}{\f1  _____________}

[SoftwareVersion]

:Synopse mORMot Framework
|%23%18%15%44
|\b File Name|Date|Version|Description\b0
|...|...|...|...
|%

[KnownIssues]

|%15%85
|\b Request|Description\b0
|...|...
|%

[SoftwareHistory]

:mORMot Framework
|%8%17%75
|\b Version|Date|Remarks\b0
|1.00|...|...
|%

[GPL]
:GNU General Public License
\include gpl-3.0.txt

[License]
:34License
The framework source code is licensed under a disjunctive three-@**license@ giving the user the choice of one of the three following sets of free software/open source licensing terms:
- {\i @*Mozilla Public License@}, version 1.1 or later (MPL);
- {\i GNU @*General Public License@}, version 2.0 or later (GPL);
- {\i GNU @*Lesser General Public License@}, version 2.1 or later (LGPL).
This allows the use of the framework code in a wide variety of software projects, while still maintaining copy-left on code Synopse wrote.
In short:
- For GPL projects, use the GPL license - see @http://www.gnu.org/licenses/gpl-2.0.html
- For LGPL license, use the LGPL license - see @http://www.gnu.org/licenses/lgpl-2.1.html
- For commercial projects, use the MPL License - see @http://www.mozilla.org/MPL/MPL-1.1.html - which is the most permissive.
In all cases, any modification made to this source code {\b should} be published by any mean (e.g. a download link), even in case of MPL. If you need any additional feature, use the forums and we may introduce a patch to the main framework trunk.
You do not have to pay any fee for using our MPL/GPL/LGPL libraries.
But please do not forget to put somewhere in your credit window or documentation, a link to @http://synopse.info if you use any of the units published under this tri-license.
For instance, if you select the MPL license, here are the requirements:
- You accept the license terms with no restriction - see @http://www.mozilla.org/MPL/2.0/FAQ.html for additional information;
- You have to publish any modified unit (e.g. {\f1\fs20 SynTaskDialog.pas}) in a public web site (e.g. {\f1\fs20 http://SoftwareCompany.com/MPL}), with a description of applied modifications, and no removal of the original license header in source code;
- You make appear some notice available in the program (About box, documentation, online help), stating e.g.\line {\i This software uses some third-party code (C) 2012 Arnaud Bouchez provided by Synopse - {\f1\fs20 http://synopse.info} - under Mozilla Public License 1.1; modified source code is available at {\f1\fs20 http://SoftwareCompany.com/MPL}.}
Note that this documentation is under GPL license only, as stated in this document front page.

[SCRS]
Owner=SRS
;Order=SRS
; no Order= specified, so that the body of this [SCRS] document section will be written as plain text only
Name=Software Change Request Summary Form
Purpose=Cross-reference all software changes
ItemName=SCRS
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=1.8
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
YesNo=Yes
; this message will be used for 'Yes / No' value in table
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose,RiskTable
WriteTableOfContent=Yes
; Write global Table Of Contents at the end of the file
TitleFlat=Yes
; so the titles will be all numerical and hierachical (without any big sections)
Landscape=Yes
; full body is to be written as Landscape

:Software Changes
\TableSoftwareChanges
:Cross References
: Reference and Related Documents
\TableDocuments
: Traceability Matrix
\TableTraceabilityMatrix
:Package Content
=[SoftwareVersion]
:Known issues
=[KnownIssues]

[Release]
Owner=DI
;Order=DI
; no Order= specified, so that the body of this [Release] document section will be written as plain text only
Name=Release Notes
Purpose=Present all software modifications introduced in the current release
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=1.8
RevisionDate=
RevisionDescription=Initial version
; Revision* multiple revision Table: ignored values are taken from current
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails
WriteTableOfContent=Yes
; Write global Table Of Contents at the beginning of the file
TitleFlat=Yes
; so the titles will be all numerical and hierachical (without any big sections)

:Introduction
: Document Purpose
This @Release@ applies to the upgrade software for the {\i Synopse mORMot Framework}, implementing the main specifications detailed in the @DI@.
It describes the software requirements and bug corrections involved in this release.
: Software Version
This release updates the {\i mORMot Framework} software modules to the following versions.
=[SoftwareVersion]
: Software specifications
This release is compatible with the following software:
- Windows XP (or later) Operating system;
- Delphi 7 up to Delphi 2010.
:Release Notes
: New features
New features implemented in this release are listed below.
\TableNewFeatures
; all DI with Request=SCR #123 will be listed here
: Bug fixes
The following bugs reported in earlier versions have been fixed.
\TableBugFixes
; all DI with SCR #65,Module 2.0+Other Module 3.0,Low will be listed here
: Installation Instructions
The Installation of this release follows the steps detailed in the {\i mORMot Framework User Manual}, and did not change from previous version.
: Special Instructions for Use
As this release is mainly a bug fix, the instructions for use did not change, and the former {\i mORMot Framework User Manual} can be seen as a valid reference document for the User.
: Open Issues
In a general manner, the User has to follow strictly the procedures detailed in the documentation shipped with the {\i mORMot} Framework, and the corresponding software updates which may have been installed.
: Testing Status
The tests below were performed by strictly following the test protocols as stated by the @VV@, after having installed the release on a dedicated test computer.\line
\TableTests=...Date of test
All tests passed successfully. Therefore, this release should not prevent users from using features.
: Significant Faults
There are no significant remaining faults, which prevent using features.
See {\b Appendix B - Known Faults list} for the current open issues identified in the release, and are planned to be corrected.
\landscape
:Appendices
: Appendix A - Software Revision History
=[SoftwareHistory]
: Appendix B - Known Faults List
The following table lists the significant bugs that were found in this release:
=[KnownIssues]
: Appendix C - Release Documentation Audit
The following table is a partial list of related documentation, including the current revision numbers and revision dates for all documentation applicable to this release.
\TableDocuments=DI,SRS,Risk,SAD,SDD,VV,SCRS
: Appendix D - ISO 123456 cross reference
Here is a table of the implementation of the {\i ISO 123456} standard in all the documentation:
\TableImplements=ISO
At the beginning of the @RK@, @SRS@, @SAD@ and @SDD@, a dedicated table will list all {\i ISO 123456} requirements implemented in this document, with its associated page.

