/*==============================================================*/
/* Database name:  MS SQL                                       */
/* DBMS name:      Microsoft SQL Server 7.x                     */
/* Created on:     04.02.2003 20:01:43                          */
/*==============================================================*/

IF DB_ID('zeoslib') IS NOT NULL
  drop database zeoslib
go

CREATE DATABASE zeoslib COLLATE SQL_Latin1_General_Cp437_BIN
go

IF DB_ID('zeoslib') IS NULL
  CREATE DATABASE zeoslib
go


use Zeoslib
go
set quoted_identifier on

if exists (select 1
            from  sysindexes
           where  id    = object_id('cargo')
            and   name  = 'cargo_FK'
            and   indid > 0
            and   indid < 255)
   drop index cargo.cargo_FK
go


if exists (select 1
            from  sysindexes
           where  id    = object_id('equipment2')
            and   name  = 'equipment2_FK'
            and   indid > 0
            and   indid < 255)
   drop index equipment2.equipment2_FK
go


if exists (select 1
            from  sysindexes
           where  id    = object_id('equipment2')
            and   name  = 'equipment_FK'
            and   indid > 0
            and   indid < 255)
   drop index equipment2.equipment_FK
go


if exists (select 1
            from  sysindexes
           where  id    = object_id('people')
            and   name  = 'people_FK'
            and   indid > 0
            and   indid < 255)
   drop index people.people_FK
go


if exists (select 1
            from  sysobjects
           where  id = object_id('cargo')
            and   type = 'U')
   drop table cargo


if exists (select 1
            from  sysobjects
           where  id = object_id('department')
            and   type = 'U')
   drop table department
go


if exists (select 1
            from  sysobjects
           where  id = object_id('equipment')
            and   type = 'U')
   drop table equipment
go


if exists (select 1
            from  sysobjects
           where  id = object_id('equipment2')
            and   type = 'U')
   drop table equipment2
go



if exists (select 1
            from  sysobjects
           where  id = object_id('people')
            and   type = 'U')
   drop table people
go


if exists (select 1
            from  sysobjects
           where  id = object_id('"Case_Sensitive"')
            and   type = 'U')
   drop table string_values
go

if exists (select 1
            from  sysobjects
           where  id = object_id('case_sensitive')
            and   type = 'U')
   drop table string_values
go

if exists (select 1
            from  sysobjects
           where  id = object_id('high_load')
            and   type = 'U')
   drop table string_values
go

/*==============================================================*/
/* Table : cargo                                                */
/*==============================================================*/
create table cargo (
c_id                 int                  not null,
c_dep_id             smallint             null,
c_name               char(10)             null,
c_seal               tinyint              null,
c_date_came          datetime             null,
c_date_out           datetime             null,
c_weight             float                null,
c_width              int                  null,
c_height             int                  null,
c_cost               money                null,
c_attributes         binary(10)           null,
primary key  (c_id)
)
go


/*==============================================================*/
/* Index: cargo_FK                                              */
/*==============================================================*/
create   index cargo_FK on cargo (
c_dep_id
)
go


/*==============================================================*/
/* Table : department                                           */
/*==============================================================*/
create table department (
dep_id               smallint             not null,
dep_name             varchar(20)          null,
dep_shname           char(5)              null,
dep_address          varchar(255)         null,
primary key  (dep_id)
)
go


/*==============================================================*/
/* Table : equipment                                            */
/*==============================================================*/
create table equipment (
eq_id                int                  not null,
eq_name              varchar(30)          null,
eq_type              smallint             null,
eq_cost              numeric(9,4)         null,
eq_date              datetime             null,
woff_date            datetime             null,
primary key  (eq_id)
)
go


/*==============================================================*/
/* Table : equipment2                                           */
/*==============================================================*/
create table equipment2 (
dep_id               smallint             not null,
eq_id                int                  not null,
primary key  (dep_id, eq_id)
)
go


/*==============================================================*/
/* Index: equipment_FK                                          */
/*==============================================================*/
create   index equipment_FK on equipment2 (
dep_id
)
go


/*==============================================================*/
/* Index: equipment2_FK                                         */
/*==============================================================*/
create   index equipment2_FK on equipment2 (
eq_id
)
go


/*==============================================================*/
/* Table : people                                               */
/*==============================================================*/
create table people (
p_id                 smallint             not null,
p_dep_id             smallint             null,
p_name               varchar(40)          null,
p_begin_work         datetime             null,
p_end_work           datetime             null,
p_picture            image                null,
p_resume             text                 null,
p_redundant          tinyint              null,
primary key  (p_id)
)
go


/*==============================================================*/
/* Index: people_FK                                             */
/*==============================================================*/
create   index people_FK on people (
p_dep_id
)
go


/*==============================================================*/
/* Table : Case_Sensitive                                       */
/*==============================================================*/
create table [Case_Sensitive] (
cs_id                 INTEGER           not null,
[Cs_Data1]            INTEGER		null,
[cs_data1]            INTEGER		null,
[cs data1]            INTEGER		null,
primary key (cs_id)
)
go

/*==============================================================*/
/* Table : case_sensitive                                       */
/*==============================================================*/
create table case_sensitive (
cs_id                 INTEGER                        not null,
"CS_DATA1"            INTEGER		null,
"CS_Data2"            INTEGER		null,
"Cs_Data3"            INTEGER		null,
primary key (cs_id)
)
go

/*==============================================================*/
/* Table : high_load                                            */
/*==============================================================*/
create table high_load (
hl_id		      INTEGER NOT NULL,
data1		      FLOAT,
data2		      CHAR(10),
primary key (hl_id)
)
go

alter table cargo
   add foreign key (c_dep_id) references department (dep_id)
go


alter table equipment2
   add foreign key (dep_id) references department (dep_id)
go


alter table equipment2
   add foreign key (eq_id) references equipment (eq_id)
go


alter table people
   add foreign key (p_dep_id) references department (dep_id)
go


INSERT INTO department VALUES (2,'Container agency','USA','Krasnodar Komsomolskaya st. 17');
INSERT INTO department VALUES (1,'Line agency','RUS','Novorossiysk Lenina st. 2');

INSERT INTO equipment VALUES (1,'Volvo',1,15000.0000,'1998-03-04',NULL);
INSERT INTO equipment VALUES (2,'Laboratoy',10,40000.0000,'2001-10-07',NULL);
INSERT INTO equipment VALUES (3,'Computer',7,900.0000,'1999-09-03',NULL);
INSERT INTO equipment VALUES (4,'Radiostation',19,400.0000,'2000-07-08',NULL);

INSERT INTO equipment2 VALUES (1,1);
INSERT INTO equipment2 VALUES (1,2);
INSERT INTO equipment2 VALUES (1,4);
INSERT INTO equipment2 VALUES (2,1);
INSERT INTO equipment2 VALUES (2,3);

INSERT INTO people VALUES (1,1,'Vasia Pupkin','09:00:00','18:00:00',NULL,NULL,0);
INSERT INTO people VALUES (2,2,'Andy Karto','08:30:00','17:30:00',NULL,NULL,0);
INSERT INTO people VALUES (3,1,'Kristen Sato','09:00:00','18:00:00',NULL,NULL,0);
INSERT INTO people VALUES (4,2,'Aleksey Petrov','08:30:00','17:30:00',NULL,NULL,1);
INSERT INTO people VALUES (5,3,'Yan Pater','08:00:00','17:00:00',NULL,NULL,1);

INSERT INTO cargo VALUES (1,2,'Grain',1,'2002-12-20 02:00:00','2002-12-20 02:00:00',5000,NULL,NULL,1769.4300,NULL);
INSERT INTO cargo VALUES (2,1,'Paper',2,'2002-12-19 14:00:00','2002-12-23 00:00:00',1000,10,10,986.4700,'#14#‘®àâ2');
INSERT INTO cargo VALUES (3,1,'Wool',0,'2002-12-20 18:00:00',NULL,400,7,4,643.1100,NULL);
INSERT INTO cargo VALUES (4,2,'Suagr',1,'2002-12-21 10:20:00','2002-12-26 00:00:00',2034,NULL,NULL,1964.8700,NULL);
