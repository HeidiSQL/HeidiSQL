/*==============================================================*/
/*create database "zeoslib.gdb" user "SYSDBA" password "masterkey" page_size=4096;*/
/*==============================================================*/

drop table cargo;

drop table people;

drop table equipment2;

drop table equipment;

drop table department;

drop table "Case_Sensitive";

drop table case_sensitive;

drop table high_load;

/*==============================================================*/
/* Table : cargo                                                */
/*==============================================================*/
create table cargo (
c_id                 INTEGER                        not null,
c_dep_id             SMALLINT,
c_name               CHAR(10),
c_seal               SMALLINT,
c_date_came          TIMESTAMP,
c_date_out           TIMESTAMP,
c_weight             FLOAT,
c_width              INTEGER,
c_height             INTEGER,
c_cost               NUMERIC(12,4),
c_attributes         BLOB SUB_TYPE TEXT,
primary key (c_id)
);

/*==============================================================*/
/* Table : department                                           */
/*==============================================================*/
create table department (
dep_id               SMALLINT                       not null,
dep_name             VARCHAR(20),
dep_shname           CHAR(5),
dep_address          VARCHAR(255),
primary key (dep_id)
);

/*==============================================================*/
/* Table : equipment                                            */
/*==============================================================*/
create table equipment (
eq_id                INTEGER                        not null,
eq_name              VARCHAR(30),
eq_type              SMALLINT,
eq_cost              NUMERIC(9,4),
eq_date              DATE,
woff_date            DATE,
primary key (eq_id)
);

/*==============================================================*/
/* Table : equipment2                                           */
/*==============================================================*/
create table equipment2 (
dep_id               SMALLINT                       not null,
eq_id                INTEGER                        not null,
primary key (dep_id, eq_id)
);

/*==============================================================*/
/* Table : people                                               */
/*==============================================================*/
create table people (
p_id                 SMALLINT                       not null,
p_dep_id             SMALLINT,
p_name               VARCHAR(40),
p_begin_work         TIME,
p_end_work           TIME,
p_picture            BLOB,
p_resume             BLOB SUB_TYPE TEXT,
p_redundant          SMALLINT,
primary key (p_id)
);

/*==============================================================*/
/* Table : Case_Sensitive                                       */
/*==============================================================*/
create table "Case_Sensitive" (
cs_id                 INTEGER                        not null,
"Cs_Data1"            INTEGER,
"cs_data1"            INTEGER,
"cs data1"            INTEGER,
primary key (cs_id)
);

/*==============================================================*/
/* Table : case_sensitive                                       */
/*==============================================================*/
create table case_sensitive (
cs_id                 INTEGER                        not null,
"CS_DATA1"            INTEGER,
"CS_Data2"            INTEGER,
"Cs_Data3"            INTEGER,
primary key (cs_id)
);

/*==============================================================*/
/* Table : high_load                                            */
/*==============================================================*/
create table high_load (
hl_id		      INTEGER NOT NULL,
data1		      FLOAT,
data2		      CHAR(10),
primary key (hl_id)
);

alter table cargo
   add foreign key (c_dep_id) references department (dep_id);

alter table equipment2
   add foreign key (dep_id) references department (dep_id);

alter table equipment2
   add foreign key (eq_id) references equipment (eq_id);

alter table people
   add foreign key (p_dep_id) references department (dep_id);


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
INSERT INTO cargo VALUES (2,1,'Paper',2,'2002-12-19 14:00:00','2002-12-23 00:00:00',1000,10,10,986.4700,'#14#СЃав2');
INSERT INTO cargo VALUES (3,1,'Wool',0,'2002-12-20 18:00:00',NULL,400,7,4,643.1100,NULL);
INSERT INTO cargo VALUES (4,2,'Suagr',1,'2002-12-21 10:20:00','2002-12-26 00:00:00',2034,NULL,NULL,1964.8700,NULL);
