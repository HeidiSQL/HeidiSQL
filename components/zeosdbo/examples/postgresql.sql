/*==============================================================*/
/* Database name:  PostgreSql                                   */
/* DBMS name:      PostgreSQL 7                                 */
/* Created on:     04.02.2003 19:59:06                          */
/*==============================================================*/

/*==============================================================*/
/* Table : department                                           */
/*==============================================================*/
create table department (
dep_id               SERIAL               not null,
dep_name             VARCHAR(20)          null,
dep_shname           CHAR(5)              null,
dep_address          VARCHAR(255)         null,
primary key (dep_id)
);

/*==============================================================*/
/* Table : equipment                                            */
/*==============================================================*/
create table equipment (
eq_id                SERIAL               not null,
eq_name              VARCHAR(30)          null,
eq_type              INT2                 null,
eq_cost              NUMERIC(9,4)         null,
eq_date              DATE                 null,
woff_date            DATE                 null,
primary key (eq_id)
);

/*==============================================================*/
/* Table : equipment2                                           */
/*==============================================================*/
create table equipment2 (
dep_id               INT4                 not null,
eq_id                INT4                 not null,
primary key (dep_id, eq_id),
foreign key (dep_id) references department (dep_id)
   on delete restrict on update restrict,
foreign key (eq_id) references equipment (eq_id)
   on delete restrict on update restrict
);

/*==============================================================*/
/* Table : people                                               */
/*==============================================================*/
create table people (
p_id                 SERIAL               not null,
p_dep_id             INT2                 null,
p_name               VARCHAR(40)          null,
p_begin_work         TIME without time zone null,
p_end_work           TIME without time zone null,
p_picture            BYTEA	          null,
p_resume             TEXT                 null,
p_redundant          INT2                 null,
primary key (p_id),
foreign key (p_dep_id) references department (dep_id)
   on delete restrict on update restrict
);

/*==============================================================*/
/* Table : cargo                                                */
/*==============================================================*/
create table cargo (
c_id                 SERIAL               not null,
c_dep_id             INT2                 null,
c_name               CHAR(10)             null,
c_seal               INT2                 null,
c_date_came          TIMESTAMP without time zone null,
c_date_out           TIMESTAMP without time zone null,
c_weight             FLOAT8               null,
c_width              INT4                 null,
c_height             INT4                 null,
c_cost               FLOAT4               null,
c_attributes         CHAR(10)             null,
primary key (c_id),
foreign key (c_dep_id) references department (dep_id)
   on delete restrict on update restrict
);

/*==============================================================*/
/* Table : Case_Sensitive                                       */
/*==============================================================*/
create table "Case_Sensitive" (
cs_id                 INT4                        not null,
"Cs_Data1"            INT4,
"cs_data1"            INT4,
"cs data1"            INT4,
primary key (cs_id)
);

/*==============================================================*/
/* Table : case_sensitive                                       */
/*==============================================================*/
create table case_sensitive (
cs_id                 INT4                        not null,
"CS_DATA1"            INT4,
"CS_Data2"            INT4,
"Cs_Data3"            INT4,
primary key (cs_id)
);

/*==============================================================*/
/* Table : high_load                                            */
/*==============================================================*/
create table high_load (
hl_id		      INT4 NOT NULL,
data1		      FLOAT4,
data2		      CHAR(10),
primary key (hl_id)
);

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
