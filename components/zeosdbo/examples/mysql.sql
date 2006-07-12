
/*==============================================================*/
/* Database name:  MySql                                        */
/* DBMS name:      MySQL 3.23                                   */
/* Created on:     04.02.2003 19:48:39                          */
/*==============================================================*/

drop database if exists zeoslib;

create database zeoslib;

use zeoslib;

INSERT INTO blob_values VALUES (1, NULL, NULL);

/*==============================================================*/
/* Table : date_values                                          */
/*==============================================================*/
create table if not exists date_values
(
   d_id                           int                    not null,
   d_date                         date,
   d_time                         time,
   d_datetime                     datetime,
   d_timestamp                    timestamp,
   primary key (d_id)
);

/*==============================================================*/
/* Table : department                                           */
/*==============================================================*/
create table if not exists department
(
   dep_id                         smallint not null auto_increment,
   dep_name                       varchar(20),
   dep_shname                     char(5),
   dep_address                    varchar(255),
   primary key (dep_id)
);

/*==============================================================*/
/* Table : equipment                                            */
/*==============================================================*/
create table if not exists equipment
(
   eq_id                          int not null auto_increment,
   eq_name                        varchar(30),
   eq_type                        smallint,
   eq_cost                        numeric(9,4),
   eq_date                        date,
   woff_date                      date,
   primary key (eq_id)
);

/*==============================================================*/
/* Table : equipment2                                           */
/*==============================================================*/
create table if not exists equipment2
(
   dep_id                         smallint not null,
   eq_id                          int not null,
   primary key (dep_id, eq_id)   
);

/*==============================================================*/
/* Index: equipment_FK                                          */
/*==============================================================*/
create index equipment_FK on equipment2
(
   dep_id
);

/*==============================================================*/
/* Index: equipment2_FK                                         */
/*==============================================================*/
create index equipment2_FK on equipment2
(
   eq_id
);

/*==============================================================*/
/* Table : extension                                            */
/*==============================================================*/
create table if not exists extension
(
   ext_id                         CHAR(10),
   ext_set1                       SET('Y', 'N'),
   ext_set2                       SET('White', 'Black', 'Yellow'),
   ext_enum                       ENUM('Car', 'House', 'Work', 'Dog', 'Wife', 'Child')
);

/*==============================================================*/
/* Table : people                                               */
/*==============================================================*/
create table if not exists people
(
   p_id                           smallint  not null auto_increment,
   p_dep_id                       smallint,
   p_name                         varchar(40),
   p_begin_work                   time,
   p_end_work                     time,
   p_picture                      longblob,
   p_resume                       text,
   p_redundant                    tinyint(1),
   primary key (p_id)   
);

/*==============================================================*/
/* Index: people_FK                                             */
/*==============================================================*/
create index people_FK on people
(
   p_dep_id
);

/*==============================================================*/
/* Table : cargo                                                */
/*==============================================================*/
create table if not exists cargo
(
   c_id                           bigint not null auto_increment,
   c_dep_id                       smallint,
   c_name                         CHAR(10),
   c_seal                         tinyint(1),
   c_date_came                    datetime,
   c_date_out                     datetime,
   c_weight                       float,
   c_width                        int,
   c_height                       int,
   c_cost                         float(12,4),
   c_attributes                   blob,
   primary key (c_id)   
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

/*==============================================================*/
/* Index: cargo_FK                                              */
/*==============================================================*/
create index cargo_FK on cargo
(
   c_dep_id
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
