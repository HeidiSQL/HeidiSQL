{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Universal Dataset component               }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDataset;

interface

{$I ZComponent.inc}

uses ZAbstractRODataset, ZAbstractDataset, ZAbstractTable;

type

  {** Implements an universal SQL query for read/only data access. }
  TZReadOnlyQuery = class (TZAbstractRODataSet)
  published
    property Active;
  {$IFDEF VER130BELOW}
    property IsUniDirectional;
  {$ENDIF}
    property SQL;
    property ParamCheck;
    property Params;
    property Properties;
    property DataSource;
    property MasterFields;
    property MasterSource;
    property LinkedFields; {renamed by bangfauzan}
    property IndexFieldNames; {bangfauzan addition}
    property Options;
  end;

  {** Implements an universal SQL query for read/write data access. }
  TZQuery = class (TZAbstractDataSet)
  published
    property Active;
    property ReadOnly default False;
    property SQL;
    property ParamCheck;
    property Params;
    property ShowRecordTypes;
    property Properties;
    property DataSource;
    property MasterFields;
    property MasterSource;
    property LinkedFields; {renamed by bangfauzan}
    property IndexFieldNames; {bangfauzan addition}
    property UpdateMode;
    property WhereMode;
    property Options;
    property Sequence;
    property SequenceField;
  end;

  {** Implements an universal SQL query for single table access. }
  TZTable = class (TZAbstractTable)
  published
    property Active;
    property ReadOnly default False;
    property TableName;
    property ShowRecordTypes;
    property Properties;
    property MasterFields;
    property MasterSource;
    property LinkedFields; {renamed by bangfauzan}
    property IndexFieldNames; {bangfauzan addition}
    property UpdateMode;
    property WhereMode;
    property Options;
    property Sequence;
    property SequenceField;
  end;

implementation

end.

