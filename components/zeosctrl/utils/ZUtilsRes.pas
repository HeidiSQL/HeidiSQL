{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                  Common resources                      }
{                                                        }
{       Copyright (c) 1999-2000 Sergey Seroukhov         }
{                                                        }
{********************************************************}

unit ZUtilsRes;

interface

uses Controls, ZUtilsConst;

const
  IDC_HANDSHOW = 14000;
  IDC_HANDGRAB = 14001;
  IDC_HANDFLAT = 14002;
  IDC_HANDCUR  = 14003;
  IDC_DRAGCUR  = 14004;

const
  crHand     = TCursor(IDC_HANDSHOW);
  crDragHand = TCursor(IDC_HANDGRAB);
  crHandFlat = TCursor(IDC_HANDFLAT);
  crHandGrab = TCursor(IDC_HANDCUR);
  crHandShow = TCursor(IDC_DRAGCUR);

const
{ TBitmap.GetTransparentColor from GRAPHICS.PAS use this value }
  PaletteMask = $02000000;

{$IFDEF VER100}
const
  SDelphiKey = 'Software\Borland\Delphi\3.0';
{$ENDIF}

{$IFDEF VER120}
const
  SDelphiKey = 'Software\Borland\Delphi\4.0';
{$ENDIF}

implementation

uses Windows, Forms;

{$R ..\resources\ZUtilsRes.RES}

initialization
  Screen.Cursors[crHand]     := LoadCursor(hInstance, 'IDC_HANDCUR');
  Screen.Cursors[crDragHand] := LoadCursor(hInstance, 'IDC_DRAGCUR');
  Screen.Cursors[crHandFlat] := LoadCursor(HInstance,'IDC_HANDFLAT');
  Screen.Cursors[crHandGrab] := LoadCursor(HInstance,'IDC_HANDGRAB');
  Screen.Cursors[crHandShow] := LoadCursor(HInstance,'IDC_HANDSHOW');
end.