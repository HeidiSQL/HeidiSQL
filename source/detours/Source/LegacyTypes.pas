// **************************************************************************************************
//
// https://github.com/MahdiSafsafi/DDetours
//
// **************************************************************************************************

unit LegacyTypes;

interface

{$I DDetoursDefs.inc}

type

{$IFNDEF FPC}
{$IFNDEF DELPHI_XE_UP}
  NativeInt   = Integer;
  NativeUInt  = Cardinal;
  PNativeInt  = ^NativeInt;
  PNativeUInt = ^NativeUInt;
{$IFDEF MSWINDOWS}
  TThreadID = LongWord;
{$ENDIF MSWINDOWS}
{$ENDIF DELPHI_XE_UP}
{$ENDIF FPC}
  Int8 = Shortint;
  Int16 = Smallint;
  Int32 = Integer;

  UInt8 = Byte;
  UInt16 = Word;
  UInt32 = Cardinal;

  PInt8 = ^Int8;
  PInt16 = ^Int16;
  PInt32 = ^Int32;
  PInt64 = ^Int64;

  PUInt8 = ^UInt8;
  PUInt16 = ^UInt16;
  PUInt32 = ^UInt32;
  PUInt64 = ^UInt64;

  SIZE_T = NativeUInt;

implementation

end.
