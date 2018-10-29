// **************************************************************************************************
// Delphi Detours Library
// Unit GenericsCast
// http://code.google.com/p/delphi-detours-library/

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is GenericsCast.pas.
// The Initial Developer of the Original Code is Mahdi Safsafi [SMP3].
// Portions created by Mahdi Safsafi . are Copyright (C) 2013-2014 Mahdi Safsafi .
// All Rights Reserved.
//
// **************************************************************************************************

unit GenericsCast;

interface

uses
  System.Math, WinApi.Windows;

type
  { T = Type ; TT = ToType }
TGenericsCast < T, TT >= class(TObject)
/// <summary> Cast T type to TT type .
/// </summary>
  class function Cast(const T: T): TT;
end;

implementation

{ TGenericsCast<T, TT> }

class function TGenericsCast<T, TT>.Cast(const T: T): TT;
begin
  Result := Default (TT);
  CopyMemory(@Result, @T, Min(SizeOf(T), SizeOf(TT)));
end;

end.
