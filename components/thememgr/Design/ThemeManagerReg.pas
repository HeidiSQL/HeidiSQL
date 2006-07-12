unit ThemeManagerReg;

// Comment out the following if using Standard versions of Delphi or C++ Builder
// that do not come with the database components, or if you do not want to
// link to the database VCL units.

{$define THEMEDATABASE}

//----------------------------------------------------------------------------------------------------------------------
// Windows XP Theme Manager is freeware. You may freely use it in any software, including commercial software, provided
// you accept the following conditions:
//
// 1) The software may not be included into component collections and similar compilations which are sold. If you want
//    to distribute this software for money then contact me first and ask for my permission.
// 2) My copyright notices in the source code may not be removed or modified.
// 3) If you modify and/or distribute the code to any third party then you must not veil the original author. It must
//    always be clearly identifiable that I, Mike Lischke, am the original author.
// Although it is not required it would be a nice move to recognize my work by adding a citation to the application's
// about box or a similar place.
//
// The original code is ThemeMgr.pas, released 01. January 2002.
//
// The initial developer of the original code is:
//   Mike Lischke (public@soft-gems.net, www.soft-gems.net).
//
// Portions created by Mike Lischke are
// (C) 2001-2005 Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  Windows, Messages, Classes, ThemeMgr {$ifdef THEMEDATABASE}, ThemeMgrDB {$endif};

procedure Register;

//----------------------------------------------------------------------------------------------------------------------

implementation

//-------------------------------------------------------------------------------------------------------------------

procedure Register;

begin
  RegisterComponents('XP', [TThemeManager {$ifdef THEMEDATABASE}, TThemeManagerDB {$endif}]);
end;

//-------------------------------------------------------------------------------------------------------------------

end.

