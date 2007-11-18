
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntFileCtrl;

{$INCLUDE TntCompilers.inc}

interface

{$WARN UNIT_PLATFORM OFF}

uses
  Classes, Windows, FileCtrl;

{TNT-WARN SelectDirectory}
function WideSelectDirectory(const Caption: WideString; const Root: WideString;
  var Directory: WideString): Boolean;

implementation

uses
  SysUtils, Forms, ActiveX, ShlObj, ShellApi, TntSysUtils, TntWindows;

function SelectDirCB_W(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
begin
  if (uMsg = BFFM_INITIALIZED) and (lpData <> 0) then
    SendMessageW(Wnd, BFFM_SETSELECTIONW, Integer(True), lpdata);
  result := 0;
end;

function WideSelectDirectory(const Caption: WideString; const Root: WideString;
  var Directory: WideString): Boolean;
{$IFNDEF COMPILER_7_UP}
const
  BIF_NEWDIALOGSTYLE     = $0040;
  BIF_USENEWUI = BIF_NEWDIALOGSTYLE or BIF_EDITBOX;
{$ENDIF}
var
  WindowList: Pointer;
  BrowseInfo: TBrowseInfoW;
  Buffer: PWideChar;
  OldErrorMode: Cardinal;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
  AnsiDirectory: AnsiString;
begin
  if (not Win32PlatformIsUnicode) then begin
    AnsiDirectory := Directory;
    Result := SelectDirectory{TNT-ALLOW SelectDirectory}(Caption, Root, AnsiDirectory);
    Directory := AnsiDirectory;
  end else begin
    Result := False;
    if not WideDirectoryExists(Directory) then
      Directory := '';
    FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
    if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
    begin
      Buffer := ShellMalloc.Alloc(MAX_PATH * SizeOf(WideChar));
      try
        RootItemIDList := nil;
        if Root <> '' then
        begin
          SHGetDesktopFolder(IDesktopFolder);
          IDesktopFolder.ParseDisplayName(Application.Handle, nil,
            POleStr(Root), Eaten, RootItemIDList, Flags);
        end;
        with BrowseInfo do
        begin
          {$IFDEF COMPILER_9_UP}
          hWndOwner := Application.ActiveFormHandle;
          {$ELSE}
          hWndOwner := Application.Handle;
          {$ENDIF}
          pidlRoot := RootItemIDList;
          pszDisplayName := Buffer;
          lpszTitle := PWideChar(Caption);
          ulFlags := BIF_RETURNONLYFSDIRS;
          if Win32MajorVersion >= 5 then
            ulFlags := ulFlags or BIF_USENEWUI;
          if Directory <> '' then
          begin
            lpfn := SelectDirCB_W;
            lParam := Integer(PWideChar(Directory));
          end;
        end;
        WindowList := DisableTaskWindows(0);
        OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
        try
          ItemIDList := Tnt_ShBrowseForFolderW(BrowseInfo);
        finally
          SetErrorMode(OldErrorMode);
          EnableTaskWindows(WindowList);
        end;
        Result :=  ItemIDList <> nil;
        if Result then
        begin
          Tnt_ShGetPathFromIDListW(ItemIDList, Buffer);
          ShellMalloc.Free(ItemIDList);
          Directory := Buffer;
        end;
      finally
        ShellMalloc.Free(Buffer);
      end;
    end;
  end;
end;

end.
