program IdeBplInstall;

{$APPTYPE CONSOLE}

uses
  Windows,
  Registry,
  Classes,
  SysUtils;

const
  baseKey = '\Software\Borland';
  pkgKey = 'Known Packages';
  envKey = 'Environment Variables';

var
  path: string;
  bpl: string;
  descr: string;
  reg: TRegistry;
  productKeys: TStrings;
  versionKeys: TStrings;
  found: boolean;
  i, j: integer;
  s: string;

begin
  if ParamCount <> 3 then begin
    WriteLn('');
    WriteLn('Usage:');
    WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' <fully qualified bpl path> <filename> <description>');
    WriteLn('');
    ExitCode := 1;
    Exit;
  end;

  path := ParamStr(1);
  bpl := path + '\' + ParamStr(2);
  descr := ParamStr(3);
  if
    ((bpl[1] = '"') and (bpl[Length(bpl)] = '"')) or
    ((bpl[1] = '''') and (bpl[Length(bpl)] = '''')) then begin
    bpl := Copy(bpl, 2, Length(bpl) - 2);
  end;
  if
    ((descr[1] = '"') and (descr[Length(descr)] = '"')) or
    ((descr[1] = '''') and (descr[Length(descr)] = '''')) then begin
    descr := Copy(descr, 2, Length(descr) - 2);
  end;

  if not FileExists(bpl) then begin
    WriteLn('');
    WriteLn('Error: Could not open file ' + bpl + '.');
    WriteLn('');
    WriteLn('Make sure that the file exists (look for compilation success/failure).');
    WriteLn('');
    ExitCode := 2;
    Exit;
  end;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    s := baseKey;
    reg.Access := KEY_READ;
    if not reg.OpenKeyReadOnly(s) then begin
      WriteLn('');
      WriteLn('Error: Could not open key ' + s + '.');
      WriteLn('');
      WriteLn('To fix this, try starting, then exiting, the IDE once to create default keys.');
      WriteLn('');
      ExitCode := 3;
      Exit;
    end;
    productKeys := TStringList.Create;
    try
      reg.GetKeyNames(productKeys);
      reg.CloseKey;
      if productKeys.Count > 0 then for i := 0 to productKeys.Count - 1 do begin
        s := baseKey + '\' + productKeys[i];
        reg.Access := KEY_READ;
        if not reg.OpenKeyReadOnly(s) then begin
          WriteLn('Error: Could not open key ' + s + '.');
          ExitCode := 4;
          Exit;
        end;
        versionKeys := TStringList.Create;
        try
          reg.GetKeyNames(versionKeys);
          reg.CloseKey;
          if versionKeys.Count > 0 then for j := 0 to versionKeys.Count - 1 do begin
            // Install package in IDE.
            s := baseKey + '\' + productKeys[i] + '\' + versionKeys[j] + '\' + pkgKey;
            reg.Access := KEY_WRITE;
            found := reg.KeyExists(s);
            if found then begin
              if not reg.OpenKey(s, false) then begin
                WriteLn('Error: Could not open key ' + s + '.');
                ExitCode := 5;
                Exit;
              end;
              reg.WriteString(bpl, descr);
              reg.CloseKey;
            end;
            // Work around Delphi bug #23225.
            s := baseKey + '\' + productKeys[i] + '\' + versionKeys[j] + '\' + envKey;
            reg.Access := KEY_READ or KEY_WRITE;
            if found and not reg.KeyExists(s) then reg.CreateKey(s);
            if reg.KeyExists(s) then begin
              if not reg.OpenKey(s, false) then begin
                WriteLn('Error: Could not open key ' + s + '.');
                ExitCode := 6;
                Exit;
              end;
              s := reg.ReadString('Path');
              s := StringReplace(s, path, '', [rfReplaceAll, rfIgnoreCase]);
              s := s + ';' + path;
              while Pos(';;', s) > 0 do s := StringReplace(s, ';;', ';', [rfReplaceAll]);
              while (Length(s) > 0) and (s[1] = ';') do Delete(s, 1, 1);
              while (Length(s) > 0) and (s[Length(s)] = ';') do Delete(s, 1, 1);
              reg.WriteString('Path', s);
              reg.CloseKey;
            end;
          end;
        finally
          FreeAndNil(versionKeys);
        end;
      end;
    finally
      FreeAndNil(productKeys);
    end;
  finally
    FreeAndNil(reg);
  end;
end.
