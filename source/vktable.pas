unit vktable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLType;

type
  TVirtualKeyInfo = record
    Code: Word;
    Name: string;
  end;
  function GetVKIndexByCode(Code: Word): Integer;
  procedure GetVKNames(Items: TStrings);

const
  VKcodes: array[0..192] of TVirtualKeyInfo = (
    (Code: VK_UNKNOWN;              Name: 'UNKNOWN'),
    (Code: VK_LBUTTON;              Name: 'LBUTTON'),
    (Code: VK_RBUTTON;              Name: 'RBUTTON'),
    (Code: VK_CANCEL;               Name: 'CANCEL'),
    (Code: VK_MBUTTON;              Name: 'MBUTTON'),
    (Code: VK_XBUTTON1;             Name: 'XBUTTON1'),
    (Code: VK_XBUTTON2;             Name: 'XBUTTON2'),
    (Code: VK_BACK;                 Name: 'BACK'),
    (Code: VK_TAB;                  Name: 'TAB'),
    (Code: VK_CLEAR;                Name: 'CLEAR'),
    (Code: VK_RETURN;               Name: 'RETURN'),
    (Code: VK_SHIFT;                Name: 'SHIFT'),
    (Code: VK_CONTROL;              Name: 'CONTROL'),
    (Code: VK_MENU;                 Name: 'MENU'),
    (Code: VK_PAUSE;                Name: 'PAUSE'),
    (Code: VK_CAPITAL;              Name: 'CAPITAL'),
    (Code: VK_KANA;                 Name: 'KANA'),
    (Code: VK_HANGUL;               Name: 'HANGUL'),
    (Code: VK_JUNJA;                Name: 'JUNJA'),
    (Code: VK_FINAL;                Name: 'FINAL'),
    (Code: VK_HANJA;                Name: 'HANJA'),
    (Code: VK_KANJI;                Name: 'KANJI'),
    (Code: VK_ESCAPE;               Name: 'ESCAPE'),
    (Code: VK_CONVERT;              Name: 'CONVERT'),
    (Code: VK_NONCONVERT;           Name: 'NONCONVERT'),
    (Code: VK_ACCEPT;               Name: 'ACCEPT'),
    (Code: VK_MODECHANGE;           Name: 'MODECHANGE'),
    (Code: VK_SPACE;                Name: 'SPACE'),
    (Code: VK_PRIOR;                Name: 'PRIOR'),
    (Code: VK_NEXT;                 Name: 'NEXT'),
    (Code: VK_END;                  Name: 'END'),
    (Code: VK_HOME;                 Name: 'HOME'),
    (Code: VK_LEFT;                 Name: 'LEFT'),
    (Code: VK_UP;                   Name: 'UP'),
    (Code: VK_RIGHT;                Name: 'RIGHT'),
    (Code: VK_DOWN;                 Name: 'DOWN'),
    (Code: VK_SELECT;               Name: 'SELECT'),
    (Code: VK_PRINT;                Name: 'PRINT'),
    (Code: VK_EXECUTE;              Name: 'EXECUTE'),
    (Code: VK_SNAPSHOT;             Name: 'SNAPSHOT'),
    (Code: VK_INSERT;               Name: 'INSERT'),
    (Code: VK_DELETE;               Name: 'DELETE'),
    (Code: VK_HELP;                 Name: 'HELP'),
    (Code: VK_0;                    Name: '0'),
    (Code: VK_1;                    Name: '1'),
    (Code: VK_2;                    Name: '2'),
    (Code: VK_3;                    Name: '3'),
    (Code: VK_4;                    Name: '4'),
    (Code: VK_5;                    Name: '5'),
    (Code: VK_6;                    Name: '6'),
    (Code: VK_7;                    Name: '7'),
    (Code: VK_8;                    Name: '8'),
    (Code: VK_9;                    Name: '9'),
    (Code: VK_A;                    Name: 'A'),
    (Code: VK_B;                    Name: 'B'),
    (Code: VK_C;                    Name: 'C'),
    (Code: VK_D;                    Name: 'D'),
    (Code: VK_E;                    Name: 'E'),
    (Code: VK_F;                    Name: 'F'),
    (Code: VK_G;                    Name: 'G'),
    (Code: VK_H;                    Name: 'H'),
    (Code: VK_I;                    Name: 'I'),
    (Code: VK_J;                    Name: 'J'),
    (Code: VK_K;                    Name: 'K'),
    (Code: VK_L;                    Name: 'L'),
    (Code: VK_M;                    Name: 'M'),
    (Code: VK_N;                    Name: 'N'),
    (Code: VK_O;                    Name: 'O'),
    (Code: VK_P;                    Name: 'P'),
    (Code: VK_Q;                    Name: 'Q'),
    (Code: VK_R;                    Name: 'R'),
    (Code: VK_S;                    Name: 'S'),
    (Code: VK_T;                    Name: 'T'),
    (Code: VK_U;                    Name: 'U'),
    (Code: VK_V;                    Name: 'V'),
    (Code: VK_W;                    Name: 'W'),
    (Code: VK_X;                    Name: 'X'),
    (Code: VK_Y;                    Name: 'Y'),
    (Code: VK_Z;                    Name: 'Z'),
    (Code: VK_LWIN;                 Name: 'LWIN'),
    (Code: VK_RWIN;                 Name: 'RWIN'),
    (Code: VK_APPS;                 Name: 'APPS'),
    (Code: VK_SLEEP;                Name: 'SLEEP'),
    (Code: VK_NUMPAD0;              Name: 'NUMPAD0'),
    (Code: VK_NUMPAD1;              Name: 'NUMPAD1'),
    (Code: VK_NUMPAD2;              Name: 'NUMPAD2'),
    (Code: VK_NUMPAD3;              Name: 'NUMPAD3'),
    (Code: VK_NUMPAD4;              Name: 'NUMPAD4'),
    (Code: VK_NUMPAD5;              Name: 'NUMPAD5'),
    (Code: VK_NUMPAD6;              Name: 'NUMPAD6'),
    (Code: VK_NUMPAD7;              Name: 'NUMPAD7'),
    (Code: VK_NUMPAD8;              Name: 'NUMPAD8'),
    (Code: VK_NUMPAD9;              Name: 'NUMPAD9'),
    (Code: VK_MULTIPLY;             Name: 'MULTIPLY'),
    (Code: VK_ADD;                  Name: 'ADD'),
    (Code: VK_SEPARATOR;            Name: 'SEPARATOR'),
    (Code: VK_SUBTRACT;             Name: 'SUBTRACT'),
    (Code: VK_DECIMAL;              Name: 'DECIMAL'),
    (Code: VK_DIVIDE;               Name: 'DIVIDE'),
    (Code: VK_F1;                   Name: 'F1'),
    (Code: VK_F2;                   Name: 'F2'),
    (Code: VK_F3;                   Name: 'F3'),
    (Code: VK_F4;                   Name: 'F4'),
    (Code: VK_F5;                   Name: 'F5'),
    (Code: VK_F6;                   Name: 'F6'),
    (Code: VK_F7;                   Name: 'F7'),
    (Code: VK_F8;                   Name: 'F8'),
    (Code: VK_F9;                   Name: 'F9'),
    (Code: VK_F10;                  Name: 'F10'),
    (Code: VK_F11;                  Name: 'F11'),
    (Code: VK_F12;                  Name: 'F12'),
    (Code: VK_F13;                  Name: 'F13'),
    (Code: VK_F14;                  Name: 'F14'),
    (Code: VK_F15;                  Name: 'F15'),
    (Code: VK_F16;                  Name: 'F16'),
    (Code: VK_F17;                  Name: 'F17'),
    (Code: VK_F18;                  Name: 'F18'),
    (Code: VK_F19;                  Name: 'F19'),
    (Code: VK_F20;                  Name: 'F20'),
    (Code: VK_F21;                  Name: 'F21'),
    (Code: VK_F22;                  Name: 'F22'),
    (Code: VK_F23;                  Name: 'F23'),
    (Code: VK_F24;                  Name: 'F24'),
    (Code: VK_NUMLOCK;              Name: 'NUMLOCK'),
    (Code: VK_SCROLL;               Name: 'SCROLL'),
    (Code: VK_LSHIFT;               Name: 'LSHIFT'),
    (Code: VK_RSHIFT;               Name: 'RSHIFT'),
    (Code: VK_LCONTROL;             Name: 'LCONTROL'),
    (Code: VK_RCONTROL;             Name: 'RCONTROL'),
    (Code: VK_LMENU;                Name: 'LMENU'),
    (Code: VK_RMENU;                Name: 'RMENU'),
    (Code: VK_BROWSER_BACK;         Name: 'BROWSER_BACK'),
    (Code: VK_BROWSER_FORWARD;      Name: 'BROWSER_FORWARD'),
    (Code: VK_BROWSER_REFRESH;      Name: 'BROWSER_REFRESH'),
    (Code: VK_BROWSER_STOP;         Name: 'BROWSER_STOP'),
    (Code: VK_BROWSER_SEARCH;       Name: 'BROWSER_SEARCH'),
    (Code: VK_BROWSER_FAVORITES;    Name: 'BROWSER_FAVORITES'),
    (Code: VK_BROWSER_HOME;         Name: 'BROWSER_HOME'),
    (Code: VK_VOLUME_MUTE;          Name: 'VOLUME_MUTE'),
    (Code: VK_VOLUME_DOWN;          Name: 'VOLUME_DOWN'),
    (Code: VK_VOLUME_UP;            Name: 'VOLUME_UP'),
    (Code: VK_MEDIA_NEXT_TRACK;     Name: 'MEDIA_NEXT_TRACK'),
    (Code: VK_MEDIA_PREV_TRACK;     Name: 'MEDIA_PREV_TRACK'),
    (Code: VK_MEDIA_STOP;           Name: 'MEDIA_STOP'),
    (Code: VK_MEDIA_PLAY_PAUSE;     Name: 'MEDIA_PLAY_PAUSE'),
    (Code: VK_LAUNCH_MAIL;          Name: 'LAUNCH_MAIL'),
    (Code: VK_LAUNCH_MEDIA_SELECT;  Name: 'LAUNCH_MEDIA_SELECT'),
    (Code: VK_LAUNCH_APP1;          Name: 'LAUNCH_APP1'),
    (Code: VK_LAUNCH_APP2;          Name: 'LAUNCH_APP2'),
    (Code: VK_OEM_1;                Name: 'OEM_1'),
    (Code: VK_OEM_PLUS;             Name: 'OEM_PLUS'),
    (Code: VK_OEM_COMMA;            Name: 'OEM_COMMA'),
    (Code: VK_OEM_MINUS;            Name: 'OEM_MINUS'),
    (Code: VK_OEM_PERIOD;           Name: 'OEM_PERIOD'),
    (Code: VK_OEM_2;                Name: 'OEM_2'),
    (Code: VK_OEM_3;                Name: 'OEM_3'),
    (Code: VK_OEM_4;                Name: 'OEM_4'),
    (Code: VK_OEM_5;                Name: 'OEM_5'),
    (Code: VK_OEM_6;                Name: 'OEM_6'),
    (Code: VK_OEM_7;                Name: 'OEM_7'),
    (Code: VK_OEM_8;                Name: 'OEM_8'),
    (Code: VK_OEM_102;              Name: 'OEM_102'),
    (Code: VK_PROCESSKEY;           Name: 'PROCESSKEY'),
    (Code: VK_ATTN;                 Name: 'ATTN'),
    (Code: VK_CRSEL;                Name: 'CRSEL'),
    (Code: VK_EXSEL;                Name: 'EXSEL'),
    (Code: VK_EREOF;                Name: 'EREOF'),
    (Code: VK_PLAY;                 Name: 'PLAY'),
    (Code: VK_ZOOM;                 Name: 'ZOOM'),
    (Code: VK_NONAME;               Name: 'NONAME'),
    (Code: VK_PA1;                  Name: 'PA1'),
    (Code: VK_OEM_CLEAR;            Name: 'OEM_CLEAR'),
    (Code: VK_HIGHESTVALUE;         Name: 'HIGHESTVALUE'),
    (Code: VK_UNDEFINED;            Name: 'UNDEFINED'),
    (Code: VK_LCL_EQUAL;            Name: 'LCL_EQUAL'),
    (Code: VK_LCL_COMMA;            Name: 'LCL_COMMA'),
    (Code: VK_LCL_POINT;            Name: 'LCL_POINT'),
    (Code: VK_LCL_SLASH;            Name: 'LCL_SLASH'),
    (Code: VK_LCL_SEMI_COMMA;       Name: 'LCL_SEMI_COMMA'),
    (Code: VK_LCL_MINUS;            Name: 'LCL_MINUS'),
    (Code: VK_LCL_OPEN_BRACKET;     Name: 'LCL_OPEN_BRACKET'),
    (Code: VK_LCL_CLOSE_BRACKET;    Name: 'LCL_CLOSE_BRACKET'),
    (Code: VK_LCL_BACKSLASH;        Name: 'LCL_BACKSLASH'),
    (Code: VK_LCL_TILDE;            Name: 'LCL_TILDE'),
    (Code: VK_LCL_QUOTE;            Name: 'LCL_QUOTE'),
    (Code: VK_LCL_ALT;              Name: 'LCL_ALT'),
    (Code: VK_LCL_LALT;             Name: 'LCL_LALT'),
    (Code: VK_LCL_RALT;             Name: 'LCL_RALT'),
    (Code: VK_LCL_CAPSLOCK;         Name: 'LCL_CAPSLOCK'),
    (Code: VK_LCL_POWER;            Name: 'LCL_POWER'),
    (Code: VK_LCL_CALL;             Name: 'LCL_CALL'),
    (Code: VK_LCL_ENDCALL;          Name: 'LCL_ENDCALL'),
    (Code: VK_LCL_AT;               Name: 'LCL_AT')
  );

implementation

function GetVKIndexByCode(Code: Word): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i:=Low(VKcodes) to High(VKCodes) do begin
    if VKcodes[i].Code = Code then begin
      Result := i;
      Break;
    end;
  end;
end;

procedure GetVKNames(Items: TStrings);
var
  i: Integer;
begin
  Items.Clear;
  for i:=Low(VKcodes) to High(VKcodes) do begin
    Items.Add(VKcodes[i].Name);
  end;
end;

end.

