SizeGrip.pas, SizeGripThemed.pas

  Delphi Komponenten zum Hinzufügen eines "size grip" (wie bei einem
  StatusBar) in der rechten unteren Ecke eines TWinControl Elements
  (wie z.B. TForm selbst). "SizeGripThemed.pas" benutzt das aktuell
  eingestellte visuelle Layout.

SizeGripHWND.pas

  Delphi Unit für nonVCL-Anwendungen, um denselben Effekt ohne die
  VCL zu erreichen.

Version 1.2a - die aktuelle Version gibt's immer unter
http://flocke.vssd.de/prog/code/pascal/sizegrip/

Copyright (C) 2005, 2006 Volker Siebert <flocke@vssd.de>
Alle Rechte vorbehalten.

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.

---------------------------------------------------------------------------

Delphi-Versionen: 5, 6, 7, 2005 und 2006.

SizeGripThemed benötigt UxTheme.pas - wenn euer Delphi keins vorinstalliert
hat, dann benutzt das von Mike Lischkes Homepage:
http://www.lischke-online.de/ThemeManager.php.

HINWEIS:

Die Komponente "TSizeGripXP" ist nur noch zur Kompatibilität vorhanden;
benutzt statt dessen einfach "TSizeGrip" mit "NewStyle = true".

BENUTZUNG:

Einfach im Ereignis "FormCreate" eine neue Komponente vom Typ TSizeGrip
oder TSizeGripThemed erzeugen und die Eigenschaft "TargetControl" zuweisen.

Beispiel:
+-------------------------------------------------------------------------
| uses
|   SizeGrip;
|
| ...
|
| procedure TForm1.FormCreate(Sender: TObject);
| begin
|   ...
|   with TSizeGrip.Create(Self) do
|     TargetControl := Self;
| end;
+-------------------------------------------------------------------------

Danach 'gehört' die neue Komponente dem Formular und wird bei dessen
Freigabe automatisch freigegeben.

- TSizeGrip zeichnet ein Grip mit diagonalen Linien oder 6 Vertiefungen
  mit den aktuellen 3D-Farben.
- TSizeGripThemed zeichnet das Grip mit dem aktuell eingestellten
  visuellen Stil (falls vorhanden) oder wie TSizeGrip (sonst).

INSTALLATION:

Natürlich kann man die Komponenten auch in die Werkzeugpalette
installieren - dazu muss man nur die Units dem Benutzerpackage hinzufügen
und compilieren - sie erscheinen dann auf der Seite "System".

NON-VCL BENUTZUNG:

Einfach die Unit SizeGripHWND.pas einbinden und die einzige öffentliche
Funktion "SetWindowSizeGrip" mit dem Fensterhandle und true oder false
aufrufen.

Beispiel:
+-------------------------------------------------------------------------
| uses
|   SizeGripHWND;
|
| ...
|
| begin
|   ...
|   hWndMain := CreateWindowEx(...);
|   SetWindowSizeGrip(hWndMain, true);
|   ...
| end;
+-------------------------------------------------------------------------

Diese Funktion zeichnet das Grip nicht selbst (so wie die anderen), sondern
benutzt einfach die API-Funktion "DrawFrameControl".
