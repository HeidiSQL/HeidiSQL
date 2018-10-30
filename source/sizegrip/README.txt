SizeGrip.pas, SizeGripThemed.pas

  Delphi components to add a size grip (like if you use a status bar) to the
  lower right corner of any TWinControl (like TForm). "SizeGripThemed.pas"
  is the themed version using the currently selected visual style. See the
  included README.txt for more information and how to use it.

SizeGripHWND.pas

  Delphi unit for nonVCL application to get a similar effect using only API
  functions.

Version 1.2a - Always find the most current version at
http://flocke.vssd.de/prog/code/pascal/sizegrip/

Copyright (C) 2005, 2006 Volker Siebert <flocke@vssd.de>
All rights reserved.

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

Delphi versions: 5, 6, 7, 2005, and 2006.

SizeGripThemed requires UxTheme.pas - if your Delphi doesn't have one
installed, use the one from Mike Lischke's homepage:
http://www.lischke-online.de/ThemeManager.php.

USAGE:

Just create a component of type TSizeGrip or TSizeGripThemed in your Form's
"FormCreate" event and assign the "TargetControl" property.

Example:
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

After creation, the component is owned by the form and automatically freed
when the form is destroyed.

- TSizeGrip draws a size grip like the standard (un-themed) statusbar does
  using diagonal lines or 6 holes.
- TSizeGripThemed draws the size grip using the currently selected
  visual style (if any) or like TSizeGrip (otherwise).

INSTALLATION:

You can also install the components into your tool palette - just add the
units to your custom control package - they will appear under "System".

NON-VCL USAGE:

Just use the unit SizeGripHWND.pas and call the only public function
"SetWindowSizeGrip" with you window handle and true or false.

Example:
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

This function does not draw the grip itself (like the other two modules)
but simply uses the "DrawFrameControl" API function to do that.
