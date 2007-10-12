ScanTokensDemo.dpr
------------------

- Needs Delphi 4 or higher, but it can be compiled under Delphi 2 if all the
  errors when opening the form are ignored.

- Demonstrates how to scan a source text for various tokens.  This is done in
  a background thread, to keep the UI as responsive as possible.  Changing the
  editor text triggers the thread to process the new text.  In an application
  the OnChange event should only disable and re-enable a timer, with the
  OnTimer event handler getting the background thread to start its work.

