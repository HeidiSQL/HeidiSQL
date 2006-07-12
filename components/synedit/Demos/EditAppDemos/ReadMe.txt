EditAppDemos.bpg
----------------

- Needs Delphi 4 or higher.

- Shows how to use the SynEdit control to create editor applications in MDI, SDI
  or Workbook style.  It uses interfaces to hide implementation details; this
  proves extremely useful if the app has other types of subforms too that need
  to respond to the same menu/toolbar commands.  Each subform defines internally
  what commands it handles and how, the application main form need not to be
  changed when new types of subforms are added. 

