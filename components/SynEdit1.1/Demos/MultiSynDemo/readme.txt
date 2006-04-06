MultiHighlight.dpr
------------------

- I used Delphi 5.  It can be compiled with Delphi 2, but there's no way to
  change the schemes there.

The purpose of this demo is to show how to implement the TSynMultiSyn control
allowing you to syntax highlight documents with many highlighters based on
schemes that you define.  I provide this for Web pages - HTML documents with
Cascading Style Sheets and JavaScript embedded in them.  Since not everyone is
familiar with these languages, I have provided two text files to load into the
Completion and AutoComplete proposals so that you may use the demo at runtime
and see it highlight.  I am also providing a complete HTML document based on
this readme file that you may load into the demo editor to see that highlighted.

- Leon Brown
email: LeonBrown77@hotmail.com

Steps involved
1) Create new project
2) Place SynEdit control on form
3) Add several higlighters to form
4) Add SynMultiSyn highlighter to form
5) OnFormCreate set SynMultiSyn.DefaultFilter to the main highlighters
6) Select SynMultiSyn so you may modify it in the Object Inspector
7) Set DefaultHighlighter to the main languages highlighter
8) Add Schemes
9) Each scheme needs its own StartExpr and EndExpr expressions and highlighter.
   So select one of the highlighters you put on the form in step 3.  Then supply
   the keywords/phrases that will begin and end use of this highlighter.

