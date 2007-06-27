AutoCompleteDemo.dpr
--------------------

- Needs Delphi 2 or higher

- Demonstrates the TSynAutoComplete class from the SynEditAutoComplete.pas file.
  This is not registered on the component palette, it is created at runtime.
  Later the various components in SynEditAutoComplete.pas and
  SynCompletionProposal.pas should be integrated.
- The advantages compared with the old auto completion class are:
    * Reads Delphi DCI style files.
    * Undo is performed in one step.
    * Recognizes partial shortcuts ("pro" instead of "proc" if there is no other
      completion that starts with "pro").
