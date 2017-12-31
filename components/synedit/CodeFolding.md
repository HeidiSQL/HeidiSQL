***Code folding support*** 

**Introduction**

This pull request adds code folding support for SynEdit.  It blends well
with the Synedit highligting infrastructure and provides fast and
efficient code folding that can cope with files with tens of thousands
of lines without lags.

**Converting existing highlighters**

To support code folding a Highlighter must inherit from
TSynCustomCodeFoldingHighlighter and implement one abstact procedure
```
ScanForFoldRanges(FoldRanges: TSynFoldRanges;
LinesToScan: TStrings; FromLine: Integer; ToLine: Integer);
```
For each line, ScanForFoldRanges needs to call one of the following:
```
FoldRanges.StartFoldRange
FoldRanges.StopFoldRange
FoldRanges.NoFoldInfo
```
ScanForFoldRanges  is called after the standard highlighter scanning has
taken place so one can use the Range information stored inside
LinesToScan, which is a TSynEditStringList, to avoid duplicating effort.

Initally two hightlighters have been converted SynHighlighterJScript and
SynHighlighterPython, to serve as examples of adding code folding
support to brace-based and indentation-based languagges.

Alternatively, code folding support can be provided just by implementing
the SynEdit OnScanForFoldRangesEvent event.

**Demo of Coding Folding**

A Folding demo has been added that demonstrates the use of the JScript
and Python highlighters as well as the use of the
OnScanForFoldRangesEvent event to support code folding in C++ files.

**Synedit Commants and Shortcuts**

The following commands have been added:
ecFoldAll, ecUnfoldAll, ecFoldNearest, ecUnfoldNearest, ecFoldLevel1,
ecFoldLevel2, ecFoldLevel3,, ecUnfoldLevel1, ecUnfoldLevel2,
ecUnfoldLevel3, ecFoldRegions, ecUnfoldRegions.

The default customisable shortcuts are:
```
AddKey(ecFoldAll, VK_OEM_MINUS, [ssCtrl, ssShift]);   //- _
AddKey(ecUnfoldAll,  VK_OEM_PLUS, [ssCtrl, ssShift]); //= +
AddKey(ecFoldNearest, VK_OEM_2, [ssCtrl]);  // Divide //'/'
AddKey(ecUnfoldNearest, VK_OEM_2, [ssCtrl, ssShift]);
AddKey(ecFoldLevel1, ord('K'), [ssCtrl], Ord('1'), [ssCtrl]);
AddKey(ecFoldLevel2, ord('K'), [ssCtrl], Ord('2'), [ssCtrl]);
AddKey(ecFoldLevel3, ord('K'), [ssCtrl], Ord('3'), [ssCtrl]);
AddKey(ecUnfoldLevel1, ord('K'), [ssCtrl, ssShift], Ord('1'), [ssCtrl,
ssShift]);
AddKey(ecUnfoldLevel2, ord('K'), [ssCtrl, ssShift], Ord('2'), [ssCtrl,
ssShift]);
AddKey(ecUnfoldLevel3, ord('K'), [ssCtrl, ssShift], Ord('3'), [ssCtrl,
ssShift]);
```

**Limitations**

- Code folding can not be used simultaneously with Wordwrap.  Synedit
takes care of that.

- The code uses generic collections, so it cannot be used with Delphi
versions prior to Delphi 2009.

**Improvements**

Although the code folding infrastructure is fairly complete,
improvements can be made in providing the user with more painting
options (folding hints etc.)

**Technical details**

The main code folding structure is TSynFoldRanges in
SynEditCodefolding.pas.  It contains a public
`TList<TSynFoldRange>` (sorted by starting line numbers).  This list is
used by Synedit to paint the gutter and lines, fold and unfold ranges
etc. Internally, TSynFoldRange maintains a `TList<TLineFoldInfo>` that
is
modified during scanning.  The `TList<TSynFoldRange>` is reconstructed
from the
`TList<TLineFoldInfo>` only when it is necessary.
