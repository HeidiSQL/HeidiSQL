{$ifndef FPC}
  {$ifdef ENHANCEDRTL}
    FastMM4, // FastMM4 is not integrated by default in our Enhanced Run Time Library
  {$else}
    {$IFDEF CONDITIONALEXPRESSIONS}
      {$if CompilerVersion <= 17} // Delphi 2006 (and up) have FastMM4 integrated
        FastMM4,
      {$ifend}
    {$ELSE}
      FastMM4, // no conditional expressions -> versions older than Delphi 6
    {$ENDIF}
  {$endif}
{$endif}
