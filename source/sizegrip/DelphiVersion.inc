{
  DelphiVersion.inc

  Delphi compiler version detection include file.
  Created with some inspiration from Jordan Russell's TB2Ver.inc.

  The main goal is to support future versions of Delphi / C++-Builder
  without modifications: if we used $IFDEF instead of $IFNDEF, we would
  have to change it each time.

  Version 1.0.10 - no online archive yet
  http://flocke.vssd.de/

  Copyright (C) 2006 Volker Siebert <flocke@vssd.de>
  All rights reserved.
}

{$IFNDEF VER80} // Delphi 1.0
  {$DEFINE DELPHI_2_UP}
  {$IFNDEF VER90} // Delphi 2.0
  {$IFNDEF VER93} // C++ Builder 1.0
    {$DEFINE DELPHI_3_UP}
    {$IFNDEF VER100} // Delphi 3.0
    {$IFNDEF VER110} // C++ Builder 3.0
      {$DEFINE DELPHI_4_UP}
      {$IFNDEF VER120} // Delphi 4.0
      {$IFNDEF VER125} // C++ Builder 4.0
        {$DEFINE DELPHI_5_UP}
        {$IFNDEF VER130} // Delphi 5.0 / C++ Builder 5.0
          {$DEFINE DELPHI_6_UP}
          {$IFNDEF VER140} // Delphi 6.0 / C++ Builder 6.0
            {$DEFINE DELPHI_7_UP}
            {$IFNDEF VER150} // Delphi 7.0
              {$DEFINE DELPHI_8_UP}
              {$IFNDEF VER160} // Delphi 8.0
                {$DEFINE DELPHI_9_UP}
                {$IFNDEF VER170} // Delphi 2005
                  {$DEFINE DELPHI_10_UP}
                  {$IFNDEF VER180} // Delphi 2006 / C++ Builder 2006
                    // Next compiler will be here!
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
      {$ENDIF}
    {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$DEFINE CPPBUILDER}
{$IFNDEF VER93} // C++ Builder 1.0
  {$IFNDEF VER110} // C++ Builder 3.0
    {$IFNDEF VER125} // C++ Builder 4.0
      {$IFNDEF BCB} // C++ Builder 5.0 or higher
        {$UNDEF CPPBUILDER}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
