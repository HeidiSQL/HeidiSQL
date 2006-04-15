{********************************************************}
{                                                        }
{                Zeos Database Controls                  }
{                Global definition file                  }
{                                                        }
{       Copyright (c) 1999-2000 Sergey Seroukhov         }
{                                                        }
{********************************************************}

{ Supported language. Now available languages: }
{ ENGLISH, RUSSIAN, GERMAN, PORTUGUESE, FRENCH, }
{ POLISH, CZECH, ITALIAN, SPANISH, HUNGARY }
{$DEFINE ENGLISH}

{ Turn on/off property editors }
{$DEFINE WITH_PROPEDIT}

{ Turn on/off GIFImage registration }
{//$DEFINE GIF_REGISTER}

{ Compiler options }
{$B-} {- Complete Boolean Evaluation }
{$R-} {- Range-Checking }
{$V-} {- Var-String Checking }
{$T-} {- Typed @ operator }
{$X+} {- Extended syntax }
{$P+} {- Open string params }
{$J+} {- Writeable structured consts }
{$H+} {- Use long strings by default }
{//$D-} {- Include debug information }
{//$L-} {- Include local symbols information }

{ Zeos palette names }
const
  ZEOS_PALETTE = 'Zeos Controls';