{$DEFINE UseInline}
{$DEFINE BuildThreadSafe}
{$DEFINE UseGenerics}
{$DEFINE UseMultiBytesNop}
//Define HookInternalFuncs if you want to hook internal functions used by DDL core!
{.$DEFINE HookInternalFuncs}
//----------------------------------------------
{$IFDEF FPC}
  {$IFDEF CPU64}
    {$IFNDEF CPUX64}
      {$DEFINE CPUX64}
    {$ENDIF !CPUX64}
  {$ENDIF CPU64}
  {$ASMMODE INTEL}
{$ENDIF FPC}

{$IFNDEF CPUX64}
  {$IFNDEF CPUX86}
    {$DEFINE CPUX86}
  {$ENDIF !CPUX86}
{$ENDIF !CPUX64}

{$IFDEF DEBUG}
{$R+} // Range check On
{$ENDIF}

{$IFNDEF FPC}
  {$IF CompilerVersion >17}
    {$DEFINE CanInline}
  {$IFEND}
  {$IF CompilerVersion >=21}
    {$DEFINE GenericsExist }
  {$IFEND}
  {$IF CompilerVersion >=23}
    {$DEFINE DXE2UP }
  {$IFEND}  
  {$IF CompilerVersion >=24}
    {$DEFINE DXE3UP }
  {$IFEND}    
{$ENDIF !FPC}

{$IF DEFINED(UseInline) and DEFINED(CanInline)}
  {$DEFINE MustInline}
{$IFEND}

{$IF DEFINED(GenericsExist) and DEFINED(UseGenerics)}
  {$DEFINE MustUseGenerics}
{$IFEND}
