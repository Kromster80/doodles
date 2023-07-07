// WDC/FPC defines are used because Delphi/Lazarus have some RTL differences (mostly in uses sections)
//----------------------------------------------------------------------------------------------------
// If you use any Delphi compiler that is not listed here - you need to add it
{$IFDEF VER220} {$DEFINE WDC} {$ENDIF} // Delphi XE
{$IFDEF VER230} {$DEFINE WDC} {$ENDIF} // Delphi XE2
{$IFDEF VER240} {$DEFINE WDC} {$ENDIF} // Delphi XE3
{$IFDEF VER250} {$DEFINE WDC} {$ENDIF} // Delphi XE4
{$IFDEF VER260} {$DEFINE WDC} {$ENDIF} // Delphi XE5
{$IFDEF VER270} {$DEFINE WDC} {$ENDIF} // Delphi XE6
{$IFDEF VER280} {$DEFINE WDC} {$ENDIF} // Delphi XE7
{$IFDEF VER290} {$DEFINE WDC} {$ENDIF} // Delphi XE8
{$IFDEF VER300} {$DEFINE WDC} {$ENDIF} // Delphi 10 Seattle - untested
{$IFDEF VER310} {$DEFINE WDC} {$ENDIF} // Delphi 10.1 Berlin - untested
{$IFDEF VER320} {$DEFINE WDC} {$ENDIF} // Delphi 10.2 Tokyo - untested
{$IFDEF VER340} {$DEFINE WDC} {$ENDIF} // Delphi 10.4 Sydney - untested
{$IFDEF VER350} {$DEFINE WDC} {$ENDIF} // Delphi 11
{.$DEFINE FPC} // Define Free Pascal compiler (defined automatically by Lazarus)


// Lazarus compiler specific settings
//----------------------------------------------------------------------------------------------------
{$IFDEF FPC}
  // Set FPC to work in Delphi mode
  {$MODE Delphi}

  // Currently FPC does not define UNICODE define because in Delphi it means more than just a
  // default string type but a full RTL implementation : using Windows 'W' functions instead of 'A',
  // another class declarations(TStringStream for example).
  {$IFDEF FPC_UNICODESTRINGS}
    {$DEFINE UNICODE}
  {$ENDIF}
{$ENDIF}


// Aggregate defines for platforms
//----------------------------------------------------------------------------------------------------
{$IFDEF MSWINDOWS} {$DEFINE DESKTOP} {$ENDIF}
{$IFDEF UNIX}      {$DEFINE DESKTOP} {$ENDIF}
{$IFDEF MACOS}{$IFNDEF IOS}{$DEFINE DESKTOP}{$ENDIF}{$ENDIF} // Note that MACOS also indicates iOS
{$IFDEF ANDROID}   {$DEFINE TABLET} {$ENDIF}
{$IFDEF IOS}       {$DEFINE TABLET} {$ENDIF}


// Temp workaround for mission scripting support on tablets
//----------------------------------------------------------------------------------------------------
{$IFDEF DESKTOP} {$DEFINE SCRIPTING_DESKTOP} {$ENDIF}
{$IFDEF TABLET} {$DEFINE SCRIPTING_TABLET} {$ENDIF}

// Features
//----------------------------------------------------------------------------------------------------
{.$DEFINE USE_RECAST}
{.$DEFINE RNG_SPY}
{.$DEFINE POINTER_SPY}
{.$DEFINE EXTAI_API_DLL}
{$DEFINE EXTAI_API_EXE}

