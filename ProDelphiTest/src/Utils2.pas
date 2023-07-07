unit Utils2;
{$I CompilerDirectives.inc}
interface

uses
  SysUtils;

type
  TTestType2 = (ttOne, ttTwo);

const
  REV1 = {$I Rev.inc}; // 'r11179'
  REV2 = {$I ..\Rev.inc}; // 'r11179'


implementation

end.
