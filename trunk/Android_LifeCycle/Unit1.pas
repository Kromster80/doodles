unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Platform,
  FMX.Memo, FMX.StdCtrls, System.IOUtils;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    function HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
    procedure Log(s: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
var
  aFMXApplicationEventService: IFMXApplicationEventService;
  M: TMemoryStream;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(aFMXApplicationEventService)) then
    aFMXApplicationEventService.SetApplicationEventHandler(HandleAppEvent)
  else
    Log('Application Event Service is not supported.');

  Memo1.Lines.Append('FormCreate');
  Memo1.Lines.Append(TPath.GetTempPath);
  Memo1.Lines.Append(TPath.GetHomePath);
  Memo1.Lines.Append(TPath.GetDocumentsPath);
  Memo1.Lines.Append(TPath.GetSharedDocumentsPath);
  Memo1.Lines.Append(TPath.GetLibraryPath);
  Memo1.Lines.Append(TPath.GetPublicPath);

{  ForceDirectories(TPath.GetHomePath);
  ForceDirectories(TPath.GetDocumentsPath);
  ForceDirectories(TPath.GetSharedDocumentsPath);
  ForceDirectories(TPath.GetLibraryPath);
  ForceDirectories(TPath.GetPublicPath);

  M := TMemoryStream.Create;
  M.SaveToFile(TPath.GetTempPath + 'GetTempPath.txt');
  M.SaveToFile(TPath.GetHomePath + 'GetHomePath.txt');
  M.SaveToFile(TPath.GetDocumentsPath + 'GetDocumentsPath.txt');
  M.SaveToFile(TPath.GetSharedDocumentsPath + 'GetSharedDocumentsPath.txt');
//  M.SaveToFile(TPath.GetLibraryPath + 'GetLibraryPath.txt');
  M.SaveToFile(TPath.GetPublicPath + 'GetPublicPath.txt');
  M.Free;}
end;


function TForm1.HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  case AAppEvent of
    aeFinishedLaunching: Log('Finished Launching');
    aeBecameActive: Log('Became Active');
    aeWillBecomeInactive: Log('Will Become Inactive');
    aeEnteredBackground: Log('Entered Background');
    aeWillBecomeForeground: Log('Will Become Foreground');
    aeWillTerminate: Log('Will Terminate');
    aeLowMemory: Log('Low Memory');
    aeTimeChange: Log('Time Change');
    aeOpenURL: Log('Open URL');
  end;
  Result := True;
end;

procedure TForm1.Log(s: string);
begin
  Memo1.Lines.Add(TimeToStr(Now) + ': ' + s);
end;


end.
