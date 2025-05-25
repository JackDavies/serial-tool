unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Menus, synaser, FileUtil;

type

  { TMainForm }

  TMainForm = class(TForm)
    BottomPanel: TPanel;
    Button1: TButton;
    BaudRateCombo: TComboBox;
    PortsComboBox: TComboBox;
    ConnectButton: TButton;
    InputTypeComboBox1: TComboBox;
    OutputSettingsPanel: TPanel;
    SendButton: TButton;
    CenterPanel: TPanel;
    InputTypeComboBox: TComboBox;
    InputEdit: TEdit;
    OutputMemo: TMemo;
    RefreshPortsButton: TSpeedButton;
    TopPanel: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RefreshPortsButtonClick(Sender: TObject);
  private
    procedure PopulatePortsList();
    function GetPortNames() : TStringList;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

{$REGION 'Private Methods'}

{ Adapted from synaser.GetSerialPortNames: string; }
function TMainForm.GetPortNames() : TStringList;
const
  ATTR = {$IFDEF POSIX}$7FFFFFFF{$ELSE}$FFFFFFFF{$ENDIF};
var
  sr : TSearchRec;
  portName : string;
  names : TStringList;
begin
{$DEFINE IncttyACM}
{$DEFINE IncttyAM}
{$DEFINE IncttyS}
{$DEFINE IncttyUSB}

  names := TStringList.Create();

{$IFDEF IncttyACM}

  if FindFirst('/dev/ttyACM*', ATTR, sr) = 0 then begin
    repeat
      if (sr.Attr and ATTR) = Sr.Attr then begin
        portName := '/dev/' + sr.Name;
        names.Add(portName);
      end;
    until FindNext(sr) <> 0;
  end;
  FindClose(sr);

{$ENDIF}

{$IFDEF IncttyAM}

  if FindFirst('/dev/ttyAM*', ATTR, sr) = 0 then begin
    repeat
      if (sr.Attr and ATTR) = Sr.Attr then begin
        portName := '/dev/' + sr.Name;
        names.Add(portName);
      end;
    until FindNext(sr) <> 0;
  end;
  FindClose(sr);

{$ENDIF}

{$IFDEF IncttyS}

  if FindFirst('/dev/ttyS*', ATTR, sr) = 0 then begin
    repeat
      if (sr.Attr and ATTR) = Sr.Attr then begin
        portName := '/dev/' + sr.Name;
        names.Add(portName);
      end;
    until FindNext(sr) <> 0;
  end;
  FindClose(sr);

{$ENDIF}

{$IFDEF IncttyUSB}

  if FindFirst('/dev/ttyUSB*', ATTR, sr) = 0 then begin
    repeat
      if (sr.Attr and ATTR) = Sr.Attr then begin
        portName := '/dev/' + sr.Name;
        names.Add(portName);
      end;
    until FindNext(sr) <> 0;
  end;
  FindClose(sr);

{$ENDIF}

  Result := names;
end;

procedure TMainForm.PopulatePortsList();
var
  ttyFiles : TStringList;
  tty : string;
begin
  PortsComboBox.Clear();
  ttyFiles := GetPortNames();

  for tty in ttyFiles do begin
    PortsComboBox.Items.Add(tty);
  end;
end;

{$ENDREGION}


{$REGION 'Event Handlers'}

procedure TMainForm.FormShow(Sender: TObject);
begin
  PopulatePortsList();
end;

procedure TMainForm.RefreshPortsButtonClick(Sender: TObject);
begin
  PopulatePortsList();
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  serial : TBlockSerial;
  s : string;
begin
  serial := TBlockSerial.Create();

  serial.LinuxLock := false;

  serial.Connect('/dev/ttyACM0');

  serial.Config(9600, 8, 'N', SB1, false, false);

  //serial.SendString('x');

  if serial.CanRead(5000) then begin
    s := serial.Recvstring(5000);
  end;

  OutputMemo.Lines.Add(s);
end;

{$ENDREGION}

end.

