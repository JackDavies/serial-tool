unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Menus, synaser, FileUtil, uSerialComms;

type

  { TMainForm }

  TMainForm = class(TForm)
    BaudRateCombo: TComboBox;
    BottomPanel: TPanel;
    BaudRatePanel: TPanel;
    PortLabel: TLabel;
    BaudLabel: TLabel;
    PortListPanel: TPanel;
    ConnectButton: TButton;
    InputTypeComboBox1: TComboBox;
    OutputSettingsPanel: TPanel;
    PortsComboBox: TComboBox;
    RefreshPortsButton: TSpeedButton;
    SendButton: TButton;
    CenterPanel: TPanel;
    InputTypeComboBox: TComboBox;
    InputEdit: TEdit;
    OutputMemo: TMemo;
    TopPanel: TPanel;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RefreshPortsButtonClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
  private
    _serialThread : TSerialThread;
  private
    procedure PopulatePortsList();
    procedure Connect();
    procedure Disconnect();
    procedure OnRX(RXData : TBytes);
    procedure OnError(ErrorMessage : string);
    procedure Send();
    function GetPortNames() : TStringList;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

{$REGION 'Private Methods'}

procedure TMainForm.Connect();
var
  options : TSerialOptions;
begin
  ConnectButton.Caption:= 'Disconnect';
  PortListPanel.Enabled := false;
  BaudRatePanel.Enabled := false;

  options := TSerialOptions.Create();
  options.Port:= PortsComboBox.Text;
  options.Baud:= StrToInt(BaudRateCombo.Text);
  options.EolChar := #10;

  _serialThread := TSerialThread.Create(options);
  _serialThread.OnRX := @OnRX;
  _serialThread.OnError := @OnError;
  _serialThread.Connect();
end;

procedure TMainForm.Disconnect();
begin
  ConnectButton.Caption:= 'Connect';
  PortListPanel.Enabled := true;
  BaudRatePanel.Enabled := true;

  _serialThread.Disconnect();
  FreeAndNil(_serialThread);
end;

procedure TMainForm.Send();
var
  data : TBytes;
  txt : string;
  i : integer;
begin
  txt := InputEdit.Text;
  SetLength(data, Length(txt));

  for i := 0 to Length(txt) - 1 do begin
    data[i] := Byte(txt.Chars[i]);
  end;

  _serialThread.Send(data);
end;

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

procedure TMainForm.OnError(ErrorMessage : string);
begin
  OutputMemo.Append('Error: ' + ErrorMessage);
end;

procedure TMainForm.OnRX(RXData : TBytes);
var
  c : AnsiChar;
  txt : AnsiString;
  i : integer;
  rxLength : integer;
begin
  rxLength := Length(RXData);
  txt := '';

  for i := 0 to rxLength - 1 do begin
    c := Char(RXData[i]);
    txt := txt + c;
  end;
  OutputMemo.Append(txt);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  PopulatePortsList();
end;

procedure TMainForm.RefreshPortsButtonClick(Sender: TObject);
begin
  PopulatePortsList();
end;

procedure TMainForm.SendButtonClick(Sender: TObject);
begin
  Send();
end;

procedure TMainForm.ConnectButtonClick(Sender: TObject);
begin
  if Assigned(_serialThread) then begin
    Disconnect();
  end
  else begin
    Connect();
  end;
end;

{$ENDREGION}

end.

