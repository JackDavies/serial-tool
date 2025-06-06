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
    EolPanel: TPanel;
    PortLabel: TLabel;
    BaudLabel: TLabel;
    EolLabel: TLabel;
    PortListPanel: TPanel;
    ConnectButton: TButton;
    OutoutFormatComboBox: TComboBox;
    OutputSettingsPanel: TPanel;
    PortsComboBox: TComboBox;
    EolCharComboBox: TComboBox;
    RefreshPortsButton: TSpeedButton;
    SendButton: TButton;
    CenterPanel: TPanel;
    InputFormatComboBox: TComboBox;
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
    function GetEolChar() : string;
    function GetPortNames() : TStringList;
    function FormatHex(txt : string) : string;
    function HexToData(txt : string) : TBytes;
    function FormatHexInput(txt : string) : string;
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
  EolPanel.Enabled := false;
  SendButton.Enabled := true;

  options := TSerialOptions.Create();
  options.Port:= PortsComboBox.Text;
  options.Baud:= StrToInt(BaudRateCombo.Text);
  options.EolChar := GetEolChar();

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
  EolPanel.Enabled := true;
  SendButton.Enabled := false;

  _serialThread.Disconnect();
  FreeAndNil(_serialThread);
end;

function TMainForm.FormatHex(txt : string) : string;
var
  b : byte;
  c : AnsiChar;
  i : integer;
begin
  Result := '';
  for i := 0 to Length(txt) do begin
    c := txt.Chars[i];
    Result := Result + ' ' + IntToHex(QWord(c), 2);
  end;
end;

function TMainForm.HexToData(txt : string) : TBytes;
var
  hexValues : TStringArray;
  i : integer;
  value : integer;
begin
  hexValues := txt.Split(' ');
  SetLength(Result, Length(hexValues));

  for i := 0 to Length(hexValues) - 1 do begin
    value := StrToInt('$' + hexValues[i]);
    if value > 255 then begin
      raise Exception.Create(hexValues[i] + ' is > 255');
    end;
    Result[i] := value;
  end;
end;


function TMainForm.FormatHexInput(txt : string) : string;
begin
  while (txt.Contains('  ')) do begin
    txt := txt.Replace('  ', ' ',[rfReplaceAll]);
  end;

  if txt.StartsWith(' ') then begin
    txt := txt.Remove(0, 1);
  end;

  if txt.EndsWith(' ') then begin
    txt := txt.Remove(Length(txt) - 1, 1);
  end;

  Result := txt;
end;

procedure TMainForm.Send();
var
  data : TBytes;
  txt : string;
  i : integer;
begin
  try
    txt := InputEdit.Text;
    SetLength(data, Length(txt));

    if InputFormatComboBox.Text = 'ASCII' then begin
      for i := 0 to Length(txt) - 1 do begin
        data[i] := Byte(txt.Chars[i]);
      end;
      OutputMemo.Append('TX: ' + txt);
    end
    else begin
      txt := FormatHexInput(txt);
      data := HexToData(txt);
      OutputMemo.Append('TX: [Hex] ' + txt);
    end;

    _serialThread.Send(data);
  except
    on e : Exception do begin
      OutputMemo.Append('Send Error: ' + e.Message);
    end;
  end;
  OutputMemo.VertScrollBar.Position := OutputMemo.VertScrollBar.Size;
end;

function TMainForm.GetEolChar() : string;
var
  selectedItem : string;
begin
  Result := '';

  selectedItem := EolCharComboBox.Text;

  case selectedItem of
    'None' : Result := '';
    'Null Char' : Result := #0;
    'Line Feed (LF)' : Result := #10;
    'Carriage return (CR)' : Result := #13;
    'CRLF' : Result := #13#10;
  end;
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

  if OutoutFormatComboBox.Text = 'Hex' then begin
    txt := '[Hex] ' +  FormatHex(txt);
  end;

  OutputMemo.Append('RX: ' + txt);
  OutputMemo.VertScrollBar.Position := OutputMemo.VertScrollBar.Size;
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
  if InputEdit.Text <> '' then begin
    Send();
  end;
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

