unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, synaser;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    OutputMemo: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

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

end.

