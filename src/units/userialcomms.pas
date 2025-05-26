unit uSerialComms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synaser,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads;
  {$ENDIF}{$ENDIF}
type

  TOnRXEvent = procedure(RXData : TBytes) of object;

  TSerialOptions = class
    public
      Baud : integer;
      Port : string;
      EolChar : char;
  end;

  TSerialThread = class (TThread)
    private
      _serial : TBlockSerial;
      _options : TSerialOptions;
    private
      procedure RXEvent();
    public
      OnRX : TOnRXEvent;
    public
      procedure Execute(); override;
      constructor Create(options : TSerialOptions); overload;

  end;

implementation

{$REGION 'Constructors / destructors'}

constructor TSerialThread.Create(options : TSerialOptions);
begin
  _options := options;

  _serial := TBlockSerial.Create();

  _serial.Config(_options.Baud, 8, 'N', SB1, false, false);
end;

{$ENDREGION}

{$REGION 'Private Methods'}

procedure TSerialThread.RXEvent();
begin

end;

{$ENDREGION}

{$REGION 'Public Methods'}

procedure TSerialThread.Execute();
begin

end;

{$ENDREGION}

end.

