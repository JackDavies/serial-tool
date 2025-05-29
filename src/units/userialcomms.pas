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
  TOnErrorEvent = procedure (ErrorMessage : string) of object;

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
      _rxData : TBytes;
      _criticalSection : TRTLCriticalSection;
      _sendTx : boolean;
      _txData : TBytes;
    private
      procedure ErrorEvent();
      procedure RXEvent();
      procedure SendTxData();
    public
      OnRX : TOnRXEvent;
      OnError : TOnErrorEvent;
    public
      procedure Connect();
      procedure Disconnect();
      procedure Execute(); override;
      procedure Send(data : TBytes);
      constructor Create(options : TSerialOptions); overload;
      destructor Destroy(); override;
  end;

implementation

{$REGION 'Constructors / destructors'}

constructor TSerialThread.Create(options : TSerialOptions);
begin
  inherited Create(true, DefaultStackSize);
  _sendTx := false;
  _options := options;

  _serial := TBlockSerial.Create();
  _serial.LinuxLock := false;

  InitCriticalSection(_criticalSection);
end;

destructor TSerialThread.Destroy();
begin
  DoneCriticalSection(_criticalSection);
  FreeAndNil(_serial);
  FreeAndNil(_options);
  inherited Destroy;
end;

{$ENDREGION}

{$REGION 'Private Methods'}

procedure TSerialThread.Connect();
begin
  Self.Start;
end;

procedure TSerialThread.Disconnect();
begin
  _serial.CloseSocket;
  Terminate;
  Self.WaitFor;
end;

procedure TSerialThread.ErrorEvent();
begin
  OnError(_serial.GetErrorDesc(_serial.LastError));
end;

procedure TSerialThread.RXEvent();
begin
  OnRX(_rxData);
end;

procedure TSerialThread.SendTxData();
var
  stream : TMemoryStream;
begin
  EnterCriticalSection(_criticalSection);

  if _sendTx then begin
    stream := TMemoryStream.Create;

    stream.Write(_txData, Length(_txData));
    stream.Position := 0;

    _serial.SendStream(stream);

    FreeAndNil(stream);

    _sendTx := false;
    SetLength(_txData, 0);
  end;


  LeaveCriticalSection(_criticalSection);
end;

{$ENDREGION}

{$REGION 'Public Methods'}

procedure TSerialThread.Send(data : TBytes);
var
  i : integer;
begin
  EnterCriticalSection(_criticalSection);

  SetLength(_txData, Length(data));

  for i := 0 to Length(data) - 1 do begin
    _txData[i] := data[i];
  end;

  _sendTx := true;

  LeaveCriticalSection(_criticalSection);
end;

procedure TSerialThread.Execute();
var
  rxLength : integer;
  temp : AnsiString;
  i : integer;
begin
  _serial.Connect(_options.Port);
  _serial.Config(_options.Baud, 8, 'N', SB1, false, false);

  while(not Terminated) do begin
    if _serial.LastError <> 0 then begin
      Synchronize(@ErrorEvent);
    end;

    SendTxData();

    if _serial.CanReadEx(1) then begin
      i := _serial.LastError;
      rxLength := _serial.WaitingDataEx();

      if rxLength <> 0 then begin
        SetLength(_rxData, rxLength);
        temp := _serial.Recvstring(1);
        for i := 0 to rxLength - 1 do begin
          _rxData[i] := byte(temp.Chars[i]);
        end;

        Synchronize(@RXEvent);
        SetLength(_rxData, 0);
      end;
    end;
    Sleep(50);
  end;
end;

{$ENDREGION}

end.

