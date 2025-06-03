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
      EolChar : AnsiString;
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
      procedure ErrorEvent(); overload;
      procedure ErrorEvent(message : string); overload;
      procedure RXEvent();
      procedure ReadRxData();
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

procedure TSerialThread.ErrorEvent(message : string);
begin
  OnError(message);
end;

procedure TSerialThread.RXEvent();
begin
  OnRX(_rxData);
end;

procedure TSerialThread.ReadRxData();
var
  rxLength : integer;
  temp : AnsiString;
  i : integer;
begin
  try
    if _serial.CanReadEx(1) then begin
      i := _serial.LastError;
      rxLength := _serial.WaitingDataEx();

      if rxLength <> 0 then begin
        SetLength(_rxData, rxLength);
        temp := _serial.RecvPacket(1);
        if (Length(temp) <> 0) then begin
          for i := 0 to rxLength - 1 do begin
            _rxData[i] := byte(temp.Chars[i]);
          end;
        end;

        Synchronize(@RXEvent);
        SetLength(_rxData, 0);
      end;
    end;
  except
    on e: Exception do begin
      ErrorEvent('ReadRxData Exception: ' + e.Message);
    end;
  end;
end;

procedure TSerialThread.SendTxData();
var
  i : integer;
  tx : AnsiString;
  txLength : integer;
begin
  EnterCriticalSection(_criticalSection);

  try
    try
      if _sendTx then begin
        txLength := Length(_txData);
        tx := '';

        for i := 0 to txLength - 1 do begin
          tx := tx + AnsiChar(_txData[i]);
        end;

        _serial.SendString(tx + _options.EolChar);

        _sendTx := false;
        SetLength(_txData, 0);
      end;
    except
      on e: Exception do begin
        ErrorEvent('SendTxData Exception: ' + e.Message);
      end;
    end;
  finally
    LeaveCriticalSection(_criticalSection);
  end;
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
begin
  _serial.Connect(_options.Port);
  _serial.Config(_options.Baud, 8, 'N', SB1, false, false);

  while(not Terminated) do begin
    if _serial.LastError <> 0 then begin
      Synchronize(@ErrorEvent);
    end;

    SendTxData();
    ReadRxData();
  end;
end;

{$ENDREGION}

end.

