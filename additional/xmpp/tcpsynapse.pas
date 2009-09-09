//
// simple client tcp socket
//
// credits: synapse
//
// contact: devi[dot]mandiri[at]gmail[dot]com
//

unit tcpsynapse;

{$IFDEF FPC}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, blcksock, ssl_openssl, SyncObjs;

type
  TTCPClient=class;

  THostInfo = record
    Host: String;
    Port: String;
    // TODO: Proxy
  end;

  TTCPStatus = (tsConnect, tsDisconnect, tsSSLConnect,{tsSSLDisconnect,}tsData);
  TTCPCommand = class
    Status:TTCPStatus;
    Info:THostInfo;
    Data:string;
  end;

  TTCPThread=class(TThread)
  private
    sock:TTCPBlockSocket;
    FOwner:TTCPClient;
    FData:string;
    FErrMsg:string;
  protected
    procedure Execute;override;
    procedure DoAfterConnect(Sender:TObject);
    procedure SyncOnConnect;
    procedure SyncOnDisconnect;
    procedure SyncOnData;
    procedure SyncOnError;
    procedure SyncAfterUpgradedToSSL;
    procedure SyncOnSSLFailed;
    procedure SockCallback(Sender: TObject; Reason: THookSocketReason;
        const Value: string);
  public
    constructor Create(AOwner:TTCPClient);
    destructor Destroy;override;
  end;

  TTCPEvent  = procedure(Sender:TObject;Value:string) of object;

  TTCPClient=class
  private
    FTCPHandle,
    FCommands:TList;
    FOnConnected,
    FOnDisconnected,
    FOnAfterSSL:TNotifyEvent;
    FOnSSLFailed:TTCPEvent;
    FOnData,
    FOnError:TTCPEvent;
    FCS:TCriticalSection;
    FHost,FPort:string;
    FConnected:Boolean;
  protected
    procedure CreateTCPThread;
    procedure FreeTCPThread;
    procedure PushCommand(Value:TTCPCommand);
    function  PopCommand:TTCPCommand;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Connect;
    procedure Disconnect;
    function  IsConnected:Boolean;

    procedure UpgradeConnectionWithOpenSSL;
//    procedure DowngradeSSLConnection;

    procedure SendData(Data:string);

    property Host:string read FHost write FHost;
    property Port:string read FPort write FPort;

    property OnConnected:TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected:TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnData:TTCPEvent read FOnData write FOnData;
    property OnError:TTCPEvent read FOnError write FOnError;
    property OnAfterUpgradedToSSL:TNotifyEvent read FOnAfterSSL write FOnAfterSSL;
    property OnSSLFailed:TTCPEvent read FOnSSLFailed write FOnSSLFailed;
  end;

implementation

{ TTCPThread }

constructor TTCPThread.Create(AOwner:TTCPClient);
begin
  inherited Create(True);
  FOwner := AOwner;
  sock := TTCPBlockSocket.CreateWithSSL(TSSLOpenSSL);
  sock.OnAfterConnect := DoAfterConnect;
  sock.OnStatus := SockCallback;
end;

destructor TTCPThread.Destroy;
begin
  sock.Free;
  inherited;
end;

procedure TTCPThread.SockCallback(Sender: TObject;
  Reason: THookSocketReason; const Value: string);
begin
  case Reason of
  HR_Error,HR_SocketClose:
    begin
      if FOwner.FConnected then
        Synchronize(SyncOnDisconnect)
      else begin
        FErrMsg := Value;
        if Length(FErrMsg)>0 then
          Synchronize(SyncOnError);
      end;
    end;
  end;
end;

procedure TTCPThread.DoAfterConnect(Sender: TObject);
begin
  Synchronize(SyncOnConnect);
end;

procedure TTCPThread.SyncOnConnect;
begin
  FOwner.FConnected := True;
  if Assigned(FOwner.OnConnected) then
    FOwner.FOnConnected(FOwner);
end;

procedure TTCPThread.SyncOnData;
begin
  if Assigned(FOwner.OnData) then
    FOwner.FOnData(FOwner,FData);
end;

procedure TTCPThread.SyncOnDisconnect;
begin
  FOwner.FConnected := False;
  if Assigned(FOwner.OnDisconnected) then
    FOwner.FOnDisconnected(FOwner);
end;

procedure TTCPThread.SyncOnError;
begin
  if Assigned(FOwner.OnError) then
    FOwner.FOnError(FOwner,FErrMsg);
end;

procedure TTCPThread.SyncAfterUpgradedToSSL;
begin
  if Assigned(FOwner.OnAfterUpgradedToSSL) then
    FOwner.FOnAfterSSL(FOwner);
end;

procedure TTCPThread.SyncOnSSLFailed;
begin
  if Assigned(FOwner.OnSSLFailed) then
    FOwner.FOnSSLFailed(FOwner,FErrMsg);
end;

procedure TTCPThread.Execute;
const
  YAHOO_DATA_MAX = 65535 +20;
var
  J,C:TTCPCommand;
begin
  while not Terminated do begin
    J := FOwner.PopCommand;
    if Assigned(J) then
    begin
      if (TTCPCommand(J).Status=tsConnect) then
      begin
        sock.Connect(J.Info.Host,J.Info.Port);
        while (sock.LastError=0) and (not Terminated) do
        begin
          C := FOwner.PopCommand;
          if Assigned(C) then begin
            case TTCPCommand(C).Status of
              tsDisconnect:
                begin
                  sock.CloseSocket;
                  Break;
                end;
              tsSSLConnect:
                begin
                  sock.SSLDoConnect;
                  if sock.SSL.LastError=0 then
                    Synchronize(SyncAfterUpgradedToSSL)
                  else begin
                    FErrMsg := sock.SSL.LastErrorDesc;
                    if Length(FErrMsg)>0 then
                      Synchronize(SyncOnSSLFailed);
                  end;
                end;
              tsData:
                sock.SendString(TTCPCommand(C).Data);
            end;
            C.Free;
          end;
          if sock.CanRead(20) then
          begin
            FData:= sock.RecvPacket(0);
            if (sock.LastError=0) and (Length(FData)>0) then
              Synchronize(SyncOnData);
          end;
        end;
        sock.CloseSocket;
      end;
      J.Free;
    end else
    Sleep(200);
  end;
end;

{ TTCPClient }

constructor TTCPClient.Create;
begin
  inherited;
  FCS := TCriticalSection.Create;
  FTCPHandle := TList.Create;
  FCommands := TList.Create;
end;

destructor TTCPClient.Destroy;
var i:integer;
begin
  FreeTCPThread;
  FTCPHandle.Free;
  for i:=0 to FCommands.Count-1 do
    TTCPCommand(FCommands[i]).Free;
  FCommands.Free;
  FCS.Free;
  inherited;
end;

procedure TTCPClient.CreateTCPThread;
var FThread:TTCPThread;
begin
  FThread := TTCPThread.Create(Self);
  FThread.Resume;
  FTCPHandle.Add(FThread);
end;

procedure TTCPClient.FreeTCPThread;
var i:integer;
begin
  for i:=0 to FTCPHandle.Count-1 do
  begin
    TTCPThread(FTCPHandle[i]).Terminate;
    // TODO
    {$IFDEF WIN32}
    TTCPThread(FTCPHandle[i]).WaitFor;
    {$ENDIF}
    TTCPThread(FTCPHandle[i]).Free;
  end;
end;

procedure TTCPClient.Connect;
var C:TTCPCommand;
begin
  if FConnected then
    Exit; 
  C := TTCPCommand.Create;
  C.Info.Host := FHost;
  C.Info.Port := FPort;
  C.Status := tsConnect;
  PushCommand(C);
end;

procedure TTCPClient.Disconnect;
var C:TTCPCommand;
begin
  if not FConnected then
    Exit;
  C := TTCPCommand.Create;
  C.Status := tsDisconnect;
  PushCommand(C);
end;

procedure TTCPClient.PushCommand(Value:TTCPCommand);
begin
  if FTCPHandle.Count < 1 then
    CreateTCPThread;
  FCS.Enter;
  FCommands.Add(Value);
  FCS.Leave;
end;

function TTCPClient.PopCommand: TTCPCommand;
begin
  FCS.Enter;
  if FCommands.Count>0 then
  begin
    Result := TTCPCommand(FCommands[0]);
    FCommands.Delete(0);
  end else
  Result := nil;
  FCS.Leave;
end;

function TTCPClient.IsConnected: Boolean;
begin
  Result := FConnected;
end;

procedure TTCPClient.SendData(Data: string);
var C:TTCPCommand;
begin
  if not FConnected then
    Exit;
  C := TTCPCommand.Create;
  C.Status := tsData;
  C.Data := Data;
  PushCommand(C);
end;

procedure TTCPClient.UpgradeConnectionWithOpenSSL;
var C:TTCPCommand;
begin
  if not FConnected then
    Exit;
  C := TTCPCommand.Create;
  C.Status := tsSSLConnect;
  PushCommand(C);
end;

// TODO
{
procedure TTCPClient.DowngradeSSLConnection;
var C:TTCPCommand;
begin
  if not FConnected then
    Exit;
  C := TTCPCommand.Create;
  C.Status := tsSSLDisconnect;
  PushCommand(C);
end;
}

end.
