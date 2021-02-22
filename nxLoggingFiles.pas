{ **************************************************************************** }
{ ***** Dateiname               : nxLoggingFiles.pas ************************* }
{ ***** Autor                   : Navimatix GmbH, Matthias Heunecke ********** }
{ ***** Erstellt am             : 10.08.2012 ********************************* }
{ ***** Letzte Änderung         : 17.08.2012 ********************************* }
{ ***** Beschreibung            : Enthält Klassen, Komponenten, Funktionen *** }
{ ******************************* für das Logging von Hinweisen, Fehlern,...** }
{ **************************************************************************** }
{ ***** Gruppe                  : Freeware *********************************** }
{ **************************************************************************** }
{ ***** Status                  : aktiv ************************************** }
{ **************************************************************************** }
unit nxLoggingFiles;

interface

uses System.Classes, System.Types, System.SyncObjs, System.SysUtils,
     nxLogging, IdTCPClient, IdThreadSafe;

type

  TNxLogFileThreadUpdateEvent = procedure(Sender : TObject; aStrings : TStrings; aMachineIdent : String) of object;
  TNxLogFileThreadFilterEvent = procedure(Sender : TObject; aLine : String; aMessage : TNxLoggerMessage; var doAddLine : Boolean) of object;
  TNxLogTCPThreadFilterEvent = procedure(Sender : TObject; aLine : String; aMessage : TNxLoggerMessage; aMachineIdent : String; var doAddLine : Boolean) of object;

  TNxLogFile = class;

  TNxLogFileThread = class(TThread)
  private
    fParent         : TNxLogFile;
    fOnUpdate       : TNxLogFileThreadUpdateEvent;
    fOnUpdateAsync  : TNxLogFileThreadUpdateEvent;
    fOnFilter       : TNxLogFileThreadFilterEvent;
    fLogFormater    : TNxLogFormater;
    fWatchStream    : TStream;
    fCurrentLine    : String;
    fCurrentLines   : TStrings;
    fEncoding       : TEncoding;
    fPreTailSize    : Integer;
    fMaxLineLength  : Integer;

    procedure clearStrings(aStrings : TStrings);
  protected
    procedure informReadError(aErrorText : String); virtual;
    procedure informCheckError(aErrorText : String); virtual;
    function  extractLines(aSource : String; aDestination : TStrings) : String; virtual;
    procedure update(aCurrentSize, aStreamSize : Int64); virtual;
    function  checkStreamValid : Boolean; virtual;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;

    property Parent         : TNxLogFile read fParent write fParent;

    property OnUpdate       : TNxLogFileThreadUpdateEvent read fOnUpdate write fOnUpdate;
    property OnUpdateAsync  : TNxLogFileThreadUpdateEvent read fOnUpdateAsync write fOnUpdateAsync;
    property OnFilter       : TNxLogFileThreadFilterEvent read fOnFilter write fOnFilter;
    property WatchStream    : TStream read fWatchStream write fWatchStream;
    property Encoding       : TEncoding read fEncoding write fEncoding;
    property PreTailSize    : Integer read fPreTailSize write fPreTailSize;
    property MaxLineLength  : Integer read fMaxLineLength write fMaxLineLength;
    property LogFormater    : TNxLogFormater read fLogFormater write fLogFormater;
  end;

  TNxLogFolder  = class;
  TNxLogFolderStruct = class;

  TNxLogFolderThread = class(TThread)
  private
    fParent         : TNxLogFolder;
    fReadonly       : Boolean;
    fOnUpdate       : TNxLogFileThreadUpdateEvent;
    fOnUpdateAsync  : TNxLogFileThreadUpdateEvent;
    fOnFilter       : TNxLogFileThreadFilterEvent;
    fLoggerFileFormatSettings : TFormatSettings;
    fEncoding       : TEncoding;
    fPreTailSize    : Integer;
    fMaxLineLength  : Integer;

    procedure initWatchfiles;
    procedure doneWatchfiles;
    procedure clearStrings(aStrings : TStrings);
  protected
    procedure informReadError(aErrorText : String); virtual;
    procedure informCheckError(aErrorText : String); virtual;
    procedure informFilesStart(aCount : Integer); virtual;
    procedure informFilesDone; virtual;
    procedure informNewFile(aNewFilename : String); virtual;
    function  extractLines(aLogFormater : TNxLogFormater; aSource : String; aDestination : TStrings) : String; virtual;
    procedure update(aStruct : TNxLogFolderStruct); virtual;
    function  checkStreamValid : Boolean; virtual;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;

    property Parent         : TNxLogFolder read fParent write fParent;
    property Readonly       : Boolean read fReadonly write fReadonly;

    property OnUpdate       : TNxLogFileThreadUpdateEvent read fOnUpdate write fOnUpdate;
    property OnUpdateAsync  : TNxLogFileThreadUpdateEvent read fOnUpdateAsync write fOnUpdateAsync;
    property OnFilter       : TNxLogFileThreadFilterEvent read fOnFilter write fOnFilter;
    property Encoding       : TEncoding read fEncoding write fEncoding;
    property PreTailSize    : Integer read fPreTailSize write fPreTailSize;
    property MaxLineLength  : Integer read fMaxLineLength write fMaxLineLength;
  end;

  TNxLogTCP = class;


  TNxLogTCPThread = class(TThread)
  private
    fParent             : TNxLogTCP;
    fOnUpdate           : TNxLogFileThreadUpdateEvent;
    fOnUpdateAsync      : TNxLogFileThreadUpdateEvent;
    fOnFilter           : TNxLogTCPThreadFilterEvent;
    fLogFormater        : TNxLogFormater;
    fTCPClient          : TIdTCPClient;
    fEncoding           : TEncoding;
    fMaxLineLength      : Integer;
    fCurrentLines       : TStrings;

    procedure clearStrings(aStrings : TStrings);
  protected
    procedure informReadError(aErrorText : String); virtual;
    procedure informConnectError(aErrorText : String); virtual;
    procedure informTailActiveError(aErrorText : String); virtual;
    procedure update(aMessage : String); virtual;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;

    property Parent             : TNxLogTCP read fParent write fParent;
    property OnUpdate           : TNxLogFileThreadUpdateEvent read fOnUpdate write fOnUpdate;
    property OnUpdateAsync      : TNxLogFileThreadUpdateEvent read fOnUpdateAsync write fOnUpdateAsync;
    property OnFilter           : TNxLogTCPThreadFilterEvent read fOnFilter write fOnFilter;
    property Encoding           : TEncoding read fEncoding write fEncoding;
    property TCPClient          : TIdTCPClient read fTCPClient write fTCPClient;
    property MaxLineLength      : Integer read fMaxLineLength write fMaxLineLength;
    property LogFormater        : TNxLogFormater read fLogFormater write fLogFormater;
  end;

  TNxLogFileNewLinesEvent = procedure(Sender : TObject; NewLines : TStrings; aMachineIdent : String) of object;
  TNxLogFilterEvent = procedure(Sender : TObject; aLine : String; aMessage : TNxLoggerMessage; aMachineIdent : String; var doAddLine : Boolean) of object;

  TNxLogConnectionConnectEvent = procedure(Sender : TObject; aConnected : Boolean) of object;
  TNxLogConnectionTailActiveEvent = procedure(Sender : TObject; aTailActive : Boolean) of object;

  TNxLogConnection  = class(TComponent)
  protected
    fLogFormater      : TNxLogFormater;

    fDoUpdateSync     : Boolean;
    fDoUpdateAsync    : Boolean;
    fDoTail           : TIdThreadSafeBoolean;

    fOnNewLines       : TNxLogFileNewLinesEvent;
    fOnNewLinesAsync  : TNxLogFileNewLinesEvent;
    fOnFilter         : TNxLogFilterEvent;

    fConnected          : TIdThreadSafeBoolean;
    fOnConnectionChange : TNxLogConnectionConnectEvent;

    fTailActive         : TIdThreadSafeBoolean;
    fOnTailActiveChange : TNxLogConnectionTailActiveEvent;

    procedure setLogFormater(aValue : TNxLogFormater); virtual;
    function  getDoTail : Boolean;
    procedure setDoTail(aValue : Boolean);
  protected
    function  getConnected : Boolean; virtual;
    procedure setConnected(aValue : Boolean); virtual;
    function  getTailActive : Boolean; virtual;
    procedure setTailActive(aValue : Boolean); virtual;

    procedure raiseConnectedEvent(aConnected : Boolean); virtual;
    procedure raiseTailActiveEvent(aActive : Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Connected  : Boolean read getConnected;
    property TailActive : Boolean read getTailActive;
  published
    property DoTail             : Boolean read getDoTail write setDoTail;
    property DoUpdateSync       : Boolean read fDoUpdateSync write fDoUpdateSync;
    property DoUpdateAsync      : Boolean read fDoUpdateAsync write fDoUpdateAsync;

    property LogFormater        : TNxLogFormater read fLogFormater write setLogFormater;

    property OnNewLines         : TNxLogFileNewLinesEvent read fOnNewLines write fOnNewLines;
    property OnNewLinesAsync    : TNxLogFileNewLinesEvent read fOnNewLinesAsync write fOnNewLinesAsync;
    property OnFilter           : TNxLogFilterEvent read fOnFilter write fOnFilter;
    property OnConnectionChange : TNxLogConnectionConnectEvent read fOnConnectionChange write fOnConnectionChange;
    property OnTailActiveChange : TNxLogConnectionTailActiveEvent read fOnTailActiveChange write fOnTailActiveChange;
  end;

  TNxLogFile = class(TNxLogConnection)
  private
    fFilename         : String;
    fReference        : String;
    fFileStream       : TFileStream;
    fEncoding         : TEncoding;
    fReadonly         : Boolean;
    fWatchThread      : TNxLogFileThread;


    procedure updateCB(Sender:TObject; aStrings : TStrings; aMachineIdent : String);
    procedure updateAsyncCB(Sender:TObject; aStrings : TStrings; aMachineIdent : String);
    procedure filterCB(Sender : TObject; aLine : String; aMessage : TNxLoggerMessage; var doAddLine : Boolean);

    //procedure setFilename(aValue : String);
    function  getFilename : String;

    procedure setReference(aValue : String);
    function  getReference : String;

  protected
    procedure setLogFormater(aValue : TNxLogFormater); override;
    function  checkStreamValid : Boolean; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Filename         : String read getFilename;
    property Reference        : String read getReference write setReference;
    property Readonly         : Boolean read fReadonly write fReadonly;
  end;

  TNxLogFolderStruct  = class
  private
    fCSAccess     : TCriticalSection;
    fBase         : String;
    fTail         : String;
    fFullFilename : String;
    fStrategy     : TNxLogAppenderFileStrategy;
    fWatchStream  : TFileStream;
    fLogFormater  : TNxLogFormater;
    fStreamSize   : Int64;
    fCurrentSize  : Int64;
    fCurrentLine  : String;
    fCurrentLines : TStrings;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TNxLogFolderReadErrorEvent = procedure(Sender : TObject; Connected : Boolean; ErrorText : String) of object;
  TNxLogFolderCheckErrorEvent = procedure(Sender : TObject; Connected : Boolean; ErrorText : String) of object;
  TNxLogFolderFilesStartEvent = procedure(Sender : TObject; Count : Integer) of object;
  TNxLogFolderFilesDoneEvent = procedure(Sender : TObject) of object;
  TNxLogFolderNewFileEvent = procedure(Sender : TObject; Connected : Boolean; aNewFilename : String) of object;

  TNxLogFolder = class(TNxLogConnection)
  private
    fFoldername       : String;
    fFileStructs      : TStrings;
    fCSFileStructs    : TCriticalSection;
    fReference        : String;
    fEncoding         : TEncoding;
    fReadonly         : Boolean;
    fWatchThread      : TNxLogFolderThread;
    fOnFilesStart     : TNxLogFolderFilesStartEvent;
    fOnFilesDone      : TNxLogFolderFilesDoneEvent;
    fOnReadError      : TNxLogFolderReadErrorEvent;
    fOnCheckError     : TNxLogFolderCheckErrorEvent;
    fOnNewFile        : TNxLogFolderNewFileEvent;


    procedure updateCB(Sender:TObject; aStrings : TStrings; aMachineIdent : String);
    procedure updateAsyncCB(Sender:TObject; aStrings : TStrings; aMachineIdent : String);
    procedure filterCB(Sender : TObject; aLine : String; aMessage : TNxLoggerMessage; var doAddLine : Boolean);

    function  getFoldername : String;

    procedure setReference(aValue : String);
    function  getReference : String;

  protected
    procedure informFilesStart(aCount : Integer); virtual;
    procedure informFilesDone; virtual;
    procedure informNewFile(aNewFilename : String); virtual;

    procedure informReadError(aErrorText : String); virtual;
    procedure informCheckError(aErrorText : String); virtual;

    procedure setLogFormater(aValue : TNxLogFormater); override;
    function  checkStreamValid : Boolean; virtual;

    procedure analyseLogFolder(aFoldername : String); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Foldername       : String read getFoldername;
    property Reference        : String read getReference write setReference;
    property Readonly         : Boolean read fReadonly write fReadonly;

    property OnReadError      : TNxLogFolderReadErrorEvent read fOnReadError write fOnReadError;
    property OnCheckError     : TNxLogFolderCheckErrorEvent read fOnCheckError write fOnCheckError;
    property OnFilesStart     : TNxLogFolderFilesStartEvent read fOnFilesStart write fOnFilesStart;
    property OnFilesDone      : TNxLogFolderFilesDoneEvent read fOnFilesDone write fOnFilesDone;
    property OnNewFile        : TNxLogFolderNewFileEvent read fOnNewFile write fOnNewFile;
  end;



  TNxLogTCPReadErrorEvent = procedure(Sender : TObject; Connected : Boolean; ErrorText : String) of object;
  TNxLogTCPConnectErrorEvent = procedure(Sender : TObject; Connected : Boolean; ErrorText : String) of object;
  TNxLogTCPTailActiveErrorEvent = procedure(Sender : TObject; TailActive : Boolean; ErrorText : String) of object;

  TNxLogTCP = class(TNxLogConnection)
  private
    fHostname           : TIdThreadSafeString;
    fPort               : TIdThreadSafeInteger;
    fUsername           : TIdThreadSafeString;
    fPassword           : TIdThreadSafeString;
    fFilters            : TFMThreadSafeList;
    fEncoding           : TEncoding;
    fTCPClient          : TIdTCPClient;
    fWatchThread        : TNxLogTCPThread;

    fOnReadError        : TNxLogTCPReadErrorEvent;
    fOnConnectError     : TNxLogTCPConnectErrorEvent;
    fOnTailActiveError  : TNxLogTCPTailActiveErrorEvent;

    procedure updateCB(Sender:TObject; aStrings : TStrings; aMachineIdent : String);
    procedure updateAsyncCB(Sender:TObject; aStrings : TStrings; aMachineIdent : String);
    procedure filterCB(Sender : TObject; aLine : String; aMessage : TNxLoggerMessage; aMachineIdent : String; var doAddLine : Boolean);

    function  getFiltersAsText  : String;
    procedure setHostname(aValue : String);
    function  getHostname : String;
    procedure setPort(aValue : Integer);
    function  getPort : Integer;
    procedure setUsername(aValue : String);
    function  getUsername : String;
    procedure setPassword(aValue : String);
    function  getPassword : String;

  protected
    procedure informReadError(aErrorText : String); virtual;
    procedure informConnectError(aErrorText : String); virtual;
    procedure informTailActiveError(aErrorText : String); virtual;

    procedure clearFilters; virtual;
    procedure setLogFormater(aValue : TNxLogFormater); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure assignFilters(aFilters : TFMList);
    function  getMachineIdents(aToStrings : TStrings; aUsername : String = ''; aPassword : String = '') : Boolean; virtual;
    function  getApplicationIds(aToStrings : TStrings; aUsername : String = ''; aPassword : String = '') : Boolean; virtual;
  published
    property ServerHostname : String read getHostname write setHostname;
    property ServerPort     : Integer read getPort write setPort;
    property ServerUsername : String read getUsername write setUsername;
    property ServerPassword : String read getPassword write setPassword;

    property OnReadError        : TNxLogTCPReadErrorEvent read fOnReadError write fOnReadError;
    property OnConnectError     : TNxLogTCPConnectErrorEvent read fOnConnectError write fOnConnectError;
    property OnTailActiveError  : TNxLogTCPTailActiveErrorEvent read fOnTailActiveError write fOnTailActiveError;
  end;


implementation

uses System.Math, IdGlobal, System.IOUtils;

const

  CodesForBase64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';


procedure clearStrings(aStrings : TStrings);
var i : Integer;
begin
  if aStrings <> nil then
  begin
    for i := 0 to aStrings.Count - 1 do
    begin
      aStrings.Objects[i].Free;
      aStrings.Objects[i] := nil;
    end;
    aStrings.Clear;
  end;
end;

function encodeBase64(S: string): string;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do
  begin
    x := Ord(s[i]);
    b := b * 256 + x;
    a := a + 8;
    while a >= 6 do
    begin
      a := a - 6;
      x := b div (1 shl a);
      b := b mod (1 shl a);
      Result := Result + CodesForBase64[x + 1];
    end;
  end;
  if a > 0 then
  begin
    x := b shl (6 - a);
    Result := Result + CodesForBase64[x + 1];
  end;

  if (length(s) mod 3) = 0 then Result := Result+'';
  if (length(s) mod 3) = 1 then Result := Result+'==';
  if (length(s) mod 3) = 2 then Result := Result+'=';
end;

{ **************************************************************************** }
{ ***** TNxLogFileThread ***************************************************** }
{ **************************************************************************** }
constructor TNxLogFileThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  fOnUpdate       := nil;
  fOnUpdateAsync  := nil;
  fOnFilter       := nil;
  fWatchStream    := nil;
  fCurrentLine    := '';
  fCurrentLines   := TStringList.Create;
  fEncoding       := TEncoding.UTF8;
  fPreTailSize    := 1024 * 8;
  fMaxLineLength  := 1024 * 512;
  fLogFormater    := nil;
end;

destructor TNxLogFileThread.Destroy;
begin
  fLogFormater    := nil;
  fOnUpdate       := nil;
  fOnUpdateAsync  := nil;
  fOnFilter       := nil;
  fWatchStream    := nil;
  fCurrentLine    := '';
  clearStrings(fCurrentLines);
  FreeAndNil(fCurrentLines);
  inherited Destroy;
end;

procedure TNxLogFileThread.clearStrings(aStrings : TStrings);
var i : Integer;
    o : TObject;
begin
  if aStrings <> nil then
  begin
    aStrings.BeginUpdate;
    try
      for i := 0 to aStrings.Count - 1 do
      begin
        o := aStrings.Objects[i];
        aStrings.Objects[i] := nil;
        FreeAndNil(o);
      end;
      aStrings.Clear;
    finally
      aStrings.EndUpdate;
    end;
  end;
end;

procedure TNxLogFileThread.Execute;
var currentsize, streamsize : Int64;
    zs                      : String;
begin
  fParent.setConnected(true);
  fParent.raiseConnectedEvent(true);
  streamsize  := fWatchStream.Size;
  if fPreTailSize <= streamsize then
  begin
    currentsize := fWatchStream.Size - fPreTailSize;
  end else
  begin
    currentsize := 0;
  end;
  while not Terminated do
  begin
    streamsize  := fWatchStream.Size;
    if currentsize < streamsize then
    begin
      try
        update(currentsize, streamsize);
      except
        on e : exception do
        begin
          zs  := e.Message;
          informReadError(zs);
        end;
      end;
      currentsize := streamsize;
    end else
    begin
      try
        if not checkStreamValid then
        begin
          if fPreTailSize <= streamsize then
          begin
            currentsize := fWatchStream.Size - fPreTailSize;
          end else
          begin
            currentsize := 0;
          end;
        end;
      except
        on e : exception do
        begin
          zs  := e.Message;
          informCheckError(zs);
        end;
      end;
    end;
    sleep(500);
  end;
end;

procedure TNxLogFileThread.update(aCurrentSize, aStreamSize : Int64);
var diff              : Int64;
    cBuffer           : TBytes;
    zs                : String;
begin
  diff  := astreamsize - acurrentsize;
  if diff > 0 then
  begin
    setlength(cBuffer, diff);
    fWatchStream.Position := aCurrentSize;
    fWatchStream.Read(cBuffer[0], diff);
    zs  := fEncoding.GetString(cBuffer);
    fCurrentLine  := fCurrentLine + zs;
    clearStrings(fCurrentLines);
    if length(fCurrentLine) > fMaxLineLength then
    begin
      fCurrentLine  := fCurrentLine + #13;
    end;
    fCurrentLine  := extractLines(fCurrentLine, fCurrentLines);
    if fCurrentLines.Count > 0 then
    begin
      if Assigned(fOnUpdateAsync) then
      begin
        fOnUpdateAsync(self, fCurrentLines, '');
      end;
      if Assigned(fOnUpdate) then
      begin
        Synchronize(procedure
        begin
          fOnUpdate(self, fCurrentLines, '');
        end);
      end;
    end;
  end;
end;

function  TNxLogFileThread.checkStreamValid : Boolean;
begin
  result  := fParent.checkStreamValid;
end;

function  TNxLogFileThread.extractLines(aSource : String; aDestination : TStrings) : String;
var i     : Integer;
    cc    : Char;
    zs    : String;
    ac    : Boolean;
    al    : Boolean;
    msg   : TNxLoggerMessage;
begin
  zs  := '';
  ac  := false;
  for i := 1 to length(aSource) do
  begin
    cc  := aSource[i];
    if CharInSet(cc, [#10, #13, #0]) then
    begin
      if ac then        // Wenn wir vorher in "AddChar" waren, dann neuen String hinzunehmen...
      begin
        al  := true;
        ac  := false;   // Ab jetzt sind wir nicht mehr in "AddChar"...
        if fLogFormater <> nil then
        begin
          msg := fLogFormater.parseMessage(zs);
          if assigned(fOnFilter) then
          begin
            fOnFilter(Self, zs, msg, al);
          end;
          if al then
          begin
            aDestination.AddObject(zs, msg);
          end else
          begin
            FreeAndNil(msg);
          end;
        end else
        begin
          if assigned(fOnFilter) then
          begin
            fOnFilter(Self, zs, nil, al);
          end;
          if al then aDestination.Add(zs);
        end;
        zs  := '';
      end else
      begin
        //todo: Hier Leerzeilen beachten!
      end;
    end else
    begin
      ac  := true;    // Status "AddChar" setzen...
      zs  := zs + cc;
    end;
  end;
  result  := zs;
end;

procedure TNxLogFileThread.informReadError(aErrorText : String);
begin

end;

procedure TNxLogFileThread.informCheckError(aErrorText : String);
begin

end;

{ **************************************************************************** }
{ ***** TNxLogFolderThread *************************************************** }
{ **************************************************************************** }
constructor TNxLogFolderThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  fOnUpdate       := nil;
  fOnUpdateAsync  := nil;
  fOnFilter       := nil;
  fEncoding       := TEncoding.UTF8;
  fPreTailSize    := 1024 * 8;
  fMaxLineLength  := 1024 * 512;
  nxLogging.initLoggerFormatSettings(fLoggerFileFormatSettings);
end;

destructor TNxLogFolderThread.Destroy;
begin
  fOnUpdate       := nil;
  fOnUpdateAsync  := nil;
  fOnFilter       := nil;
  inherited Destroy;
end;

procedure TNxLogFolderThread.clearStrings(aStrings : TStrings);
var i : Integer;
    o : TObject;
begin
  if aStrings <> nil then
  begin
    aStrings.BeginUpdate;
    try
      for i := 0 to aStrings.Count - 1 do
      begin
        o := aStrings.Objects[i];
        aStrings.Objects[i] := nil;
        FreeAndNil(o);
      end;
      aStrings.Clear;
    finally
      aStrings.EndUpdate;
    end;
  end;
end;

procedure TNxLogFolderThread.initWatchfiles;
var i                       : Integer;
    cs                      : TNxLogFolderStruct;
    cfon, cfin              : String;
begin
  fParent.fCSFileStructs.Enter;
  try
    for i := 0 to fParent.fFileStructs.Count - 1 do
    begin
      cs  := TNxLogFolderStruct(fParent.fFileStructs.Objects[i]);
      case cs.fStrategy of
        NXLFS_SINGLEFILE:
        begin
          if cs.fWatchStream = nil then
          begin
            if FileExists(cs.fFullFilename) then
            begin
              if fReadonly then
              begin
                cs.fWatchStream := TFileStream.Create(cs.fFullFilename, fmOpenRead +  fmShareDenyNone);
              end else
              begin
                cs.fWatchStream := TFileStream.Create(cs.fFullFilename, fmOpenReadWrite +  fmShareDenyNone);
              end;
            end else
            begin
              cs.fWatchStream := nil;
            end;
          end;
        end;
        NXLFS_NEWFILES:
        begin
          if cs.fWatchStream = nil then
          begin
            //cFilename := fDirectory + fFilenameBase + '_' + DateToStr(nowUTC, fFormatSettings) + '.nxlog';
            cfon  := fParent.fFoldername;
            if cfon <> '' then if cfon[length(cfon)] <> '\' then cfon  := cfon + '\';
            cfin  := cfon + cs.fBase + '_' + DateToStr(nowUTC, fLoggerFileFormatSettings) + '.nxlog';
            if cs.fFullFilename <> cfin then cs.fFullFilename := cfin;
            if FileExists(cfin) then
            begin
              if fReadonly then
              begin
                cs.fWatchStream := TFileStream.Create(cs.fFullFilename, fmOpenRead +  fmShareDenyNone);
              end else
              begin
                cs.fWatchStream := TFileStream.Create(cs.fFullFilename, fmOpenReadWrite +  fmShareDenyNone);
              end;
            end else
            begin
              cs.fWatchStream := nil;
            end;
          end else
          begin
            cfon  := fParent.fFoldername;
            if cfon <> '' then if cfon[length(cfon)] <> '\' then cfon  := cfon + '\';
            cfin  := cfon + cs.fBase + '_' + DateToStr(nowUTC, fLoggerFileFormatSettings) + '.nxlog';
            if cs.fFullFilename <> cfin then
            begin
              FreeAndNil(cs.fWatchStream);
              cs.fFullFilename := cfin;
              if FileExists(cfin) then
              begin
                if fReadonly then
                begin
                  cs.fWatchStream := TFileStream.Create(cs.fFullFilename, fmOpenRead +  fmShareDenyNone);
                end else
                begin
                  cs.fWatchStream := TFileStream.Create(cs.fFullFilename, fmOpenReadWrite +  fmShareDenyNone);
                end;
              end else
              begin
                cs.fWatchStream := nil;
              end;
            end;
          end;
        end;
        NXLFS_RENAME:
        begin
          if cs.fWatchStream = nil then
          begin
            if FileExists(cs.fFullFilename) then
            begin
              if fReadonly then
              begin
                cs.fWatchStream := TFileStream.Create(cs.fFullFilename, fmOpenRead +  fmShareDenyNone);
              end else
              begin
                cs.fWatchStream := TFileStream.Create(cs.fFullFilename, fmOpenReadWrite +  fmShareDenyNone);
              end;
            end else
            begin
              cs.fWatchStream := nil;
            end;
          end;
        end;
      end;
    end;
  finally
    fParent.fCSFileStructs.Leave;
  end;
end;

procedure TNxLogFolderThread.doneWatchfiles;
var i                       : Integer;
    cs                      : TNxLogFolderStruct;
begin
  fParent.fCSFileStructs.Enter;
  try
    for i := 0 to fParent.fFileStructs.Count - 1 do
    begin
      cs  := TNxLogFolderStruct(fParent.fFileStructs.Objects[i]);
      FreeAndNil(cs.fWatchStream);
      case cs.fStrategy of
        NXLFS_SINGLEFILE:
        begin
        end;
        NXLFS_NEWFILES:
        begin
        end;
        NXLFS_RENAME:
        begin
        end;
      end;
    end;
  finally
    fParent.fCSFileStructs.Leave;
  end;
end;

procedure TNxLogFolderThread.Execute;
var zs                      : String;
    i                       : Integer;
    cs                      : TNxLogFolderStruct;
begin
  fParent.setConnected(true);
  fParent.raiseConnectedEvent(true);
  initWatchfiles;
  try
    fParent.fCSFileStructs.Enter;
    try
      for i := 0 to fParent.fFileStructs.Count - 1 do
      begin
        cs  := TNxLogFolderStruct(fParent.fFileStructs.Objects[i]);
        if cs.fWatchStream <> nil then
        begin
          cs.fStreamSize  := cs.fWatchStream.Size;
          if fPreTailSize <= cs.fStreamSize then
          begin
            cs.fCurrentSize := cs.fWatchStream.Size - fPreTailSize;
          end else
          begin
            cs.fCurrentSize := 0;
          end;
        end else
        begin
          cs.fStreamSize  := 0;
          cs.fCurrentSize := 0;
        end;
      end;
    finally
      fParent.fCSFileStructs.Leave;
    end;
  finally
    doneWatchfiles;
  end;

  while not Terminated do
  begin
    try
      initWatchfiles;
      try
        fParent.fCSFileStructs.Enter;
        try
          informFilesStart(fParent.fFileStructs.Count);
          for i := 0 to fParent.fFileStructs.Count - 1 do
          begin
            cs  := TNxLogFolderStruct(fParent.fFileStructs.Objects[i]);
            if cs.fWatchStream <> nil then
            begin
              cs.fStreamSize  := cs.fWatchStream.Size;
              if cs.fStreamSize < cs.fCurrentSize then
              begin
                // Neuer Tah vorbei...
                cs.fCurrentSize := 0;
                informNewFile(cs.fFullFilename);
              end;
              if cs.fCurrentSize < cs.fStreamSize then
              begin
                try
                  update(cs);
                except
                  on e : exception do
                  begin
                    zs  := e.Message;
                    informReadError(zs);
                  end;
                end;
                cs.fCurrentSize := cs.fStreamSize;
              end else
              begin
                try
                  if not checkStreamValid then
                  begin
                    if fPreTailSize <= cs.fStreamSize then
                    begin
                      cs.fCurrentSize := cs.fWatchStream.Size - fPreTailSize;
                    end else
                    begin
                      cs.fCurrentSize := 0;
                    end;
                  end;
                except
                  on e : exception do
                  begin
                    zs  := e.Message;
                    informCheckError(zs);
                  end;
                end;
              end;
            end else
            begin
              // Datei existiert nicht...

            end;
          end;
        finally
          fParent.fCSFileStructs.Leave;
        end;
        informFilesDone;
      finally
        doneWatchfiles;
      end;
      sleep(500);
    except
      on e : exception do
      begin
        zs  := 'unknown exception:'+e.Message;
        informReadError(zs);
      end;
    end;
  end;
end;

procedure TNxLogFolderThread.update(aStruct : TNxLogFolderStruct);
var diff              : Int64;
    cBuffer           : TBytes;
    zs                : String;
    cLineCount        : Integer;
    sl                : TStrings;
begin
  //cLineCount  := 0;
  sl  := TStringList.Create;
  try
    aStruct.fCSAccess.Enter;
    try
      diff  := aStruct.fStreamSize - aStruct.fCurrentSize;
      if diff > 0 then
      begin
        setlength(cBuffer, diff);
        aStruct.fWatchStream.Position := aStruct.fCurrentSize;
        aStruct.fWatchStream.Read(cBuffer[0], diff);
        zs  := fEncoding.GetString(cBuffer);
        aStruct.fCurrentLine  := aStruct.fCurrentLine + zs;
        clearStrings(aStruct.fCurrentLines);
        if length(aStruct.fCurrentLine) > fMaxLineLength then
        begin
          aStruct.fCurrentLine  := aStruct.fCurrentLine + #13;
        end;
        aStruct.fCurrentLine  := extractLines(aStruct.fLogFormater, aStruct.fCurrentLine, aStruct.fCurrentLines);
      end;
      cLineCount  := aStruct.fCurrentLines.Count;
      if cLineCount > 0 then
      begin
        sl.Assign(aStruct.fCurrentLines);
      end;
    finally
      aStruct.fCSAccess.Leave;
    end;

    if cLineCount > 0 then
    begin
      if Assigned(fOnUpdateAsync) then
      begin
        fOnUpdateAsync(self, sl, '');
      end;
      if Assigned(fOnUpdate) then
      begin
        Synchronize(procedure
        begin
          fOnUpdate(self, sl, '');
        end);
      end;
    end;
  finally
    FreeAndNil(sl);
  end;
end;

function  TNxLogFolderThread.checkStreamValid : Boolean;
begin
  result  := fParent.checkStreamValid;
end;

function  TNxLogFolderThread.extractLines(aLogFormater : TNxLogFormater; aSource : String; aDestination : TStrings) : String;
var i     : Integer;
    cc    : Char;
    zs    : String;
    ac    : Boolean;
    al    : Boolean;
    msg   : TNxLoggerMessage;
begin
  zs  := '';
  ac  := false;
  for i := 1 to length(aSource) do
  begin
    cc  := aSource[i];
    if CharInSet(cc, [#10, #13, #0]) then
    begin
      if ac then        // Wenn wir vorher in "AddChar" waren, dann neuen String hinzunehmen...
      begin
        al  := true;
        ac  := false;   // Ab jetzt sind wir nicht mehr in "AddChar"...
        if aLogFormater <> nil then
        begin
          msg := aLogFormater.parseMessage(zs);
          if assigned(fOnFilter) then
          begin
            fOnFilter(Self, zs, msg, al);
          end;
          if al then
          begin
            aDestination.AddObject(zs, msg);
          end else
          begin
            FreeAndNil(msg);
          end;
        end else
        begin
          if assigned(fOnFilter) then
          begin
            fOnFilter(Self, zs, nil, al);
          end;
          if al then aDestination.Add(zs);
        end;
        zs  := '';
      end else
      begin
        //todo: Hier Leerzeilen beachten!
      end;
    end else
    begin
      ac  := true;    // Status "AddChar" setzen...
      zs  := zs + cc;
    end;
  end;
  result  := zs;
end;

procedure TNxLogFolderThread.informReadError(aErrorText : String);
begin
  fParent.informReadError(aErrorText);
end;

procedure TNxLogFolderThread.informCheckError(aErrorText : String);
begin
  fParent.informCheckError(aErrorText);
end;

procedure TNxLogFolderThread.informFilesStart(aCount : Integer);
begin
  fParent.informFilesStart(aCount);
end;

procedure TNxLogFolderThread.informFilesDone;
begin
  fParent.informFilesDone;
end;

procedure TNxLogFolderThread.informNewFile(aNewFilename : String);
begin
  fParent.informNewFile(aNewFilename);
end;


{ **************************************************************************** }
{ ***** TNxLogTCPThread ****************************************************** }
{ **************************************************************************** }
constructor TNxLogTCPThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  fOnUpdate       := nil;
  fOnUpdateAsync  := nil;
  fOnFilter       := nil;
  fEncoding       := TEncoding.UTF8;
  fMaxLineLength  := 1024 * 512;
  fLogFormater    := nil;
  fCurrentLines   := TStringList.Create;
end;

destructor TNxLogTCPThread.Destroy;
begin
  fLogFormater    := nil;
  fOnUpdate       := nil;
  fOnUpdateAsync  := nil;
  fOnFilter       := nil;
  FreeAndNil(fCurrentLines);
  inherited Destroy;
end;

procedure TNxLogTCPThread.clearStrings(aStrings : TStrings);
var i : Integer;
    o : TObject;
begin
  if aStrings <> nil then
  begin
    aStrings.BeginUpdate;
    try
      for i := 0 to aStrings.Count - 1 do
      begin
        o := aStrings.Objects[i];
        aStrings.Objects[i] := nil;
        FreeAndNil(o);
      end;
      aStrings.Clear;
    finally
      aStrings.EndUpdate;
    end;
  end;
end;

procedure TNxLogTCPThread.Execute;
var zs, resstr    : String;
    wassplit      : Boolean;
    ctout         : Integer;

  procedure _registerTail;
  begin
    try
      if fTCPClient.IOHandler <> nil then
      begin
        if fParent.fFilters.Count = 0 then
        begin
          if (fParent.getUsername = '') and (fParent.getPassword = '') then
          begin
            zs  := 'tail:';
          end else
          begin
            zs  := Format('tail:%0:s|%1:s', [fParent.getUsername, fParent.getPassword]);
          end;
        end else
        begin
          zs  := Format('tail:%0:s|%1:s|%2:s', [fParent.getUsername, fParent.getPassword, encodeBase64(fParent.getFiltersAsText)]);
        end;
        {$if CompilerVersion > 24}
        fTCPClient.IOHandler.WriteLn(zs, IndyTextEncoding(fEncoding));
        zs  := fTCPClient.IOHandler.ReadLn(EOL, 30000, -1, IndyTextEncoding(fEncoding));
        {$else}
        fTCPClient.IOHandler.WriteLn(zs, fEncoding);
        zs  := fTCPClient.IOHandler.ReadLn(EOL, 30000, -1, fEncoding);
        {$ifend}
        zs  := lowercase(zs);
        if zs = 'ok' then
        begin
          fParent.setTailActive(true);
          Synchronize(
            procedure
            begin
              fParent.raiseTailActiveEvent(true);
            end
          );
        end else
        begin
          fParent.setTailActive(false);
          informTailActiveError(zs);
          Synchronize(
            procedure
            begin
              fParent.raiseTailActiveEvent(false);
            end
          );
        end;
      end else
      begin
        zs  := 'no tcp connection to server';
        fParent.setTailActive(false);
          informTailActiveError(zs);
          Synchronize(
            procedure
            begin
              fParent.raiseTailActiveEvent(false);
            end
          );
      end;
    except
      on e : exception do
      begin
        fParent.setTailActive(false);
        zs  := e.Message;
        informTailActiveError(zs);
        Synchronize(
          procedure
          begin
            fParent.raiseTailActiveEvent(false);
          end
        );
      end;
    end;
  end;

  procedure _unregisterTail;
  begin

  end;

  procedure _connect;
  begin
    try
      fTCPClient.Connect;
      zs  := 'ok';
      if zs = 'ok' then
      begin
        fParent.setConnected(true);
        Synchronize(
          procedure
          begin
            fParent.raiseConnectedEvent(true);
          end
        );
      end else
      begin
        fParent.setConnected(false);
        informConnectError(zs);
        Synchronize(
          procedure
          begin
            fParent.raiseConnectedEvent(false);
          end
        );
      end;
    except
      on e : exception do
      begin
        fParent.setConnected(false);
        zs  := e.Message;
        informConnectError(zs);
        Synchronize(
          procedure
          begin
            fParent.raiseConnectedEvent(false);
          end
        );
      end;
    end;
  end;

  procedure _disconnect;
  begin
    fTCPClient.Disconnect;
    fParent.setConnected(false);
    Synchronize(
      procedure
      begin
        fParent.raiseConnectedEvent(false);
      end
    );
  end;

begin
  while not Terminated do
  begin
    if fTCPClient.Connected then
    begin
      if fParent.DoTail then
      begin
        try
          ctout := idglobal.IdTimeoutDefault;
          {$if CompilerVersion > 24}
          resstr := fTCPClient.IOHandler.ReadLnSplit(wassplit, idglobal.LF, ctout, -1, IndyTextEncoding(fEncoding));
          while wassplit do
          begin
            resstr := resstr + fTCPClient.IOHandler.ReadLnSplit(wassplit, idglobal.LF, ctout, -1, IndyTextEncoding(fEncoding));
          end;
          {$else}
          resstr := fTCPClient.IOHandler.ReadLnSplit(wassplit, idglobal.LF, ctout, -1, fEncoding);
          while wassplit do
          begin
            resstr := resstr + fTCPClient.IOHandler.ReadLnSplit(wassplit, idglobal.LF, ctout, -1, fEncoding);
          end;
          {$ifend}
          if resstr <> '' then
          begin
            update(resstr);
          end;
        except
          on e : exception do
          begin
            zs  := e.Message;
            informReadError(zs);
            try
              _disconnect;
            except
            end;
          end;
        end;
      end else
      begin
        if fParent.getTailActive then
        begin
          _unregisterTail;
        end else
        begin
          sleep(500);
        end;
      end;
    end else
    begin
      // versuch zu verbinden...
      _connect;
      if fParent.DoTail then
      begin
        _registerTail;
      end;
    end;
    sleep(500);
  end;
  try
    _disconnect;
  except
  end;
end;

procedure TNxLogTCPThread.update(aMessage : String);
var cm    : TNxLoggerMessage;
    cmach : String;
    al    : Boolean;
begin
  cmach := '';
  clearStrings(fCurrentLines);
  if fLogFormater = nil then
  begin
    fLogFormater  := TNxLogFormaterTCP.Create;
  end;
  if fLogFormater is TNxLogFormaterTCP then
  begin
    cm  := (fLogFormater as TNxLogFormaterTCP).parseMessageMachine(aMessage, cmach);
  end else
  begin
    cm  := fLogFormater.parseMessage(aMessage);
  end;
  al := true;
  if assigned(fOnFilter) then
  begin
    fOnFilter(Self, aMessage, cm, cmach, al);
  end;
  if al then
  begin
    fCurrentLines.AddObject(aMessage, cm);
  end else
  begin
    FreeAndNil(cm);
  end;
  if Assigned(fOnUpdateAsync) then
  begin
    fOnUpdateAsync(self, fCurrentLines, cmach);
  end;
  if Assigned(fOnUpdate) then
  begin
    Synchronize(procedure
    begin
      fOnUpdate(self, fCurrentLines, cmach);
    end);
  end;
end;

procedure TNxLogTCPThread.informReadError(aErrorText : String);
begin
  fParent.informReadError(aErrorText);
end;

procedure TNxLogTCPThread.informConnectError(aErrorText : String);
begin
  fParent.informConnectError(aErrorText);
end;

procedure TNxLogTCPThread.informTailActiveError(aErrorText : String);
begin
  fParent.informTailActiveError(aErrorText);
end;


{ **************************************************************************** }
{ ***** TNxLogConnection ***************************************************** }
{ **************************************************************************** }
constructor TNxLogConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDoUpdateSync     := true;
  fDoUpdateAsync    := true;
  fOnNewLines       := nil;
  fOnNewLinesAsync  := nil;
  fOnFilter         := nil;
  fLogFormater      := nil;
  fDoTail             := TIdThreadSafeBoolean.Create;
  fDoTail.Value       := true;
  fConnected          := TIdThreadSafeBoolean.Create;
  fConnected.Value    := false;
  fOnConnectionChange := nil;
  fTailActive         := TIdThreadSafeBoolean.Create;
  fTailActive.Value   := false;
  fOnTailActiveChange := nil;
end;

destructor TNxLogConnection.Destroy;
begin
  fLogFormater      := nil;
  fOnNewLines       := nil;
  fOnNewLinesAsync  := nil;
  fOnFilter         := nil;
  FreeAndNil(fConnected);
  FreeAndNil(fTailActive);
  FreeAndNil(fDoTail);
  inherited Destroy;
end;

procedure TNxLogConnection.raiseConnectedEvent(aConnected : Boolean);
begin
  if assigned(fOnConnectionChange) then
  begin
    fOnConnectionChange(self, aConnected);
  end;
end;

procedure TNxLogConnection.raiseTailActiveEvent(aActive : Boolean);
begin
  if assigned(fOnTailActiveChange) then
  begin
    fOnTailActiveChange(self, aActive);
  end;
end;

procedure TNxLogConnection.setLogFormater(aValue : TNxLogFormater);
begin
  if fLogFormater <> aValue then
  begin
    fLogFormater  := aValue;
  end;
end;

function  TNxLogConnection.getDoTail : Boolean;
begin
  result  := fDoTail.Value;
end;

procedure TNxLogConnection.setDoTail(aValue : Boolean);
begin
  fDoTail.Value := aValue;
end;

function  TNxLogConnection.getConnected : Boolean;
begin
  result  := fConnected.Value;
end;

procedure TNxLogConnection.setConnected(aValue : Boolean);
begin
  fConnected.Value  := aValue;
end;

function  TNxLogConnection.getTailActive : Boolean;
begin
  result  := fTailActive.Value;
end;

procedure TNxLogConnection.setTailActive(aValue : Boolean);
begin
  fTailActive.Value := aValue;
end;

{ **************************************************************************** }
{ ***** TNxLogFile *********************************************************** }
{ **************************************************************************** }
constructor TNxLogFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fReadonly         := true;
  fFilename         := '';
  fFileStream       := nil;
  fEncoding         := TEncoding.UTF8;
end;

destructor TNxLogFile.Destroy;
begin
  if fWatchThread <> nil then
  begin
    fWatchThread.Terminate;
    fWatchThread.WaitFor;
    FreeAndNil(fWatchThread);
  end;
  FreeAndNil(fFileStream);
  inherited Destroy;
end;

procedure TNxLogFile.setLogFormater(aValue : TNxLogFormater);
begin
  inherited setLogFormater(aValue);
  if fWatchThread <> nil then
  begin
    fWatchThread.LogFormater  := fLogFormater;
  end;
end;

function  TNxLogFile.checkStreamValid : Boolean;
begin
  result  := true;
end;

procedure TNxLogFile.updateCB(Sender:TObject; aStrings : TStrings; aMachineIdent : String);
begin
  if assigned(fOnNewLines) then
  begin
    fOnNewLines(self, aStrings, aMachineIdent);
  end;
end;

procedure TNxLogFile.updateAsyncCB(Sender:TObject; aStrings : TStrings; aMachineIdent : String);
begin
  if assigned(fOnNewLinesAsync) then
  begin
    fOnNewLinesAsync(self, aStrings, aMachineIdent);
  end;
end;

procedure TNxLogFile.filterCB(Sender : TObject; aLine : String; aMessage : TNxLoggerMessage; var doAddLine : Boolean);
begin
  if assigned(fOnFilter) then
  begin
    fOnFilter(self, aLine, aMessage, '', doAddLine);
  end;
end;

procedure TNxLogFile.setReference(aValue : String);
begin
  if aValue <> fReference then
  begin
    if aValue <> '' then
    begin
      fReference := aValue;
      if FileExists(fReference) then
      begin
        // Datei angegeben...
        fFilename := fReference;
        if fFileStream <> nil then
        begin
          FreeAndNil(fFileStream);
        end;
        if fReadonly then
        begin
          fFileStream := TFileStream.Create(fReference, fmOpenRead +  fmShareDenyNone);
        end else
        begin
          fFileStream := TFileStream.Create(fReference, fmOpenReadWrite +  fmShareDenyNone);
        end;
        fWatchThread  := TNxLogFileThread.Create(true);
        fWatchThread.Parent := self;
        if fDoUpdateSync then fWatchThread.OnUpdate       := updateCB;
        if fDoUpdateAsync then fWatchThread.OnUpdateAsync := updateAsyncCB;
        fWatchThread.OnFilter       := filterCB;
        fWatchThread.WatchStream    := fFileStream;
        fWatchThread.LogFormater    := fLogFormater;
        fWatchThread.Start;
      end else
      begin
        fFilename := '';
      end;
    end else
    begin
      fWatchThread.Terminate;
      FreeAndNil(fWatchThread);
      fReference := '';
      FreeAndNil(fFileStream);
    end;
  end;
end;

function  TNxLogFile.getReference : String;
begin
  result  := fReference;
end;

function  TNxLogFile.getFilename : String;
begin
  result  := fFilename;
end;


{ **************************************************************************** }
{ ***** TNxLogFolderStruct *************************************************** }
{ **************************************************************************** }
constructor TNxLogFolderStruct.Create;
begin
  inherited Create;
  fCSAccess     := TCriticalSection.Create;
  fCSAccess.Enter;
  try
    fBase         := '';
    fTail         := '';
    fFullFilename := '';
    fStrategy     := NXLFS_NEWFILES;
    fWatchStream  := nil;
    fStreamSize   := 0;
    fCurrentSize  := 0;
    fLogFormater  := TNxLogFormaterDefault.Create;
    fCurrentLine  := '';
    fCurrentLines := TStringList.Create;
  finally
    fCSAccess.Leave;
  end;
end;

destructor TNxLogFolderStruct.Destroy;
begin
  fCSAccess.Enter;
  try
    FreeAndNil(fWatchStream);
    FreeAndNil(fLogFormater);
    FreeAndNil(fCurrentLines);
  finally
    fCSAccess.Leave;
  end;
  FreeAndNil(fCSAccess);
  inherited Destroy;
end;


{ **************************************************************************** }
{ ***** TNxLogFolder ********************************************************* }
{ **************************************************************************** }
constructor TNxLogFolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fReadonly         := true;
  fFoldername       := '';
  fEncoding         := TEncoding.UTF8;
  fFileStructs      := TStringList.Create;
  fCSFileStructs    := TCriticalSection.Create;
end;

destructor TNxLogFolder.Destroy;
begin
  if fWatchThread <> nil then
  begin
    fWatchThread.Terminate;
    fWatchThread.WaitFor;
    FreeAndNil(fWatchThread);
  end;
  FreeAndNil(fFileStructs);
  FreeAndNil(fCSFileStructs);
  inherited Destroy;
end;

procedure TNxLogFolder.informFilesStart(aCount : Integer);
begin
  if assigned(fOnFilesStart) then fOnFilesStart(self, acount);
end;

procedure TNxLogFolder.informFilesDone;
begin
  if assigned(fOnFilesDone) then fOnFilesDone(self);
end;

procedure TNxLogFolder.informReadError(aErrorText : String);
begin
  if assigned(fOnReadError) then fOnReadError(self, self.getConnected, aErrorText);
end;

procedure TNxLogFolder.informCheckError(aErrorText : String);
begin
  if assigned(fOnCheckError) then fOnCheckError(self, self.getConnected, aErrorText);
end;

procedure TNxLogFolder.informNewFile(aNewFilename : String);
begin
  if assigned(fOnNewFile) then fOnNewFile(self, self.getConnected, aNewFilename);
end;

procedure TNxLogFolder.setLogFormater(aValue : TNxLogFormater);
begin
  inherited setLogFormater(aValue);
end;

function  TNxLogFolder.checkStreamValid : Boolean;
begin
  result  := true;
end;

procedure TNxLogFolder.analyseLogFolder(aFoldername : String);
var cFiles        : TStringDynArray;
    sl, slbases   : TStrings;
    cBase, cTail  : String;
    i             : Integer;
    cs            : TNxLogFolderStruct;
begin
  fCSFileStructs.Enter;
  try
    clearStrings(fFileStructs);
  finally
    fCSFileStructs.Leave;
  end;
  try
    cFiles := TDirectory.GetFiles(aFoldername, '*.nxlog', TSearchOption.soAllDirectories);
  except
    on e: Exception do
    begin
      setlength(cFiles, 0);
    end;
  end;
  if length(cFiles) > 0 then
  begin
    sl  := TStringList.Create;
    slbases  := TStringList.Create;
    try
      for i := 0 to length(cFiles) - 1 do
      begin
        sl.LineBreak  := '_';
        sl.Text := ChangeFileExt(ExtractFileName(cFiles[i]), '');
        if sl.Count > 0 then
        begin
          if sl[sl.Count-1] = sl.LineBreak then sl.Delete(sl.Count-1);
          cTail := sl[sl.Count-1];
          sl.Delete(sl.Count-1);
          cBase := sl.Text;
          if cBase <> '' then
          begin
            if cBase[length(cBase)] = '_' then cBase  := Copy(cBase, 1, length(cBase)-1);
          end;
          if lowercase(cTail) = 'all' then
          begin
            fCSFileStructs.Enter;
            try
              if fFileStructs.IndexOf(cBase) < 0 then
              begin
                cs  := TNxLogFolderStruct.Create;
                cs.fBase  := cBase;
                cs.fTail  := cTail;
                cs.fFullFilename  := cFiles[i];
                cs.fStrategy  := NXLFS_SINGLEFILE;
                fFileStructs.AddObject(cBase, cs);
              end;
            finally
              fCSFileStructs.Leave;
            end;
          end else
          begin
            if lowercase(cTail) = 'current' then
            begin
              fCSFileStructs.Enter;
              try
                if fFileStructs.IndexOf(cBase) < 0 then
                begin
                  cs  := TNxLogFolderStruct.Create;
                  cs.fBase  := cBase;
                  cs.fTail  := cTail;
                  cs.fFullFilename  := cFiles[i];
                  cs.fStrategy  := NXLFS_RENAME;
                  fFileStructs.AddObject(cBase, cs);
                end;
              finally
                fCSFileStructs.Leave;
              end;
            end else
            begin
              // NEWFILES...
              fCSFileStructs.Enter;
              try
                if fFileStructs.IndexOf(cBase) < 0 then
                begin
                  cs  := TNxLogFolderStruct.Create;
                  cs.fBase  := cBase;
                  cs.fTail  := cTail;
                  cs.fFullFilename  := '';
                  cs.fStrategy  := NXLFS_NEWFILES;
                  fFileStructs.AddObject(cBase, cs);
                end;
              finally
                fCSFileStructs.Leave;
              end;
            end;
          end;
        end else
        begin
          cTail := '';
          cBase := '';
        end;
      end;
    finally
      FreeAndNil(sl);
      FreeAndNil(slbases);
    end;
  end else
  begin
    // Ordner vermutlich noch mehr...
  end;
end;

procedure TNxLogFolder.updateCB(Sender:TObject; aStrings : TStrings; aMachineIdent : String);
begin
  if assigned(fOnNewLines) then
  begin
    fOnNewLines(self, aStrings, aMachineIdent);
  end;
end;

procedure TNxLogFolder.updateAsyncCB(Sender:TObject; aStrings : TStrings; aMachineIdent : String);
begin
  if assigned(fOnNewLinesAsync) then
  begin
    fOnNewLinesAsync(self, aStrings, aMachineIdent);
  end;
end;

procedure TNxLogFolder.filterCB(Sender : TObject; aLine : String; aMessage : TNxLoggerMessage; var doAddLine : Boolean);
begin
  if assigned(fOnFilter) then
  begin
    fOnFilter(self, aLine, aMessage, '', doAddLine);
  end;
end;

procedure TNxLogFolder.setReference(aValue : String);
begin
  if aValue <> fReference then
  begin
    if aValue <> '' then
    begin
      fReference := aValue;
      if DirectoryExists(fReference) then
      begin
        // Datei angegeben...
        fFoldername := fReference;
        analyseLogFolder(fFoldername);

        fWatchThread  := TNxLogFolderThread.Create(true);
        fWatchThread.Parent := self;
        fWatchThread.Readonly := fReadonly;
        if fDoUpdateSync then fWatchThread.OnUpdate       := updateCB;
        if fDoUpdateAsync then fWatchThread.OnUpdateAsync := updateAsyncCB;
        fWatchThread.OnFilter       := filterCB;
        fWatchThread.Start;
      end else
      begin
        fFoldername := '';
      end;
    end else
    begin
      fWatchThread.Terminate;
      FreeAndNil(fWatchThread);
      fReference := '';
    end;
  end;
end;

function  TNxLogFolder.getReference : String;
begin
  result  := fReference;
end;

function  TNxLogFolder.getFoldername : String;
begin
  result  := fFoldername;
end;


{ **************************************************************************** }
{ ***** TNxLogTCP ************************************************************ }
{ **************************************************************************** }
constructor TNxLogTCP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fEncoding     := TEncoding.UTF8;
  fHostname     := TIdThreadSafeString.Create;
  fPort         := TIdThreadSafeInteger.Create;
  fUsername     := TIdThreadSafeString.Create;
  fPassword     := TIdThreadSafeString.Create;
  fFilters      := TFMThreadSafeList.Create;
  fHostname.Value := '';
  fPort.Value     := 0;
  fUsername.Value := '';
  fPassword.Value := '';
  fTCPClient    := nil;
  fWatchThread  := nil;
end;

destructor TNxLogTCP.Destroy;
begin
  if fWatchThread <> nil then
  begin
    fWatchThread.Terminate;
    FreeAndNil(fTCPClient);
    fWatchThread.WaitFor;
    FreeAndNil(fWatchThread);
  end;

  FreeAndNil(fHostname);
  FreeAndNil(fPort);
  FreeAndNil(fUsername);
  FreeAndNil(fPassword);
  clearFilters;
  FreeAndNil(fFilters);

  inherited Destroy;
end;

procedure TNxLogTCP.assignFilters(aFilters : TFMList);
var i     : Integer;
    fltrA : TNxLoggerMessageFilter;
    fltrB : TNxLoggerMessageFilter;
    cc    : TClass;
begin
  clearFilters;
  if aFilters <> nil then
  begin
    for i := 0 to aFilters.Count - 1 do
    begin
      fltrA := TNxLoggerMessageFilter(aFilters[i]);
      cc    := fltrA.ClassType;
      fltrB := TNxLoggerMessageFilter(cc.NewInstance);
      fltrB.Create;
      fltrB.Assign(fltrA);
      fFilters.Add(fltrB);
    end;
  end;
end;

function  TNxLogTCP.getMachineIdents(aToStrings : TStrings; aUsername : String = ''; aPassword : String = '') : Boolean;
var zs, resstr  : String;
    ctout       : Integer;
    wassplit    : Boolean;
//    ind         : Integer;
    sl          : TStrings;
begin
  result  := false;
  if (aToStrings <> nil) and (fTCPClient <> nil) and (fTCPClient.Connected) and (self.Connected) then
  begin
    if (aUsername <> '')  or (aPassword <> '') then
    begin
      zs  := Format('retrievemachineidents:%0:s;%1:s', [aUsername, aPassword]);
    end else
    begin
      zs  := 'retrievemachineidents:';
    end;
    {$if CompilerVersion > 24}
    fTCPClient.IOHandler.WriteLn(zs, IndyTextEncoding(TEncoding.UTF8));
    //ctout := idglobal.IdTimeoutDefault;
    ctout := 20000;
    resstr := fTCPClient.IOHandler.ReadLnSplit(wassplit, idglobal.LF, ctout, -1, IndyTextEncoding(fEncoding));
    while wassplit do
    begin
      resstr := resstr + fTCPClient.IOHandler.ReadLnSplit(wassplit, idglobal.LF, ctout, -1, IndyTextEncoding(fEncoding));
    end;
    {$else}
    fTCPClient.IOHandler.WriteLn(zs, TEncoding.UTF8);
    //ctout := idglobal.IdTimeoutDefault;
    ctout := 20000;
    resstr := fTCPClient.IOHandler.ReadLnSplit(wassplit, idglobal.LF, ctout, -1, fEncoding);
    while wassplit do
    begin
      resstr := resstr + fTCPClient.IOHandler.ReadLnSplit(wassplit, idglobal.LF, ctout, -1, fEncoding);
    end;
    {$ifend}
    if resstr <> '' then
    begin
      // Auswertung...
      if Pos('machineidents', resstr) = 1 then
      begin
        resstr  := Copy(resstr, 15, length(resstr)-15);
        sl  := TStringList.Create;
        try
          sl.LineBreak  := ';';
          sl.Text := resstr;
          aToStrings.AddStrings(sl);
          result  := true;
        finally
          FreeAndNil(sl);
        end;
      end;
    end;
  end;
end;

function  TNxLogTCP.getApplicationIds(aToStrings : TStrings; aUsername : String = ''; aPassword : String = '') : Boolean;
var zs, resstr  : String;
    ctout       : Integer;
    wassplit    : Boolean;
    sl          : TStrings;
begin
  result  := false;
  if (aToStrings <> nil) and (fTCPClient <> nil) and (fTCPClient.Connected) and (self.Connected) then
  begin
    if (aUsername <> '')  or (aPassword <> '') then
    begin
      zs  := Format('retrieveapplicationids:%0:s;%1:s', [aUsername, aPassword]);
    end else
    begin
      zs  := 'retrieveapplicationids:';
    end;
    {$if CompilerVersion > 24}
    fTCPClient.IOHandler.WriteLn(zs, IndyTextEncoding(TEncoding.UTF8));
    //ctout := idglobal.IdTimeoutDefault;
    ctout := 20000;
    resstr := fTCPClient.IOHandler.ReadLnSplit(wassplit, idglobal.LF, ctout, -1, IndyTextEncoding(fEncoding));
    while wassplit do
    begin
      resstr := resstr + fTCPClient.IOHandler.ReadLnSplit(wassplit, idglobal.LF, ctout, -1, IndyTextEncoding(fEncoding));
    end;
    {$else}
    fTCPClient.IOHandler.WriteLn(zs, TEncoding.UTF8);
    //ctout := idglobal.IdTimeoutDefault;
    ctout := 20000;
    resstr := fTCPClient.IOHandler.ReadLnSplit(wassplit, idglobal.LF, ctout, -1, fEncoding);
    while wassplit do
    begin
      resstr := resstr + fTCPClient.IOHandler.ReadLnSplit(wassplit, idglobal.LF, ctout, -1, fEncoding);
    end;
    {$ifend}
    if resstr <> '' then
    begin
      // Auswertung...
      if Pos('applicationids', resstr) = 1 then
      begin
        resstr  := Copy(resstr, 16, length(resstr)-16);
        sl  := TStringList.Create;
        try
          sl.LineBreak  := ';';
          sl.Text := resstr;
          aToStrings.AddStrings(sl);
          result  := true;
        finally
          FreeAndNil(sl);
        end;
      end;
    end;
  end;
end;

procedure TNxLogTCP.setLogFormater(aValue : TNxLogFormater);
begin
  inherited setLogFormater(aValue);
  if fWatchThread <> nil then
  begin
    fWatchThread.LogFormater  := fLogFormater;
  end;
end;

procedure TNxLogTCP.clearFilters;
var lst : TFMList; i  : Integer; cf : nxLogging.TNxLoggerMessageFilter;
begin
  lst := fFilters.LockList;
  try
    for i := 0 to lst.Count - 1 do
    begin
      cf  := nxLogging.TNxLoggerMessageFilter(lst[i]);
      lst[i]  := nil;
      FreeAndNil(cf);
    end;
    lst.Clear;
  finally
    fFilters.UnlockList;
  end;
end;

procedure TNxLogTCP.informReadError(aErrorText : String);
begin
  if assigned(fOnReadError) then fOnReadError(self, self.getConnected, aErrorText);
end;

procedure TNxLogTCP.informConnectError(aErrorText : String);
begin
  if assigned(fOnConnectError) then fOnConnectError(self, self.getConnected, aErrorText);
end;

procedure TNxLogTCP.informTailActiveError(aErrorText : String);
begin
  if assigned(fOnTailActiveError) then fOnTailActiveError(self, self.getTailActive, aErrorText);
end;

procedure TNxLogTCP.updateCB(Sender:TObject; aStrings : TStrings; aMachineIdent : String);
begin
  if assigned(fOnNewLines) then
  begin
    fOnNewLines(self, aStrings, aMachineIdent);
  end;
end;

procedure TNxLogTCP.updateAsyncCB(Sender:TObject; aStrings : TStrings; aMachineIdent : String);
begin
  if assigned(fOnNewLinesAsync) then
  begin
    fOnNewLinesAsync(self, aStrings, aMachineIdent);
  end;
end;

procedure TNxLogTCP.filterCB(Sender : TObject; aLine : String; aMessage : TNxLoggerMessage; aMachineIdent : String; var doAddLine : Boolean);
begin
  if assigned(fOnFilter) then
  begin
    fOnFilter(self, aLine, aMessage, aMachineIdent, doAddLine);
  end;
end;

function  TNxLogTCP.getFiltersAsText  : String;
var lst : TFMList; i  : Integer;
    cf : nxLogging.TNxLoggerMessageFilter;
    s  : TStringStream;
    w  : TWriter;
begin
  result  := '';
  s   := TStringStream.Create;
  w   := TWriter.Create(s, 1024);
  try
    w.WriteString('LogFilterList');
    lst := fFilters.LockList;
    try
      w.WriteInteger(lst.Count);
      w.FlushBuffer;
      for i := 0 to lst.Count - 1 do
      begin
        cf  := nxLogging.TNxLoggerMessageFilter(lst[i]);
        w.WriteString(cf.ClassName);
        w.FlushBuffer;
        cf.saveToStream(s);
      end;
      result  := s.DataString;
    finally
      fFilters.UnlockList;
    end;
  finally
    FreeAndNil(w);
    FreeAndNil(s);
  end;
end;

procedure TNxLogTCP.setHostname(aValue : String);
begin
  if aValue <> fHostname.Value then
  begin
    if aValue <> '' then
    begin
      fHostname.Value := aValue;

      fTCPClient  := TIdTCPClient.Create(nil);
      fTCPClient.Host := fHostname.Value;
      fTCPClient.Port := fPort.Value;


      fWatchThread  := TNxLogTCPThread.Create(true);
      fWatchThread.Parent := self;
      if fDoUpdateSync then fWatchThread.OnUpdate       := updateCB;
      if fDoUpdateAsync then fWatchThread.OnUpdateAsync := updateAsyncCB;
      fWatchThread.OnFilter       := filterCB;
      fWatchThread.TCPClient      := fTCPClient;
      fWatchThread.LogFormater    := fLogFormater;
      fWatchThread.Start;
    end else
    begin
      fWatchThread.Terminate;
      FreeAndNil(fWatchThread);

      if fTCPClient.Connected then
      begin
        fTCPClient.Disconnect;
      end;
      FreeAndNil(fTCPClient);
    end;
  end;
end;

function  TNxLogTCP.getHostname : String;
begin
  result  := fHostname.Value;
end;

procedure TNxLogTCP.setPort(aValue : Integer);
var ch  : String;
begin
  if fPort.Value <> aValue then
  begin
    fPort.Value := aValue;
    ch  := self.getHostname;
    if ch <> '' then
    begin
      self.setHostname('');
      self.setHostname(ch);
    end;
  end;
end;

function  TNxLogTCP.getPort : Integer;
begin
  result  := fPort.Value;
end;

procedure TNxLogTCP.setUsername(aValue : String);
var ch  : String;
begin
  if fUsername.Value <> aValue then
  begin
    fUsername.Value := aValue;
    ch  := self.getHostname;
    if ch <> '' then
    begin
      self.setHostname('');
      self.setHostname(ch);
    end;
  end;
end;

function  TNxLogTCP.getUsername : String;
begin
  result  := fUsername.Value;
end;

procedure TNxLogTCP.setPassword(aValue : String);
var ch  : String;
begin
  if fPassword.Value <> aValue then
  begin
    fPassword.Value := aValue;
    ch  := self.getHostname;
    if ch <> '' then
    begin
      self.setHostname('');
      self.setHostname(ch);
    end;
  end;
end;

function  TNxLogTCP.getPassword : String;
begin
  result  := fPassword.Value;
end;




end.
