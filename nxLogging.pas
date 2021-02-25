{ ****************************************************************************
 ***** Dateiname               : nxLogging.pas ******************************
 ***** Autor                   : Navimatix GmbH, Matthias Heunecke **********
 ***** Erstellt am             : 08.07.2012 *********************************
 ***** Letzte Änderung         : 08.10.2019 *********************************
 ***** Zugehörigkeit           : nxLogging **********************************
 ***** Beschreibung            : Enthält Klassen, Komponenten, Funktionen ***
 ******************************* für das Logging von Hinweisen, Fehlern,...**
 ****************************************************************************
 ***** Gruppe                  : Freeware ***********************************
 ******************************* Freie Nutzung privat und kommerziell *******
 ****************************************************************************
 ***** Version                 : 1.4.3.1 ************************************
 ****************************************************************************
 ***** Status                  : aktiv **************************************
 ****************************************************************************
 ***** URL                     : http://www.navimatix.de ********************
 ***** URL                     : http://www.navimatix.de/loesungen/log-server
 ***** Email                   : info@navimatix.de **************************
 ***** Adresse                 : Navimatix GmbH, Moritz-von-Rohr-Str. 1a, ***
 *****                           07745 Jena, Germany ************************
 ****************************************************************************
 ***** Tutorial:  http://www.navimatix.de/loesungen/log-server/ *************
 *****            logging-fuer-delphi-nxlogging/tutorial/ *******************
 **************************************************************************** }

{$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
  {$DEFINE _WASLEGACYIFDEFSYNTAX}
{$IFEND}

unit nxLogging;

interface

uses
  {$IF Defined(ANDROID) or Defined(IOS)}
  System.Generics.Collections,
  {$ELSE}

  {$IFEND}
  {$IF CompilerVersion >= 24.0 }
    System.Classes, System.Types, System.SyncObjs, System.SysUtils,
  {$ELSE}
    Classes, Types, SyncObjs, SysUtils,
  {$IFEND}

  IdTCPClient, IdThreadSafe;

const


  /// <summary>
  /// <para>Diese Konstante gibt die maximale Anzahl der zwischenzuspeichernden (auf Clientseite) Logeinträge an.</para>
  /// <para>Beim Überlauf dieses Puffers gehen die weiteren Logeinträge verloren.</para>
  /// </summary>
  IC_DEFAULTMAXTCPENTRIES           = 5000;   // Maximale Anzahl an Logeinträgen

                                              // für den TCP-Puffer
  /// <summary>
  /// <para>Gibt den Namensteil für eine Logdatei im Modus "NXLFS_SINGLEFILE" an.</para>
  /// </summary>
  SC_ALLFILETAIL                    = 'all';  // Additional Name-Part for single logfiles


  /// <summary>
  /// <para>Gibt den Namensteil der aktuellen Logdatei im Modus "NXLFS_RENAME" an.</para>
  /// </summary>
  SC_CURRENTFILETAIL                = 'current';  // Additional Name-Part for a current logfile


  /// <summary>
  /// <para>Gibt den Namensteil der bereits umbenannten Logdateien im Modus "NXLFS_RENAME" an.</para>
  /// </summary>
  SC_FILEFROM                       = 'from'; // Additional Name-Part for renamed logfiles


  // Konstanten für nxLogProtokoll-TCP...
  NXLOGPTCP_OK                            = 200;
  NXLOGPTCP_BADREQUEST                    = 400;
  NXLOGPTCP_UNAUTHORIZED                  = 401;
  NXLOGPTCP_PAYMENTREQUIRED               = 402;
  NXLOGPTCP_FORBIDDEN                     = 403;
  NXLOGPTCP_NOTFOUND                      = 404;
  NXLOGPTCP_METHODNOTALLOWED              = 405;
  NXLOGPTCP_INTERNALSERVERERROR           = 500;
  NXLOGPTCP_NOTIMPLEMENTED                = 501;
  NXLOGPTCP_SERVICETEMPORARYUNAVAILABLE   = 503;

  SC_WRONGSTRINGLENGTHSHOULDBEEVEN         =
      'the string to convert is of wrong length, should be even.';

type

  //{$IFDEF ANDROID}
  {$IF Defined(ANDROID) or Defined(IOS)}
  {$IF CompilerVersion < 34.0 }
  TFMThreadSafeList = TIdThreadSafeList<TObject>;
  TFMList = TList<TObject>;
  {$ELSE}
  // from DX10.4 on
  TFMThreadSafeList = TIdThreadSafeList;
  TFMList = TList;
  {$IFEND}
  {$ELSE}
  TFMThreadSafeList = TIdThreadSafeList;
  TFMList = TList;
  {$IFEND}


  ///  <summary>
  ///  <para>
  ///  Die Werte dieser Enumeration definieren für einen Logentry die Kategorie, zu dieser gehört.
  ///  Die Kategorie kann optional bei den Methoden zum Loggen angegeben werden, default ist NONE.
  ///  </para>
  ///  </summary>
  TNxLoggerCategory = (   NXLCAT_NONE,
                          NXLCAT_OPTIONS,      // Fehler bei den Einstellungen
                          NXLCAT_RUNNING,      // Fehler während des "normalen" Laufens
                          NXLCAT_CALCULATING,  // Fehler während eine komplexen Rechenoperation.
                          NXLCAT_FASTRUNNING,  // Fehler während des Laufens, der jedoch extrem oft, oder ständig auftauchen kann.
                          NXLCAT_STARTUP,      // Fehler beim Erzeugen von Dingen
                          NXLCAT_SHUTDOWN,     // Fehler beim Aufräumen
                          NXLCAT_REFERENCE,    // ungültige, falsche Objektreferenz
                          NXLCAT_COM,          // Fehler im COM-Server
                          NXLCAT_DCOM,         // Fehler im DCOM-Server
                          NXLCAT_PLUGIN        // Fehler in einem Plugin
                      );


  ///  <summary>
  ///  <para>
  ///  Die Werte dieser Enumeration definieren für einen Logentry das Level, zu dem dieser gehört.
  ///  Das Level ist nicht optional und muss immer mit angegeben werden.
  ///  Die Methoden trace(), info(), error(), etc... geben das entsprechende Level bereits vor.
  ///  </para>
  ///  </summary>
  TNxLoggerLevel    = (   NXLL_TRACE,       // ganz zartes Fehlerchen.. :-)
                          NXLL_DEBUG,       // Fehlerchen zum/beim Debuggen
                          NXLL_INFO,        // Information über Unregelmäßigkeit
                          NXLL_WARN,        // Warnung, es ist etwas unschönes passiert, aber keine Panik..
                          NXLL_ERROR,       // kritischer Fehler, kann Absturz zur Folge haben
                          NXLL_FATAL        // fataler Fehler, eigentlich hat es keinen Zweck mehr, Programm töten !
                      );


  ///  <summary>
  ///  <para>
  ///  Das ist eine Menge von Logleveln, diese wird z.B. in Filtern genutzt.
  ///  </para>
  ///  </summary>
  TNxLoggerLevelSet = set of TNxLoggerLevel;


  ///  <summary>
  ///  <para>
  ///  Diese Klasse kapselt eine Lognachricht. Jede Logausgabe erzeugt eine Instanz dieser Klasse und gibt diese intern an die
  ///  entsprechenden Appender weiter. Hier sind alle wichtigen Informationen zu einer Nachricht enthalten.
  ///  </para>
  ///  </summary>
  /// <remarks>
  /// <para>Eine solche Instanz kann selbst erzeugt werden, oder sie wird automatisch von den Methoden trace(), info(), etc... erzeugt.</para>
  /// <para>Erbt von <c>TPersistent</c> und kann mit der Methode <c>Assign()</c> <b>Inhalte</b> (nicht nur die Referenz) einer anderen Instanz zuweisen.</para>
  /// </remarks>
  TNxLoggerMessage = class(TPersistent)
  private
    fApplicationId      : String;
    fInstanceId         : String;
    fLogUser            : String;
    fThreadId           : String;
    fLevel              : TNxLoggerLevel;
    fCategory           : String;
    fModule             : String;
    fMessage            : String;
    fTimestamp          : TDateTime;
    fException          : Exception;
    fExceptionClassName : String;
    fLanguage           : String;     // ISO 639-2
    fStackTrace         : String;
  protected
    /// <summary>
    /// Diese Methode weist eine Message einer anderen zu. Die Zugewiesene ist eine exakte Kopie des Originals.
    /// </summary>
    /// <param name="Dest"> Definiert die Instanz, der zugewiesen werden soll.</param>
    /// <remarks>Die Methode "Assign() von TPersistent kann dadurch genutzt werden"</remarks>
    procedure AssignTo(Dest: TPersistent); override;

    procedure setApplicationId(aValue : String);
    procedure setInstanceId(aValue : String);
    procedure setLogUser(aValue : String);
    procedure setThreadId(aValue : String);
    procedure setLevel(aValue  : TNxLoggerLevel);
    procedure setCategory(aValue : String);
    procedure setModule(aValue : String);
    procedure setMessage(aValue  : String);
    procedure setTimestamp(aValue  : TDateTime);
    procedure setException(aValue  : Exception);
    procedure setLanguage(aValue : String);
    procedure setExceptionClassName(aValue : String);
    procedure setStackTrace(aValue : String);
  public
    /// <summary>
    /// Konstruktor der Klasse, Parameter werden direkt angegeben.
    /// </summary>
    /// <param name="aApplicationId">Definiert die ID der loggenden Anwendung.</param>
    /// <param name="aInstanceId">Definiert die ID der loggenden Instanz (laufendes Programm, wenn mehrere).</param>
    /// <param name="aLogUser">Definiert die ID Users unter dessen Namen geloggt wird.</param>
    /// <remarks>Die Paremeter sind erste Vorgaben, über die Properties können die Inhalte geändert werden</remarks>
    constructor Create(const aApplicationId, aInstanceId, aLogUser : String; aLevel : TNxLoggerLevel; const aMessage : String); overload; virtual;
    constructor Create(const aApplicationId, aInstanceId, aLogUser : String; aLevel : TNxLoggerLevel; const aModule, aMessage : String; const aCategory, aLanguage : String; const aException : Exception); overload; virtual;
    constructor Create(const aApplicationId, aInstanceId, aLogUser : String; aLevel : TNxLoggerLevel; const aModule, aMessage : String; const aCategory, aLanguage, aThreadId : String; const aException : Exception); overload; virtual;
    destructor Destroy; override;

    function getApplicationId       : String;
    function getInstanceId          : String;
    function getLogUser             : String;
    function getThreadId            : String;
    function getLevel               : TNxLoggerLevel;
    function getCategory            : String;
    function getModule              : String;
    function getMessage             : String;
    function getTimestamp           : TDateTime;
    function getException           : Exception;
    function getLanguage            : String;
    function getExceptionClassName  : String;
    function getStackTrace          : String;

    property ApplicationId          : String read getApplicationId write setApplicationId;
    property InstanceId             : String read getInstanceId write setInstanceId;
    property LogUser                : String read getLogUser write setLogUser;
    property ThreadId               : String read getThreadId write setThreadId;
    property LogLevel               : TNxLoggerLevel read fLevel write setLevel;
    property LogCategory            : String read fCategory write setCategory;
    property LogModule              : String read getModule write setModule;
    property LogMessage             : String read getMessage write setMessage;
    property LogTimestamp           : TDateTime read getTimestamp write setTimestamp;
    property LogException           : Exception read getException write setException;
    property LogExceptionClassName  : String read getExceptionClassName write setExceptionClassName;
    property Language               : String read getLanguage write setLanguage;
    property StackTrace             : String read getStackTrace write setStackTrace;
  end;


  TNxLoggerMessageEvent = procedure(aSender : TObject; aMessage : TNxLoggerMessage) of object;


  ///  <summary>
  ///  <para>
  ///  Das ist die Grundklasse für alle Filter. Weitere Filter müssen von dieser Klasse erben.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Die Methode match() muss überschrieben werden. Hier wird per Rückgabe definiert, ob die übergebene Nachricht zum Filter passt oder nicht.
  ///  </para>
  ///  <para>
  ///  Mit loadFromStream() und saveToStream erhält ein Filter die Möglichkeit persistent zu bleiben, oder übermittelt zu werden.
  ///  </para>
  ///  </remarks>
  TNxLoggerMessageFilter = class(TPersistent)
  public
    procedure reset; virtual; abstract;
    function  match(aMessage : TNxLoggerMessage) : Boolean; virtual; abstract;
    procedure saveToStream(aStream : TStream); virtual; abstract;
    procedure loadFromStream(aStream : TStream); virtual; abstract;
  end;


  ///  <summary>
  ///  <para>
  ///  Dieser Filter entscheidet nach einer Menge von LogLeveln. Ist das Loglevel der übergebenen Nachricht in der Menge enthalten, so
  ///  liefert match() true zurück.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Mit setLevelAndAbove() kann einfach eine untere Schwelle angegeben werden.
  ///  </para>
  ///  <para>
  ///  setLevel(), unsetLevel() und setLevelSet() dienen der Mengenverwaltung.
  ///  </para>
  ///  </remarks>
  TNxLoggerMessageFilterLevelSet = class(TNxLoggerMessageFilter)
  private
    fLevelSet       : TNxLoggerLevelSet;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure saveToStream(aStream : TStream); override;
    procedure loadFromStream(aStream : TStream); override;

    procedure setLevel(aLevel : TNxLoggerLevel);
    procedure unsetLevel(aLevel : TNxLoggerLevel);

    procedure setLevelAndAbove(aLevel : TNxLoggerLevel);

    procedure setLevelSet(aLevelSet : TNxLoggerLevelSet);

    function  isLevelInSet(aLevel : TNxLoggerLevel) : Boolean;

    procedure reset; override;
    function  match(aMessage : TNxLoggerMessage) : Boolean; override;

    property LevelSet : TNxLoggerLevelSet read fLevelSet;
  end;


  ///  <summary>
  ///  <para>
  ///  Dieser Filter ist eine Grundklasse für alle, welche nach einem gegebenen Text entscheidet
  ///  Die Grundklasse hält lediglich den zu testenden Text als Attribut und Property.
  ///  Die abgeleiteten Klassen müssen die Methode match() überschreiben.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  setText(), getText() und reset() dienen der Texthandhabung.
  ///  </para>
  ///  </remarks>
  TNxLoggerMessageFilterByText = class(TNxLoggerMessageFilter)
  private
    fText     : String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure saveToStream(aStream : TStream); override;
    procedure loadFromStream(aStream : TStream); override;

    procedure setText(aValue : String);
    function  getText : String;

    procedure reset; override;

    property FilterText : String read fText write fText;
  end;


  ///  <summary>
  ///  <para>
  ///  Dieser Filter ist ein Textfilter, er entscheidet nach TNxLoggerMessage.LogModule.
  ///  Dabei muss der Text darin gleich zum Filtertext sein.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Testeigenschaft der Nachricht ist: <c>TNxLoggerMessage.LogModule</c>
  ///  </para>
  ///  <para>
  ///  Der Textvergleich ist nicht case sensitive, Groß-Kleinschreibung ist egal.
  ///  </para>
  ///  </remarks>
  TNxLoggerMessageFilterModuleEquals = class(TNxLoggerMessageFilterByText)
  public
    function  match(aMessage : TNxLoggerMessage) : Boolean; override;
  end;


  ///  <summary>
  ///  <para>
  ///  Dieser Filter ist ein Textfilter, er entscheidet nach TNxLoggerMessage.LogModule.
  ///  Dabei muss der Text darin mit dem Filtertext anfangen.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Testeigenschaft der Nachricht ist: <c>TNxLoggerMessage.LogModule</c>
  ///  </para>
  ///  <para>
  ///  Der Textvergleich ist nicht case sensitive, Groß-Kleinschreibung ist egal.
  ///  </para>
  ///  </remarks>
  TNxLoggerMessageFilterModuleStarting = class(TNxLoggerMessageFilterByText)
  public
    function  match(aMessage : TNxLoggerMessage) : Boolean; override;
  end;


  ///  <summary>
  ///  <para>
  ///  Dieser Filter ist ein Textfilter, er entscheidet nach TNxLoggerMessage.LogModule.
  ///  Dabei muss der Filtertext irgendwo im LogModule enthalten sein.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Testeigenschaft der Nachricht ist: <c>TNxLoggerMessage.LogModule</c>
  ///  </para>
  ///  <para>
  ///  Der Textvergleich ist nicht case sensitive, Groß-Kleinschreibung ist egal.
  ///  </para>
  ///  </remarks>
  TNxLoggerMessageFilterModuleContains = class(TNxLoggerMessageFilterByText)
  public
    function  match(aMessage : TNxLoggerMessage) : Boolean; override;
  end;


  ///  <summary>
  ///  <para>
  ///  Dieser Filter ist ein Textfilter, er entscheidet nach TNxLoggerMessage.LogMessage.
  ///  Dabei muss der Text darin gleich zum Filtertext sein.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Testeigenschaft der Nachricht ist: <c>TNxLoggerMessage.LogMessage</c>
  ///  </para>
  ///  <para>
  ///  Der Textvergleich ist nicht case sensitive, Groß-Kleinschreibung ist egal.
  ///  </para>
  ///  </remarks>
  TNxLoggerMessageFilterMessageEquals = class(TNxLoggerMessageFilterByText)
  public
    function  match(aMessage : TNxLoggerMessage) : Boolean; override;
  end;


  ///  <summary>
  ///  <para>
  ///  Dieser Filter ist ein Textfilter, er entscheidet nach TNxLoggerMessage.LogMessage.
  ///  Dabei muss der Text darin mit dem Filtertext anfangen.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Testeigenschaft der Nachricht ist: <c>TNxLoggerMessage.LogMessage</c>
  ///  </para>
  ///  <para>
  ///  Der Textvergleich ist nicht case sensitive, Groß-Kleinschreibung ist egal.
  ///  </para>
  ///  </remarks>
  TNxLoggerMessageFilterMessageStarting = class(TNxLoggerMessageFilterByText)
  public
    function  match(aMessage : TNxLoggerMessage) : Boolean; override;
  end;


  ///  <summary>
  ///  <para>
  ///  Dieser Filter ist ein Textfilter, er entscheidet nach TNxLoggerMessage.LogMessage.
  ///  Dabei muss der Filtertext irgendwo im LogModule enthalten sein.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Testeigenschaft der Nachricht ist: <c>TNxLoggerMessage.LogMessage</c>
  ///  </para>
  ///  <para>
  ///  Der Textvergleich ist nicht case sensitive, Groß-Kleinschreibung ist egal.
  ///  </para>
  ///  </remarks>
  TNxLoggerMessageFilterMessageContains = class(TNxLoggerMessageFilterByText)
  public
    function  match(aMessage : TNxLoggerMessage) : Boolean; override;
  end;


  ///  <summary>
  ///  <para>
  ///  Dieser Filter ist ein Textfilter, er wird nicht direkt genutzt, ist die
  ///  Basisklasse für alle Filter mit MachineIdent.
  ///  Dabei muss der Text darin gleich zum Filtertext sein.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Testeigenschaft der Nachricht ist: <c>TNxLoggerMessage.MachineIdent</c>
  ///  </para>
  ///  <para>
  ///  Der Textvergleich ist nicht case sensitive, Groß-Kleinschreibung ist egal.
  ///  </para>
  ///  </remarks>

  TNxLoggerMessageFilterMachine = class;
  TNxLoggerMessageFilterMachineMIREvent = procedure(Sender : TNxLoggerMessageFilterMachine; LogMessage : TNxLoggerMessage; var MachineIdent : String) of object;

  TNxLoggerMessageFilterMachine = class(TNxLoggerMessageFilterByText)
  protected
    fOnMachineIdentRequired       : TNxLoggerMessageFilterMachineMIREvent;

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;

    function  matchMachineIdent(aMessage : TNxLoggerMessage; const aMachineIdent : String) : Boolean; virtual; abstract;

    property OnMachineIdentRequired       : TNxLoggerMessageFilterMachineMIREvent read fOnMachineIdentRequired write fOnMachineIdentRequired;
  end;

  ///  <summary>
  ///  <para>
  ///  Dieser Filter ist ein Textfilter, er entscheidet nach TNxLoggerMessage.MachineIdent.
  ///  Dabei muss der Text darin gleich zum Filtertext sein.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Testeigenschaft der Nachricht ist: <c>TNxLoggerMessage.MachineIdent</c>
  ///  </para>
  ///  <para>
  ///  Der Textvergleich ist nicht case sensitive, Groß-Kleinschreibung ist egal.
  ///  </para>
  ///  </remarks>

  TNxLoggerMessageFilterMachineEquals = class(TNxLoggerMessageFilterMachine)
  public
    function  matchMachineIdent(aMessage : TNxLoggerMessage; const aMachineIdent : String) : Boolean; override;
    function  match(aMessage : TNxLoggerMessage) : Boolean; override;
  end;

  ///  <summary>
  ///  <para>
  ///  Dieser Filter ist ein Textfilter, er entscheidet nach TNxLoggerMessage.MachineIdent (startet mit).
  ///  Dabei muss der Text darin mit Filtertext anfangen.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Testeigenschaft der Nachricht ist: <c>TNxLoggerMessage.MachineIdent</c>
  ///  </para>
  ///  <para>
  ///  Der Textvergleich ist nicht case sensitive, Groß-Kleinschreibung ist egal.
  ///  </para>
  ///  </remarks>

  TNxLoggerMessageFilterMachineStarting = class(TNxLoggerMessageFilterMachine)
  public
    function  matchMachineIdent(aMessage : TNxLoggerMessage; const aMachineIdent : String) : Boolean; override;
    function  match(aMessage : TNxLoggerMessage) : Boolean; override;
  end;

  ///  <summary>
  ///  <para>
  ///  Dieser Filter ist ein Textfilter, er entscheidet nach TNxLoggerMessage.MachineIdent (beinhaltet).
  ///  Dabei muss der Filtertext in dem MachineIdent der Nachricht enthalten sein.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Testeigenschaft der Nachricht ist: <c>TNxLoggerMessage.MachineIdent</c>
  ///  </para>
  ///  <para>
  ///  Der Textvergleich ist nicht case sensitive, Groß-Kleinschreibung ist egal.
  ///  </para>
  ///  </remarks>

  TNxLoggerMessageFilterMachineContains = class(TNxLoggerMessageFilterMachine)
  public
    function  matchMachineIdent(aMessage : TNxLoggerMessage; const aMachineIdent : String) : Boolean; override;
    function  match(aMessage : TNxLoggerMessage) : Boolean; override;
  end;

  ///  <summary>
  ///  <para>
  ///  Dieser Filter ist ein Textfilter, er entscheidet nach TNxLoggerMessage.ApplicationId.
  ///  Dabei muss der Text darin gleich zum Filtertext sein.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Testeigenschaft der Nachricht ist: <c>TNxLoggerMessage.ApplicationId</c>
  ///  </para>
  ///  <para>
  ///  Der Textvergleich ist nicht case sensitive, Groß-Kleinschreibung ist egal.
  ///  </para>
  ///  </remarks>
  TNxLoggerMessageFilterApplicationEquals = class(TNxLoggerMessageFilterByText)
  public
    function  match(aMessage : TNxLoggerMessage) : Boolean; override;
  end;

  ///  <summary>
  ///  <para>
  ///  Dieser Filter ist ein Textfilter, er entscheidet nach TNxLoggerMessage.ApplicationId.
  ///  Dabei muss der Text darin mit Filtertext starten.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Testeigenschaft der Nachricht ist: <c>TNxLoggerMessage.ApplicationId</c>
  ///  </para>
  ///  <para>
  ///  Der Textvergleich ist nicht case sensitive, Groß-Kleinschreibung ist egal.
  ///  </para>
  ///  </remarks>
  TNxLoggerMessageFilterApplicationStarting = class(TNxLoggerMessageFilterByText)
  public
    function  match(aMessage : TNxLoggerMessage) : Boolean; override;
  end;

  ///  <summary>
  ///  <para>
  ///  Dieser Filter ist ein Textfilter, er entscheidet nach TNxLoggerMessage.ApplicationId.
  ///  Dabei muss der Filtertext in ApplicationId der Message enthalten sein.
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Testeigenschaft der Nachricht ist: <c>TNxLoggerMessage.ApplicationId</c>
  ///  </para>
  ///  <para>
  ///  Der Textvergleich ist nicht case sensitive, Groß-Kleinschreibung ist egal.
  ///  </para>
  ///  </remarks>
  TNxLoggerMessageFilterApplicationContains = class(TNxLoggerMessageFilterByText)
  public
    function  match(aMessage : TNxLoggerMessage) : Boolean; override;
  end;


  ///  <summary>
  ///  <para>
  ///  Das ist die Grundklasse aller LogFormater. Diese haben die Aufgabe, eine
  ///  Lognachricht nach bestimmten Kriterien zu formatieren. Entweder eine Lognachricht in
  ///  einen String (formatMessage()), oder einen String zurück in eine Lognachricht (parseMessage()).
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Die Klassenmethoden "convert***()" dienen der Umwandlung von Level, bzw. Kategorie in Strings und zurück.
  ///  </para>
  ///  </remarks>
  TNxLogFormater = class(TPersistent)
  public
    function  formatMessage(aMessage : TNxLoggerMessage) : String; virtual; abstract;
    function  parseMessage(aMessageText : String) : TNxLoggerMessage; virtual; abstract;
    function  formatMessageBase64(aMessage : TNxLoggerMessage) : String; virtual; abstract;
    function  parseMessageBase64(aMessageText : String) : TNxLoggerMessage; virtual; abstract;

    class function isLevelString(aString : String) : Boolean;
    class function convertToLevel(aString : String; aIgnoreIllegalStrings : Boolean = true) : TNxLoggerLevel;
    class function convertLevelToString(const aLevel : TNxLoggerLevel; const isException : Boolean = false) : String;
    class function convertToCategory(aString : String) : TNxLoggerCategory;
    class function convertCategoryToString(const aCategory : TNxLoggerCategory) : String;
  end;

  ///  <summary>
  ///  <para>
  ///  Dieser LogFormater dient als Grundlage für unbekannte Log-Formate.
  ///  Die einzelnen Werte einer Lognachricht werden eventuell mit "|" getrennt,
  ///  werden aber nicht danach getrennt.
  ///  die Lognachrichten selbst müssen mit CR LF getrennt sein.
  ///  </para>
  ///  <para>
  ///  Format: Die ganze Zeile geht in "Message"
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  LogFormater haben die Aufgabe, eine Lognachricht nach bestimmten Kriterien zu formatieren.
  ///  Entweder eine Lognachricht in einen String (formatMessage()), oder einen String zurück in eine Lognachricht (parseMessage()).
  ///  </para>
  ///  <para>
  ///  Es existieren jeweils Varianten zur Base64-kodierung der Teilwerte, ACHTUNG!
  ///  Hier wird nicht der ganze String kodiert, nur die Teile applicationid, module und message.
  ///  </para>
  ///  </remarks>
  TNxLogFormaterPlain = class(TNxLogFormater)
  private
    fFormatSettings   : TFormatSettings;
  public
    constructor Create;
    destructor Destroy; override;

    function  formatMessage(aMessage : TNxLoggerMessage) : String; override;
    function  formatMessageBase64(aMessage : TNxLoggerMessage) : String; override;
    function  parseMessage(aMessageText : String) : TNxLoggerMessage; override;
    function  parseMessageBase64(aMessageText : String) : TNxLoggerMessage; override;
  end;


  ///  <summary>
  ///  <para>
  ///  Dieser LogFormater dient als Standard, z.B. für Dateien.
  ///  Die einzelnen Werte einer Lognachricht werden jeweils mit "|" getrennt,
  ///  die Lognachrichten selbst mit CR LF.
  ///  </para>
  ///  <para>
  ///  Format: applicationid|timestamp|level.module|messagetext
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  LogFormater haben die Aufgabe, eine Lognachricht nach bestimmten Kriterien zu formatieren.
  ///  Entweder eine Lognachricht in einen String (formatMessage()), oder einen String zurück in eine Lognachricht (parseMessage()).
  ///  </para>
  ///  <para>
  ///  Es existieren jeweils Varianten zur Base64-kodierung der Teilwerte, ACHTUNG!
  ///  Hier wird nicht der ganze String kodiert, nur die Teile applicationid, module und message.
  ///  </para>
  ///  </remarks>
  TNxLogFormaterDefault = class(TNxLogFormater)
  private
    fFormatSettings   : TFormatSettings;
  public
    constructor Create;
    destructor Destroy; override;

    function  formatMessage(aMessage : TNxLoggerMessage) : String; override;
    function  formatMessageBase64(aMessage : TNxLoggerMessage) : String; override;
    function  parseMessage(aMessageText : String) : TNxLoggerMessage; override;
    function  parseMessageBase64(aMessageText : String) : TNxLoggerMessage; override;
  end;


  ///  <summary>
  ///  <para>
  ///  Dieser LogFormater dient zur Übermittlung von Lognachrichten per Netzwerk.
  ///  Die einzelnen Werte einer Lognachricht werden jeweils mit "|" getrennt,
  ///  die Lognachrichten selbst mit CR LF.
  ///  </para>
  ///  <para>
  ///  Dieser Formater führt einen Wert "MachineIdent" neu ein, dieser soll einen
  ///  eindeutigen Wert beinhalten, welcher den Logclient identifiziert, z.B. der Rechnername.
  ///  Da hier die Lognachrichten per Netzwerk übertragen werden, ist es wichtig von wo diese kommen.
  ///  </para>
  ///  <para>
  ///  Format: applicationid|timestamp|level.module|messagetext
  ///  </para>
  ///  </summary>
  ///  <remarks>
  ///  <para>
  ///  Es existieren jeweils Varianten zur Base64-kodierung der Teilwerte, ACHTUNG!
  ///  Hier wird nicht der ganze String kodiert, nur die Teile applicationid, module und message.
  ///  </para>
  ///  <para>
  ///  Bei den format***()-Methoden wird der Wert "MachineIdent" mit in den String kodiert, da dieser beim Parsen auch
  ///  verfügbar sein muss, gibt es die beiden zusätzlichen parse***()-Methoden mir den Rückgabeparametern "ResMachineIdent".
  ///  Beim Aufruf wird hier der im String kodierte MachineIdent-Wert übergeben.
  ///  </para>
  ///  <para>
  ///  LogFormater haben die Aufgabe, eine Lognachricht nach bestimmten Kriterien zu formatieren.
  ///  Entweder eine Lognachricht in einen String (formatMessage()), oder einen String zurück in eine Lognachricht (parseMessage()).
  ///  </para>
  ///  </remarks>
  TNxLogFormaterTCP = class(TNxLogFormater)
  private
    fFormatSettings   : TFormatSettings;
    fMachineIdent     : String;
  public
    constructor Create;
    destructor Destroy; override;

    function  formatMessage(aMessage : TNxLoggerMessage) : String; override;
    function  formatMessageBase64(aMessage : TNxLoggerMessage) : String; override;
    function  formatMessageExtra(aMessage : TNxLoggerMessage; aExtraDataId : Int64; aExtraData : TBytes) : String;
    function  formatMessageExtraBase64(aMessage : TNxLoggerMessage; aExtraDataId : Int64; aExtraData : TBytes) : String;
    function  parseMessage(aMessageText : String) : TNxLoggerMessage; override;
    function  parseMessageBase64(aMessageText : String) : TNxLoggerMessage; override;
    function  parseMessageMachine(aMessageText : String; var ResMachineIdent : String) : TNxLoggerMessage;
    function  parseMessageMachineBase64(aMessageText : String; var ResMachineIdent : String) : TNxLoggerMessage;
    function  parseMessageMachineExtra(aMessageText : String; var ResMachineIdent : String; var ResExtraID : Int64; var ResExtra : TBytes) : TNxLoggerMessage;
    function  parseMessageMachineExtraBase64(aMessageText : String; var ResMachineIdent : String; var ResExtraID : Int64; var ResExtra : TBytes) : TNxLoggerMessage;

    property MachineIdent     : String read fMachineIdent write fMachineIdent;
  end;

  TNxLogAppender = class(TComponent)
  private

  public
    procedure append(const aEvent : TNxLoggerMessage); virtual; abstract;
  end;

  TNxLogAppenderFileStrategy = (NXLFS_SINGLEFILE, NXLFS_NEWFILES, NXLFS_RENAME);

  TNxLogAppenderFile = class(TNxLogAppender)
  private
    fFormater       : TNxLogFormater;
    fFormatSettings : TFormatSettings;
    fDirectory      : String;
    fFilenameBase   : String;
    fRetryCount     : Integer;
    fStrategy       : TNxLogAppenderFileStrategy;
    fLastDate       : TDate;
  protected
    function  isNextDay : Boolean;

    procedure writeString(const aToStream : TStream; const aString : String);

    procedure appendSingleFile(const aEvent : TNxLoggerMessage); virtual;
    procedure appendNewFiles(const aEvent : TNxLoggerMessage); virtual;
    procedure appendRename(const aEvent : TNxLoggerMessage); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure append(const aEvent : TNxLoggerMessage); override;

    property Strategy : TNxLogAppenderFileStrategy read fStrategy write fStrategy;
  end;

  TNxLogger = class;

  TNxLogAppenderChain = class(TNxLogAppender)
  private
    fChainedLogger    : TNxLogger;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure append(const aEvent : TNxLoggerMessage); override;

    property ChainedLogger : TNxLogger read fChainedLogger write fChainedLogger;
  end;

  INxLogAppenderTCPThreadAdapter = interface
  ['{0B98E7CF-DA90-4FC2-A530-F5516DF0CCA7}']
    function  consumeEntry : String;

  end;

  TNxLogAppenderTCPThread = class(TThread)
  private
    fTCP                : TIdTCPClient;
    fFormatSettings     : TFormatSettings;
    fMachineIdent       : String;
    fHostOrIP           : String;
    fPort               : Integer;
    fRetryCount         : Integer;
    fUsername           : String;
    fPassword           : String;
    fAdapter            : INxLogAppenderTCPThreadAdapter;
    fDisconnectSeconds  : Int64;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;

    property FormatSettings : TFormatSettings read fFormatSettings write fFormatSettings;
    property MachineIdent   : String read fMachineIdent write fMachineIdent;
    property HostOrIP       : String read fHostOrIP write fHostOrIP;
    property Port           : Integer read fPort write fPort;
    property RetryCount     : Integer read fRetryCount write fRetryCount;
    property Username       : String read fUsername write fUsername;
    property Password       : String read fPassword write fPassword;
    property Adapter        : INxLogAppenderTCPThreadAdapter read fAdapter write fAdapter;
    property DisconnectSeconds  : Int64 read fDisconnectSeconds write fDisconnectSeconds;
  end;

  TNxLogAppenderOverflowEvent = procedure(aMaxCount : Integer) of object;

  TNxLogAppenderTCP = class(TNxLogAppender, INxLogAppenderTCPThreadAdapter)
  private
    fFormatSettings : TFormatSettings;
    fMachineIdent   : String;
    fHostOrIP       : String;
    fPort           : Integer;
    fRetryCount     : Integer;
    fUsername       : String;
    fPassword       : String;
    fMaxEntries     : TIdThreadSafeInteger;

    fLevelFilter    : TNxLoggerMessageFilterLevelSet;
    fUserFilter     : TNxLoggerMessageFilter;
    fFormater       : TNxLogFormaterTCP;
    fBuffer         : TIdThreadSafeStringList;
    fWorker         : TNxLogAppenderTCPThread;
    fDisconnectSeconds  : Int64;

    fOnOverflow     : TNxLogAppenderOverflowEvent;

    procedure setMachineIdent(aIdent : String);
    procedure setMaxEntries(aValue : Integer);
    function  getMaxEntries : Integer;
    function  getCurrentEntries : Integer;
  protected
    function  consumeEntry : String; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  isStarted : Boolean;
    procedure start; virtual;
    procedure stop; virtual;

    procedure clearBuffer; virtual;
    procedure append(const aEvent : TNxLoggerMessage); override;

    property LevelFilter    : TNxLoggerMessageFilterLevelSet read fLevelFilter;
    property UserFilter     : TNxLoggerMessageFilter read fUserFilter write fUserFilter;
    property MaxEntries     : Integer read getMaxEntries write setMaxEntries;
    property CurrentEntries : Integer read getCurrentEntries;

    property DisconnectSeconds  : Int64 read fDisconnectSeconds write fDisconnectSeconds;
    property MachineIdent   : String read fMachineIdent write setMachineIdent;

    property OnOverflow     : TNxLogAppenderOverflowEvent read fOnOverflow write fOnOverflow;
  end;

  TNxLoggerCollectionItem = class(TCollectionItem)
  private
    fAppender   : TNxLogAppender;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure append(const aEvent : TNxLoggerMessage);
  published
    property Appender   : TNxLogAppender read fAppender write fAppender;
  end;

  TNxLoggerCollection = class(TCollection)
  private

  protected

  public

  published

  end;

  TNxLoggerLogEvent = procedure(const aLevel : TNxLoggerLevel; const aModule, aMessage : String; aCategory : TNxLoggerCategory; aException : Exception; aLanguage : String) of object;

  INxLogger = interface
  ['{95CD2F15-2680-4740-9EC2-DEBDC79021DC}']
    procedure log(const aEvent : TNxLoggerMessage); overload;
    procedure log(const aLevel : TNxLoggerLevel; const aMessage : String); overload;
    procedure log(const aLevel : TNxLoggerLevel; const aModule, aMessage : String); overload;
    procedure log(const aLevel : TNxLoggerLevel; const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception); overload;
    procedure log(const aLevel : TNxLoggerLevel; const aModule, aMessage : String; const aCategoryAsString : String; const aException : exception); overload;
    procedure log(const aLevel : TNxLoggerLevel; const aModule, aMessage : String; const aCategoryAsString, aLanguage : String; const aException : exception); overload;
    procedure fatal(const aMessage : String); overload;
    procedure fatal(const aModule, aMessage : String); overload;
    procedure fatal(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil); overload;
    procedure error(const aMessage : String); overload;
    procedure error(const aModule, aMessage : String); overload;
    procedure error(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil); overload;
    procedure warn(const aMessage : String); overload;
    procedure warn(const aModule, aMessage : String); overload;
    procedure warn(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil); overload;
    procedure info(const aMessage : String); overload;
    procedure info(const aModule, aMessage : String); overload;
    procedure info(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil); overload;
    procedure debug(const aMessage : String); overload;
    procedure debug(const aModule, aMessage : String); overload;
    procedure debug(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil); overload;
    procedure trace(const aMessage : String); overload;
    procedure trace(const aModule, aMessage : String); overload;
    procedure trace(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil); overload;

    function  isTrace : Boolean;
    function  isDebug : Boolean;
    function  isInfo  : Boolean;
    function  isWarn  : Boolean;
    function  isError : Boolean;
    function  isFatal : Boolean;
  end;

  TNxLogger = class(TComponent, INxLogger)
  private
    fCSLog                : TCriticalSection;
    fLogFormatSettings    : TFormatSettings;
    fApplicationID        : String;
    fInstanceID           : String;
    fUserIdent            : String;
    fLanguage             : String;
    fCurrentLevel         : TNxLoggerLevel;
    fCSCurrentLevel       : TCriticalSection;
    fAppenders            : TNxLoggerCollection;
    fFilters              : TFMThreadSafeList;

    fOnAppend             : TNxLoggerMessageEvent;
    fOnLog                : TNxLoggerMessageEvent;

    function  getAppenderCount : Integer;
    function  getAppender(aIndex : Integer) : TNxLogAppender;

    function  getCurrentLevel : TNxLoggerLevel;
    procedure setCurrentLevel(aLevel : TNxLoggerLevel);
  protected
    function  matchFilters(const aEvent : TNxLoggerMessage) : Boolean; virtual;
    function  getMachineIdent : String; virtual;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;

    class function isImportantLogLevel(const aRelativeBaseLevel, aLevelToTest : TNxLoggerLevel) : Boolean;

    procedure initializeFileLogging(const aApplicationId, aLogDirectory, aBaseFilename : String; aRetryCount : Integer = 10; aStrategy : TNxLogAppenderFileStrategy = NXLFS_NEWFILES);
    procedure initializeServerTCPLogging(const aApplicationId, aHostOrIP : String; aPort : Integer; aMachineIdent : String = ''; aRetryCount : Integer = 10; aUsername : String = ''; aPassword : String = ''; aLevelFilter : TNxLoggerLevelSet = [NXLL_FATAL, NXLL_ERROR, NXLL_WARN]; aAutoDisconnectSeconds : Int64 = 0);
    class procedure doneLogging;

    procedure addAppender(aAppender : TNxLogAppender);
    function  deleteAppender(aIndex : Integer) : TNxLogAppender;
    procedure clearAppenders;

    procedure log(const aEvent : TNxLoggerMessage); overload;
    procedure log(const aLevel : TNxLoggerLevel; const aMessage : String); overload;
    procedure log(const aLevel : TNxLoggerLevel; const aModule, aMessage : String); overload;
    procedure log(const aLevel : TNxLoggerLevel; const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception); overload;
    procedure log(const aLevel : TNxLoggerLevel; const aModule, aMessage : String; const aCategoryAsString : String; const aException : exception); overload;
    procedure log(const aLevel : TNxLoggerLevel; const aModule, aMessage : String; const aCategoryAsString, aLanguage : String; const aException : exception); overload;
    procedure fatal(const aMessage : String); overload;
    procedure fatal(const aModule, aMessage : String); overload;
    procedure fatal(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil); overload;
    procedure error(const aMessage : String); overload;
    procedure error(const aModule, aMessage : String); overload;
    procedure error(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil); overload;
    procedure warn(const aMessage : String); overload;
    procedure warn(const aModule, aMessage : String); overload;
    procedure warn(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil); overload;
    procedure info(const aMessage : String); overload;
    procedure info(const aModule, aMessage : String); overload;
    procedure info(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil); overload;
    procedure debug(const aMessage : String); overload;
    procedure debug(const aModule, aMessage : String); overload;
    procedure debug(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil); overload;
    procedure trace(const aMessage : String); overload;
    procedure trace(const aModule, aMessage : String); overload;
    procedure trace(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil); overload;

    function  isTrace : Boolean;
    function  isDebug : Boolean;
    function  isInfo  : Boolean;
    function  isWarn  : Boolean;
    function  isError : Boolean;
    function  isFatal : Boolean;

    property AppenderCount  : Integer read getAppenderCount;
    property Appenders[index : Integer] : TNxLogAppender read getAppender;
    property CurrentLevel         : TNxLoggerLevel read getCurrentLevel write setCurrentLevel;
    property MachineIdent   : String read getMachineIdent;

    property OnAppend             : TNxLoggerMessageEvent read fOnAppend write fOnAppend;
    property OnLog                : TNxLoggerMessageEvent read fOnLog write fOnLog;
  end;

  ENxLoggingConvertError  = class(Exception)

  end;

  // ***** Funktionen und Prozeduren...
  function  NowUTC: TDateTime;
  procedure initLoggerFormatSettings(var FormatSettings : TFormatSettings);

  function Logger : TNxLogger;

  function isIPv4 : Boolean;


implementation

uses

{$IFDEF MSWINDOWS}
windows,
{$ELSE}
Posix.Stdio,
{$ENDIF}

{$IF CompilerVersion >= 24.0 }
  System.TypInfo, System.Math, System.DateUtils,
{$ELSE}
  TypInfo, Math, DateUtils,
{$IFEND}

  IdHashMessageDigest, IdGlobal, IdCoderMIME, IdStack;

var

  fDefaultLogger : TNxLogger;


{ **************************************************************************** }
{ ***** allgemeine Funktionen ************************************************ }
{ **************************************************************************** }

function NowUTC: TDateTime;
begin
  result := TTimeZone.Local.ToUniversalTime(Now);
end;

{$IFDEF MSWINDOWS}
function internalGetCurrentProcessId : String;
begin
  result := IntToStr(Windows.GetCurrentProcessId);
end;

function internalGetCurrentThreadId : String;
begin
  result  := IntToStr(Windows.GetCurrentThreadId);
  //result  := IntToStr(TThread.Current.ThreadID);
end;
{$ELSE}
function internalGetCurrentProcessId : String;
begin
  //result  := Application.Title;
  result := '1';
end;

function internalGetCurrentThreadId : String;
begin
  //result  := IntToStr(TThread.Current.ThreadID);
  result  := '1';
end;
{$ENDIF}

function isIPv4 : Boolean;
var
  tcp   : TIdTCPClient;
  cHost : String;
begin
  //result := true;
  tcp := TIdTCPClient.Create(nil);
  tcp.Port := 80;
  cHost := 'www.google.de';
  try
    try
      tcp.Host := GStack.ResolveHost(cHost, Id_IPv4);
      tcp.IPVersion := Id_IPv4;
      tcp.Connect;
      result := true;
    except
      try
        if tcp.Connected() then
        begin
          tcp.Disconnect;
        end;
        tcp.Host := GStack.ResolveHost(cHost, Id_IPv6);
        tcp.IPVersion := Id_IPv6;
        tcp.Connect;
        result := false;
      except
        result := true;
      end;
    end;
  finally
    FreeAndNil(tcp);
  end;
end;

{$if CompilerVersion > 24}

function encodeBase64(const aText: string): String;
var cCoder: TIdEncoderMIME;
begin
  cCoder := TIdEncoderMIME.Create(nil);
  try
    result  := cCoder.EncodeString(aText, IndyTextEncoding_UTF8);
  finally
    FreeAndNil(cCoder);
  end;
end;

function decodeBase64(const EncodedText: string): String;
var cCoder: TIdDecoderMIME;
    zs8   : String;
begin
  cCoder := TIdDecoderMIME.Create(nil);
  try
    zs8  := String(cCoder.DecodeString(EncodedText));
    result  := String(zs8);
  finally
    FreeAndNil(cCoder);
  end;
end;

function GetLocalComputerName: string;
begin
  // todo: implement me...
  result := '';
end;

function convertBytesToString(aBytes : TBytes) : String;
begin
  result  := TEncoding.UTF8.GetString(aBytes);
end;

function convertStringToBytes(aString : String) : TBytes;
begin
  result := TEncoding.UTF8.GetBytes(aString);
end;

function getUserFromOS: string;
begin
  // todo: implement me...
  result := '';
end;

{$else}

function encodeBase64(const aText: string): String;
var cCoder: TIdEncoderMIME;
begin
  cCoder := TIdEncoderMIME.Create(nil);
  try
    result  := cCoder.EncodeString(aText, TEncoding.UTF8);
  finally
    FreeAndNil(cCoder);
  end;
end;

function decodeBase64(const EncodedText: string): String;
var cCoder: TIdDecoderMIME;
    zs8   : UTF8String;
begin
  cCoder := TIdDecoderMIME.Create(nil);
  try
    zs8  := UTF8String(cCoder.DecodeString(EncodedText));
    result  := String(zs8);
  finally
    FreeAndNil(cCoder);
  end;
end;

procedure StrResetLength(var S: String; aCount : Integer);
begin
  //SetLength(S, StrLen(PWideChar(S)));
  SetLength(S, aCount);
end;

function GetLocalComputerName: string;
var
  Count: DWORD;
  zws : widestring;
  zwWideToStr : String;
begin
  Count := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(zws, Count);
  if GetComputerName(PWideChar(zws), Count) then
  begin
    zwWideToStr := zws;
    StrResetLength(zwWideToStr, Count);
  end else
  begin
    zwWideToStr := '';
  end;
  result := zwWideToStr;
end;

function convertBytesToString(aBytes : TBytes) : String;
var zbr: RawByteString;
    pas, pad: PAnsiChar;
begin
  setlength(zbr, length(aBytes) * 2);
  pas := PAnsiChar(@aBytes[0]);
  pad := PAnsiChar(@zbr[1]);
  BinToHex(pas, pad, length(aBytes));
  result := trim(String(zbr));
end;

function convertStringToBytes(aString : String) : TBytes;
var size, len     : longint;
    pc            : PByte;
begin
  len := length(aString);
  if (len mod 2) = 0 then
  begin
    // ok...
    size  := len div 2;
    setlength(result, size);
    pc  := @result[0];
    HexToBin(pchar(aString), pc, size);
  end else
  begin
    raise ENxLoggingConvertError.Create(SC_WRONGSTRINGLENGTHSHOULDBEEVEN);
  end;
end;

function getUserFromOS: string;
var
  UserName    : string;
  UserNameLen : Dword;
begin
  UserNameLen := 255;
  SetLength(userName, UserNameLen);
  if GetUserName(PChar(UserName), UserNameLen) then
  begin
    result := Copy(UserName,1,UserNameLen-1);
  end else
  begin
    result := '';
  end;
end;

{$ifend}

function _TryStrToDateTime(const S: string; out Value: TDateTime; const aFormatSettings: TFormatSettings) : Boolean;
var ind   : Integer;
    msts  : String;
begin
  result  := TryStrToDateTime(s, Value, aFormatSettings);
  if result then
  begin
    ind := Pos(' ', s);
    if ind > 0 then
    begin
      // mit Zeit...
      msts  := Copy(s, ind+1, length(s)-ind);
      ind := Pos('.', msts);
      if ind > 0 then
      begin
        // Mit Millisekunden...
        msts  := Copy(msts, ind+1, length(msts)-ind);
        {$IF CompilerVersion >= 24.0 }
          Value := System.DateUtils.IncMilliSecond(Value, StrToIntDef(msts, 0));
        {$ELSE}
          Value := DateUtils.IncMilliSecond(Value, StrToIntDef(msts, 0));
        {$IFEND}
      end;
    end;
  end;
end;

function  Logger : TNxLogger;
begin
  if fDefaultLogger = nil then
  begin
    fDefaultLogger := TNxLogger.Create(nil);
    fDefaultLogger.addAppender(TNxLogAppenderFile.Create(fDefaultLogger));
  end;
  result  := fDefaultLogger;
end;

procedure initLoggerFormatSettings(var FormatSettings : TFormatSettings);
begin
  with FormatSettings do
  begin
    CurrencyFormat:=3;
    NegCurrFormat:=8;
    ThousandSeparator:='.';
    DecimalSeparator:=',';
    CurrencyDecimals:=2;
    DateSeparator:='.';
    TimeSeparator:=':';
    ListSeparator:=';';
    CurrencyString:='';
    ShortDateFormat:='yyyy.MM.dd';//'dd.MM.yyyy';
    LongDateFormat:='dddd, d. MMMM yyyy';
    TimeAMString:='';
    TimePMString:='';
    ShortTimeFormat:='hh:mm';
    LongTimeFormat:='hh:mm:ss.zzz';
    ShortMonthNames[1]:='Jan';
    ShortMonthNames[2]:='Feb';
    ShortMonthNames[3]:='Mrz';
    ShortMonthNames[4]:='Apr';
    ShortMonthNames[5]:='Mai';
    ShortMonthNames[6]:='Jun';
    ShortMonthNames[7]:='Jul';
    ShortMonthNames[8]:='Aug';
    ShortMonthNames[9]:='Sep';
    ShortMonthNames[10]:='Okt';
    ShortMonthNames[11]:='Nov';
    ShortMonthNames[12]:='Dez';
    LongMonthNames[1]:='Januar';
    LongMonthNames[2]:='Februar';
    LongMonthNames[3]:='März';
    LongMonthNames[4]:='April';
    LongMonthNames[5]:='Mai';
    LongMonthNames[6]:='Juni';
    LongMonthNames[7]:='Juli';
    LongMonthNames[8]:='August';
    LongMonthNames[9]:='September';
    LongMonthNames[10]:='Oktober';
    LongMonthNames[11]:='November';
    LongMonthNames[12]:='Dezember';
    ShortDayNames[1]:='So';
    ShortDayNames[2]:='Mo';
    ShortDayNames[3]:='Di';
    ShortDayNames[4]:='Mi';
    ShortDayNames[5]:='Do';
    ShortDayNames[6]:='Fr';
    ShortDayNames[7]:='Sa';
    LongDayNames[1]:='Sonntag';
    LongDayNames[2]:='Montag';
    LongDayNames[3]:='Dienstag';
    LongDayNames[4]:='Mittwoch';
    LongDayNames[5]:='Donnerstag';
    LongDayNames[6]:='Freitag';
    LongDayNames[7]:='Samstag';
    TwoDigitYearCenturyWindow:=0;
  end;
end;

{ **************************************************************************** }
{ ***** TNxLogFormater ******************************************************* }
{ **************************************************************************** }
class function TNxLogFormater.isLevelString(aString : String) : Boolean;
var csec    : String;
begin
  csec    := lowercase(aString);
  result  := false;
  if csec = 'trace' then
  begin
    result  := true;
    exit;
  end;
  if csec = 'finest' then
  begin
    result  := true;
    exit;
  end;
  if csec = 'debug' then
  begin
    result  := true;
    exit;
  end;
  if csec = 'info' then
  begin
    result  := true;
    exit;
  end;
  if csec = 'warn' then
  begin
    result  := true;
    exit;
  end;
  if csec = 'warning' then
  begin
    result  := true;
    exit;
  end;
  if csec = 'error' then
  begin
    result  := true;
    exit;
  end;
  if csec = 'exception' then
  begin
    result  := true;
    exit;
  end;
  if csec = 'fatal' then
  begin
    result  := true;
    exit;
  end;
  if csec = 'critical' then
  begin
    result  := true;
    exit;
  end;
end;

class function TNxLogFormater.convertToLevel(aString : String; aIgnoreIllegalStrings : Boolean = true) : TNxLoggerLevel;
var csec    : String;
begin
  csec    := lowercase(aString);
  result  := NXLL_FATAL;
  if csec = 'trace' then
  begin
    result  := NXLL_TRACE;
    exit;
  end;
  if csec = 'finest' then
  begin
    result  := NXLL_TRACE;
    exit;
  end;
  if csec = 'debug' then
  begin
    result  := NXLL_DEBUG;
    exit;
  end;
  if csec = 'info' then
  begin
    result  := NXLL_INFO;
    exit;
  end;
  if csec = 'warn' then
  begin
    result  := NXLL_WARN;
    exit;
  end;
  if csec = 'warning' then
  begin
    result  := NXLL_WARN;
    exit;
  end;
  if csec = 'error' then
  begin
    result  := NXLL_ERROR;
    exit;
  end;
  if csec = 'exception' then
  begin
    result  := NXLL_ERROR;
    exit;
  end;
  if csec = 'fatal' then
  begin
    result  := NXLL_FATAL;
    exit;
  end;
  if csec = 'critical' then
  begin
    result  := NXLL_FATAL;
    exit;
  end;
  if not aIgnoreIllegalStrings then
  begin
    raise ENxLoggingConvertError.Create(Format('Can not convert string "%0:s" to loglevel!', [aString]));
  end;
end;

class function TNxLogFormater.convertLevelToString(const aLevel : TNxLoggerLevel; const isException : Boolean = false) : String;
begin
  case aLevel of
    NXLL_TRACE: result  := 'trace';
    NXLL_DEBUG: result  := 'debug';
    NXLL_INFO: result  := 'info';
    NXLL_WARN: result  := 'warn';
    NXLL_ERROR:
    begin
      if isException then
      begin
        result  := 'exception';
      end else
      begin
        result  := 'error';
      end;
    end;
    NXLL_FATAL: result  := 'fatal';
  end;
end;

class function TNxLogFormater.convertToCategory(aString : String) : TNxLoggerCategory;
var csec  : String;
begin
  csec  := lowercase(aString);
  result  := NXLCAT_NONE;
  if csec = 'options' then
  begin
    result  := NXLCAT_OPTIONS;
    exit;
  end;
  if csec = 'running' then
  begin
    result  := NXLCAT_RUNNING;
    exit;
  end;
  if csec = 'calculating' then
  begin
    result  := NXLCAT_CALCULATING;
    exit;
  end;
  if csec = 'fastrunning' then
  begin
    result  := NXLCAT_FASTRUNNING;
    exit;
  end;
  if csec = 'startup' then
  begin
    result  := NXLCAT_STARTUP;
    exit;
  end;
  if csec = 'shutdown' then
  begin
    result  := NXLCAT_SHUTDOWN;
    exit;
  end;
  if csec = 'reference' then
  begin
    result  := NXLCAT_REFERENCE;
    exit;
  end;
  if csec = 'com' then
  begin
    result  := NXLCAT_COM;
    exit;
  end;
  if csec = 'dcom' then
  begin
    result  := NXLCAT_DCOM;
    exit;
  end;
  if csec = 'plugin' then
  begin
    result  := NXLCAT_PLUGIN;
    exit;
  end;
end;

class function  TNxLogFormater.convertCategoryToString(const aCategory : TNxLoggerCategory) : String;
begin
  case aCategory of
    NXLCAT_NONE: result := 'none';
    NXLCAT_OPTIONS: result := 'options';
    NXLCAT_RUNNING: result := 'running';
    NXLCAT_CALCULATING: result := 'calculating';
    NXLCAT_FASTRUNNING: result := 'fastrunning';
    NXLCAT_STARTUP: result := 'startup';
    NXLCAT_SHUTDOWN: result := 'shutdown';
    NXLCAT_REFERENCE: result := 'reference';
    NXLCAT_COM: result := 'com';
    NXLCAT_DCOM: result := 'dcom';
    NXLCAT_PLUGIN: result := 'plugin';
  else
    result := ''
  end;
end;


{ **************************************************************************** }
{ ***** TNxLogFormaterPlain ************************************************** }
{ **************************************************************************** }
constructor TNxLogFormaterPlain.Create;
begin
  inherited Create;
  initLoggerFormatSettings(fFormatSettings);
  fFormatSettings.ShortDateFormat:='yyyy-MM-dd';
  fFormatSettings.LongTimeFormat:='hh:mm:ss,zzz';
end;

destructor TNxLogFormaterPlain.Destroy;
begin
  inherited Destroy;
end;

function  TNxLogFormaterPlain.formatMessage(aMessage : TNxLoggerMessage) : String;
begin
  if aMessage.LogModule <> '' then
  begin
    result  := Format('%3:s|%1:s|%0:s|%2:s|%4:s|%5:s|%6:s|%7:s|%8:s|%9:s',
        [ StringReplace(aMessage.ApplicationId, '|', '-', [rfReplaceAll]),                        // 0
          TNxLogFormater.convertLevelToString(aMessage.LogLevel, aMessage.LogException <> nil),   // 1
          StringReplace(aMessage.LogModule, '|', '-', [rfReplaceAll]),                            // 2
          DateTimeToStr(aMessage.LogTimestamp, fFormatSettings),                                  // 3
          StringReplace(aMessage.LogMessage, '|', '-', [rfReplaceAll]),                           // 4
          StringReplace(aMessage.InstanceId, '|', '-', [rfReplaceAll]),                           // 5
          StringReplace(aMessage.LogUser, '|', '-', [rfReplaceAll]),                              // 6
          StringReplace(aMessage.LogCategory, '|', '-', [rfReplaceAll]),                          // 7
          StringReplace(aMessage.Language, '|', '-', [rfReplaceAll]),                             // 8
          StringReplace(aMessage.ThreadId, '|', '-', [rfReplaceAll])                              // 9
          ]);
  end else
  begin
    result  := Format('%2:s|%1:s|%0:s|%3:s|%4:s|%5:s|%6:s|%7:s|%8:s',
        [ StringReplace(aMessage.ApplicationId, '|', '-', [rfReplaceAll]),                        // 0
          TNxLogFormater.convertLevelToString(aMessage.LogLevel, aMessage.LogException <> nil),   // 1
          DateTimeToStr(aMessage.LogTimestamp, fFormatSettings),                                  // 2
          StringReplace(aMessage.LogMessage, '|', '-', [rfReplaceAll]),                           // 3
          StringReplace(aMessage.InstanceId, '|', '-', [rfReplaceAll]),                           // 4
          StringReplace(aMessage.LogUser, '|', '-', [rfReplaceAll]),                              // 5
          StringReplace(aMessage.LogCategory, '|', '-', [rfReplaceAll]),                          // 6
          StringReplace(aMessage.Language, '|', '-', [rfReplaceAll]),                             // 7
          StringReplace(aMessage.ThreadId, '|', '-', [rfReplaceAll])                              // 8
          ]);
  end;
end;

function  TNxLogFormaterPlain.formatMessageBase64(aMessage : TNxLoggerMessage) : String;
begin
  if aMessage.LogModule <> '' then
  begin
    result  := Format('%3:s|%1:s|%0:s|%2:s|%4:s|%5:s|%6:s|%7:s|%8:s|%9:s',
        [ encodeBase64(aMessage.ApplicationId),                         // 0
          TNxLogFormater.convertLevelToString(aMessage.LogLevel, aMessage.LogException <> nil),   // 1
          encodeBase64(aMessage.LogModule),                             // 2
          DateTimeToStr(aMessage.LogTimestamp, fFormatSettings),        // 3
          encodeBase64(aMessage.LogMessage),                            // 4
          encodeBase64(aMessage.InstanceId),                            // 5
          encodeBase64(aMessage.LogUser),                               // 6
          encodeBase64(aMessage.LogCategory),                           // 7
          StringReplace(aMessage.Language, '|', '-', [rfReplaceAll]),   // 8
          StringReplace(aMessage.ThreadId, '|', '-', [rfReplaceAll])    // 9
          ]);
  end else
  begin
    result  := Format('%2:s|%1:s|%0:s|%3:s|%4:s|%5:s|%6:s|%7:s|%8:s',
        [ encodeBase64(aMessage.ApplicationId),                        // 0
          TNxLogFormater.convertLevelToString(aMessage.LogLevel, aMessage.LogException <> nil),   // 1
          DateTimeToStr(aMessage.LogTimestamp, fFormatSettings),                                  // 2
          encodeBase64(aMessage.LogMessage),                            // 3
          encodeBase64(aMessage.InstanceId),                            // 4
          encodeBase64(aMessage.LogUser),                               // 5
          encodeBase64(aMessage.LogCategory),                           // 6
          StringReplace(aMessage.Language, '|', '-', [rfReplaceAll]),   // 7
          StringReplace(aMessage.ThreadId, '|', '-', [rfReplaceAll])    // 8
          ]);
  end;
end;

function  TNxLogFormaterPlain.parseMessage(aMessageText : String) : TNxLoggerMessage;
var sl        : TStrings;
    cAppId    : String;
    cInstId   : String;
    cUser     : String;
    cThreadId : String;
    cLevel    : TNxLoggerLevel;
    cModule,
    cMessage  : String;
    cLanguage : String;
    cCategory : String;
    cTime     : TDateTime;
    i         : Integer;
    cLevelWasSet  : Boolean;
begin
  cAppId    := '';
  cInstId   := '';
  cUser     := '';
  cThreadId := '';
  cLevel    := nxLogging.NXLL_FATAL;
  cModule   := '';
  cMessage  := '';
  cLanguage := '';
  cCategory := '';
  cTime     := nan;
  result    := nil;
  if aMessageText <> '' then
  begin
    sl  := TStringList.Create;
    try
      sl.LineBreak  := ' ';
      sl.Text := aMessageText;
      if sl.Count >= 2 then
      begin
        // Timestamp...
        if not _TryStrToDateTime(sl[0], cTime, fFormatSettings) then
        begin
          // irgendwo eine Zeit suchen...
          for i := 1 to sl.Count - 1 do
          begin
            if _TryStrToDateTime(sl[i], cTime, fFormatSettings) then
            begin
              break;
            end;
          end;
          if IsNan(cTime) then
          begin
            //exit;
          end;
        end;
        // Level...
        cLevelWasSet  := false;
        for i := 1 to sl.Count - 1 do
        begin
          cLevel  := nxLogging.TNxLogFormater.convertToLevel(sl[i]);
          if cLevel <> NXLL_FATAL then
          begin
            cLevelWasSet  := true;
            break;
          end;
        end;
        if cLevelWasSet then
        begin
          cCategory := nxLogging.TNxLogFormater.convertLevelToString(cLevel);
        end;
        cMessage  := aMessageText;
        result  := TNxLoggerMessage.Create(cAppId, cInstId, cUser, cLevel, cModule, cMessage, cCategory, cLanguage, cThreadId, nil);
        result.fTimestamp := cTime;
      end else
      begin
        cMessage  := aMessageText;
        cLevelWasSet  := false;
        if (not cLevelWasSet) and (Pos('fatal', cMessage) > 0) then begin cLevel := NXLL_FATAL; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('error', cMessage) > 0) then begin cLevel := NXLL_ERROR; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('warn', cMessage) > 0) then begin cLevel := NXLL_WARN; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('warning', cMessage) > 0) then begin cLevel := NXLL_WARN; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('info', cMessage) > 0) then begin cLevel := NXLL_INFO; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('debug', cMessage) > 0) then begin cLevel := NXLL_DEBUG; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('fine', cMessage) > 0) then begin cLevel := NXLL_DEBUG; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('trace', cMessage) > 0) then begin cLevel := NXLL_TRACE; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('finer', cMessage) > 0) then begin cLevel := NXLL_TRACE; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('finest', cMessage) > 0) then begin cLevel := NXLL_TRACE; cLevelWasSet := true; end;
        if cLevelWasSet then
        begin
          cCategory := nxLogging.TNxLogFormater.convertLevelToString(cLevel);
        end;
        result  := TNxLoggerMessage.Create(cAppId, cInstId, cUser, cLevel, cModule, cMessage, cCategory, cLanguage, cThreadId, nil);
        result.fTimestamp := nan;
      end;
    finally
      FreeAndNil(sl);
    end;
  end;
end;

function  TNxLogFormaterPlain.parseMessageBase64(aMessageText : String) : TNxLoggerMessage;
var sl        : TStrings;
    cAppId    : String;
    cInstId   : String;
    cUser     : String;
    cThreadId : String;
    cLevel    : TNxLoggerLevel;
    cModule,
    cMessage  : String;
    cLanguage : String;
    cCategory : String;
    cTime     : TDateTime;
    i         : Integer;
    cLevelWasSet  : Boolean;
begin
  cAppId    := '';
  cInstId   := '';
  cUser     := '';
  cThreadId := '';
  cLevel    := nxLogging.NXLL_FATAL;
  cModule   := '';
  cMessage  := '';
  cLanguage := '';
  cCategory := '';
  cTime     := nan;
  result    := nil;
  if aMessageText <> '' then
  begin
    sl  := TStringList.Create;
    try
      sl.LineBreak  := '|';
      try
        sl.Text := decodeBase64(aMessageText);
      except
        on e : exception do
        begin
          // vermutlich kein Base64...
          sl.Text := aMessageText;
        end;
      end;
      if sl.Count >= 1 then
      begin
        // Timestamp...
        if not _TryStrToDateTime(sl[0], cTime, fFormatSettings) then
        begin
          // irgendwo eine Zeit suchen...
          for i := 1 to sl.Count - 1 do
          begin
            if _TryStrToDateTime(sl[i], cTime, fFormatSettings) then
            begin
              break;
            end;
          end;
          if IsNan(cTime) then
          begin
            exit;
          end;
        end;
        // Level...
        cLevelWasSet  := false;
        for i := 1 to sl.Count - 1 do
        begin
          cLevel  := nxLogging.TNxLogFormater.convertToLevel(sl[i]);
          if cLevel <> NXLL_FATAL then
          begin
            cLevelWasSet  := true;
            break;
          end;
        end;
        if cLevelWasSet then
        begin
          cCategory := nxLogging.TNxLogFormater.convertLevelToString(cLevel);
        end;
        result  := TNxLoggerMessage.Create(cAppId, cInstId, cUser, cLevel, cModule, cMessage, cCategory, cLanguage, cThreadId, nil);
        result.fTimestamp := cTime;
      end else
      begin
        cMessage  := decodeBase64(aMessageText);
        cLevelWasSet  := false;
        if (not cLevelWasSet) and (Pos('fatal', cMessage) > 0) then begin cLevel := NXLL_FATAL; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('error', cMessage) > 0) then begin cLevel := NXLL_ERROR; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('warn', cMessage) > 0) then begin cLevel := NXLL_WARN; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('warning', cMessage) > 0) then begin cLevel := NXLL_WARN; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('info', cMessage) > 0) then begin cLevel := NXLL_INFO; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('debug', cMessage) > 0) then begin cLevel := NXLL_DEBUG; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('fine', cMessage) > 0) then begin cLevel := NXLL_DEBUG; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('trace', cMessage) > 0) then begin cLevel := NXLL_TRACE; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('finer', cMessage) > 0) then begin cLevel := NXLL_TRACE; cLevelWasSet := true; end;
        if (not cLevelWasSet) and (Pos('finest', cMessage) > 0) then begin cLevel := NXLL_TRACE; {cLevelWasSet := true;} end;
        result  := TNxLoggerMessage.Create(cAppId, cInstId, cUser, cLevel, cModule, cMessage, cCategory, cLanguage, cThreadId, nil);
        result.fTimestamp := nan;
      end;
    finally
      FreeAndNil(sl);
    end;
  end;
end;



{ **************************************************************************** }
{ ***** TNxLogFormaterDefault ************************************************ }
{ **************************************************************************** }
constructor TNxLogFormaterDefault.Create;
begin
  inherited Create;
  initLoggerFormatSettings(fFormatSettings);
end;

destructor TNxLogFormaterDefault.Destroy;
begin
  inherited Destroy;
end;

function  TNxLogFormaterDefault.formatMessage(aMessage : TNxLoggerMessage) : String;
begin
  if aMessage.LogModule <> '' then
  begin
    result  := Format('%0:s|%3:s|%1:s.%2:s|%4:s|%5:s|%6:s|%7:s|%8:s|%9:s',
        [ StringReplace(aMessage.ApplicationId, '|', '-', [rfReplaceAll]),                        // 0
          TNxLogFormater.convertLevelToString(aMessage.LogLevel, aMessage.LogException <> nil),   // 1
          StringReplace(aMessage.LogModule, '|', '-', [rfReplaceAll]),                            // 2
          DateTimeToStr(aMessage.LogTimestamp, fFormatSettings),                                  // 3
          StringReplace(aMessage.LogMessage, '|', '-', [rfReplaceAll]),                           // 4
          StringReplace(aMessage.InstanceId, '|', '-', [rfReplaceAll]),                           // 5
          StringReplace(aMessage.LogUser, '|', '-', [rfReplaceAll]),                              // 6
          StringReplace(aMessage.LogCategory, '|', '-', [rfReplaceAll]),                          // 7
          StringReplace(aMessage.Language, '|', '-', [rfReplaceAll]),                             // 8
          StringReplace(aMessage.ThreadId, '|', '-', [rfReplaceAll])                              // 9
          ]);
  end else
  begin
    result  := Format('%0:s|%2:s|%1:s|%3:s|%4:s|%5:s|%6:s|%7:s|%8:s',
        [ StringReplace(aMessage.ApplicationId, '|', '-', [rfReplaceAll]),                        // 0
          TNxLogFormater.convertLevelToString(aMessage.LogLevel, aMessage.LogException <> nil),   // 1
          DateTimeToStr(aMessage.LogTimestamp, fFormatSettings),                                  // 2
          StringReplace(aMessage.LogMessage, '|', '-', [rfReplaceAll]),                           // 3
          StringReplace(aMessage.InstanceId, '|', '-', [rfReplaceAll]),                           // 4
          StringReplace(aMessage.LogUser, '|', '-', [rfReplaceAll]),                              // 5
          StringReplace(aMessage.LogCategory, '|', '-', [rfReplaceAll]),                          // 6
          StringReplace(aMessage.Language, '|', '-', [rfReplaceAll]),                             // 7
          StringReplace(aMessage.ThreadId, '|', '-', [rfReplaceAll])                              // 8
          ]);
  end;
end;

function  TNxLogFormaterDefault.formatMessageBase64(aMessage : TNxLoggerMessage) : String;
begin
  if aMessage.LogModule <> '' then
  begin
    result  := Format('%0:s|%3:s|%1:s.%2:s|%4:s|%5:s|%6:s|%7:s|%8:s|%9:s',
        [ encodeBase64(aMessage.ApplicationId),                         // 0
          TNxLogFormater.convertLevelToString(aMessage.LogLevel, aMessage.LogException <> nil),   // 1
          encodeBase64(aMessage.LogModule),                             // 2
          DateTimeToStr(aMessage.LogTimestamp, fFormatSettings),        // 3
          encodeBase64(aMessage.LogMessage),                            // 4
          encodeBase64(aMessage.InstanceId),                            // 5
          encodeBase64(aMessage.LogUser),                               // 6
          encodeBase64(aMessage.LogCategory),                           // 7
          StringReplace(aMessage.Language, '|', '-', [rfReplaceAll]),   // 8
          StringReplace(aMessage.ThreadId, '|', '-', [rfReplaceAll])    // 9
          ]);
  end else
  begin
    result  := Format('%0:s|%2:s|%1:s|%3:s|%4:s|%5:s|%6:s|%7:s|%8:s',
        [ encodeBase64(aMessage.ApplicationId),                        // 0
          TNxLogFormater.convertLevelToString(aMessage.LogLevel, aMessage.LogException <> nil),   // 1
          DateTimeToStr(aMessage.LogTimestamp, fFormatSettings),                                  // 2
          encodeBase64(aMessage.LogMessage),                            // 3
          encodeBase64(aMessage.InstanceId),                            // 4
          encodeBase64(aMessage.LogUser),                               // 5
          encodeBase64(aMessage.LogCategory),                           // 6
          StringReplace(aMessage.Language, '|', '-', [rfReplaceAll]),   // 7
          StringReplace(aMessage.ThreadId, '|', '-', [rfReplaceAll])    // 8
          ]);
  end;
end;

function  TNxLogFormaterDefault.parseMessage(aMessageText : String) : TNxLoggerMessage;
var sl, sli   : TStrings;
    cAppId    : String;
    cInstId   : String;
    cUser     : String;
    cThreadId : String;
    cLevel    : TNxLoggerLevel;
    cModule,
    cMessage  : String;
    cLanguage : String;
    cCategory : String;
    cTime     : TDateTime;
    i         : Integer;
begin
  cAppId    := '';
  cInstId   := '';
  cUser     := '';
  cThreadId := '';
  //cLevel    := nxLogging.NXLL_FATAL;
  cModule   := '';
  cMessage  := '';
  cLanguage := '';
  cCategory := '';
  cTime     := nan;
  result    := nil;
  if aMessageText <> '' then
  begin
    sl  := TStringList.Create;
    sli := TStringList.Create;
    try
      sl.LineBreak  := '|';
      sli.LineBreak := '.';
      sl.Text := aMessageText;
      if sl.Count >= 4 then
      begin
        // ApplicationId...
        cAppId  := sl[0];
        // Timestamp...
        if not _TryStrToDateTime(sl[1], cTime, fFormatSettings) then
        begin
          exit;
        end;
        // Level und Modules...
        sli.Text  := sl[2];
        if sli.Count > 1 then
        begin
          cLevel  := TNxLogFormater.convertToLevel(sli[0]);
          cModule := sli[1];
          for i := 2 to sli.Count - 1 do
          begin
            cModule := cModule + '.' + sli[i];
          end;
        end else
        begin
          if sli.Count > 0 then
          begin
            cLevel  := TNxLogFormater.convertToLevel(sli[0]);
          end else
          begin
            exit;
          end;
        end;
        // Message...
        cMessage  := sl[3];
        if sl.Count > 4 then
        begin
          cInstId := sl[4];
          if sl.Count > 5 then
          begin
            cUser := sl[5];
            if sl.Count > 6 then
            begin
              cCategory := sl[6];
              if sl.Count > 7 then
              begin
                cLanguage := sl[7];
                if sl.Count > 8 then
                begin
                  cThreadId := sl[8];
                end;
              end;
            end;
          end;
        end;
        result  := TNxLoggerMessage.Create(cAppId, cInstId, cUser, cLevel, cModule, cMessage, cCategory, cLanguage, cThreadId, nil);
        result.fTimestamp := cTime;
      end;
    finally
      FreeAndNil(sl);
      FreeAndNil(sli);
    end;
  end;
end;

function  TNxLogFormaterDefault.parseMessageBase64(aMessageText : String) : TNxLoggerMessage;
var sl, sli   : TStrings;
    cAppId    : String;
    cInstId   : String;
    cUser     : String;
    cThreadId : String;
    cLevel    : TNxLoggerLevel;
    cModule,
    cMessage  : String;
    cCategory : String;
    cLanguage : String;
    cTime     : TDateTime;
begin
  cAppId    := '';
  cInstId   := '';
  cUser     := '';
  cThreadId := '';
  //cLevel    := nxLogging.NXLL_FATAL;
  cModule   := '';
  cMessage  := '';
  cLanguage := '';
  cCategory := '';
  cTime     := nan;
  result    := nil;
  if aMessageText <> '' then
  begin
    sl  := TStringList.Create;
    sli := TStringList.Create;
    try
      sl.LineBreak  := '|';
      sli.LineBreak := '.';
      sl.Text := aMessageText;
      if sl.Count >= 4 then
      begin
        // ApplicationId...
        cAppId  := decodeBase64(sl[0]);
        // Timestamp...
        if not _TryStrToDateTime(sl[1], cTime, fFormatSettings) then
        begin
          exit;
        end;
        // Level und Modules...
        sli.Text  := sl[2];
        if sli.Count > 1 then
        begin
          cLevel  := TNxLogFormater.convertToLevel(sli[0]);
          cModule := decodeBase64(sli[1]);
          {for i := 2 to sli.Count - 1 do
          begin
            cModule := cModule + '.' + sli[i];
          end;}
        end else
        begin
          if sli.Count > 0 then
          begin
            cLevel  := TNxLogFormater.convertToLevel(sli[0]);
          end else
          begin
            exit;
          end;
        end;
        // Message...
        cMessage  := decodeBase64(sl[3]);
        if sl.Count > 4 then
        begin
          cInstId := decodeBase64(sl[4]);
          if sl.Count > 5 then
          begin
            cUser := decodeBase64(sl[5]);
            if sl.Count > 6 then
            begin
              cCategory := decodeBase64(sl[6]);
              if sl.Count > 7 then
              begin
                cLanguage := sl[7];
                if sl.Count > 8 then
                begin
                  cThreadId := sl[8];
                end;
              end;
            end;
          end;
        end;
        result  := TNxLoggerMessage.Create(cAppId, cInstId, cUser, cLevel, cModule, cMessage, cCategory, cLanguage, cThreadId, nil);
        result.fTimestamp := cTime;
      end;
    finally
      FreeAndNil(sl);
      FreeAndNil(sli);
    end;
  end;
end;

{ **************************************************************************** }
{ ***** TNxLogFormaterTCP **************************************************** }
{ **************************************************************************** }
constructor TNxLogFormaterTCP.Create;
begin
  inherited Create;
  initLoggerFormatSettings(fFormatSettings);
  fMachineIdent := '';
end;

destructor TNxLogFormaterTCP.Destroy;
begin
  inherited Destroy;
end;

function  TNxLogFormaterTCP.formatMessage(aMessage : TNxLoggerMessage) : String;
var cClassName      : String;
    stt, smsg, smod : String;
begin
  if aMessage.fException <> nil then
  begin
    cClassName  := aMessage.fException.ClassName;
  end else
  begin
    cClassName  := aMessage.fExceptionClassName;
  end;
  stt := aMessage.StackTrace;
  stt := StringReplace(stt, '|', '-', [rfReplaceAll]);
  stt := StringReplace(stt, #10, '~#10~', [rfReplaceAll]);
  stt := StringReplace(stt, #13, '~#13~', [rfReplaceAll]);

  smod := StringReplace(aMessage.LogModule, '|', '-', [rfReplaceAll]);
  smod := StringReplace(smod, #10, '~#10~', [rfReplaceAll]);
  smod := StringReplace(smod, #13, '~#13~', [rfReplaceAll]);

  smsg := StringReplace(aMessage.LogMessage, '|', '-', [rfReplaceAll]);
  smsg := StringReplace(smsg, #10, '~#10~', [rfReplaceAll]);
  smsg := StringReplace(smsg, #13, '~#13~', [rfReplaceAll]);

  result  := Format('%0:s|%3:s|%1:s|%2:s|%4:s|%5:s|%6:s|%7:s|%8:s|%9:s|%10:s|%11:s|%12:s',
      [ StringReplace(aMessage.ApplicationId, '|', '-', [rfReplaceAll]),                          // 0
        TNxLogFormater.convertLevelToString(aMessage.LogLevel, aMessage.LogException <> nil),     // 1
        smod,                              // 2
        DateTimeToStr(aMessage.LogTimestamp, fFormatSettings),                                    // 3
        smsg,                             // 4
        aMessage.LogCategory,                                                                     // 5
        StringReplace(aMessage.Language, '|', '-', [rfReplaceAll]),                               // 6
        StringReplace(cClassName, '|', '-', [rfReplaceAll]),                                      // 7
        stt,                                                                                      // 8
        StringReplace(aMessage.InstanceId, '|', '-', [rfReplaceAll]),                             // 9
        StringReplace(aMessage.LogUser, '|', '-', [rfReplaceAll]),                                // 10
        StringReplace(aMessage.ThreadId, '|', '-', [rfReplaceAll]),                               // 11
        fMachineIdent                                                                             // 12
        ]);
end;

function  TNxLogFormaterTCP.formatMessageBase64(aMessage : TNxLoggerMessage) : String;
var cClassName  : String;
    stt         : String;
begin
  if aMessage.fException <> nil then
  begin
    cClassName  := aMessage.fException.ClassName;
  end else
  begin
    cClassName  := aMessage.fExceptionClassName;
  end;
  stt := aMessage.StackTrace;
  stt := StringReplace(stt, '|', '-', [rfReplaceAll]);
  stt := StringReplace(stt, #10, '~#10~', [rfReplaceAll]);
  stt := StringReplace(stt, #13, '~#13~', [rfReplaceAll]);
  result  := Format('%0:s|%3:s|%1:s|%2:s|%4:s|%5:s|%6:s|%7:s|%8:s|%9:s|%10:s|%11:s|%12:s',
      [ encodeBase64(aMessage.ApplicationId),                           // 0
        TNxLogFormater.convertLevelToString(aMessage.LogLevel, aMessage.LogException <> nil),  // 1
        encodeBase64(aMessage.LogModule),                               // 2
        DateTimeToStr(aMessage.LogTimestamp, fFormatSettings),          // 3
        encodeBase64(aMessage.LogMessage),                              // 4
        encodeBase64(aMessage.LogCategory),                             // 5
        aMessage.Language,                                              // 6
        encodeBase64(cClassName),                                       // 7
        encodeBase64(stt),                                              // 8
        encodeBase64(aMessage.InstanceId),                              // 9
        encodeBase64(aMessage.LogUser),                                 // 10
        encodeBase64(aMessage.ThreadId),                                // 11
        encodeBase64(fMachineIdent)                                     // 12
        ]);
end;

function  TNxLogFormaterTCP.formatMessageExtra(aMessage : TNxLoggerMessage; aExtraDataId : Int64; aExtraData : TBytes) : String;
var zs, cData  : String;
begin
  zs  := formatMessage(aMessage);
  if length(aExtraData) > 0 then
  begin
    cData := convertBytesToString(aExtraData);
  end else
  begin
    cData := '';
  end;
  result  := Format('%0:s|%1:s|%2:s', [IntToStr(aExtraDataId), cData]);
end;

function  TNxLogFormaterTCP.formatMessageExtraBase64(aMessage : TNxLoggerMessage; aExtraDataId : Int64; aExtraData : TBytes) : String;
var zs, cData  : String;
begin
  zs  := formatMessage(aMessage);
  if length(aExtraData) > 0 then
  begin
    cData := convertBytesToString(aExtraData);
  end else
  begin
    cData := '';
  end;
  result  := Format('%0:s|%1:s|%2:s', [IntToStr(aExtraDataId), cData]);
end;

function  TNxLogFormaterTCP.parseMessage(aMessageText : String) : TNxLoggerMessage;
var sl          : TStrings;
    cAppId      : String;
    cLevel      : TNxLoggerLevel;
    cModule,
    cMessage,
    cLng,
    cClassName,
    cStackTrace : String;
    cThreadId   : String;
    cInstId     : String;
    cUser       : String;
    cCategory   : String;
    cTime       : TDateTime;
begin
  result  := nil;
  if aMessageText <> '' then
  begin
    sl  := TStringList.Create;
    try
      sl.LineBreak  := '|';
      sl.Text := aMessageText;
      if sl.Count >= 9 then
      begin
        // ApplicationId...
        cAppId  := sl[0];
        // Timestamp...
        if not _TryStrToDateTime(sl[1], cTime, fFormatSettings) then
        begin
          exit;
        end;
        // Level...
        cLevel  := TNxLogFormater.convertToLevel(sl[2]);
        // Modules...
        cModule := sl[3];
        // Message...
        cMessage  := sl[4];
        // Category...
        cCategory := sl[5];
        // Language...
        cLng  := sl[6];
        // Classname...
        cClassName  := sl[7];
        // StackTrace...
        cStackTrace := sl[8];
        cStackTrace := StringReplace(cStackTrace, '~#10~', #10, [rfReplaceAll]);
        cStackTrace := StringReplace(cStackTrace, '~#13~', #13, [rfReplaceAll]);
        // InstanceId...
        if sl.Count > 9 then
        begin
          cInstId := sl[9];
        end;
        // LogUser...
        if sl.Count > 10 then
        begin
          cUser   := sl[10];
        end;
        // ThreadId...
        if sl.Count > 11 then
        begin
          cThreadId := sl[11];
        end;
        result  := TNxLoggerMessage.Create(cAppId, cInstId, cUser, cLevel, cModule, cMessage, cCategory, cLng, cThreadId, nil);
        result.fTimestamp := cTime;
        result.fLanguage  := cLng;
        result.fExceptionClassName  := cClassName;
        result.fStackTrace  := cStackTrace;
      end;
    finally
      FreeAndNil(sl);
    end;
  end;
end;

function  TNxLogFormaterTCP.parseMessageBase64(aMessageText : String) : TNxLoggerMessage;
var sl          : TStrings;
    cAppId      : String;
    cInstId     : String;
    cUser       : String;
    cLevel      : TNxLoggerLevel;
    cModule,
    cMessage,
    cLng,
    cClassName,
    cStackTrace : String;
    cThreadId   : String;
    cCategory   : String;
    cTime       : TDateTime;
begin
  result  := nil;
  if aMessageText <> '' then
  begin
    sl  := TStringList.Create;
    try
      sl.LineBreak  := '|';
      sl.Text := aMessageText;
      if sl.Count >= 9 then
      begin
        // ApplicationId...
        cAppId  := decodeBase64(sl[0]);
        // Timestamp...
        if not _TryStrToDateTime(sl[1], cTime, fFormatSettings) then
        begin
          exit;
        end;
        // Level...
        cLevel  := TNxLogFormater.convertToLevel(sl[2]);
        // Modules...
        cModule := decodeBase64(sl[3]);
        // Message...
        cMessage  := decodeBase64(sl[4]);
        // Category...
        cCategory := decodeBase64(sl[5]);
        // Language...
        cLng  := sl[6];
        // Classname...
        cClassName  := decodeBase64(sl[7]);
        // StackTrace...
        cStackTrace := decodeBase64(sl[8]);
        // InstanceId...
        if sl.Count > 9 then
        begin
          cInstId := decodeBase64(sl[9]);
        end;
        // LogUser...
        if sl.Count > 10 then
        begin
          cUser   := decodeBase64(sl[10]);
        end;
        // ThreadId...
        if sl.Count > 11 then
        begin
          cThreadId := decodeBase64(sl[11]);
        end;
        result  := TNxLoggerMessage.Create(cAppId, cInstId, cUser, cLevel, cModule, cMessage, cCategory, cLng, cThreadId, nil);
        result.fTimestamp := cTime;
        result.fLanguage  := cLng;
        result.fExceptionClassName  := cClassName;
        result.fStackTrace  := cStackTrace;
      end;
    finally
      FreeAndNil(sl);
    end;
  end;
end;

function  TNxLogFormaterTCP.parseMessageMachine(aMessageText : String; var ResMachineIdent : String) : TNxLoggerMessage;
var sl  : TStrings;
begin
  ResMachineIdent := '';
  result  := parseMessage(aMessageText);
  if result <> nil then
  begin
    if aMessageText <> '' then
    begin
      sl  := TStringList.Create;
      try
        sl.LineBreak  := '|';
        sl.Text := aMessageText;
        if sl.Count > 12 then
        begin
          ResMachineIdent := sl[12];
        end;
      finally
        FreeAndNil(sl);
      end;
    end;
  end;
end;

function  TNxLogFormaterTCP.parseMessageMachineBase64(aMessageText : String; var ResMachineIdent : String) : TNxLoggerMessage;
var sl  : TStrings;
begin
  ResMachineIdent := '';
  result  := parseMessageBase64(aMessageText);
  if result <> nil then
  begin
    if aMessageText <> '' then
    begin
      sl  := TStringList.Create;
      try
        sl.LineBreak  := '|';
        sl.Text := aMessageText;
        if sl.Count > 12 then
        begin
          ResMachineIdent := decodeBase64(sl[12]);
        end;
      finally
        FreeAndNil(sl);
      end;
    end;
  end;
end;

function  TNxLogFormaterTCP.parseMessageMachineExtra(aMessageText : String; var ResMachineIdent : String; var ResExtraID : Int64; var ResExtra : TBytes) : TNxLoggerMessage;
var sl  : TStrings;
begin
  result  := parseMessageMachine(aMessageText, ResMachineIdent);
  if result <> nil then
  begin
    if aMessageText <> '' then
    begin
      sl  := TStringList.Create;
      try
        sl.LineBreak  := '|';
        sl.Text := aMessageText;
        if sl.Count > 13 then
        begin
          ResExtraID := StrToIntDef(sl[13], 0);
        end;
        if sl.Count > 14 then
        begin
          ResExtra  := convertStringToBytes(sl[14]);
        end;
      finally
        FreeAndNil(sl);
      end;
    end;
  end;
end;

function  TNxLogFormaterTCP.parseMessageMachineExtraBase64(aMessageText : String; var ResMachineIdent : String; var ResExtraID : Int64; var ResExtra : TBytes) : TNxLoggerMessage;
var sl  : TStrings;
begin
  result  := parseMessageMachineBase64(aMessageText, ResMachineIdent);
  if result <> nil then
  begin
    if aMessageText <> '' then
    begin
      sl  := TStringList.Create;
      try
        sl.LineBreak  := '|';
        sl.Text := aMessageText;
        if sl.Count > 13 then
        begin
          ResExtraID := StrToIntDef(sl[13], 0);
        end;
        if sl.Count > 14 then
        begin
          ResExtra  := convertStringToBytes(sl[14]);
        end;
      finally
        FreeAndNil(sl);
      end;
    end;
  end;
end;


{ **************************************************************************** }
{ ***** TNxLogAppenderFile *************************************************** }
{ **************************************************************************** }
constructor TNxLogAppenderFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRetryCount   := 10;
  fFormater     := TNxLogFormaterDefault.Create;
  fDirectory    := ExtractFilePath(ParamStr(0));
  fFilenameBase := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  initLoggerFormatSettings(fFormatSettings);
  fStrategy     := NXLFS_NEWFILES;
  fLastDate     := trunc(NowUTC);
end;

destructor TNxLogAppenderFile.Destroy;
begin
  FreeAndNil(fFormater);
  inherited Destroy;
end;

function  TNxLogAppenderFile.isNextDay : Boolean;
var cd  : TDate;
begin
  cd  := trunc(NowUTC);
  if cd > fLastDate then
  begin
    fLastDate := cd;
    result  := true;
  end else
  begin
    result  := false;
  end;
end;

procedure TNxLogAppenderFile.writeString(const aToStream : TStream; const aString : String);
var ba  : TBytes; len : Integer;
begin
  if aToStream <> nil then
  begin
    ba  := TEncoding.UTF8.GetBytes(aString);
    len := length(ba);
    if len > 0 then
    begin
      aToStream.Write(ba[0], len);
    end;
  end;
end;

procedure TNxLogAppenderFile.append(const aEvent : TNxLoggerMessage);
begin
  case fStrategy of
    NXLFS_SINGLEFILE:
    begin
      appendSingleFile(aEvent);
    end;
    NXLFS_NEWFILES:
    begin
      appendNewFiles(aEvent);
    end;
    NXLFS_RENAME:
    begin
      appendRename(aEvent);
    end;
  else
    appendNewFiles(aEvent);
  end;
end;

procedure TNxLogAppenderFile.appendSingleFile(const aEvent : TNxLoggerMessage);
var cFilename     : String;
    retrycount    : Integer;
    logout        : String;
    zs            : String;
    fs            : TFileStream;
label lblretry;
begin
  retrycount := fRetryCount;
  lblretry:
  try
    if not DirectoryExists(fDirectory) then ForceDirectories(fDirectory);
    cFilename := fDirectory + fFilenameBase + '_' + SC_ALLFILETAIL + '.nxlog';
    logout  := String(fFormater.formatMessage(aEvent));
    if FileExists(cFilename) then
    begin
      fs := TFileStream.Create(cFilename, fmOpenWrite + fmShareDenyWrite);
      fs.Position := fs.Size;
    end
    else
    begin
      fs := TFileStream.Create(cFilename, fmCreate);
    end;
    try
      zs := logout + #13#10;
      //fs.Write(zs[1], length(zs));
      writeString(fs, zs);
      retrycount := 0;
    finally
      FreeAndNil(fs);
    end;
  except
    on e : Exception do
    begin
      zs := zs + String(#13#10 + '   ###logfail: "' + e.Message + '"');
    end;
  end;
  if retrycount > 0 then
  begin
    dec(retrycount);
    sleep(50);
    goto lblretry;
  end;
end;

procedure TNxLogAppenderFile.appendNewFiles(const aEvent : TNxLoggerMessage);
var cFilename     : String;
    retrycount    : Integer;
    logout        : String;
    zs            : String;
    fs            : TFileStream;
label lblretry;
begin
  retrycount := fRetryCount;
  lblretry:
  try
    if not DirectoryExists(fDirectory) then ForceDirectories(fDirectory);
    cFilename := fDirectory + fFilenameBase + '_' + DateToStr(nowUTC, fFormatSettings) + '.nxlog';
    logout  := String(fFormater.formatMessage(aEvent));
    if FileExists(cFilename) then
    begin
      fs := TFileStream.Create(cFilename, fmOpenWrite + fmShareDenyWrite);
      fs.Position := fs.Size;
    end
    else
    begin
      fs := TFileStream.Create(cFilename, fmCreate);
    end;
    try
      zs := logout + #13#10;
      //fs.Write(zs[1], length(zs));
      writeString(fs, zs);
      retrycount := 0;
    finally
      FreeAndNil(fs);
    end;
  except
    on e : Exception do
    begin
      zs := zs + String(#13#10 + '   ###logfail: "' + e.Message + '"');
    end;
  end;
  if retrycount > 0 then
  begin
    dec(retrycount);
    sleep(50);
    goto lblretry;
  end;
end;

procedure TNxLogAppenderFile.appendRename(const aEvent : TNxLoggerMessage);
var cFilename     : String;
    cNewFilename  : String;
    retrycount    : Integer;
    logout        : String;
    zs            : String;
    fs            : TFileStream;
label lblretry;
begin
  retrycount := fRetryCount;
  lblretry:
  try
    if not DirectoryExists(fDirectory) then ForceDirectories(fDirectory);
    cFilename := fDirectory + fFilenameBase + '_' + SC_CURRENTFILETAIL + '.nxlog';
    if isNextDay then
    begin
      if FileExists(cFilename) then
      begin
        cNewFilename := fDirectory + fFilenameBase + '_' + SC_FILEFROM + '_'+ DateToStr(nowUTC, fFormatSettings) + '.nxlog';
        {$IF CompilerVersion >= 24.0 }
          if not System.SysUtils.RenameFile(cFilename, cNewFilename) then
        {$ELSE}
          if not SysUtils.RenameFile(cFilename, cNewFilename) then
        {$IFEND}
        begin
          raise Exception.Create(Format('could not rename logfile "%0:s" to "%1:s".', [cFilename, cNewFilename]));
        end;
      end;
    end;
    logout  := String(fFormater.formatMessage(aEvent));
    if FileExists(cFilename) then
    begin
      fs := TFileStream.Create(cFilename, fmOpenWrite + fmShareDenyWrite);
      fs.Position := fs.Size;
    end
    else
    begin
      fs := TFileStream.Create(cFilename, fmCreate);
    end;
    try
      zs := logout + #13#10;
      //fs.Write(zs[1], length(zs));
      writeString(fs, zs);
      retrycount := 0;
    finally
      FreeAndNil(fs);
    end;
  except
    on e : Exception do
    begin
      zs := zs + String(#13#10 + '   ###logfail: "' + e.Message + '"');
    end;
  end;
  if retrycount > 0 then
  begin
    dec(retrycount);
    sleep(50);
    goto lblretry;
  end;
end;


{ **************************************************************************** }
{ ***** TNxLogAppenderChain ************************************************** }
{ **************************************************************************** }

constructor TNxLogAppenderChain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fChainedLogger  := nil;
end;

destructor TNxLogAppenderChain.Destroy;
begin
  fChainedLogger  := nil;
  inherited Destroy;
end;

procedure TNxLogAppenderChain.append(const aEvent : TNxLoggerMessage);
begin
  if fChainedLogger <> nil then
  begin
    fChainedLogger.log(aEvent);
  end;
end;

{ **************************************************************************** }
{ ***** TNxLogAppenderTCPThread ********************************************** }
{ **************************************************************************** }

{$if CompilerVersion > 24}
function getMD5OfString(const aString : string) : string;
var idmd5 : TIdHashMessageDigest5;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  try
    result  := idmd5.HashStringAsHex(aString, IndyTextEncoding_UTF8);
  finally
   idmd5.Free;
  end;
end;
{$else}
function getMD5OfString(const aString : string) : string;
var idmd5 : TIdHashMessageDigest5;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  try
    result  := idmd5.HashStringAsHex(aString, TEncoding.UTF8);
  finally
   idmd5.Free;
  end;
end;
{$ifend}

constructor TNxLogAppenderTCPThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  fTCP            := nil;
  fMachineIdent   := '';
  fHostOrIP       := '';
  fPort           := 0;
  fRetryCount     := 10;
  fAdapter        := nil;
  fUsername       := '';
  fPassword       := '';
  fDisconnectSeconds  := 0;  // 0 is for no disconnect..., if > 0 then automatic disconnect!
  fAdapter        := nil;
end;

destructor TNxLogAppenderTCPThread.Destroy;
begin
  fAdapter := nil;
  inherited Destroy;
end;

procedure TNxLogAppenderTCPThread.Execute;

var cEntry              : String;
    cRes                : String;
    cResCode            : Integer;
    cCurrentTry         : Integer;
    _cPWD               : String;
    cResCodeS           : String;
    cResCodeT           : String;
    ind                 : Integer;
    cLastLogMessage     : TDateTime;

  procedure _connect(aIPVersion : TIdIPVersion);
  begin
    fTCP  := TIdTCPClient.Create(nil);
    try
      fTCP.IPVersion := aIPVersion;
      fTCP.Host := fHostOrIP;
      fTCP.Port := fPort;
      fTCP.Connect;
      if (fUsername <> '') then
      begin
        if fPassword <> '' then
        begin
          _cPWD  := getMD5OfString(fPassword);
        end else
        begin
          _cPWD := '';
        end;
        {$if CompilerVersion > 24}
        fTCP.IOHandler.WriteLn(fUsername, IndyTextEncoding_UTF8);
        fTCP.IOHandler.WriteLn(_cPWD, IndyTextEncoding_UTF8);
        {$else}
        fTCP.IOHandler.WriteLn(fUsername, TEncoding.UTF8);
        fTCP.IOHandler.WriteLn(_cPWD, TEncoding.UTF8);
        {$ifend}
      end;
    except
      FreeAndNil(fTCP);
    end;
  end;

  procedure _disconnect;
  begin
    if fTCP <> nil then
    begin
      try
        fTCP.Disconnect;
      except
      end;
    end;
    FreeAndNil(fTCP);
  end;

begin
  cLastLogMessage := Now;
  try
    while not Terminated do
    begin
      // dann sind wir auch verbunden...
      cEntry  := fAdapter.consumeEntry;
      if cEntry <> '' then
      begin
        cLastLogMessage := Now;
        if fTCP = nil then
        begin
          // verbinden...
          if isIPv4 then
          begin
            _connect(Id_IPv4);
          end else
          begin
            _connect(Id_IPv6);
          end;
        end;
        cCurrentTry := 0;
        repeat
          try
            if (fTCP <> nil) and (fTCP.IOHandler <> nil) then
            begin
              {$if CompilerVersion > 24}
              fTCP.IOHandler.WriteLn('log:'+cEntry, IndyTextEncoding_UTF8);
              cres := ftcp.IOHandler.ReadLnWait(2, IndyTextEncoding_UTF8);
              //cRes  := fTCP.IOHandler.ReadLn(idglobal.EOL, 5000, -1, IndyTextEncoding_UTF8);
              {$else}
              fTCP.IOHandler.WriteLn('log:'+cEntry, TEncoding.UTF8);
              cRes  := fTCP.IOHandler.ReadLn(idglobal.EOL, 5000, -1, TEncoding.UTF8);
              {$IFEND}
            end else
            begin
              _disconnect;
              break;
            end;
            {$if CompilerVersion > 24}
            ind := cRes.IndexOf(':');
            {$else}
            ind := Pos(':', cRes);
            {$IFEND}
            if ind > 0 then
            begin
              {$if CompilerVersion > 24}
              cResCodeS := cRes.Substring(0, ind);
              cResCodeT := cRes.Substring(ind+1);
              {$else}
              cResCodeS := Copy(cRes, 1, ind);
              cResCodeT := Copy(cRes, ind+1, length(cRes)-ind);
              {$IFEND}
              if cResCodeS = '' then raise Exception.Create('server result code was empty');
              if not TryStrToInt(cResCodeS, cResCode) then raise Exception.Create('server result contained no result code');
              case cResCode of
                NXLOGPTCP_OK                            :
                begin
                  // Alles gut...
                end;
                NXLOGPTCP_BADREQUEST                    :
                begin
                  raise Exception.Create('NXLOGPTCP_BADREQUEST'+':'+cResCodeT);
                end;
                NXLOGPTCP_UNAUTHORIZED                  :
                begin
                  raise Exception.Create('NXLOGPTCP_UNAUTHORIZED'+':'+cResCodeT);
                end;
                NXLOGPTCP_PAYMENTREQUIRED               :
                begin
                  raise Exception.Create('NXLOGPTCP_PAYMENTREQUIRED'+':'+cResCodeT);
                end;
                NXLOGPTCP_FORBIDDEN                     :
                begin
                  raise Exception.Create('NXLOGPTCP_FORBIDDEN'+':'+cResCodeT);
                end;
                NXLOGPTCP_NOTFOUND                      :
                begin
                  raise Exception.Create('NXLOGPTCP_NOTFOUND'+':'+cResCodeT);
                end;
                NXLOGPTCP_METHODNOTALLOWED              :
                begin
                  raise Exception.Create('NXLOGPTCP_METHODNOTALLOWED'+':'+cResCodeT);
                end;
                NXLOGPTCP_INTERNALSERVERERROR           :
                begin
                  raise Exception.Create('NXLOGPTCP_INTERNALSERVERERROR'+':'+cResCodeT);
                end;
                NXLOGPTCP_NOTIMPLEMENTED                :
                begin
                  raise Exception.Create('NXLOGPTCP_NOTIMPLEMENTED'+':'+cResCodeT);
                end;
                NXLOGPTCP_SERVICETEMPORARYUNAVAILABLE   :
                begin
                  raise Exception.Create('NXLOGPTCP_SERVICETEMPORARYUNAVAILABLE'+':'+cResCodeT);
                end;
              else
                raise Exception.Create('unknown result code in server result');
              end;
            end else
            begin
              if cRes = '' then raise Exception.Create('server result was empty');
              if not TryStrToInt(cRes, cResCode) then raise Exception.Create('server result contained no result code');
              case cResCode of
                NXLOGPTCP_OK                            :
                begin
                  // Alles gut...
                  cCurrentTry := 0;
                end;
                NXLOGPTCP_BADREQUEST                    :
                begin
                  raise Exception.Create('NXLOGPTCP_BADREQUEST');
                end;
                NXLOGPTCP_UNAUTHORIZED                  :
                begin
                  raise Exception.Create('NXLOGPTCP_UNAUTHORIZED');
                end;
                NXLOGPTCP_PAYMENTREQUIRED               :
                begin
                  raise Exception.Create('NXLOGPTCP_PAYMENTREQUIRED');
                end;
                NXLOGPTCP_FORBIDDEN                     :
                begin
                  raise Exception.Create('NXLOGPTCP_FORBIDDEN');
                end;
                NXLOGPTCP_NOTFOUND                      :
                begin
                  raise Exception.Create('NXLOGPTCP_NOTFOUND');
                end;
                NXLOGPTCP_METHODNOTALLOWED              :
                begin
                  raise Exception.Create('NXLOGPTCP_METHODNOTALLOWED');
                end;
                NXLOGPTCP_INTERNALSERVERERROR           :
                begin
                  raise Exception.Create('NXLOGPTCP_INTERNALSERVERERROR');
                end;
                NXLOGPTCP_NOTIMPLEMENTED                :
                begin
                  raise Exception.Create('NXLOGPTCP_NOTIMPLEMENTED');
                end;
                NXLOGPTCP_SERVICETEMPORARYUNAVAILABLE   :
                begin
                  raise Exception.Create('NXLOGPTCP_SERVICETEMPORARYUNAVAILABLE');
                end;
              else
                raise Exception.Create('unknown result code in server result');
              end;
            end;
          except
            on e : exception do
            begin
              inc(cCurrentTry);
              sleep(500);
            end;
          end;
        until (cCurrentTry = 0) or (cCurrentTry > fRetryCount);
        if cCurrentTry > fRetryCount then
        begin
          // Das waren also mehrere Fehler hintereinander...
          _disconnect;
          sleep(1500);
        end;
      end else
      begin
        if fTCP <> nil then
        begin
          {$IF CompilerVersion >= 24.0 }
            if (fDisconnectSeconds > 0) and (System.DateUtils.SecondsBetween(cLastLogMessage, Now) > fDisconnectSeconds) then
          {$ELSE}
            if (fDisconnectSeconds > 0) and (DateUtils.SecondsBetween(cLastLogMessage, Now) > fDisconnectSeconds) then
          {$IFEND}
          begin
            cLastLogMessage := Now;
            try
              if (fTCP <> nil) and fTCP.Connected then
              begin
                _disconnect;
              end;
            except
            end;
          end;
        end;
        sleep(1000);
      end;
    end;
  finally
    if fTCP <> nil then
    begin
      try
        if fTCP.Connected then
        begin
          fTCP.Disconnect;
        end;
      except
      end;
    end;
    FreeAndNil(fTCP);
  end;
end;


{ **************************************************************************** }
{ ***** TNxLogAppenderTCP **************************************************** }
{ **************************************************************************** }
constructor TNxLogAppenderTCP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  initLoggerFormatSettings(fFormatSettings);
  fMaxEntries     := TIdThreadSafeInteger.Create;
  fMaxEntries.Value := IC_DEFAULTMAXTCPENTRIES;
  fHostOrIP       := '';
  fPort           := 0;
  fRetryCount     := 10;
  fBuffer         := TIdThreadSafeStringList.Create;
  fWorker         := nil;
  fFormater       := TNxLogFormaterTCP.Create;
  fLevelFilter    := TNxLoggerMessageFilterLevelSet.Create;
  fLevelFilter.unsetLevel(NXLL_TRACE);
  fLevelFilter.unsetLevel(NXLL_DEBUG);
  fLevelFilter.unsetLevel(NXLL_INFO);
  fUserFilter     := nil;
  fUsername       := '';
  fPassword       := '';
  fDisconnectSeconds  := 0;
end;

destructor TNxLogAppenderTCP.Destroy;
begin
  if isStarted then
  begin
    stop;
  end;
  clearBuffer;
  FreeAndNil(fBuffer);
  FreeAndNil(fFormater);
  fUserFilter     := nil;
  FreeAndNil(fLevelFilter);
  FreeAndNil(fMaxEntries);
  inherited Destroy;
end;

function  TNxLogAppenderTCP.consumeEntry : String;
var lst : TStringList;
begin
  lst := fBuffer.Lock;
  try
    if lst.Count > 0 then
    begin
      result  := lst[0];
      lst.Delete(0);
    end else
    begin
      result  := '';
    end;
  finally
    fBuffer.Unlock;
  end;
end;

procedure TNxLogAppenderTCP.setMachineIdent(aIdent : String);
begin
  if fMachineIdent <> aIdent then
  begin
    fMachineIdent := aIdent;
    if fFormater <> nil then
    begin
      fFormater.MachineIdent  := fMachineIdent;
    end;
    if fWorker <> nil then
    begin
      fWorker.MachineIdent  := fMachineIdent;
    end;
  end;
end;

procedure TNxLogAppenderTCP.setMaxEntries(aValue : Integer);
begin
  fMaxEntries.Value := aValue;
end;

function  TNxLogAppenderTCP.getMaxEntries : Integer;
begin
  result  := fMaxEntries.Value;
end;

function  TNxLogAppenderTCP.getCurrentEntries : Integer;
var s   : TStringList;
begin
  s := fBuffer.Lock;
  try
    result  := s.Count;
  finally
    fBuffer.Unlock;
  end;
end;

function  TNxLogAppenderTCP.isStarted : Boolean;
begin
  result  := fWorker <> nil;
end;

procedure TNxLogAppenderTCP.start;
begin
  if not isStarted then
  begin
    fWorker := TNxLogAppenderTCPThread.Create(true);
    fWorker.FormatSettings  := self.fFormatSettings;
    fWorker.MachineIdent    := self.fMachineIdent;
    fWorker.HostOrIP        := self.fHostOrIP;
    fWorker.Port            := self.fPort;
    fWorker.RetryCount      := self.fRetryCount;
    fWorker.Username        := self.fUsername;
    fWorker.Password        := self.fPassword;
    fWorker.Adapter         := self;
    fWorker.DisconnectSeconds := fDisconnectSeconds;
    fWorker.Start;
  end;
end;

procedure TNxLogAppenderTCP.stop;
begin
  if isStarted then
  begin
    fWorker.Terminate;
    fWorker.WaitFor;
    fWorker.Adapter := nil;
    FreeAndNil(fWorker);
  end;
end;

procedure TNxLogAppenderTCP.clearBuffer;
begin
  if fBuffer <> nil then fBuffer.Clear;
end;

procedure TNxLogAppenderTCP.append(const aEvent : TNxLoggerMessage);
var zs    : String;

  procedure _doAppend;
  var _strs   : TStringList;
      _maxc   : Integer;
      _wasbo  : Boolean;
  begin
    _maxc := fMaxEntries.Value;
    _strs := fBuffer.Lock;
    try
      if _strs.Count < _maxc then
      begin
        // Alles OK...
        zs  := fFormater.formatMessage(aEvent);
        _strs.Add(zs);
        _wasbo  := false;
      end else
      begin
        // Pufferüberlauf...
        _wasbo  := true;
      end;
    finally
      fBuffer.Unlock;
    end;
    if _wasbo then
    begin
      if assigned(fOnOverflow) then
      begin
        fOnOverflow(_maxc);
      end;
    end;
  end;

begin
  if not isStarted then
  begin
    start;
  end;
  if fLevelFilter.match(aEvent) then
  begin
    if fUserFilter <> nil then
    begin
      if fUserFilter.match(aEvent) then
      begin
        _doAppend;
      end;
    end else
    begin
      _doAppend;
    end;
  end;
end;

{ **************************************************************************** }
{ ***** TNxLoggerCollectionItem ********************************************** }
{ **************************************************************************** }
constructor TNxLoggerCollectionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  fAppender := nil;
end;

destructor TNxLoggerCollectionItem.Destroy;
begin
  fAppender := nil;
  inherited Destroy;
end;

procedure TNxLoggerCollectionItem.append(const aEvent : TNxLoggerMessage);
begin
  if fAppender <> nil then
  begin
    fAppender.append(aEvent);
  end;
end;



{ **************************************************************************** }
{ ***** TNxLogger ************************************************************ }
{ **************************************************************************** }
constructor TNxLogger.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fAppenders  := TNxLoggerCollection.Create(TNxLoggerCollectionItem);
  fCSLog  := TCriticalSection.Create;
  fCSCurrentLevel := TCriticalSection.Create;
  initLoggerFormatSettings(fLogFormatSettings);
  fApplicationID    := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  fInstanceID       := internalGetCurrentProcessId;
  fUserIdent        := getUserFromOS;
  fLanguage         := '';
  fCurrentLevel     := NXLL_INFO;
  fFilters  := TFMThreadSafeList.Create;
  fOnAppend := nil;
  fOnLog    := nil;
end;

destructor  TNxLogger.Destroy;
begin
  fOnAppend := nil;
  fOnLog    := nil;
  FreeAndNil(fCSCurrentLevel);
  FreeAndNil(fCSLog);
  FreeAndNil(fAppenders);
  FreeAndNil(fFilters);
  inherited Destroy;
end;

class function TNxLogger.isImportantLogLevel(const aRelativeBaseLevel, aLevelToTest : TNxLoggerLevel) : Boolean;
begin
  result  := aLevelToTest >= aRelativeBaseLevel;
end;

function  TNxLogger.matchFilters(const aEvent : TNxLoggerMessage) : Boolean;
var lst : TFMList;
    i   : Integer;
    flt : TNxLoggerMessageFilter;
begin
  result  := true;
  lst := fFilters.LockList;
  try
    for i := 0 to lst.Count - 1 do
    begin
      flt := TNxLoggerMessageFilter(lst[i]);
      result  := result and flt.match(aEvent);
    end;
  finally
    fFilters.UnlockList;
  end;
end;

function  TNxLogger.getMachineIdent : String;
begin
  result  := GetLocalComputerName;
end;

procedure TNxLogger.initializeFileLogging(const aApplicationId, aLogDirectory, aBaseFilename : String; aRetryCount : Integer = 10; aStrategy : TNxLogAppenderFileStrategy = NXLFS_NEWFILES);
var i     : TCollectionItem;
    x     : TNxLoggerCollectionItem;
    found : Boolean;
    fapp  : TNxLogAppenderFile;
begin
  found := false;
  fCSLog.Enter;
  try
    for i in fAppenders do
    begin
      x := i as TNxLoggerCollectionItem;
      if x.fAppender is TNxLogAppenderFile then
      begin
        fapp  := (x.fAppender as TNxLogAppenderFile);
        fapp.fDirectory    := aLogDirectory;
        fapp.fFilenameBase := aBaseFilename;
        fapp.fRetryCount   := aRetryCount;
        fapp.Strategy      := aStrategy;
        found := true;
      end;
    end;
    if not found then
    begin
      fapp  := TNxLogAppenderFile.Create(self);
      fapp.fDirectory    := aLogDirectory;
      fapp.fFilenameBase := aBaseFilename;
      fapp.fRetryCount   := aRetryCount;
      fapp.Strategy      := aStrategy;
      addAppender(fapp);
    end;
    fApplicationID := aApplicationId;
  finally
    fCSLog.Leave;
  end;
end;

procedure TNxLogger.initializeServerTCPLogging(const aApplicationId, aHostOrIP : String; aPort : Integer; aMachineIdent : String = ''; aRetryCount : Integer = 10; aUsername : String = ''; aPassword : String = ''; aLevelFilter : TNxLoggerLevelSet = [NXLL_FATAL, NXLL_ERROR, NXLL_WARN]; aAutoDisconnectSeconds : Int64 = 0);
var i     : TCollectionItem;
    x     : TNxLoggerCollectionItem;
    found : Boolean;
    fapp  : TNxLogAppenderTCP;
begin
  found := false;
  fCSLog.Enter;
  try
    if aMachineIdent = '' then
    begin
      aMachineIdent := getMachineIdent;
    end;
    for i in fAppenders do
    begin
      x := i as TNxLoggerCollectionItem;
      if x.fAppender is TNxLogAppenderTCP then
      begin
        fapp  := (x.fAppender as TNxLogAppenderTCP);
        fapp.fMachineIdent  := aMachineIdent;
        fapp.fHostOrIP      := aHostOrIP;
        fapp.fPort          := aPort;
        fapp.fRetryCount    := aRetryCount;
        fapp.fUsername      := aUsername;
        fapp.fPassword      := aPassword;
        fapp.fLevelFilter.setLevelSet(aLevelFilter);
        fapp.DisconnectSeconds := aAutoDisconnectSeconds;
        found := true;
      end;
    end;
    if not found then
    begin
      fapp  := TNxLogAppenderTCP.Create(self);
      fapp.setMachineIdent(aMachineIdent);
      fapp.fHostOrIP      := aHostOrIP;
      fapp.fPort          := aPort;
      fapp.fRetryCount    := aRetryCount;
      fapp.fUsername      := aUsername;
      fapp.fPassword      := aPassword;
      fapp.fLevelFilter.setLevelSet(aLevelFilter);
      fapp.DisconnectSeconds := aAutoDisconnectSeconds;
      addAppender(fapp);
    end;
    fApplicationID := aApplicationId;
  finally
    fCSLog.Leave;
  end;
end;

class procedure TNxLogger.doneLogging;
begin
  if fDefaultLogger <> nil then
  begin
    fDefaultLogger.clearAppenders;
  end;
  FreeAndNil(fDefaultLogger);
end;

procedure TNxLogger.log(const aEvent : TNxLoggerMessage);
var i : TCollectionItem;
    x : TNxLoggerCollectionItem;
begin
  if isImportantLogLevel(getCurrentLevel, aEvent.LogLevel) and matchFilters(aEvent) then
  begin
    fCSLog.Enter;
    try
      for i in fAppenders do
      begin
        x := i as TNxLoggerCollectionItem;
        x.append(aEvent);
      end;
    finally
      fCSLog.Leave;
    end;
    if assigned(fOnAppend) then
    begin
      fOnAppend(self, aEvent);
    end;
  end;
  if assigned(fOnLog) then
  begin
    fOnLog(self, aEvent);
  end;
end;

procedure TNxLogger.log(const aLevel : TNxLoggerLevel; const aMessage : String);
var cMessage  : TNxLoggerMessage;
begin
  cMessage  := TNxLoggerMessage.Create(fApplicationID, fInstanceId, fUserIdent, aLevel, aMessage);
  log(cMessage);
  FreeAndNil(cMessage);
end;

procedure TNxLogger.log(const aLevel : TNxLoggerLevel; const aModule, aMessage : String);
var cMessage  : TNxLoggerMessage;
begin
  cMessage  := TNxLoggerMessage.Create(fApplicationID, fInstanceId, fUserIdent, aLevel, aModule, aMessage, TNxLogFormater.convertCategoryToString(NXLCAT_NONE), fLanguage, nil);
  log(cMessage);
  FreeAndNil(cMessage);
end;

procedure TNxLogger.log(const aLevel : TNxLoggerLevel; const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception);
var cMessage  : TNxLoggerMessage;
begin
  cMessage  := TNxLoggerMessage.Create(fApplicationID, fInstanceId, fUserIdent, aLevel, aModule, aMessage, TNxLogFormater.convertCategoryToString(aCategory), fLanguage, aException);
  log(cMessage);
  FreeAndNil(cMessage);
end;

procedure TNxLogger.log(const aLevel : TNxLoggerLevel; const aModule, aMessage : String; const aCategoryAsString : String; const aException : exception);
var cMessage  : TNxLoggerMessage;
begin
  cMessage  := TNxLoggerMessage.Create(fApplicationID, fInstanceId, fUserIdent, aLevel, aModule, aMessage, aCategoryAsString, fLanguage, aException);
  log(cMessage);
  FreeAndNil(cMessage);
end;

procedure TNxLogger.log(const aLevel : TNxLoggerLevel; const aModule, aMessage : String; const aCategoryAsString, aLanguage : String; const aException : exception);
var cMessage  : TNxLoggerMessage;
begin
  cMessage  := TNxLoggerMessage.Create(fApplicationID, fInstanceId, fUserIdent, aLevel, aModule, aMessage, aCategoryAsString, aLanguage, aException);
  log(cMessage);
  FreeAndNil(cMessage);
end;

procedure TNxLogger.fatal(const aMessage : String);
begin
  log(NXLL_FATAL, aMessage);
end;

procedure TNxLogger.fatal(const aModule, aMessage : String);
begin
  log(NXLL_FATAL, aModule, aMessage, NXLCAT_NONE, nil);
end;

procedure TNxLogger.fatal(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil);
begin
  log(NXLL_FATAL, aModule, aMessage, aCategory, aException);
end;

procedure TNxLogger.error(const aMessage : String);
begin
  log(NXLL_ERROR, aMessage);
end;

procedure TNxLogger.error(const aModule, aMessage : String);
begin
  log(NXLL_ERROR, aModule, aMessage, NXLCAT_NONE, nil);
end;

procedure TNxLogger.error(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil);
begin
  log(NXLL_ERROR, aModule, aMessage, aCategory, aException);
end;

procedure TNxLogger.warn(const aMessage : String);
begin
  log(NXLL_WARN, aMessage);
end;

procedure TNxLogger.warn(const aModule, aMessage : String);
begin
  log(NXLL_WARN, aModule, aMessage, NXLCAT_NONE, nil);
end;

procedure TNxLogger.warn(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil);
begin
  log(NXLL_WARN, aModule, aMessage, aCategory, aException);
end;

procedure TNxLogger.info(const aMessage : String);
begin
  log(NXLL_INFO, aMessage);
end;

procedure TNxLogger.info(const aModule, aMessage : String);
begin
  log(NXLL_INFO, aModule, aMessage, NXLCAT_NONE, nil);
end;

procedure TNxLogger.info(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil);
begin
  log(NXLL_INFO, aModule, aMessage, aCategory, aException);
end;

procedure TNxLogger.debug(const aMessage : String);
begin
  log(NXLL_DEBUG, aMessage);
end;

procedure TNxLogger.debug(const aModule, aMessage : String);
begin
  log(NXLL_DEBUG, aModule, aMessage, NXLCAT_NONE, nil);
end;

procedure TNxLogger.debug(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil);
begin
  log(NXLL_DEBUG, aModule, aMessage, aCategory, aException);
end;

procedure TNxLogger.trace(const aMessage : String);
begin
  log(NXLL_TRACE, aMessage);
end;

procedure TNxLogger.trace(const aModule, aMessage : String);
begin
  log(NXLL_TRACE, aModule, aMessage, NXLCAT_NONE, nil);
end;

procedure TNxLogger.trace(const aModule, aMessage : String; const aCategory : TNxLoggerCategory; const aException : exception = nil);
begin
  log(NXLL_TRACE, aModule, aMessage, aCategory, aException);
end;

function  TNxLogger.getAppenderCount : Integer;
begin
  result  := fAppenders.Count;
end;

function  TNxLogger.getAppender(aIndex : Integer) : TNxLogAppender;
begin
  if (aIndex >= 0) and (aIndex < fAppenders.Count) then
  begin
    result  := TNxLoggerCollectionItem(fAppenders.Items[aIndex]).Appender;
  end else
  begin
    result  := nil;
  end;
end;

procedure TNxLogger.addAppender(aAppender : TNxLogAppender);
begin
  TNxLoggerCollectionItem(fAppenders.Add).Appender  := aAppender;
end;

function  TNxLogger.deleteAppender(aIndex : Integer) : TNxLogAppender;
begin
  if (aIndex >= 0) and (aIndex < fAppenders.Count) then
  begin
    result  := TNxLoggerCollectionItem(fAppenders.Items[aIndex]).Appender;
    fAppenders.Delete(aIndex);
  end else
  begin
    result  := nil;
  end;
end;

procedure TNxLogger.clearAppenders;
begin
  fAppenders.Clear;
end;

function  TNxLogger.getCurrentLevel : TNxLoggerLevel;
begin
  fCSCurrentLevel.Enter;
  try
    result  := fCurrentLevel;
  finally
    fCSCurrentLevel.Leave;
  end;
end;

procedure TNxLogger.setCurrentLevel(aLevel : TNxLoggerLevel);
begin
  fCSCurrentLevel.Enter;
  try
    fCurrentLevel := aLevel;
  finally
    fCSCurrentLevel.Leave;
  end;
end;

function  TNxLogger.isTrace : Boolean;
begin
  result  := CurrentLevel <= NXLL_TRACE;
end;

function  TNxLogger.isDebug : Boolean;
begin
  result  := CurrentLevel <= NXLL_DEBUG;
end;

function  TNxLogger.isInfo  : Boolean;
begin
  result  := CurrentLevel <= NXLL_INFO;
end;

function  TNxLogger.isWarn  : Boolean;
begin
  result  := CurrentLevel <= NXLL_WARN;
end;

function  TNxLogger.isError : Boolean;
begin
  result  := CurrentLevel <= NXLL_ERROR;
end;

function  TNxLogger.isFatal : Boolean;
begin
  result  := CurrentLevel <= NXLL_FATAL;
end;



{ **************************************************************************** }
{ ***** TNxLoggerMessage ***************************************************** }
{ **************************************************************************** }
constructor TNxLoggerMessage.Create(const aApplicationId, aInstanceId, aLogUser : String; aLevel : TNxLoggerLevel; const aMessage : String);
begin
  self.Create(aApplicationId, aInstanceId, aLogUser, aLevel, '', aMessage, '', '', nil);
end;

constructor TNxLoggerMessage.Create(const aApplicationId, aInstanceId, aLogUser : String; aLevel : TNxLoggerLevel; const aModule, aMessage : String; const aCategory, aLanguage : String; const aException : Exception);
begin
  self.Create(aApplicationId, aInstanceId, aLogUser, aLevel, aModule, aMessage, '', '', '', nil);
end;

constructor TNxLoggerMessage.Create(const aApplicationId, aInstanceId, aLogUser : String; aLevel : TNxLoggerLevel; const aModule, aMessage : String; const aCategory, aLanguage, aThreadId : String; const aException : Exception);
begin
  inherited Create;
  fApplicationId  := aApplicationId;
  fInstanceId     := aInstanceId;
  fLogUser        := aLogUser;
  if aThreadId = '' then
  begin
    fThreadId     := internalGetCurrentThreadId;
  end else
  begin
    fThreadId     := aThreadId;
  end;
  fLevel          := aLevel;
  fCategory       := aCategory;
  fModule         := aModule;
  fMessage        := aMessage;
  fTimestamp      := nowUTC;
  fLanguage       := aLanguage;
  fException      := aException;
  if fException <> nil then
  begin
    fExceptionClassName := fException.ClassName;
  end else
  begin
    fExceptionClassName := '';
  end;
end;

destructor TNxLoggerMessage.Destroy;
begin
  inherited Destroy;
end;

procedure TNxLoggerMessage.AssignTo(Dest: TPersistent);
var d : TNxLoggerMessage;
begin
  if Dest is TNxLoggerMessage then
  begin
    d := Dest as TNxLoggerMessage;
    d.fApplicationId      := self.fApplicationId;
    d.fInstanceId         := self.fInstanceId;
    d.fLogUser            := self.fLogUser;
    d.fLevel              := self.fLevel;
    d.fCategory           := self.fCategory;
    d.fModule             := self.fModule;
    d.fMessage            := self.fMessage;
    d.fTimestamp          := self.fTimestamp;
    d.fException          := self.fException;
    d.fExceptionClassName := self.fExceptionClassName;
    d.fLanguage           := self.fLanguage;
    d.fStackTrace         := self.fStackTrace;
    d.fThreadId           := self.fThreadId;
  end else
  begin
    inherited Assign(Dest);
  end;
end;

function TNxLoggerMessage.getApplicationId : String;
begin
  result  := fApplicationId;
end;

function TNxLoggerMessage.getInstanceId : String;
begin
  result  := fInstanceId;
end;

function TNxLoggerMessage.getLogUser : String;
begin
  result  := fLogUser;
end;

function TNxLoggerMessage.getThreadId : String;
begin
  result  := fThreadId;
end;

function TNxLoggerMessage.getLevel : TNxLoggerLevel;
begin
  result  := fLevel;
end;

function TNxLoggerMessage.getCategory : String;
begin
  result  := fCategory;
end;

function TNxLoggerMessage.getModule : String;
begin
  result  := fModule;
end;

function TNxLoggerMessage.getMessage : String;
begin
  result  := fMessage;
end;

function TNxLoggerMessage.getTimestamp : TDateTime;
begin
  result  := fTimestamp;
end;

function TNxLoggerMessage.getException : Exception;
begin
  result  := fException;
end;

function TNxLoggerMessage.getLanguage : String;
begin
  result  := fLanguage;
end;

function TNxLoggerMessage.getExceptionClassName : String;
begin
  result  := fExceptionClassName;
end;

function TNxLoggerMessage.getStackTrace : String;
begin
  result  := fStackTrace;
end;

procedure TNxLoggerMessage.setApplicationId(aValue : String);
begin
  fApplicationId  := aValue;
end;

procedure TNxLoggerMessage.setInstanceId(aValue : String);
begin
  fInstanceId  := aValue;
end;

procedure TNxLoggerMessage.setLogUser(aValue : String);
begin
  fLogUser  := aValue;
end;

procedure TNxLoggerMessage.setThreadId(aValue : String);
begin
  fThreadId := aValue;
end;

procedure TNxLoggerMessage.setLevel(aValue  : TNxLoggerLevel);
begin
  fLevel  := aValue;
end;

procedure TNxLoggerMessage.setCategory(aValue : String);
begin
  fCategory := aValue;
end;

procedure TNxLoggerMessage.setModule(aValue : String);
begin
  fModule := aValue;
end;

procedure TNxLoggerMessage.setMessage(aValue  : String);
begin
  fMessage  := aValue;
end;

procedure TNxLoggerMessage.setTimestamp(aValue  : TDateTime);
begin
  fTimestamp  := aValue;
end;

procedure TNxLoggerMessage.setException(aValue  : Exception);
begin
  fException  := aValue;
end;

procedure TNxLoggerMessage.setLanguage(aValue : String);
begin
  fLanguage := aValue;
end;

procedure TNxLoggerMessage.setExceptionClassName(aValue : String);
begin
  fExceptionClassName := aValue;
end;

procedure TNxLoggerMessage.setStackTrace(aValue : String);
begin
  fStackTrace := aValue;
end;


{ **************************************************************************** }
{ ***** TNxLoggerMessageFilterLevelSet *************************************** }
{ **************************************************************************** }
constructor TNxLoggerMessageFilterLevelSet.Create;
begin
  inherited Create;
  fLevelSet := [NXLL_TRACE, NXLL_DEBUG, NXLL_INFO, NXLL_WARN, NXLL_ERROR, NXLL_FATAL ];
end;

destructor TNxLoggerMessageFilterLevelSet.Destroy;
begin
  inherited Destroy;
end;

procedure TNxLoggerMessageFilterLevelSet.AssignTo(Dest: TPersistent);
begin
  if Dest is TNxLoggerMessageFilterLevelSet then
  begin
    (Dest as TNxLoggerMessageFilterLevelSet).fLevelSet := fLevelSet;
  end;
end;

function  TNxLoggerMessageFilterLevelSet.match(aMessage : TNxLoggerMessage) : Boolean;
begin
  result  := false;
  if aMessage <> nil then
  begin
    if aMessage.LogLevel in fLevelSet then
    begin
      result  := true;
    end;
  end;
end;

procedure TNxLoggerMessageFilterLevelSet.saveToStream(aStream : TStream);
var wr  : TWriter;
    i   : TNxLoggerLevel;
    zs  : String;
begin
  wr  := TWriter.Create(aStream, 1024);
  try
    wr.WriteListBegin;
    for i := Low(TNxLoggerLevel) to High(TNxLoggerLevel) do
    begin
      {$IF CompilerVersion >= 24.0 }
        zs  := System.TypInfo.GetEnumName(System.TypeInfo(TNxLoggerLevel), Integer(i));
      {$ELSE}
        zs  := TypInfo.GetEnumName(System.TypeInfo(TNxLoggerLevel), Integer(i));
      {$IFEND}
      wr.WriteIdent(zs);
      if TNxLoggerLevel(i) in fLevelSet then
      begin
        wr.WriteBoolean(true);
      end else
      begin
        wr.WriteBoolean(false);
      end;
    end;
    wr.WriteListEnd;
    wr.FlushBuffer;
  finally
    FreeAndNil(wr);
  end;
end;

procedure TNxLoggerMessageFilterLevelSet.loadFromStream(aStream : TStream);
var rd  : TReader;
    i   : TNxLoggerLevel;
    zs  : String;
begin
  rd  := TReader.Create(aStream, 1024);
  try
    fLevelSet := [];
    rd.ReadListBegin;
    for i := Low(TNxLoggerLevel) to High(TNxLoggerLevel) do
    begin
      zs  := rd.ReadIdent;
      if rd.ReadBoolean then
      begin
        fLevelSet := fLevelSet + [i];
      end else
      begin
      end;
    end;
    rd.ReadListEnd;
  finally
    FreeAndNil(rd);
  end;
end;

procedure TNxLoggerMessageFilterLevelSet.setLevel(aLevel : TNxLoggerLevel);
begin
  fLevelSet := fLevelSet + [aLevel];
end;

procedure TNxLoggerMessageFilterLevelSet.unsetLevel(aLevel : TNxLoggerLevel);
begin
  fLevelSet := fLevelSet - [aLevel];
end;

procedure TNxLoggerMessageFilterLevelSet.setLevelAndAbove(aLevel : TNxLoggerLevel);
var cl  : TNxLoggerLevel;
begin
  for cl := low(TNxLoggerLevel) to high(TNxLoggerLevel) do
  begin
    if cl < aLevel then
    begin
      unsetLevel(cl);
    end else
    begin
      setLevel(cl);
    end;
  end;
end;

procedure TNxLoggerMessageFilterLevelSet.setLevelSet(aLevelSet : TNxLoggerLevelSet);
begin
  fLevelSet := aLevelSet;
end;

function  TNxLoggerMessageFilterLevelSet.isLevelInSet(aLevel : TNxLoggerLevel) : Boolean;
begin
  result  := aLevel in fLevelSet;
end;

procedure TNxLoggerMessageFilterLevelSet.reset;
begin
  fLevelSet := [NXLL_TRACE, NXLL_DEBUG, NXLL_INFO, NXLL_WARN, NXLL_ERROR, NXLL_FATAL ];
end;

{ **************************************************************************** }
{ ***** TNxLoggerMessageFilterByText ***************************************** }
{ **************************************************************************** }
constructor TNxLoggerMessageFilterByText.Create;
begin
  inherited Create;
  fText  := '';
end;

destructor TNxLoggerMessageFilterByText.Destroy;
begin
  inherited Destroy;
end;

procedure TNxLoggerMessageFilterByText.AssignTo(Dest: TPersistent);
begin
  if Dest is TNxLoggerMessageFilterByText then
  begin
    (Dest as TNxLoggerMessageFilterByText).fText := fText;
  end;
end;

procedure TNxLoggerMessageFilterByText.saveToStream(aStream : TStream);
var wr  : TWriter;
begin
  wr  := TWriter.Create(aStream, 1024);
  try
    wr.WriteString('FilterText');
    wr.WriteString(fText);
    wr.FlushBuffer;
  finally
    FreeAndNil(wr);
  end;
end;

procedure TNxLoggerMessageFilterByText.loadFromStream(aStream : TStream);
var rd  : TReader;
    zs  : String;
begin
  rd  := TReader.Create(aStream, 1024);
  try
    zs  := rd.ReadString;
    if lowercase(zs) = 'filtertext' then
    begin
      fText := rd.ReadString;
    end else
    begin
      raise Exception.Create('parse error: "FilterText" expected!');
    end;
  finally
    FreeAndNil(rd);
  end;
end;

procedure TNxLoggerMessageFilterByText.setText(aValue : String);
begin
  fText  := aValue;
end;

function  TNxLoggerMessageFilterByText.getText : String;
begin
  result  := fText;
end;

procedure TNxLoggerMessageFilterByText.reset;
begin
  fText  := '';
end;

{ **************************************************************************** }
{ ***** TNxLoggerMessageFilterModuleEquals *********************************** }
{ **************************************************************************** }
function  TNxLoggerMessageFilterModuleEquals.match(aMessage : TNxLoggerMessage) : Boolean;
begin
  result  := false;
  if aMessage <> nil then
  begin
    if lowercase(aMessage.LogModule) = lowercase(fText)  then
    begin
      result  := true;
    end;
  end;
end;

{ **************************************************************************** }
{ ***** TNxLoggerMessageFilterModuleStarting ********************************* }
{ **************************************************************************** }
function  TNxLoggerMessageFilterModuleStarting.match(aMessage : TNxLoggerMessage) : Boolean;
begin
  result  := false;
  if aMessage <> nil then
  begin
    if Pos(lowercase(fText), lowercase(aMessage.LogModule)) = 1 then
    begin
      result  := true;
    end;
  end;
end;

{ **************************************************************************** }
{ ***** TNxLoggerMessageFilterModuleContains ********************************* }
{ **************************************************************************** }
function  TNxLoggerMessageFilterModuleContains.match(aMessage : TNxLoggerMessage) : Boolean;
begin
  result  := false;
  if aMessage <> nil then
  begin
    if Pos(lowercase(fText), lowercase(aMessage.LogModule)) > 0 then
    begin
      result  := true;
    end;
  end;
end;

{ **************************************************************************** }
{ ***** TNxLoggerMessageFilterMessageEquals ********************************** }
{ **************************************************************************** }
function  TNxLoggerMessageFilterMessageEquals.match(aMessage : TNxLoggerMessage) : Boolean;
begin
  result  := false;
  if aMessage <> nil then
  begin
    if lowercase(aMessage.LogMessage) = lowercase(fText)  then
    begin
      result  := true;
    end;
  end;
end;

{ **************************************************************************** }
{ ***** TNxLoggerMessageFilterMessageStarting ******************************** }
{ **************************************************************************** }
function  TNxLoggerMessageFilterMessageStarting.match(aMessage : TNxLoggerMessage) : Boolean;
begin
  result  := false;
  if aMessage <> nil then
  begin
    if Pos(lowercase(fText), lowercase(aMessage.LogMessage)) = 1 then
    begin
      result  := true;
    end;
  end;
end;

{ **************************************************************************** }
{ ***** TNxLoggerMessageFilterMessageContains ******************************** }
{ **************************************************************************** }
function  TNxLoggerMessageFilterMessageContains.match(aMessage : TNxLoggerMessage) : Boolean;
begin
  result  := false;
  if aMessage <> nil then
  begin
    if Pos(lowercase(fText), lowercase(aMessage.LogMessage)) > 0 then
    begin
      result  := true;
    end;
  end;
end;


{ **************************************************************************** }
{ ***** TNxLoggerMessageFilterMachine (Baseclass) **************************** }
{ **************************************************************************** }
constructor TNxLoggerMessageFilterMachine.Create;
begin
  inherited Create;
  fOnMachineIdentRequired := nil;
end;

destructor TNxLoggerMessageFilterMachine.Destroy;
begin
  fOnMachineIdentRequired := nil;
  inherited Destroy;
end;

procedure TNxLoggerMessageFilterMachine.AssignTo(Dest: TPersistent);
begin
  if Dest is TNxLoggerMessageFilterMachine then
  begin
    (Dest as TNxLoggerMessageFilterMachine).fOnMachineIdentRequired := fOnMachineIdentRequired;
  end;
end;

{ **************************************************************************** }
{ ***** TNxLoggerMessageFilterMachineEquals ********************************** }
{ **************************************************************************** }
function  TNxLoggerMessageFilterMachineEquals.matchMachineIdent(aMessage : TNxLoggerMessage; const aMachineIdent : String) : Boolean;
begin
  result  := false;
  if aMachineIdent <> '' then
  begin
    if lowercase( aMachineIdent) = lowercase(fText) then
    begin
      result := true;
    end;
  end else
  begin
    result := self.match(aMessage);
  end;
end;

function  TNxLoggerMessageFilterMachineEquals.match(aMessage : TNxLoggerMessage) : Boolean;
var cs  : String;
begin
  result  := false;
  if aMessage <> nil then
  begin
    if assigned(fOnMachineIdentRequired) then
    begin
      cs  := '';
      fOnMachineIdentRequired(self, aMessage, cs);
      if lowercase(fText) = lowercase(cs) then
      begin
        result  := true;
      end;
    end;
  end;
end;

{ **************************************************************************** }
{ ***** TNxLoggerMessageFilterMachineStarting ******************************** }
{ **************************************************************************** }
function  TNxLoggerMessageFilterMachineStarting.matchMachineIdent(aMessage : TNxLoggerMessage; const aMachineIdent : String) : Boolean;
begin
  result  := false;
  if aMachineIdent <> '' then
  begin
    if Pos(lowercase(fText), lowercase(aMachineIdent)) = 1 then
    begin
      result  := true;
    end;
  end else
  begin
    result := self.match(aMessage);
  end;
end;

function  TNxLoggerMessageFilterMachineStarting.match(aMessage : TNxLoggerMessage) : Boolean;
var cs  : String;
begin
  result  := false;
  if aMessage <> nil then
  begin
    if assigned(fOnMachineIdentRequired) then
    begin
      cs  := '';
      fOnMachineIdentRequired(self, aMessage, cs);
      if Pos(lowercase(fText), lowercase(cs)) = 1 then
      begin
        result  := true;
      end;
    end;
  end;
end;

{ **************************************************************************** }
{ ***** TNxLoggerMessageFilterMachineContains ******************************** }
{ **************************************************************************** }
function  TNxLoggerMessageFilterMachineContains.matchMachineIdent(aMessage : TNxLoggerMessage; const aMachineIdent : String) : Boolean;
begin
  result  := false;
  if aMachineIdent <> '' then
  begin
    if Pos(lowercase(fText), lowercase(aMachineIdent)) > 0 then
    begin
      result  := true;
    end;
  end else
  begin
    result := self.match(aMessage);
  end;
end;

function  TNxLoggerMessageFilterMachineContains.match(aMessage : TNxLoggerMessage) : Boolean;
var cs  : String;
begin
  result  := false;
  if aMessage <> nil then
  begin
    if assigned(fOnMachineIdentRequired) then
    begin
      cs  := '';
      fOnMachineIdentRequired(self, aMessage, cs);
      if Pos(lowercase(fText), lowercase(cs)) > 0 then
      begin
        result  := true;
      end;
    end;
  end;
end;

{ **************************************************************************** }
{ ***** TNxLoggerMessageFilterApplicationEquals ****************************** }
{ **************************************************************************** }
function  TNxLoggerMessageFilterApplicationEquals.match(aMessage : TNxLoggerMessage) : Boolean;
begin
  result  := false;
  if aMessage <> nil then
  begin
    if lowercase(fText) = lowercase(aMessage.ApplicationId) then
    begin
      result  := true;
    end;
  end;
end;

{ **************************************************************************** }
{ ***** TNxLoggerMessageFilterApplicationStarting **************************** }
{ **************************************************************************** }
function  TNxLoggerMessageFilterApplicationStarting.match(aMessage : TNxLoggerMessage) : Boolean;
begin
  result  := false;
  if aMessage <> nil then
  begin
    if Pos(lowercase(fText), lowercase(aMessage.ApplicationId)) = 1 then
    begin
      result  := true;
    end;
  end;
end;

{ **************************************************************************** }
{ ***** TNxLoggerMessageFilterApplicationContains **************************** }
{ **************************************************************************** }
function  TNxLoggerMessageFilterApplicationContains.match(aMessage : TNxLoggerMessage) : Boolean;
begin
  result  := false;
  if aMessage <> nil then
  begin
    if Pos(lowercase(fText), lowercase(aMessage.ApplicationId)) > 0 then
    begin
      result  := true;
    end;
  end;
end;








initialization
  {$IF CompilerVersion >= 24.0 }
    System.Classes.RegisterClass(TNxLoggerMessageFilterLevelSet);
    System.Classes.RegisterClass(TNxLoggerMessageFilterModuleEquals);
    System.Classes.RegisterClass(TNxLoggerMessageFilterModuleStarting);
    System.Classes.RegisterClass(TNxLoggerMessageFilterModuleContains);
    System.Classes.RegisterClass(TNxLoggerMessageFilterMessageEquals);
    System.Classes.RegisterClass(TNxLoggerMessageFilterMessageStarting);
    System.Classes.RegisterClass(TNxLoggerMessageFilterMessageContains);
    System.Classes.RegisterClass(TNxLoggerMessageFilterMachineEquals);
    System.Classes.RegisterClass(TNxLoggerMessageFilterApplicationEquals);

    System.Classes.RegisterClass(TNxLogFormaterDefault);
    System.Classes.RegisterClass(TNxLogFormaterTCP);

    System.Classes.RegisterClass(TNxLogAppenderFile);
    System.Classes.RegisterClass(TNxLogAppenderTCP);
  {$ELSE}
    Classes.RegisterClass(TNxLoggerMessageFilterLevelSet);
    Classes.RegisterClass(TNxLoggerMessageFilterModuleEquals);
    Classes.RegisterClass(TNxLoggerMessageFilterModuleStarting);
    Classes.RegisterClass(TNxLoggerMessageFilterModuleContains);
    Classes.RegisterClass(TNxLoggerMessageFilterMessageEquals);
    Classes.RegisterClass(TNxLoggerMessageFilterMessageStarting);
    Classes.RegisterClass(TNxLoggerMessageFilterMessageContains);
    Classes.RegisterClass(TNxLoggerMessageFilterMachineEquals);
    Classes.RegisterClass(TNxLoggerMessageFilterApplicationEquals);

    Classes.RegisterClass(TNxLogFormaterDefault);
    Classes.RegisterClass(TNxLogFormaterTCP);

    Classes.RegisterClass(TNxLogAppenderFile);
    Classes.RegisterClass(TNxLogAppenderTCP);
  {$IFEND}


  fDefaultLogger := TNxLogger.Create(nil);
  fDefaultLogger.addAppender(TNxLogAppenderFile.Create(fDefaultLogger));

finalization

  FreeAndNil(fDefaultLogger);
  {$IF CompilerVersion >= 24.0 }
    System.Classes.UnRegisterClass(TNxLoggerMessageFilterLevelSet);
    System.Classes.UnRegisterClass(TNxLoggerMessageFilterModuleEquals);
    System.Classes.UnRegisterClass(TNxLoggerMessageFilterModuleStarting);
    System.Classes.UnRegisterClass(TNxLoggerMessageFilterModuleContains);
    System.Classes.UnRegisterClass(TNxLoggerMessageFilterMessageEquals);
    System.Classes.UnRegisterClass(TNxLoggerMessageFilterMessageStarting);
    System.Classes.UnRegisterClass(TNxLoggerMessageFilterMessageContains);
    System.Classes.UnRegisterClass(TNxLoggerMessageFilterMachineEquals);
    System.Classes.UnRegisterClass(TNxLoggerMessageFilterApplicationEquals);

    System.Classes.UnRegisterClass(TNxLogFormaterDefault);
    System.Classes.UnRegisterClass(TNxLogFormaterTCP);

    System.Classes.UnRegisterClass(TNxLogAppenderFile);
    System.Classes.UnRegisterClass(TNxLogAppenderTCP);
  {$ELSE}
    Classes.UnRegisterClass(TNxLoggerMessageFilterLevelSet);
    Classes.UnRegisterClass(TNxLoggerMessageFilterModuleEquals);
    Classes.UnRegisterClass(TNxLoggerMessageFilterModuleStarting);
    Classes.UnRegisterClass(TNxLoggerMessageFilterModuleContains);
    Classes.UnRegisterClass(TNxLoggerMessageFilterMessageEquals);
    Classes.UnRegisterClass(TNxLoggerMessageFilterMessageStarting);
    Classes.UnRegisterClass(TNxLoggerMessageFilterMessageContains);
    Classes.UnRegisterClass(TNxLoggerMessageFilterMachineEquals);
    Classes.UnRegisterClass(TNxLoggerMessageFilterApplicationEquals);

    Classes.UnRegisterClass(TNxLogFormaterDefault);
    Classes.UnRegisterClass(TNxLogFormaterTCP);

    Classes.UnRegisterClass(TNxLogAppenderFile);
    Classes.UnRegisterClass(TNxLogAppenderTCP);
  {$IFEND}


{$IFDEF _WASLEGACYIFDEFSYNTAX}
  {$LEGACYIFEND OFF}
{$ENDIF}

end.


