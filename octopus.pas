unit Octopus;
{$modeSwitch advancedRecords}
interface

uses SysUtils, DateUtils, Classes;

const
  OctopusURL = 'https://api.octopus.energy/';
  OctopusEndPoint = 'v1/products/';
  OctopusUserAgent = 'SSCalculator';
  OctopusTariff = 'AGILE-18-02-21';
  OctopusTariff1 = 'AGILE-22-07-22';
  OctopusTariff2 = 'AGILE-22-08-31';
  OctopusTariff3 = 'AGILE-VAR-22-10-19';
  OctopusTariff4 = 'AGILE-FLEX-22-11-25';
  OctopusFault = 'Uh Oh - Something went wrong.';

const
  EasternEngland = 'A';
  EastMidlands = 'B';
  London = 'C';
  MerseysideAndNorthernWales = 'D';
  WestMidlands = 'E';
  NorthEasternEngland = 'F';
  NorthWesternEngland = 'G';
  SouthernEngland = 'H';
  SouthEasternEngland = 'J';
  SouthernWales = 'K';
  SouthWesternEngland = 'L';
  Yorkshire = 'M';
  SouthernScotland = 'N';
  NorthernScotland = 'P';
  RegionNo = 13;
  Regions : array[0..RegionNo] of string =
  ( 'Eastern England', 'East Midlands', 'London', 'Merseyside and Northern Wales',
    'West Midlands', 'North Eastern England', 'North Western England', 'Southern England',
    'South Eastern England', 'Southern Wales', 'South Western England', 'Yorkshire',
    'Southern Scotland', 'Northern Scotland'
  );
  RegionCodesSet = ['A'..'H', 'J'..'N', 'P'];
  RegionCodesStr = 'ABCDEFGHJKLMNP';
  RegionCodesNo: array [0..RegionNo] of integer =
    ( 10, 11, 12, 13, 14, 15, 16, 20, 19, 21, 22, 23, 18, 17);

  type

  { RIndex }

  RIndex = record
    col, row: integer;
  end;

  { REventSlot }

  REventSlot = record
  private
    procedure Clear;
  public
    dateTime: tDateTime;
    usage: currency;
  end;

  { REvent }

   REvent = record         // calculations in setters
   private
     FRoundTo: TDateTime;
     FSlots: integer;
     FFrom: tDateTime;
     FTill: tDateTime;
     FFromO: tDateTime;    // keep original
     FTillO: tDateTime;    // keep original
     function getSlots: integer;
     procedure SetFrom(AValue: TDateTime);
     procedure SetRoundTo(AValue: TDateTime);
     procedure SetTill(AValue: TDateTime);
   public
     property RoundTo: TDateTime read FRoundTo write SetRoundTo;
     property From: TDateTime read FFrom write SetFrom;
     property Till: TDateTime read FTill write SetTill;
     property Slots: integer read FSlots;
   end;

   { TEventSlots }

  TEventSlots = class
  protected
    FData: array of array of REventSlot;
  private
    FSlotsInUse: integer;
    FFrom, FTill: tDateTime;
    function GetDateTime(const col, row: integer): tDateTime;
    function GetDateStr(const col, row: integer): string;
    function GetDateTimeStr(const col, row: integer): string;
    function GetTimeStr(const col, row: integer): string;
    function GetUsage(const col, row: integer): currency;
    procedure SetDateTime(const col, row: integer; AValue: tDateTime);
    procedure SetUsage(const col, row: integer; AValue: currency);
    function outOfBounds(const col, row: integer): boolean;
  public
    property DateTime[const col, row: integer]: tDateTime read GetDateTime write SetDateTime;
    property DateStr[const col, row: integer]: string read GetDateStr;
    property DateTimeStr[const col, row: integer]: string read GetDateTimeStr;
    property TimeStr[const col, row: integer]: string read GetTimeStr;
    property Usage[const col, row: integer]: currency read GetUsage write SetUsage;
    procedure Clear;
    procedure setConsecutiveTimeslots(const row: integer; const firstColumn: tDatetime;
      const count: integer); overload;
    procedure setConsecutiveTimeslots(const row: integer; const AEvent: REvent); overload;
    function RoundToNearest(ADateTime, RoundTo:TDateTime):TdateTime;
    function rowUsage(const row: integer): currency;
    function columnUsage(const col: integer): currency;
    function findIndex(const ADateTime: tDateTime; var AInd: RIndex;
      const roundTo: tDateTime): boolean;
    constructor Create(const cols, rows: integer);
  end;

  RRegion = record
  private
    FValue: char;
    function getCode: integer;
    procedure setCode(const Value: integer); overload;
    function setCode(const Value: string): boolean; overload;
    function getName: String;
    procedure setCharacter(const Value: char);
    procedure setName(const Value: String);
  public
    property Character: char read FValue write setCharacter;
    property Code: integer read getCode write setCode;
    property Name: String read getName write setName;
    function GetList(const StrList: TStrings): boolean;
  end;

  TOctopus = class
  private
    FLastResponse: AnsiString;
    FLastFetched: TDateTime;
    FLastUnsuccessful: TDateTime;
    FLastResponseCode: integer;
    FLastResponseText: string;
    FAPIKey: AnsiString;
    FURL: string;
    FEndpoint: string;
    FParameters: string;
    FRegion: RRegion;
    FTariff: string;

    function getLastResponse: AnsiString;
    procedure setTariff(const Value: string);
  public
    property URL: string read FURL;
    property LastResponse: AnsiString read getLastResponse;
    property ResponseCode: integer read FLastResponseCode;
    property ResponseText: string read FLastResponseText;
    property APIKey: AnsiString write FAPIKey;
    property Region: RRegion read FRegion;
    property Tariff: string read FTariff write setTariff;
    constructor Create(const Key: String; const regionLetter: char; const tariffValue: string); overload;
    function fetch(const urlValue: string): boolean;
    procedure Refresh;
  end;

implementation

uses IdHTTP, IdSSLOpenSSL,IdCompressorZLib, dialogs;

const WaitIfFailed = 5;
      CacheHours = 4;

{ TEventSlots }

function TEventSlots.GetDateTime(const col, row: integer): tDateTime;
begin
  if not outOfBounds(col, row) then
    result := FData[col, row].dateTime;
end;

function TEventSlots.GetDateStr(const col, row: integer): string;
begin
  if not outOfBounds(col, row) then
    result := dateToStr(FData[col, row].dateTime)
  else
    result := '???';
end;

function TEventSlots.GetDateTimeStr(const col, row: integer): string;
begin
  if not outOfBounds(col, row) then
    result := datetimeToStr(FData[col, row].dateTime)
  else
    result := '???';
end;

function TEventSlots.GetTimeStr(const col, row: integer): string;
begin
  if not outOfBounds(col, row) then
    result := timeToStr(FData[col, row].dateTime)
  else
    result := '???';
end;

function TEventSlots.GetUsage(const col, row: integer): currency;
begin
  if not outOfBounds(col, row) then
    result := FData[col, row].usage;
end;

procedure TEventSlots.SetDateTime(const col, row: integer; AValue: tDateTime);
begin
  if not outOfBounds(col, row) then
  begin
    FData[col, row].dateTime := AValue;
    if FFrom > AValue then
      FFrom := AValue;
    if FTill < AValue then
      FTill := AValue;
  end;
end;

procedure TEventSlots.SetUsage(const col, row: integer; AValue: currency);
begin
  if not outOfBounds(col, row) then
    FData[col, row].usage := AValue;
end;

function TEventSlots.outOfBounds(const col, row: integer): boolean;
begin
  result := false;
  if (col < low(FData)) or (col > high(FData)) then
  begin
    raise exception.Create('Column out of bound ' + inttostr(col));
    result := true;
  end;
  if (row < low(FData[col])) or (row > high(FData[col])) then
  begin
    raise exception.Create('Row out of bound ' + inttostr(row));
    result := true;
  end;
end;

procedure TEventSlots.Clear;
var c, r: integer;
begin
  for c := low(FData) to high(FData) do
    for r := low(FData[c]) to high(FData[c]) do
      FData[c, r].Clear;
  FSlotsInUse := 0;
  FFrom := 0;
  FTill := 0;
end;

procedure TEventSlots.setConsecutiveTimeslots(const row: integer;
  const firstColumn: tDatetime; const count: integer);
var c: integer;
    t: TDateTime;
begin
  t := firstColumn;
  if FSlotsInUse < count then FSlotsInUse := count;
  for c := 0 to count - 1 do
  begin
    if not outOfBounds(c, row) then
    begin
      FData[c, row].dateTime := t;
      t := incMinute(t, 30);
    end;
  end;
  if firstColumn < FFrom then
    FFrom := firstColumn;
  if FData[count - 1, row].dateTime > FTill then
      FTill := FData[count - 1, row].dateTime;
end;

procedure TEventSlots.setConsecutiveTimeslots(const row: integer;
  const AEvent: REvent);
var c: integer;
    t: TDateTime;
begin
  t := AEvent.From;
  if FSlotsInUse < AEvent.Slots then FSlotsInUse := AEvent.Slots;
  for c := 0 to AEvent.Slots - 1 do
  begin
    if not outOfBounds(c, row) then
    begin
      FData[c, row].dateTime := t;
      t := incMinute(t, 30);
    end;
  end;
  if AEvent.From < FFrom then
    FFrom := AEvent.From;
  if FData[AEvent.Slots - 1, row].dateTime > FTill then
      FTill := FData[AEvent.Slots - 1, row].dateTime;
end;

function TEventSlots.RoundToNearest(ADateTime, RoundTo: TDateTime): TdateTime;
begin
  if 0 = RoundTo then
    result := ADateTime
  else
    result := Round(ADateTime / RoundTo) * RoundTo;
end;

function TEventSlots.rowUsage(const row: integer): currency;
var c: integer;
begin
  result := 0;
  for c := 0 to high(FData) do
    result := result + FData[c, row].usage;
end;

function TEventSlots.columnUsage(const col: integer): currency;
var r: integer;
begin
  result := 0;
  for r := 0 to high(FData[col]) do
    result := result + FData[col, r].usage;
end;

function TEventSlots.findIndex(const ADateTime: tDateTime; var AInd: RIndex; const roundTo: tDateTime): boolean;
var i, j: integer;
    t: tDateTime;
begin
  result := false;
  t := RoundToNearest(ADateTime, roundTo);
  if (t > RoundToNearest(FTill, roundTo)) then exit;
  if (t < RoundToNearest(FFrom, roundTo)) then exit;
  for i := 0 to FSlotsInUse - 1 do
    for j := 0 to high(FData[i]) do
      if t = RoundToNearest(FData[i, j].dateTime, roundTo) then
      begin
        AInd.col := i;
        AInd.row := j;
        result := true;
        exit;
      end;
end;

constructor TEventSlots.Create(const cols, rows: integer);
var i: integer;
begin
  SetLength(FData, cols);
  for i := 0 to High(FData) do
    SetLength(FData[i], rows);
  clear;
end;

{ REventSlot }

procedure REventSlot.clear;
begin
  dateTime := 0;
  usage := 0;
end;

{ REvent }

function REvent.getSlots: integer;
var i: integer;
begin
  i := minutesBetween(From, incMinute(Till, -1));
  result := abs(i div 30);
  if i mod 30 > 0 then inc(result);
end;

procedure REvent.SetFrom(AValue: TDateTime);
begin
  if FFrom = AValue then Exit;
  FFromO := AValue;
  if FRoundTo <> 0 then
    FFrom := Round(FFromO / RoundTo) * RoundTo
  else
    FFrom := AValue;
  if FTill <> 0 then
    FSlots := getSlots;

end;

procedure REvent.SetRoundTo(AValue: TDateTime);
begin
  if FRoundTo = AValue then Exit;
  FRoundTo := AValue;
  if FRoundTo <> 0 then
  begin
    FFrom := Round(FFromO / RoundTo) * RoundTo;
    FTill := Round(FTillO / RoundTo) * RoundTo;
  end;
end;

procedure REvent.SetTill(AValue: TDateTime);
begin
  if FTill = AValue then Exit;
  FTillO := AValue;
  if FRoundTo <> 0 then
    FTill := Round(FTillO / RoundTo) * RoundTo
  else
    FTill := AValue;
  if FFrom <> 0 then
    FSlots := getSlots;

end;

{ TOctopus }

constructor TOctopus.Create(const key: String; const regionLetter: char; const tariffValue: string);
begin
  inherited Create;
  FAPIKey := key;
  FURL := OctopusURL;
  FEndPoint := OctopusEndPoint;
  FRegion.Character := regionLetter;
  FLastResponse := '';
  FLastFetched := 0;
  FLastUnsuccessful := 0;
  FTariff := tariffValue;
end;

////////////////////////////////////////////////////////////////////////////////
//
// fetches API, if previously unsuccessful, it will fail further attempts for
//   [const WaitIfFailed] minutes and ResponseCode will be -1.
//
////////////////////////////////////////////////////////////////////////////////

function TOctopus.fetch(const urlValue: string): boolean;
var Id_HandlerSocket : TIdSSLIOHandlerSocketOpenSSL;
    IdHTTP1: TIdHTTP;
    s: string;
begin
  result := false;
  if FLastUnsuccessful <> 0 then
    if IncMinute(FLastUnsuccessful, WaitIfFailed) < now then
    begin
      FLastResponseCode := -1;
      FLastResponseText := 'please wait';
      exit;
    end;
  IdHTTP1 := TIdHTTP.Create(nil);
  IdHTTP1.Compressor := TIdCompressorZLib.Create(nil);
  Id_HandlerSocket := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    Id_HandlerSocket.DefaultPort := 443;
    Id_HandlerSocket.SSLOptions.Mode := sslmClient;
    Id_HandlerSocket.SSLOptions.Method := sslvTLSv1_2;
    Id_HandlerSocket.SSLOptions.SSLVersions := [sslvTLSv1_2];
    idHTTP1.IOHandler := Id_HandlerSocket;
    if (idHTTP1.Compressor = nil) or (not idHTTP1.Compressor.IsReady) then
      idHTTP1.Request.AcceptEncoding := 'identity'
    else
      idHTTP1.Request.AcceptEncoding := 'gzip,deflate,identity';
    idHTTP1.Request.BasicAuthentication := true;
    IdHTTP1.Request.UserAgent := OctopusUserAgent;
    IdHTTP1.Request.Username := FAPIKey;
    FLastUnsuccessful := now;                        // showmessage(urlvalue);
    s := IdHTTP1.Get(urlValue);
    FLastResponseCode := IdHTTP1.ResponseCode;
    FLastResponseText := IdHTTP1.ResponseText;
    if FLastResponseCode = 200 then
    begin
      result := true;
      FLastResponse := s;
      FLastFetched := now;
    end;
  finally
    Id_HandlerSocket.Free;
    if Assigned(IdHTTP1.Compressor) then IdHTTP1.Compressor.Free;
    IdHTTP1.Free;
  end;
  FLastUnsuccessful := 0;
end;

////////////////////////////////////////////////////////////////////////////////
//
// getLastResponse uses the cached result if existing and not older than
// (const CacheHours)
//
////////////////////////////////////////////////////////////////////////////////

function TOctopus.getLastResponse: AnsiString;
begin
  if IncHour(FLastFetched, CacheHours) < now then
    Refresh;
  result := FLastResponse
end;

procedure TOctopus.Refresh;
begin
  FParameters := FTariff + '/electricity-tariffs/E-1R-' + FTariff +
    '-' + FRegion.Character + '/standard-unit-rates/';
  fetch(FURL + FEndPoint + FParameters);
end;

procedure TOctopus.setTariff(const Value: string);
begin
  FTariff := Value;
  Refresh;
end;

{ RRegion }

function RRegion.getCode: integer;
begin
  case FValue of
    'A':	result := 10;	// Eastern England
    'B':	result := 11;	// East Midlands
    'C':	result := 12;	// London
    'D':	result := 13;	// Merseyside and Northern Wales
    'E':	result := 14;	// West Midlands
    'F':	result := 15;	// North Eastern England
    'G':	result := 16;	// North Western England
    'H':	result := 20;	// Southern England
    'J':	result := 19;	// South Eastern England
    'K':	result := 21;	// Southern Wales
    'L':	result := 22;	// South Western England
    'M':	result := 23;	// Yorkshire
    'N':	result := 18;	// Southern Scotland
    'P':	result := 17;	// Northern Scotland
    else result := -1;
  end;
end;

function RRegion.getName: String;
var i: integer;
begin
  i := pos(FValue, RegionCodesStr) - 1;
  if i in [0..13] then 
    result := Regions[i]
  else
    result := 'Undefined';
end;

procedure RRegion.setCharacter(const Value: char);
begin
  if charInSet(Value, RegionCodesSet) then
    FValue := Value
  else
  begin
    FValue := #0;
    raise Exception.Create('Invalid region character: ' + value);
  end;
end;

function RRegion.setCode(const Value: string): boolean;
var index, i: integer;
begin
  result := false;
  for i := 0 to RegionNo do
    if Value = Regions[i] then
    begin
      index := i;
      result := true;
      break;
    end;
  if result then
    setCode(RegionCodesNo[index]);
end;

procedure RRegion.setName(const Value: String);
begin
  setCode(Value);
end;

procedure RRegion.setCode(const Value: integer);
var i : integer;
begin
  FValue := #0;
  for i := 0 to RegionNo do
    if Value = RegionCodesNo[i] then
    begin
      FValue := RegionCodesStr[i + 1];
      break;
    end;
end;

function RRegion.GetList(const StrList: TStrings): boolean;
var i: integer;
begin
  if Assigned(StrList) then
  begin
    StrList.Clear;
    for i := 0 to RegionNo do
      StrList.Add(Regions[i]);
    result := true;
  end
  else
  result := false;
end;

end.
