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
