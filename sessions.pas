unit Sessions;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, parse;

type

  { PSession }

  PSession = record
  {     "id": 595,
        "code": "EVENT_1_151122",
        "startAt": 1668531600,
        "endAt": 1668535200,
        "rewardPerKwhInOctoPoints": 1800,
        "totalParticipants": 288300 }
    ID: integer;
    Code: string;
    From: tDateTime;
    Till: tDateTime;
    PointsPerUnit: integer;
    totalParticipants: longint;
    procedure parse(const AValue: string);
    procedure parseCSV(const AValue: string);
  end;

{ TSessions }

TSessions = class
private
const
  ErrorIndex = 'Invalid Index ';
  cFileName = 'sessions.csv';
  CSVHeader = 'Date,Till,From,PointsPerkWh';
var
  FSessions: array of PSession;
  FLastResponse: string;
  FLastResponseCode: integer;
  FLastResponseText: string;
  FLastFetched: tDateTime;
  FCSVText: TStringList;
  FCSVMD5: string;
  function fetch(const urlValue: string): boolean;
  function getDateStr(Index: integer): string;
  function getFrom(Index: integer): tDateTime;
  function getPoints(Index: integer): integer;
  function getTill(Index: integer): tDateTime;
  function makeCSV: string;
public
  constructor Create(const AURL: string);
  destructor Destroy; override;
  function isEmpty: boolean;
  function Count: integer;
  property Response: string read FLastResponse;
  property From[Index: integer]: tDateTime read getFrom;
  property Till[Index: integer]: tDateTime read getTill;
  property PointsPerUnit[Index: integer]: integer read getPoints;
  property DateStr[Index: integer]: string read getDateStr;
  procedure SavingSessionDays(const dateList: TStrings);
  function isEvent(const EventDay: tDateTime): integer;
  function loadFromURL(const URL: string): boolean;
  function loadFromFile(const fileName: string): boolean;
  function saveToFile(const fileName: string = cFileName): boolean;
end;

implementation

uses IdHTTP, IdHashMessageDigest, DateUtils;

function MD5String(str: String): String;
begin
  with TIdHashMessageDigest5.Create do
    try
      Result := HashStringAsHex(str);
    finally
      Free;
    end;
end;

{ PSession }

procedure PSession.parse(const AValue: string);
var sl: TStringlist;
    s: string;
begin
  sl := TStringList.Create;
  ParseLine(AValue, sl, ',', true);
  if sl.Count <> 6 then raise exception.Create('Unexpected session list format');
  if Pos('"id":', lowercase(sl[0])) <> 0 then
  begin
    s := trim(sl[0].Substring(pos(':', sl[0]) + 1));
    ID := strToIntDef(s, -1);
    if ID = -1 then raise exception.Create('Invalid ID');
  end;
  if Pos('"code":', lowercase(sl[1])) <> 0 then
  begin
    Code := trim(sl[1].Substring(pos(':', sl[1]) + 1));
  end;
  if Pos('"startat":', lowercase(sl[2])) <> 0 then
  begin
    s := trim(sl[2].Substring(pos(':', sl[2]) + 1));
    From := LocalLondonTime(StrToIntDef(s, 0));
    if From = 0 then raise exception.Create('Invalid start date');
  end;
  if Pos('"endat":', lowercase(sl[3])) <> 0 then
  begin
    s := trim(sl[3].Substring(pos(':', sl[3]) + 1));
    Till := LocalLondonTime(StrToIntDef(s, 0));
    if Till = 0 then raise exception.Create('Invalid end date');
  end;
  if Pos('"rewardperkwhinoctopoints":', lowercase(sl[4])) <> 0 then
  begin
    s := trim(sl[4].Substring(pos(':', sl[4]) + 1));
    PointsPerUnit := strToIntDef(s, -1);
    if PointsPerUnit = -1 then raise exception.Create('Invalid points per unit');
  end;
  if Pos('"totalparticipants"::', lowercase(sl[5])) <> 0 then
  begin
    s := trim(sl[5].Substring(pos(':', sl[5]) + 1));
    totalParticipants := strToIntDef(s, -1);
    if totalParticipants = -1 then
      raise exception.Create('Invalid participant count');
  end;
  sl.Free;
end;

procedure PSession.parseCSV(const AValue: string);
var sl: TStringlist;
begin
  // 15/11/2022,17:00,18:00,1800
  sl := TStringList.Create;
  ParseLine(AValue, sl, ',', true);
  if sl.Count <> 4 then
    raise exception.Create('Unexpected session list CSV format');
  From := strToDate(sl[0]) + strToTime(sl[1]);
  Till := strToDate(sl[0]) + strToTime(sl[2]);
  PointsPerUnit := strToInt(sl[3]);
  ID := 0;
  Code := '';
  totalParticipants := 0;
  sl.Free;
end;

{ TSessions }

function TSessions.fetch(const urlValue: string): boolean;
var IdHTTP1: TIdHTTP;
    s: string;
    sl: TStringList;
    i: integer;
begin
  result := false;
  IdHTTP1 := TIdHTTP.Create(nil);
  try
    IdHTTP1.Request.UserAgent := 'SSCalculator';
    IdHTTP1.HandleRedirects := true;
    s := IdHTTP1.Get(urlValue);
    FLastResponseCode := IdHTTP1.ResponseCode;
    FLastResponseText := IdHTTP1.ResponseText;
  finally
    IdHTTP1.Free;
  end;

  if FLastResponseCode = 200 then
  begin
    result := true;
    FLastResponse := s;
    FLastFetched := now;
    sl := TStringList.Create;
    ParseLine(FLastResponse, sl, '},', true);
    setLength(FSessions, sl.Count);
    for i := 0 to sl.Count - 1 do
      FSessions[i].parse(sl[i]);
    sl.Free;
  end;
end;

function TSessions.getDateStr(Index: integer): string;
begin
  if (Index < 0) or (Index > High(FSessions)) then
    raise Exception.Create(ErrorIndex + '[DateStr]');
  result := datetostr(FSessions[Index].From);
end;

function TSessions.getFrom(Index: integer): tDateTime;
begin
  if (Index < 0) or (Index > High(FSessions)) then
    raise Exception.Create(ErrorIndex + '[From]');
  result := FSessions[Index].From;
end;

function TSessions.getPoints(Index: integer): integer;
begin
  if (Index < 0) or (Index > High(FSessions)) then
    raise Exception.Create(ErrorIndex + '[PointsPerUnit]');
  result := FSessions[Index].PointsPerUnit;
end;

function TSessions.getTill(Index: integer): tDateTime;
begin
  if (Index < 0) or (Index > High(FSessions)) then
    raise Exception.Create(ErrorIndex + '[Till]');
  result := FSessions[Index].Till;
end;

function TSessions.makeCSV: string;
var i: integer;
begin
  FCSVText.Clear;
  FCSVText.Add('Date,Till,From,PointsPerkWh');
  for i := low(FSessions) to high(FSessions) do
    FCSVText.Add(dateToStr(FSessions[i].From) + ',' + timeToStr(FSessions[i].From) +
      ',' + timeToStr(FSessions[i].Till) + ',' + intToStr(FSessions[i].PointsPerUnit));
  result := MD5String(FCSVText.Text);
end;

constructor TSessions.Create(const AURL: string);
begin
  inherited Create;
  FCSVText := TStringList.Create;
  if LoadFromURL(AURL) then
  else
    if not loadFromFile('sessions.csv') then
    begin
      setLength(FSessions, 0);
      raise Exception.Create('Fatal error:' + #10 +#13 +
        'Session data not available from internet' + #10 +#13 +
        'and file ' + cFileName + ' not found.');
    end;
end;

destructor TSessions.Destroy;
begin
  inherited;
  FCSVText.Free;
end;

function TSessions.isEmpty: boolean;
begin
  result := (Length(FSessions) = 0);
end;

function TSessions.Count: integer;
begin
  result := Length(FSessions);
end;

procedure TSessions.SavingSessionDays(const dateList: TStrings);
var i: integer;
begin
  dateList.Clear;
  for i := low(FSessions) to high(FSessions) do
    dateList.Add(getDateStr(i));
end;

function TSessions.isEvent(const EventDay: tDateTime): integer;
var i: integer;
begin
  for i := low(FSessions) to high(FSessions) do
    if trunc(EventDay) = trunc(FSessions[i].From) then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

function TSessions.loadFromURL(const URL: string): boolean;
var i: integer;
begin
  for i := 0 to 2 do
    if fetch(URL) then break;
  result := FLastResponseCode = 200;
end;

function TSessions.loadFromFile(const fileName: string): boolean;
var i: integer;
    f: TextFile;
    s: string;
begin
  result := false;
  AssignFile(f, fileName);
  try
    Reset(f);
    setLength(FSessions, 0);
    FCSVText.Clear;
    i := 0;
    while not eof(f) do
    begin
      readln(f, s);
      FCSVText.Add(s);
      inc(i);
    end;
    CloseFile(f);
  except
    on E: EInOutError do
      writeln('File reading error occurred. Details: ', E.ClassName, '/', E.Message);
  end;
  if FCSVText.Count < 1 then
    raise Exception.Create(fileName + ' seems to be empty');
  if CompareText(FCSVText.Strings[0], CSVHeader) <> 0 then
  begin
    FCSVText.Clear;
    raise Exception.Create('Unexpected header found in ' + fileName);
  end
  else
    result := true;
  setLength(FSessions, i - 1);
  for i := low(FSessions) to high(FSessions) do
    FSessions[i].parseCSV(FCSVText.Strings[i + 1]);
  FCSVMD5 := MD5String(FCSVText.Text);
end;

function TSessions.saveToFile(const fileName: string): boolean;
var i: integer;
    f: TextFile;
begin
  result := false;
  FCSVMD5 := makeCSV;
  begin
    AssignFile(f, fileName);
    try
      rewrite(f);
      for i := 0 to FCSVText.Count - 1 do
        writeln(f, FCSVText.Strings[i]);
      CloseFile(f);
      result := true;
    except
      on E: EInOutError do
        writeln('File writing error occurred. Details: ', E.ClassName, '/', E.Message);
    end;
  end;
  if not fileExists(fileName) then result := false;
end;

end.

