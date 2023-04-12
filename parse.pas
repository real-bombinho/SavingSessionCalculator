unit parse;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, DateUtils, tzdb, dialogs;

// https://github.com/pavkam/tzdb

type

  { REventSlot }

  REventSlot = record
  private

  public
    dateTime: tDateTime;
    Usage: currency;
    procedure Clear;
  end;

  { REntry }

  REntry = record
  private
    FLine: string;
    FExVAT: currency;
    FInclVAT: currency;
    FFrom, FTill: TDate;
    procedure setLine(const Value: string);
  public
    property ExVAT: currency read FExVAT;
    property InclVAT: currency read FInclVAT;
    property From: TDate read FFrom;
    property Till: TDate read FTill;
    property Line: string read FLine write setLine;
  end;

  { RConsumption }

  RConsumption = record
  private
    FConsumption: currency;
    FFrom, FTill: tDateTime;
    procedure setLine(AValue: string);
  public
    property Consumption: currency read FConsumption;
    property From: TDateTime read FFrom;
    property Till: TDateTime read FTill;
    property Line: string write setLine;
    procedure setValues(const AConsumption: currency; const AFrom, ATill: tDateTime);
  end;

  { RResponse }

  RResponse = record
    count: longint;
    next: string;
    prior: string;
    function parse(const value: string; const List: TStrings): boolean;
  end;

function ParseLine(const Line: String; const List: TStrings; const Separator: String;
  const Clear: boolean = false): boolean;
function UTCString(const value: tDateTime): string;
function LocalLondonTime(const unixTime: longint): tDateTime;
function parseDate(const value: string; out target: TDateTime; IsUTC: boolean = true): boolean;

implementation
var LLondon: TBundledTimeZone;

function LocalLondonTime(const unixTime: longint): tDateTime;
begin
  result := LLondon.ToLocalTime(UnixToDateTime(unixTime));
end;

function parseDaten(const value: string; out target: TDateTime; IsUTC: boolean = true): boolean;
var year, month, day: integer;
    hour, minute, seconds: integer;
    suffix: integer;
    suffixIdentifier: char;
begin
//  "2022-10-28t13:00:00+01:00" = "2022-10-28t12:00:00z"
//  "2022-10-30t07:00:00z"
  result := false;
  if (value[11] <> 't') or (not (value[20] in ['z', '+', '-'])) then
    raise Exception.Create('Unexpected chars detected. [' + value + ']');
  year := strToInt(value.Substring(0, 4));
  month := strToInt(value.Substring(5, 2));
  day := strToInt(value.Substring(8, 2));
  hour := strToInt(value.Substring(11, 2));
  minute := strToInt(value.Substring(14, 2));
  seconds := strToInt(value.Substring(17, 2));
  suffixIdentifier := value[20];
  case suffixIdentifier of
    '+': suffix := -1;
    '-': suffix := 1;
    'z': suffix := 0;
    else raise Exception.Create('invalid character ' + suffixIdentifier );
  end;
  if suffix <> 0 then
  begin
    incHour(hour, suffix * strToInt(value.Substring(20, 2)));
    incMinute(minute, suffix * strToInt(value.Substring(23, 2)));
  end;
  target := EncodeDate(year, month, day) + EncodeTime(hour, minute, seconds, 0);
  if IsUTC then target := LLondon.ToLocalTime(target);
//
//  if IsUTC then target := TTimeZone.Local.ToLocalTime(target);
end;

function UTCString(const value: tDateTime): string;
begin
  result := lowercase(LLondon.ToISO8601Format(value));
  setLength(result, length(result) - 4);
  result := result.Replace('.', 'z"');
end;

function ParseLine(const Line: String; const List: TStrings; const Separator: String;
  const Clear: boolean = false): boolean;
var x, y: integer;
    s, sLine: AnsiString;
    //EolOrComment: integer;
begin
  sLine := AnsiLowercase(Line);
  x := 1; y := 0;
  if not assigned(List) then
  begin
    result := false;
    exit;
  end
  else
    result := true;
  if Clear then List.Clear;
  while (x < length(sLine)) do
  begin                                         // parse string at Separator
    y := Pos(Separator, sLine, x);//copy(sLine, x, length(sLine) - x + 1)) + x - 1;
    if y < x then y := length(sline) + 1;
    s := copy(sline, x, y - x );
    if trim(s) <> '' then List.Add(s);
//    result[n] := copy(sline, x , y - x);
    x := y + 1;
  end;
end;

{ REventSlot }

procedure REventSlot.Clear;
begin
  dateTime := 0;
  usage := 0;
end;

{ RConsumption }

procedure RConsumption.setLine(AValue: string);
var sl: TStringlist;
    s: string;
begin
  { "consumption":0.078,"interval_start":"2023-01-26t22:00:00z","interval_end":"2023-01-26t22:30:00z"}
  s := lowerCase(AValue);
  sl := TStringList.Create;
  ParseLine(s, sl, ',');
  if sl.Count <> 3 then
    raise Exception.Create('RConsumption: invalid amount of arguments');
  if Pos('"consumption"', sl[0]) <> 0 then
  begin
    sl[0] := sl[0].Substring(pos(':', sl[0]));
  end
  else
    raise exception.Create('Result does not contain "consumption"');
  FConsumption := strtocurr(sl[0]);
  if Pos('"interval_start"', sl[1]) <> 0 then
  begin
    s := sl[1].Substring(pos(':', sl[1]) + 1, 20 );
    parseDate(s, FFrom);
  end;
  if Pos('"interval_end"', sl[2]) <> 0 then
  begin
    s := sl[2].Substring(pos(':', sl[2]) + 1, 20 );
    parseDate(s, FTill);
  end;
  sl.Free;
end;

procedure RConsumption.setValues(const AConsumption: currency; const AFrom,
  ATill: tDateTime);
begin
  FConsumption := AConsumption;
  if AFrom < ATill then
  begin
    FFrom := AFrom;
    FTill := ATill;
  end
  else
    raise exception.Create('Unexpected result From/Till');
end;

{ REntry }

procedure REntry.setLine(const Value: string);
var sl: TStringlist;
    s: string;
    i: integer;
begin
  FLine := lowerCase(Value);
  sl := TStringList.Create;
  ParseLine(Value, sl, ',');
  if sl.Count <> 4 then
    raise Exception.Create('TEntry: invalid amount of arguments');
  FLine := sl[0] + '|' + sl[1]  + '|' + sl[2] + '|' + sl[3];
  for i:= 0 to 1 do
  begin
    sl[i] := sl[i].Substring(pos(':', sl[i]));
  end;
  FExVAT := strtofloat(sl[0]);
  FInclVAT := strToFloat(sl[1]);
  if Pos('"valid_from"', sl[2]) <> 0 then
  begin
    s := sl[2].Substring(pos(':', sl[2]) + 1, 20 );
    parseDate(s, FFrom);
  end;
  if Pos('"valid_to"', sl[3]) <> 0 then
  begin
    s := sl[3].Substring(pos(':', sl[3]) + 1, 20 );
    parseDate(s, FTill);
  end;
  sl.Free;
end;

{ RResponse }

function RResponse.parse(const value: string; const List: TStrings): boolean;
var f, t: integer;
begin
  if not assigned(List) then
    raise exception.Create('StringList "List" not initialised');
  parseLine(value, List, '{', true);
  f := pos('"count":', List[0]) + 8;
  if f = 8 then raise exception.Create('Response header does not contain "count"');
  t := pos(',',  List[0]);
  if (f > 9) or (t < f) then raise exception.Create('Unexpected format, "count" in unexpected location');
  count := strtoint(List[0].Substring(f - 1, t-f));//copy(results[0], f, t - f ));

  //if count <> length(value) then raise exception.create('Unexpected length of ' + inttostr(count) + ', expected ' + inttostr(length(value)));
  List[0] := List[0].Substring(t);
  f := pos('"next":"', List[0]) + 8;
  t := pos(',',  List[0]);
  next := copy(List[0], f, t - f - 1);
  List[0] :=  List[0].Substring(t);
  if pos('"previous":null', List[0]) <> 0 then
    prior := ''
  else
  begin
    f := pos('"previous":', List[0]) + 11;
    t := pos(',',  List[0]);
    prior := List[0].Substring(f, t - f - 2);
  end;
  List.Delete(0);
end;

initialization

LLondon := TBundledTimeZone.GetTimeZone('Europe/London');

end.

