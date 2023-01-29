unit parse;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, DateUtils, tzdb, dialogs;

// https://github.com/pavkam/tzdb

type

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
    FLine: string;
    FConsumption: currency;
    FFrom, FTill: tDateTime;
    procedure setLine(AValue: string);
  public
    property Consumption: currency read FConsumption;
    property From: TDateTime read FFrom;
    property Till: TDateTime read FTill;
    property Line: string read FLine write setLine;
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
function parseDate(const value: string; out target: TDateTime; IsUTC: boolean = true): boolean;

implementation
var LLondon: TBundledTimeZone;

function parseDate(const value: string; out target: TDateTime; IsUTC: boolean = true): boolean;
var year, month, day: integer;
    hour, minute, seconds: integer;
    s: string;
begin
//  "2022-10-28t13:00:00+01:00"
//  "2022-10-30t07:00:00z"
  result := false;
  if (value[11] <> 't') or ((value[20] <> 'z') and (value[20] <> '+')) then
    raise Exception.Create('Unexpected chars detected. [' + value + ']');
  year := strToInt(value.Substring(0, 4));
  month := strToInt(value.Substring(5, 2));
  day := strToInt(value.Substring(8, 2));
  hour := strToInt(value.Substring(11, 2));
  minute := strToInt(value.Substring(14, 2));
  seconds := strToInt(value.Substring(17, 2));
  target := EncodeDate(year, month, day) + EncodeTime(hour, minute, seconds, 0);
  if IsUTC then target := LLondon.ToLocalTime(target);
//
//  if IsUTC then target := TTimeZone.Local.ToLocalTime(target);
end;

function UTCString(const value: tDateTime): string;
var utctime: tDateTime;
    p: integer;
begin
  result := lowercase(LLondon.ToISO8601Format(value));
  setLength(result, length(result) - 4);
  p := pos('.', result);
  result := result.Replace('.', 'z"');
end;

function ParseLine(const Line: String; const List: TStrings; const Separator: String;
  const Clear: boolean = false): boolean;
var x, y: integer;
    s, sLine: AnsiString;    Listt: TstringList;
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
  Listt := TStringList.Create;
  Listt.Clear;
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

{ RConsumption }

procedure RConsumption.setLine(AValue: string);
var sl: TStringlist;
    s: string;
    i: integer;
begin
  { "consumption":0.078,"interval_start":"2023-01-26t22:00:00z","interval_end":"2023-01-26t22:30:00z"}
  FLine := lowerCase(AValue);
  sl := TStringList.Create;
  ParseLine(AValue, sl, ',');
  if sl.Count <> 3 then
    raise Exception.Create('TConsumption: invalid amount of arguments');
  FLine := sl[0] + '|' + sl[1]  + '|' + sl[2];
  for i:= 0 to 0 do
  begin
    sl[i] := sl[i].Substring(pos(':', sl[i]));
  end;
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
    p: pchar;
begin
  if not assigned(List) then
    raise exception.Create('StringList "List" not initialised');
  parseLine(value, List, '{', true);
  f := pos('"count":', List[0]) + 8;
  t := pos(',',  List[0]);
  if (f > 9) or (t < f) then raise exception.Create('Unexpected format, "count" in unexpected location');
  count := strtoint(List[0].Substring(f - 1, t-f));//copy(results[0], f, t - f ));

  //if count <> length(value) then raise exception.create('Unexpected length of ' + inttostr(count) + ', expected ' + inttostr(length(value)));
  List[0] := copy(List[0], t + 1, length(List[0]) - t);
  f := pos('"next":"', List[0]) + 8;
  t := pos(',',  List[0]);
  next := copy(List[0], f, t - f - 1);
  List[0] := copy(List[0], t + 1, length(List[0]) - t);
  if pos('"previous":null', List[0]) <> 0 then
    prior := ''
  else
  begin
    f := pos('"previous":', List[0]) + 11;
    t := pos(',',  List[0]);
    p := Pchar(copy(List[0], f, t - f));
    prior := AnsiExtractQuotedStr(p, '"');
  end;
  List.Delete(0);
end;

initialization

LLondon := TBundledTimeZone.GetTimeZone('Europe/London');

end.

