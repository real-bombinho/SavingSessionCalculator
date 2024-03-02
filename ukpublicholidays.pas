unit UKpublicHolidays;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, dateUtils;

type

   UKBankHolidays = (NewYear, GoodFriday, EasterMonday, EarlyMayHoliday, MayBankHoliday, SummerBankHoliday, ChristmasDay, BoxingDay);

{RBankHoliday}

  RBankHoliday = record
    title: string;
    date: tDateTime;
    notes: string;
    bunting: boolean;
  end;     

{ RDate }

  RDate = record
  private
    function getDateTime: tDateTime;
    procedure getFromString(AValue: string);
    procedure setDateTime(AValue: tDateTime);
    function EasterSunday(year: integer): RDate;
  public
    Day: byte;
    Month: byte;
    Year: word;
    function AsString: string;
    function AsStringF: string;
    function WeekDay: integer;
    property Date: string read AsString write getFromString;
    property TDate: tDateTime read getDateTime write setDateTime;
    procedure setDate(ADay: byte; AMonth: byte; AYear: word);
    procedure setDateSubstitute(ADay: byte; AMonth: byte; AYear: word);
    procedure incDay(const AValue: integer);
    function isDate(const ADate: RDate): boolean;
    function isUKBankHoliday: boolean;
    function isLeapYear: boolean;
    function BankHoliday(AHoliday: UKBankHolidays; const AYear: word = 0): RDate;
    function isWeekend: boolean;
    function isClockChangeDay: boolean;
  end;

implementation

var
  BHengland_and_wales: array of RBankHoliday;
  BHscotland: array of RBankHoliday;
  BHnorthern_ireland: array of RBankHoliday;

{ RDate }

function RDate.getDateTime: tDateTime;
begin
  result := encodeDate(Year, Month, Day);
end;

procedure RDate.getFromString(AValue: string);
var p1, p2: integer;
begin
  p1 := AValue.IndexOf('/');
  p2 := AVAlue.IndexOf('/',p1 + 1);
  Day := StrToInt(AValue.Substring(0, p1));
  Month := StrToInt(AValue.Substring(p1 + 1, p2 - p1 - 1));
  Year := StrToInt(AValue.Substring(p2 + 1, AValue.Length - p2 + 1));
end;

procedure RDate.setDateTime(AValue: tDateTime);
var m, d: word;
begin
  decodeDate(AValue, Year, m, d);
  Month := byte(m);
  Day := byte(d);
end;

function RDate.EasterSunday(year: integer): RDate;
// A function to calculate the date of Easter Sunday using the algorithm by Oudin (1940)
var
  a, b, c, d, e, f, g, h, i, k, l, m, n, p: integer;
begin
  a := year mod 19;
  b := year div 100;
  c := year mod 100;
  d := b div 4;
  e := b mod 4;
  f := (b + 8) div 25;
  g := (b - f + 1) div 3;
  h := (19 * a + b - d - g + 15) mod 30;
  i := c div 4;
  k := c mod 4;
  l := (32 + 2 * e + 2 * i - h - k) mod 7;
  m := (a + 11 * h + 22 * l) div 451;
  n := (h + l - 7 * m + 114) div 31;
  p := (h + l - 7 * m + 114) mod 31;
  result.Day := p + 1;
  result.Month := n;
  result.Year := year;
end;

function RDate.AsString: string;
begin
  result := inttostr(Day) + '/' + intToStr(Month) + '/' + intToStr(Year);
end;

function RDate.AsStringF: string;
begin
  result := '';
  if Day < 10 then result := '0';
  result := result + inttostr(Day) + '/';
  if Month < 10 then result := result + '0';
  result := result + inttostr(Month) + '/' + intToStr(Year);
end;

function RDate.WeekDay: integer;
begin
  result :=  dateutils.DayOfTheWeek(encodeDate(Year, Month, Day));
end;

procedure RDate.setDate(ADay: byte; AMonth: byte; AYear: word);
begin
  Day := ADay;
  Month := AMonth;
  Year := AYear;
end;

procedure RDate.setDateSubstitute(ADay: byte; AMonth: byte; AYear: word);
var wd: integer;
begin
  setDate(ADay, AMonth, AYear);
  wd := weekday;
  if (ADay <> 26) or (AMonth <> 12) then
  begin
    if wd = 7 then
      incDay(1)
    else
      if wd = 6 then
        incDay(2);
  end
  else
  begin
    if wd in [6, 7] then
      Day := ADay + 2;
    if wd = 1 then
      Day := ADay + 1;
  end;
end;

procedure RDate.incDay(const AValue: integer);
var dt: tDateTime;
begin
  dt := encodeDate(Year, Month, Day);
  dt := dateUtils.IncDay(dt, Avalue);
  setDateTime(dt);
end;

function RDate.isDate(const ADate: RDate): boolean;
begin
  result := (ADate.Day = Day) and (ADate.Month = Month) and (ADate.Year = Year);
end;

function RDate.isUKBankHoliday: boolean;
begin
  result := false;
  if isDate(BankHoliday(NewYear)) or
     isDate(BankHoliday(GoodFriday)) or
     isDate(BankHoliday(EasterMonday)) or
     isDate(BankHoliday(EarlyMayHoliday)) or
     isDate(BankHoliday(MayBankHoliday)) or
     isDate(BankHoliday(SummerBankHoliday)) or
     isDate(BankHoliday(ChristmasDay)) or
     isDate(BankHoliday(BoxingDay)) then
       result := true;
end;

function RDate.isLeapYear: boolean;
begin
  result := IsInLeapYear(encodeDate(Year, Month, Day));
end;

function RDate.BankHoliday(AHoliday: UKBankHolidays; const AYear: word): RDate;
var yr: word;
begin
  if AYear = 0 then
    yr := year
  else
    yr := AYear;
  case AHoliday of
    NewYear:
      result.setDateSubstitute(1, 1, yr);
    GoodFriday:
      begin
        result := EasterSunday(yr);
        result.incDay(-2);
      end;
    EasterMonday:
      begin
        result := EasterSunday(yr);
        result.incDay(1);
      end;
    EarlyMayHoliday:
      begin
        result.setDate(1, 5, yr);              // Early May
        while not (result.WeekDay = 1) do
          result.incDay(1);
      end;
    MayBankHoliday:
      begin
        result.setDate(31, 5, yr);              // May Bank holiday
        while not (result.WeekDay = 1) do
          result.incDay(-1);
      end;
    SummerBankHoliday:
      begin
        result.setDate(31, 8, yr);              // Summer Bank holiday
        while not (result.WeekDay = 1) do
          result.incDay(-1);
      end;
    ChristmasDay:
      result.setDateSubstitute(25, 12, yr);
    BoxingDay:
      result.setDateSubstitute(26, 12, yr);
  end;
end;

function RDate.isWeekend: boolean;
begin
  result := weekday in [6, 7];
end;

function RDate.isClockChangeDay: boolean;      //last Sunday of March or October
var d: byte;
begin
  result := false;
  if (month <> 3) or (month <> 10) then exit;
  d := 31;
  while DayOfWeek(encodeDate(Year, Month, d)) <> 1 do
    dec(d);
  if d = day then result := true;
end;           

end.

