unit EventSlots;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, dateutils, Grids;

type

  { TCustomDateTime }

  TCustomDateTime = record
  strict private
    FDateTime: tDateTime;
    function getDateString: string;
    function getString: string;
    function getTimeString: string;
  public
    property DateTime: tDateTime read FDateTime write FDateTime;
    property asString: string read getString;
    property asDateStr: string read getDateString;
    property asTimeStr: string read getTimeString;
  end;

  { RIndex }

  RIndex = record
    Col, Row: integer;
    procedure init(ACol, ARow: integer);
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

  { REventSlot }

  REventSlot = record
  private
    procedure Clear;
  public
    dateTime: tDateTime;
    usage: currency;
    populated: boolean;
  end;

  { TEventSlots }

  TEventSlots = class
  protected
    FData: array of array of REventSlot;
    procedure SetUsage(const col, row: integer; AValue: currency);
  private
    FSlotsInUse: integer;
    FFrom, FTill: tDateTime;
    function GetDateTime(const col, row: integer): tCustomDateTime;
    function getRowUsage(const row: integer): currency;
    function GetTimeStr(const col, row: integer): string;
    function GetUsage(const col, row: integer): currency;
    procedure SetDateTime(const col, row: integer; AValue: tCustomDateTime);
    function getColUsage(const col: integer): currency;
    function outOfBounds(const col, row: integer): boolean;
  public
    property DateTime[const col, row: integer]: tCustomDateTime read GetDateTime write SetDateTime;
    property TimeStr[const col, row: integer]: string read GetTimeStr;
    property Usage[const col, row: integer]: currency read GetUsage write SetUsage;
    property ColumnUsage[const col: integer]: currency read getColUsage;
    property RowUsage[const row: integer]: currency read getRowUsage;
    property SlotCount: integer read FSlotsInUse;
    procedure Clear;
    procedure setConsecutiveTimeslots(const row: integer; const firstColumn: tDatetime;
      const count: integer); overload;
    procedure setConsecutiveTimeslots(const row: integer; const AEvent: REvent); overload;
    function RoundToNearest(ADateTime, RoundTo:TDateTime):TdateTime;
    function findIndex(const ADateTime: tDateTime; var AInd: RIndex;
      const roundTo: tDateTime): boolean;
    function rawData: string;
    function hasMissingData: boolean;
    constructor Create(const cols, rows: integer);
  end;

  { TEventSlotsDisplay }

  TEventSlotsDisplay = class(TEventSlots)
  protected
    procedure SetUsage(const col, row: integer; AValue: currency);
  private
    FStringGrid: TStringGrid;
    FEarliest: tDateTime;
    FPrecision: array of byte;
    FPrecisionTotal: byte;
    function getPrecision(const col: integer): byte;
    procedure setPrecision(const col: integer; AValue: byte);
    procedure setPrecisionTotal(AValue: byte);
  public
    function hasIDA(ADate: TDateTime): boolean;
    function hasIDA: boolean; overload;
    procedure setConsecutiveTimeslots(const row: integer; const firstColumn: tDatetime;
      const count: integer); overload;
    procedure setConsecutiveTimeslots(const row: integer; const AEvent: REvent); overload;
    procedure showRowTotals;
    procedure Clear;
    constructor Create(AStringGrid: TStringGrid; const Precision: byte = 3);
    property StringGrid: TStringGrid read FStringGrid;
    property Usage[const col, row: integer]: currency read GetUsage write SetUsage;
    property Earliest: tDateTime read FEarliest;
    property Precision[const col: integer]: byte read getPrecision write setPrecision;
    property PrecisionTotal: byte read FPrecisionTotal write setPrecisionTotal;
  end;

implementation

{ TCustomDateTime }

function TCustomDateTime.getDateString: string;
begin
  result := DateToStr(FDateTime);
end;

function TCustomDateTime.getString: string;
begin
  if FDateTime = 0  then
    result := '???'
  else
    result := DateTimeToStr(FDateTime);
end;

function TCustomDateTime.getTimeString: string;
begin
  result := TimeToStr(FDateTime);
end;

{ RIndex }

procedure RIndex.init(ACol, ARow: integer);
begin
  col := ACol;
  row := ARow;
end;

{ TEventSlotsDisplay }

procedure TEventSlotsDisplay.SetUsage(const col, row: integer; AValue: currency
  );
var r: integer;
begin
  inherited SetUsage(col, row, AValue);
  if row = 0 then
    r := 1
  else
    r := row + 2;
  FStringGrid.Cells[col + 1, r] := currToStrF(AValue, FFfixed, FPrecision[col]);
end;

function TEventSlotsDisplay.getPrecision(const col: integer): byte;
begin
  if not outOfBounds(col, 0) then
    result := FPrecision[col];
end;

procedure TEventSlotsDisplay.setPrecision(const col: integer; AValue: byte);
begin
  if not outOfBounds(col, 0) then
    FPrecision[col] := AValue;
end;

procedure TEventSlotsDisplay.setPrecisionTotal(AValue: byte);
begin
  if FPrecisionTotal = AValue then Exit;
  FPrecisionTotal := AValue;
end;

function TEventSlotsDisplay.hasIDA(ADate: TDateTime): boolean;
begin
  result := ADate < encodeDate(2023, 11, 1);
end;

function TEventSlotsDisplay.hasIDA: boolean;
begin
  result := hasIDA(FData[low(FData), 0].dateTime);
end;

procedure TEventSlotsDisplay.setConsecutiveTimeslots(const row: integer;
  const firstColumn: tDatetime; const count: integer);
var i, r: integer;
begin
  inherited setConsecutiveTimeslots(row, firstColumn, count);
  if (FEarliest = 0) or (FEarliest > firstColumn) then
    FEarliest := firstColumn;
  if row = 0 then
  begin
    for i := low(FData) to high(FData) do
    begin
      if i < low(FData) + count then
        FSTringGrid.Columns[i].Title.Caption := TimeToStr(FData[i, 0].dateTime)
      else
        FSTringGrid.Columns[i].Title.Caption := 'n/a';
    end;
    r := 1;
  end
  else
    r := row + 2;
  FStringGrid.Cells[0, r] := DateToStr(FData[low(FData), row].dateTime);
end;

procedure TEventSlotsDisplay.setConsecutiveTimeslots(const row: integer;
  const AEvent: REvent);
begin
  setConsecutiveTimeslots(row, AEvent.FFrom, AEvent.FSlots);
end;

procedure TEventSlotsDisplay.showRowTotals;
var i: integer;
begin
  if FStringGrid.RowCount < 2 then exit;
  FStringGrid.Cells[FStringGrid.ColCount - 1, 1] := currToStrF(getRowUsage(0), ffFixed, FPrecisionTotal);
  for i := 1 to high(FData[0]) do
    FStringGrid.Cells[FStringGrid.ColCount - 1, i + 2] := currToStrF(getRowUsage(i), ffFixed, FPrecisionTotal);
end;

procedure TEventSlotsDisplay.Clear;
var i,j: integer;
begin
  inherited Clear;
  for j := 0 to FStringGrid.RowCount - 1 do
    for i := 0 to FStringGrid.ColCount - 1 do
    begin
      FStringGrid.Cells[i, j] := '';
    end;
  FEarliest := 0;
end;

constructor TEventSlotsDisplay.Create(AStringGrid: TStringGrid; const Precision: byte);
var i, c: integer;
begin
  if AStringGrid = nil then
    exception.Create('StringGrid not initialised');
  i := AStringGrid.RowCount - 3;
  if i < 1 then i := 1;
  c := AStringGrid.ColCount - 3;
  if c < 1 then c := 1;
  inherited Create(c, i);
  SetLength(FPrecision, c);
  FStringGrid := AStringGrid;
  FEarliest := 0;
  //showmessage('Created ' + inttostr(AStringGrid.ColCount - 3) + ', ' +
  for i := low(FPrecision) to high(FPrecision) do
    FPrecision[i] := Precision;
  FPrecisionTotal := Precision;
end;

{ TEventSlots }

function TEventSlots.GetDateTime(const col, row: integer): tCustomDateTime;
begin
  if not outOfBounds(col, row) then
    result.DateTime := FData[col, row].dateTime;
end;

function TEventSlots.getRowUsage(const row: integer): currency;
var c: integer;
begin
  result := 0;
  for c := 0 to high(FData) do
    result := result + FData[c, row].usage;
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

procedure TEventSlots.SetDateTime(const col, row: integer; AValue: tCustomDateTime);
begin
  if not outOfBounds(col, row) then
  begin
    FData[col, row].dateTime := AValue.DateTime;
    if FFrom > AValue.DateTime then
      FFrom := AValue.DateTime;
    if FTill < AValue.DateTime then
      FTill := AValue.DateTime;
    FData[col, row].Populated := false;
  end;
end;

procedure TEventSlots.SetUsage(const col, row: integer; AValue: currency);
begin
  if not outOfBounds(col, row) then
  begin
    FData[col, row].usage := AValue;
    FData[col, row].Populated := true;
  end;
end;

function TEventSlots.outOfBounds(const col, row: integer): boolean;
begin
  result := false;
  if (col < low(FData)) or (col > high(FData)) then
  begin
    raise exception.Create('TEventSlot: Column out of bound ' + inttostr(col));
    result := true;
  end;
  if (row < low(FData[col])) or (row > high(FData[col])) then
  begin
    raise exception.Create('TEventSlot: Row out of bound ' + inttostr(row));
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

function TEventSlots.getColUsage(const col: integer): currency;
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
        AInd.init(i, j);
        result := true;
        exit;
      end;
end;

function TEventSlots.rawData: string;
var i, j, h: integer;
    dt: string;
begin
  result := #9;
  h := high(FData);
  if h > (FSlotsInUse - 1) then
    h := FSlotsInUse - 1;
  for j := low(FData[low(FData)]) to high(FData[low(FData)]) do
  begin
    DateTimeToString(dt, 'dd/mm/yy', FData[0, j].dateTime);
    result := result + dt + #9;
  end;
  result := result + #13;
  for i := low(FData) to h do
  begin
    DateTimeToString(dt, 'hh:mm', FData[i, low(FData[i])].dateTime);
    result := result + dt + #9;
    for j := low(FData[low(FData)]) to high(FData[low(FData)]) do
      if FData[i, j].populated = false then
        result := 'missing' + #9
      else
        result := result + currToStr(FData[i, j].usage) + #9;
    result := result + #13;
  end;
    result := result + #13;

end;

function TEventSlots.hasMissingData: boolean;
var i, j, h: integer;
begin
  result := false;
  h := high(FData);
  if h > (FSlotsInUse - 1) then
    h := FSlotsInUse - 1;
  for i := low(FData) to h do
     for j := low(FData[low(FData)]) to high(FData[low(FData)]) do
       if FData[i, j].populated = false then
         result := true;
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
  populated := false;
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

end.

