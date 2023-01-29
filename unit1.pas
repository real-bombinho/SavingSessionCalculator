unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdHTTP, uPascalTZ, Forms, Controls, Graphics, Dialogs,
  Grids, StdCtrls, DBGrids, ComCtrls, DateUtils, Octopus, parse, csvdataset, db;

type

  AEventSlots = array[1..8, 0..10] of REventSlot;

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    CSVDataset1: TCSVDataset;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private
    IDAslots: TEventSlots;
    SSslots: TEventSlots;
    IDAaverage: currency;
    UsageAverage: currency;
    Usage: currency;
    IDA: currency;
    SavingTotal: currency;
    SavingSessionDays: TStringList;
    SavingSessionEvent: RSavingSessionEvent;
    procedure fillDates(const value: tDateTime);
    function pullData: boolean;
    procedure fillIDA(const Values: TStrings);
    procedure unhandledFault(const AValue: string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function isSavingSession(const value: tDateTime; const setTimes: boolean = false): integer;
begin
  result := -1;
  Form1.CSVDataset1.First;
  if Form1.CSVDataset1.Fields[0].AsString <> 'Date' then
    showmessage('Check csv file format')
  else
  begin
    Form1.CSVDataset1.Next;
    repeat
      if Form1.CSVDataset1.Fields[0].AsDateTime = trunc(value) then
      begin
        result := Form1.CSVDataset1.RecNo;
        if setTimes then
        begin
          Form1.SavingSessionEvent.From := Form1.CSVDataset1.Fields[1].AsDateTime + trunc(value);
          Form1.SavingSessionEvent.Till := Form1.CSVDataset1.Fields[2].AsDateTime + trunc(value);
        end;
        Form1.CSVDataset1.Last;
      end;
      Form1.CSVDataset1.Next;
    until Form1.CSVDataset1.EOF;
  end;
  if (result = -1) and setTimes then
  begin
    Form1.SavingSessionEvent.From := 0;
    Form1.SavingSessionEvent.Till := 0;
  end;
end;

procedure TForm1.fillDates(const value: tDateTime);
var i, d, index: integer;
begin
  index := isSavingSession(value, true);  // sets SsvingsSessionEvent if possible
  if index = -1 then
    raise exception.Create('date is not a saving session');
  IDAslots.Clear;
  SSslots.Clear;
  SSslots.setConsecutiveTimeslots(0, SavingSessionEvent.From, SavingSessionEvent.Slots);
  IDASlots.setConsecutiveTimeslots(0, incMinute(SavingSessionEvent.From, -240), 6);
  d := 1;
  i := 1;
  if dayOfWeek(value) in [1, 7] then
    raise exception.Create('implement weekends')
  else
  begin
    StringGrid2.Cells[0, 1] := datetostr(value);
  while i <= 10 do
    begin
      if dayOfWeek(value - d) in [1, 7] then
      begin
        inc(d);
      end
      else
      begin
        while isSavingSession(value - d) <> -1 do inc(d);
        StringGrid1.Cells[0,i + 2] := datetostr(value - d);
        StringGrid2.Cells[0,i + 2] := datetostr(value - d);
        IDAslots.setConsecutiveTimeslots(i, incMinute(SavingSessionEvent.From - d, -240), 6);
        SSslots.setConsecutiveTimeslots(i, SavingSessionEvent.From - d, SavingSessionEvent.Slots);
        inc(i);
        inc(d);
      end;
    end;
  end;
  for i := 0 to 5 do
    StringGrid1.Columns[i].Title.Caption := timetoStr(IDAslots.DateTime[i, 0], DefaultFormatSettings);
  for i := 0 to SavingSessionEvent.Slots - 1 do
    StringGrid2.Columns[i].Title.Caption := timetoStr(SSslots.DateTime[i, 0], DefaultFormatSettings);
  for i := SavingSessionEvent.Slots to 7 do
  begin
    StringGrid2.Columns[i].Title.Caption := 'n/a';
    for d := 1 to 13 do
      StringGrid2.Cells[i, d] := '';
  end;

  //for i := 0 to 5 do
  //  StringGrid1.Cells[i + 1, 1] := datetimetoStr(IDAslots.DateTime[i, 0], DefaultFormatSettings);
  //for i := 0 to SavingSessionEvent.Slots - 1 do
  //  StringGrid2.Cells[i + 1, 1] := timetoStr(SSslots.DateTime[i, 0], DefaultFormatSettings);
end;

function parseDate(value: string): TDatetime;
var s: string;
    p: integer;
    dt: array[0..2] of word;
    i: integer;
begin
  s := value;
  for i := 0 to 1 do
  begin
    p := pos(DefaultFormatSettings.DateSeparator, s);
    if p = 0 then
    begin
      result := 0;
      raise Exception.Create('Missing date separator "' +
       DefaultFormatSettings.DateSeparator + '"');
      exit;
    end;
    dt[i] := strToInt(copy(s, 1, p-1));
    delete(s, 1, p);
  end;
  dt[2] := strToInt(copy(s, 1, 4));
  result := strToDate(value);
  if dayOfWeek(result) in [1,7] then
  begin
    showmessage(datetostr(result) + ' weekend');
  end
  else
  begin
    Form1.fillDates(result);
  end;

end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  progressbar1.Position := 0;
  progressbar1.Visible := true;
  Application.ProcessMessages;
  parseDate(ListBox1.Items[ListBox1.ItemIndex]);
  PullData;
  progressbar1.Position := 100;
  progressbar1.Visible := false;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DefaultFormatSettings.ShortDateFormat := 'dd/mm/yyyy';
  DefaultFormatSettings.LongTimeFormat := 'hh:mm';
  DefaultFormatSettings.DateSeparator := '/';
  SavingSessionDays := TStringlist.Create;
  SavingSessionDays.Clear;

  CSVDataSet1.LoadFromCSVFile('sessions.csv');
  CSVDataset1.First;
  if CSVDataset1.Fields[0].AsString <> 'Date' then
    showmessage('Check csv file format')
  else
  begin
    CSVDataset1.Next;
    repeat
      SavingSessionDays.Add(CSVDataset1.Fields[0].AsString);
      CSVDataset1.Next;
    until CSVDataset1.EOF;
  end;
  Form1.ListBox1.Items := SavingSessionDays;
  IDAslots := TEventSlots.Create(6, 11);
  SSslots := TEventSlots.Create(8, 11);
  ProgressBar1.Visible := false;
  CheckBox1.Checked := false;
end;

function TForm1.pullData: boolean;
var o: TOctopus;
    r: RResponse;
    i, c, pagesBetween: integer;
    s, url: string;
    firstStart, lastNeeded: tDateTime;
    ConsumptionData: array of RConsumption;
    results: TStringlist;
begin
  result := false;
  results := TStringList.Create;
  if pos('sk_live_', edit1.Text) <> 1 then
  begin
    showmessage('Invalid API key - stopped');
    exit;
  end;
  if length(edit2.Text) <> 13 then
  begin
    showmessage('MPAN seems invalid - stopped');
    exit;
  end;
  if length(edit3.Text) <> 10 then
  begin
    showmessage('Meter serial seems invalid - stopped');
    exit;
  end;
  o := TOctopus.Create(edit1.Text, SouthernScotland, '');      // region is not used here
  url := OctopusURL + '/v1/electricity-meter-points/' + edit2.Text + '/meters/' +
   edit3.Text + '/consumption/';
  try
    o.fetch(url);
  except
    unhandledFault(OctopusFault + #10 + #13 + #10 + #13 +
      'Please check your entries - stopped');
    results.Free;
    exit;
  end;
  progressbar1.Position := 10;
  Application.ProcessMessages;
  sleep(5);
//  memo1.Text := o.LastResponse;
  r.parse(o.LastResponse, results);
  c := results.Count;
  i := pos('"interval_start":"', results[0]);
  s := results.Strings[0].Substring(i + 17, 20);
  if parse.parseDate(s, firstStart) then ;
  pagesBetween := hoursBetween(SavingSessionEvent.From, firstStart) * 2 div c;
  //showmessage(inttostr(pagesbetween));
  if pagesBetween = 0 then
  begin
    memo1.Lines := results;
  end
  else
  begin
    try
      o.fetch(url + '?page=' + inttostr(pagesBetween));
    except
      unhandledFault(OctopusFault + #10 + #13 + #10 + #13 +
        'check the saving session date - stopped' + #10 + #13 +
        'Response - ' + o.ResponseText);
      results.Free;
      exit;
    end;
    r.parse(o.LastResponse, results);
    memo1.Lines := results;
  end;

  lastNeeded := IDAslots.DateTime[0, 10];
//  showmessage(IDAslots.dateTimeStr[0, 9]);
  setlength(consumptionData, 1);
  repeat
  progressbar1.Position := progressbar1.Position + 2;
  Application.ProcessMessages;
  try
    o.fetch(r.next);
  except
    unhandledFault(OctopusFault + ' - stopped' + #10 + #13 +
        'Response - ' + o.ResponseText);
    results.Free;
    exit;
  end;
  progressbar1.Position := 8 + progressbar1.Position;
  r.parse(o.LastResponse, results);
  memo1.Lines.AddStrings(results);
  consumptionData[0].Line := memo1.Lines[memo1.Lines.Count - 1];
  until consumptionData[0].From < lastNeeded;
  o.Free;

  fillIDA(memo1.Lines);
  progressbar1.Visible := false;
  result := true;
  results.Free;
end;

procedure TForm1.fillIDA(const Values: TStrings);
var ConsumptionData: array of RConsumption;
    i, pagesBetween: integer;
    total, sum: currency;
    ind: RIndex;
begin
  Application.ProcessMessages;
  setlength(consumptionData, memo1.lines.Count);
  i := 0;
  repeat //memo1.Lines.Count -1 do
    consumptionData[i].Line := memo1.Lines[i];
    //showmessage(datetimetostr(consumptionData[i].From) + ' : ' + datetimetostr(IDAslots.DateTime[0,0]));
    if IDAslots.findIndex(consumptionData[i].From, ind, encodeTime(0, 30, 0, 0)) then
    begin
      IDAslots.Usage[ind.col, ind.row] := consumptionData[i].Consumption;
      if ind.row = 0 then
        stringGrid1.Cells[ind.col + 1, 1] := currtostr(consumptionData[i].Consumption)
      else
        stringGrid1.Cells[ind.col + 1, ind.row + 2] := currtostr(consumptionData[i].Consumption);
    end;
    if SSslots.findIndex(consumptionData[i].From, ind, encodeTime(0, 30, 0, 0)) then
    begin
      SSslots.Usage[ind.col, ind.row] := consumptionData[i].Consumption;
      if ind.row = 0 then
        stringGrid2.Cells[ind.col + 1, 1] := currtostr(consumptionData[i].Consumption)
      else
        stringGrid2.Cells[ind.col + 1, ind.row + 2] := currtostr(consumptionData[i].Consumption);
    end;
    inc(i);
  until (i >= memo1.Lines.Count);
  sum := 0;
  for i := 1 to 10 do
  begin
    total := IDAslots.rowUsage(i);
    stringGrid1.Cells[8, i + 2] := currtostr(total);
    sum := sum + total;
  end;
  IDAaverage := sum / 60;
  stringGrid1.Cells[8, 13] := currtostr(sum);
  stringGrid1.Cells[6, 13] := 'Sum';
  stringGrid1.Cells[3, 13] := 'Average';
  stringGrid1.Cells[4, 13] := currtostr(IDAaverage);
  stringGrid1.Cells[0, 13] := 'IDA';
  IDA := (IDAslots.rowUsage(0) / 6) - IDAAverage;
  stringGrid1.Cells[8, 1]  := currtostr(IDAslots.rowUsage(0));
  stringGrid1.Cells[1, 13] := currtostr(IDA);
  Usage := SSslots.rowUsage(0);
  StringGrid2.Cells[10, 1] := currtostr(Usage);
  sum := 0;
  for i := 1 to 10 do
  begin
    total := SSslots.rowUsage(i);
    stringGrid2.Cells[10, i + 2] := currtostr(total);
    sum := sum + total;
  end;
  UsageAverage := sum / 10 / SavingSessionEvent.Slots;
  stringGrid2.Cells[0, 13] := 'Slot Saving';
  savingTotal := IDA * SavingSessionEvent.Slots;
  for i := 1 to SavingSessionEvent.Slots do
  begin
    stringGrid2.Cells[i, 13] := currToStr(((SSslots.columnUsage(i - 1) -
      SSslots.Usage[i - 1, 0])/10) - SSslots.Usage[i - 1, 0]);
    savingTotal := savingTotal + ((SSslots.columnUsage(i - 1) -
      SSslots.Usage[i - 1, 0])/10) - SSslots.Usage[i - 1, 0];
  end;
  stringGrid2.Cells[10, 13] := currtostr(sum);
  Edit4.text := currtostr(savingTotal);
  //stringGrid2.Cells[5, 13] := 'Average';
  //stringGrid2.Cells[6, 13] := currtostr(UsageAverage);
  //stringGrid2.Cells[1, 13] := 'Saving';
  //stringGrid2.Cells[2, 13] := currtostr(UsageAverage - usage + IDAAverage);
end;

procedure TForm1.unhandledFault(const AValue: string);
var i, j: integer;
begin
  memo1.Clear;
  for i := 0 to stringGrid1.ColCount - 1 do
    stringgrid1.Cols[i].Clear;
  for i := 0 to stringGrid2.ColCount - 1 do
    stringgrid2.Cols[i].Clear;
  Edit4.Text := '';
  showmessage(AValue);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  showmessage(UTCString(now()));
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    edit1.EchoMode := TEchoMode.emNormal;
    edit2.EchoMode := TEchoMode.emNormal;
    edit3.EchoMode := TEchoMode.emNormal;
  end
  else
  begin
    edit1.EchoMode := TEchoMode.emPassword;
    edit2.EchoMode := TEchoMode.emPassword;
    edit3.EchoMode := TEchoMode.emPassword;
  end;
end;

end.

