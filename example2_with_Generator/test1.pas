﻿unit test1;

interface

// *********************************** 
//   classes for test1.proto
//   generated by ProtoBufGenerator 
//        kami-soft 2016-2017
// ***********************************
uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  pbInput,
  pbOutput,
  pbPublic,
  uAbstractProtoBufClasses,
  TestImport1;

type

  TEnumG0=(
    g1 = 1,
    g2 = 2
  );

  TEnum1=(
    Val1 = 1,
    Val2 = 2
  );

  TTestMsg0 = class(TAbstractProtoBufClass)
  strict private
    FField1: integer;
    FField2: Int64;
  strict protected
    function LoadSingleFieldFromBuf(ProtoBuf: TProtoBufInput; FieldNumber: integer; WireType: integer): Boolean; override;
    procedure SaveFieldsToBuf(ProtoBuf: TProtoBufOutput); override;
  public
    constructor Create; override;
    destructor Destroy; override;


    property Field1:integer read FField1 write FField1;
    property Field2:Int64 read FField2 write FField2;
  end;

  TTestNested1 = class(TAbstractProtoBufClass)
  strict private
    FField1: integer;
  strict protected
    function LoadSingleFieldFromBuf(ProtoBuf: TProtoBufInput; FieldNumber: integer; WireType: integer): Boolean; override;
    procedure SaveFieldsToBuf(ProtoBuf: TProtoBufOutput); override;
  public

    property Field1:integer read FField1 write FField1;
  end;

  TTestMsg1 = class(TAbstractProtoBufClass)
  strict private
    FDefField1: integer;
    FDefField2: Int64;
    FDefField3: string;
    FDefField4: Double;
    FDefField5: Boolean;
    FDefField6: TEnumG0;
    FDefField7: Int64;
    FDefField8: integer;
    FDefField9: Single;
    FFieldMsg1: TTestMsg0;
    FFieldE1: TEnum1;
    FFieldE2: TEnum1;
    FFieldNested1: TTestNested1;
    FFieldNested2: TTestNested1;
    FFieldArr1List: TList<integer>;
    FFieldArr2List: TList<integer>;
    FFieldArr3List: TList<string>;
    FFieldArrE1List: TList<TEnum1>;
    FFieldMArr2List: TProtoBufClassList<TTestMsg0>;
    FFieldImp1: TEnumGlobal;
    FFieldImp2: TEnumGlobal;
  strict protected
    function LoadSingleFieldFromBuf(ProtoBuf: TProtoBufInput; FieldNumber: integer; WireType: integer): Boolean; override;
    procedure SaveFieldsToBuf(ProtoBuf: TProtoBufOutput); override;
  public
    constructor Create; override;
    destructor Destroy; override;


    property DefField1:integer read FDefField1 write FDefField1 default 2;
    property DefField2:Int64 read FDefField2 write FDefField2 default -1;
    property DefField3:string read FDefField3 write FDefField3; // default 'yes';
    property DefField4:Double read FDefField4 write FDefField4; // default 1.1;
    property DefField5:Boolean read FDefField5 write FDefField5; // default true;
    property DefField6:TEnumG0 read FDefField6 write FDefField6 default g2;
    property DefField7:Int64 read FDefField7 write FDefField7 default 100;
    property DefField8:integer read FDefField8 write FDefField8 default 1;
// field of message type 
    property DefField9:Single read FDefField9 write FDefField9; // default 1.23e1;
// nested enumeration 
    property FieldMsg1:TTestMsg0 read FFieldMsg1;
    property FieldE1:TEnum1 read FFieldE1 write FFieldE1;
// nested message 
    property FieldE2:TEnum1 read FFieldE2 write FFieldE2 default Val2;
    property FieldNested1:TTestNested1 read FFieldNested1;
// repeated fields 
    property FieldNested2:TTestNested1 read FFieldNested2 write FFieldNested2;
    property FieldArr1List:TList<integer> read FFieldArr1List;
    property FieldArr2List:TList<integer> read FFieldArr2List;
    property FieldArr3List:TList<string> read FFieldArr3List;
    property FieldArrE1List:TList<TEnum1> read FFieldArrE1List;
// fields of imported types 
    property FieldMArr2List:TProtoBufClassList<TTestMsg0> read FFieldMArr2List;
    property FieldImp1:TEnumGlobal read FFieldImp1 write FFieldImp1;
// extensions 1000 to 1999; 
    property FieldImp2:TEnumGlobal read FFieldImp2 write FFieldImp2;
  end;

  TTestMsg1Extension1 = class(TTestMsg1)
  strict private
    Ffield_name_test_1: integer;
    Ffield_Name_test_2: integer;
  strict protected
    function LoadSingleFieldFromBuf(ProtoBuf: TProtoBufInput; FieldNumber: integer; WireType: integer): Boolean; override;
    procedure SaveFieldsToBuf(ProtoBuf: TProtoBufOutput); override;
  public

    property field_name_test_1:integer read Ffield_name_test_1 write Ffield_name_test_1;
    property field_Name_test_2:integer read Ffield_Name_test_2 write Ffield_Name_test_2;
  end;


implementation


constructor TTestMsg0.Create;
begin
  inherited;
  RegisterRequiredField(1);
  RegisterRequiredField(2);
end;

destructor TTestMsg0.Destroy;
begin
  inherited;
end;

function TTestMsg0.LoadSingleFieldFromBuf(ProtoBuf: TProtoBufInput; FieldNumber: integer; WireType: integer): Boolean;
var
  tmpBuf: TProtoBufInput;
begin
  Result := inherited LoadSingleFieldFromBuf(ProtoBuf, FieldNumber, WireType);
  if Result then
    exit;
  case fieldNumber of
    1:
      begin
        FField1 := ProtoBuf.readInt32;
        Result := True;
      end;
    2:
      begin
        FField2 := ProtoBuf.readInt64;
        Result := True;
      end;
  end;
end;

procedure TTestMsg0.SaveFieldsToBuf(ProtoBuf: TProtoBufOutput);
var
  tmpBuf: TProtoBufOutput;
begin
  inherited;
  ProtoBuf.writeInt32(1, FField1);
  ProtoBuf.writeInt64(2, FField2);
end;


function TTestNested1.LoadSingleFieldFromBuf(ProtoBuf: TProtoBufInput; FieldNumber: integer; WireType: integer): Boolean;
begin
  Result := inherited LoadSingleFieldFromBuf(ProtoBuf, FieldNumber, WireType);
  if Result then
    exit;
  case fieldNumber of
    1:
      begin
        FField1 := ProtoBuf.readInt32;
        Result := True;
      end;
  end;
end;

procedure TTestNested1.SaveFieldsToBuf(ProtoBuf: TProtoBufOutput);
begin
  inherited;
  ProtoBuf.writeInt32(1, FField1);
end;


constructor TTestMsg1.Create;
begin
  inherited;
  FDefField1 := 2;
  FDefField2 := -1;
  FDefField3 := 'yes';
  FDefField4 := 1.1;
  FDefField5 := true;
  FDefField6 := g2;
  FDefField7 := 100;
  FDefField8 := 1;
  FDefField9 := 1.23e1;
  FFieldMsg1 := TTestMsg0.Create;
  FFieldE2 := Val2;
  FFieldNested1 := TTestNested1.Create;
  FFieldArr1List := TList<integer>.Create;
  FFieldArr2List := TList<integer>.Create;
  FFieldArr3List := TList<string>.Create;
  FFieldArrE1List := TList<TEnum1>.Create;
  FFieldMArr2List := TProtoBufClassList<TTestMsg0>.Create;
end;

destructor TTestMsg1.Destroy;
begin
  FFieldMsg1.Free;
  FFieldNested1.Free;
  FFieldArr1List.Free;
  FFieldArr2List.Free;
  FFieldArr3List.Free;
  FFieldArrE1List.Free;
  FFieldMArr2List.Free;
  inherited;
end;

function TTestMsg1.LoadSingleFieldFromBuf(ProtoBuf: TProtoBufInput; FieldNumber: integer; WireType: integer): Boolean;
var
  tmpBuf: TProtoBufInput;
begin
  Result := inherited LoadSingleFieldFromBuf(ProtoBuf, FieldNumber, WireType);
  if Result then
    exit;
  case fieldNumber of
    1:
      begin
        FDefField1 := ProtoBuf.readInt32;
        Result := True;
      end;
    2:
      begin
        FDefField2 := ProtoBuf.readInt64;
        Result := True;
      end;
    3:
      begin
        FDefField3 := ProtoBuf.readString;
        Result := True;
      end;
    4:
      begin
        FDefField4 := ProtoBuf.readDouble;
        Result := True;
      end;
    5:
      begin
        FDefField5 := ProtoBuf.readBoolean;
        Result := True;
      end;
    6:
      begin
        FDefField6 := TEnumG0(ProtoBuf.readEnum);
        Result := True;
      end;
    7:
      begin
        FDefField7 := ProtoBuf.readSInt64;
        Result := True;
      end;
    8:
      begin
        FDefField8 := ProtoBuf.readFixed32;
        Result := True;
      end;
    9:
      begin
        FDefField9 := ProtoBuf.readFloat;
        Result := True;
      end;
    20:
      begin
        tmpBuf := ProtoBuf.ReadSubProtoBufInput;
        try
          FFieldMsg1.LoadFromBuf(tmpBuf);
        finally
          tmpBuf.Free;
        end;
        Result := True;
      end;
    21:
      begin
        FFieldE1 := TEnum1(ProtoBuf.readEnum);
        Result := True;
      end;
    22:
      begin
        FFieldE2 := TEnum1(ProtoBuf.readEnum);
        Result := True;
      end;
    30:
      begin
        tmpBuf := ProtoBuf.ReadSubProtoBufInput;
        try
          FFieldNested1.LoadFromBuf(tmpBuf);
        finally
          tmpBuf.Free;
        end;
        Result := True;
      end;
    31:
      begin
        FFieldNested2 := TTestNested1(ProtoBuf.readEnum);
        Result := True;
      end;
    40:
      begin
        FFieldArr1List.Add(ProtoBuf.readInt32);
        Result := True;
      end;
    41:
      begin
        if WireType = WIRETYPE_LENGTH_DELIMITED then
          begin
            tmpBuf:=ProtoBuf.ReadSubProtoBufInput;
            try
              while tmpBuf.getPos<tmpBuf.BufSize do
                FFieldArr2List.Add(tmpBuf.readRawVarint32);
            finally
              tmpBuf.Free;
            end;
          end
        else
          FFieldArr2List.Add(ProtoBuf.readRawVarint32);
        Result := True;
      end;
    42:
      begin
        FFieldArr3List.Add(ProtoBuf.readString);
        Result := True;
      end;
    43:
      begin
        FFieldArrE1List.Add(TEnum1(ProtoBuf.readEnum));
        Result := True;
      end;
    44:
      begin
        FFieldMArr2List.AddFromBuf(ProtoBuf, fieldNumber);
        Result := True;
      end;
    50:
      begin
        FFieldImp1 := TEnumGlobal(ProtoBuf.readEnum);
        Result := True;
      end;
    51:
      begin
        FFieldImp2 := TEnumGlobal(ProtoBuf.readEnum);
        Result := True;
      end;
  end;
end;

procedure TTestMsg1.SaveFieldsToBuf(ProtoBuf: TProtoBufOutput);
var
  tmpBuf: TProtoBufOutput;
  i: integer;
begin
  inherited;
  ProtoBuf.writeInt32(1, FDefField1);
  ProtoBuf.writeInt64(2, FDefField2);
  ProtoBuf.writeString(3, FDefField3);
  ProtoBuf.writeDouble(4, FDefField4);
  ProtoBuf.writeBoolean(5, FDefField5);
  ProtoBuf.writeInt32(6, integer(FDefField6));
  ProtoBuf.writeSInt64(7, FDefField7);
  ProtoBuf.writeFixed32(8, FDefField8);
  ProtoBuf.writeFloat(9, FDefField9);
  tmpBuf:=TProtoBufOutput.Create;
  try
    FFieldMsg1.SaveToBuf(tmpBuf);
    ProtoBuf.writeMessage(20, tmpBuf);
  finally
    tmpBuf.Free;
  end;
  ProtoBuf.writeInt32(21, integer(FFieldE1));
  ProtoBuf.writeInt32(22, integer(FFieldE2));
  tmpBuf:=TProtoBufOutput.Create;
  try
    FFieldNested1.SaveToBuf(tmpBuf);
    ProtoBuf.writeMessage(30, tmpBuf);
  finally
    tmpBuf.Free;
  end;
  ProtoBuf.writeInt32(31, integer(FFieldNested2));
  for i := 0 to FFieldArr1List.Count-1 do
    ProtoBuf.writeInt32(40, FFieldArr1List[i]);
  tmpBuf:=TProtoBufOutput.Create;
  try
    for i := 0 to FFieldArr2List.Count-1 do
      tmpBuf.writeRawVarint32(FFieldArr2List[i]);
    ProtoBuf.writeMessage(41, tmpBuf);
  finally
    tmpBuf.Free;
  end;
  for i := 0 to FFieldArr3List.Count-1 do
    ProtoBuf.writeString(42, FFieldArr3List[i]);
  for i := 0 to FFieldArrE1List.Count-1 do
    ProtoBuf.writeInt32(43, integer(FFieldArrE1List[i]));
  FFieldMArr2List.SaveToBuf(ProtoBuf, 44);
  ProtoBuf.writeInt32(50, integer(FFieldImp1));
  ProtoBuf.writeInt32(51, integer(FFieldImp2));
end;


function TTestMsg1Extension1.LoadSingleFieldFromBuf(ProtoBuf: TProtoBufInput; FieldNumber: integer; WireType: integer): Boolean;
begin
  Result := inherited LoadSingleFieldFromBuf(ProtoBuf, FieldNumber, WireType);
  if Result then
    exit;
  case fieldNumber of
    187:
      begin
        Ffield_name_test_1 := ProtoBuf.readInt32;
        Result := True;
      end;
    220:
      begin
        Ffield_Name_test_2 := ProtoBuf.readInt32;
        Result := True;
      end;
  end;
end;

procedure TTestMsg1Extension1.SaveFieldsToBuf(ProtoBuf: TProtoBufOutput);
begin
  inherited;
  ProtoBuf.writeInt32(187, Ffield_name_test_1);
  ProtoBuf.writeInt32(220, Ffield_Name_test_2);
end;

end.
