unit uProtoBufGenerator;

interface

uses
  System.SysUtils,
  System.Classes,
  uAbstractProtoBufClasses,
  uProtoBufParserAbstractClasses,
  uProtoBufParserClasses;

type
  TProtoBufGenerator = class(TObject)
  private const
    /// <summary>
    /// Initialize occurs when there is a tag of a serialized element.
    /// </summary>
    InitializeProcName = 'Initialize';
    /// <summary>
    /// OnZeroLength occurs when Len of serialized element is 0.
    /// According to gRPC protocol documentation, that means, all fields of that element have default values.
    /// Keep in mind, that "default value" is a language-specitic thing.
    /// </summary>
    OnZeroLengthProcName = 'OnZeroLength';
    /// <summary>
    /// Empty class function returns an element of it's type with "_HasValue" filled by False values.
    /// </summary>
    EmptyClassFunctionName = 'Empty';
    HasValuePostfixName = '_HasValue';
  strict private
    procedure GenerateInterfaceSection(Proto: TProtoFile; SL: TStrings);
    procedure GenerateImplementationSection(Proto: TProtoFile; SL: TStrings);
  public
    procedure Generate(Proto: TProtoFile; Output: TStrings); overload;
    procedure Generate(Proto: TProtoFile; OutputStream: TStream; Encoding: TEncoding = nil); overload;
    procedure Generate(Proto: TProtoFile; const OutputDir: string; Encoding: TEncoding = nil); overload;
    procedure Generate(const InputFile: string; const OutputDir: string; Encoding: TEncoding = nil); overload;
  end;

implementation

uses
  System.StrUtils;

function ProtoPropTypeToDelphiType(const PropTypeName: string): string;
var
  StandartType: TScalarPropertyType;
  i: Integer;
begin
  StandartType := StrToPropertyType(PropTypeName);
  case StandartType of
    sptDouble:
    Result := 'Double';
    sptFloat:
    Result := 'Single';
    sptInt32:
    Result := 'Integer';
    sptInt64:
    Result := 'Int64';
    sptuInt32:
    Result := 'Cardinal';
    sptUint64:
    Result := 'UInt64';
    sptSInt32:
    Result := 'SignedInt32';
    sptSInt64:
    Result := 'SignedInt64';
    sptFixed32:
    Result := 'Integer';
    sptFixed64:
    Result := 'Int64';
    sptSFixed32:
    Result := 'Integer';
    sptSFixed64:
    Result := 'Int64';
    sptBool:
    Result := 'Boolean';
    sptString:
    Result := 'string';
    sptBytes:
    Result := 'TBytes';
    else
    i := LastDelimiter('.', PropTypeName);
    Result := 'T' + Copy(PropTypeName, i + 1, Length(PropTypeName));
  end;
end;

function PropertyIsPrimitiveNumericPacked(Prop: TProtoBufProperty): Boolean;
begin
  Result := (Prop.PropOptions.Value['packed'] = 'true') and
    (StrToPropertyType(Prop.PropType) in [sptDouble, sptFloat, sptInt32, sptInt64, sptuInt32, sptUint64, sptSInt32,
    sptSInt64, sptBool, sptFixed32, sptSFixed32, sptFixed64, sptSFixed64])
end;

function GetProtoBufMethodForScalarType(const Prop: TProtoBufProperty): string;
var
  StandartType: TScalarPropertyType;
  bPacked: Boolean;
begin
  StandartType := StrToPropertyType(Prop.PropType);
  bPacked := (Prop.PropKind = ptRepeated) and PropertyIsPrimitiveNumericPacked(Prop);

  case StandartType of
    sptComplex:
    ;
    sptDouble:
    if bPacked then
      Result := 'RawData'
    else
      Result := 'Double';
    sptFloat:
    if bPacked then
      Result := 'RawData'
    else
      Result := 'Float';
    sptInt32:
    if bPacked then
      Result := 'RawVarint32'
    else
      Result := 'Int32';
    sptInt64:
    if bPacked then
      Result := 'RawVarint64'
    else
      Result := 'Int64';
    sptuInt32:
    if bPacked then
      Result := 'RawVarint32'
    else
      Result := 'UInt32';
    sptUint64:
    if bPacked then
      Result := 'RawVarint64'
    else
      Result := 'Int64';
    sptSInt32:
    if bPacked then
      Result := 'RawSInt32'
    else
      Result := 'SInt32';
    sptSInt64:
    if bPacked then
      Result := 'RawSInt64'
    else
      Result := 'SInt64';
    sptFixed32:
    if bPacked then
      Result := 'RawData'
    else
      Result := 'Fixed32';
    sptFixed64:
    if bPacked then
      Result := 'RawData'
    else
      Result := 'Fixed64';
    sptSFixed32:
    if bPacked then
      Result := 'RawData'
    else
      Result := 'SFixed32';
    sptSFixed64:
    if bPacked then
      Result := 'RawData'
    else
      Result := 'SFixed64';
    sptBool:
    if bPacked then
      Result := 'RawBoolean'
    else
      Result := 'Boolean';
    sptString:
    Result := 'String';
    sptBytes:
    Result := 'Bytes';
  end;
end;

function ReQuoteStr(const Str: string): string;
begin
  Result := Str;
  if not StartsStr('"', Str) then
    Exit;

  Result := StringReplace(Str, '''', '''''', [rfReplaceAll]);
  Result := StringReplace(Result, '""', '"', [rfReplaceAll]);
  Result[1] := '''';
  Result[Length(Result)] := '''';
end;

type
  TDelphiProperty = record
    IsList: Boolean;
    isComplex: Boolean;
    isObject: Boolean;
    PropertyName: string;
    PropertyType: string;
    function tagName: string;
    function readOnlyDelphiProperty: Boolean;
    /// <summary>
    /// Simple type, eg. Integer, string, Boolean, etc.
    /// </summary>
    function IsSimpleType: Boolean;
  end;

procedure ParsePropType(Prop: TProtoBufProperty; Proto: TProtoFile; out DelphiProp: TDelphiProperty);
begin
  DelphiProp.IsList := Prop.PropKind = ptRepeated;
  DelphiProp.isComplex := StrToPropertyType(Prop.PropType) = sptComplex;
  if DelphiProp.isComplex then
    DelphiProp.isObject := Assigned(Proto.ProtoBufMessages.FindByName(Prop.PropType))
  else
    DelphiProp.isObject := False;
  if not DelphiProp.IsList then begin
    DelphiProp.PropertyName := Prop.Name;
    DelphiProp.PropertyType := ProtoPropTypeToDelphiType(Prop.PropType);
  end
  else begin
    DelphiProp.PropertyName := Format('%sArray', [Prop.Name]);
    DelphiProp.PropertyType := Format('TArray<%s>', [ProtoPropTypeToDelphiType(Prop.PropType)]);
  end;

end;

function MsgNeedConstructor(ProtoMsg: TProtoBufMessage; Proto: TProtoFile): Boolean;
var
  i: Integer;
  DelphiProp: TDelphiProperty;
  Prop: TProtoBufProperty;
begin
  Result := False;
  for i := 0 to ProtoMsg.Count - 1 do begin
    Prop := ProtoMsg[i];
    ParsePropType(Prop, Proto, DelphiProp);
    Result := (Prop.PropKind = ptRequired) or DelphiProp.IsList or DelphiProp.isObject or
      Prop.PropOptions.HasValue['default'];
    if Result then
      Break;
  end;
end;

function MsgContainsRepeatedFields(ProtoMsg: TProtoBufMessage): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to ProtoMsg.Count - 1 do
    if ProtoMsg[i].PropKind = TPropKind.ptRepeated then begin
      Result := True;
      Break;
    end;
end;

{TProtoBufGenerator}

procedure TProtoBufGenerator.Generate(Proto: TProtoFile; Output: TStrings);
var
  SLInterface, SLImplementation: TStrings;
begin
  // write name and interface uses
  SLInterface := TStringList.Create;
  try
    SLImplementation := TStringList.Create;
    try
      GenerateInterfaceSection(Proto, SLInterface);
      GenerateImplementationSection(Proto, SLImplementation);

      Output.Clear;
      Output.AddStrings(SLInterface);
      Output.AddStrings(SLImplementation);
    finally
      SLImplementation.Free;
    end;
  finally
    SLInterface.Free;
  end;
end;

procedure TProtoBufGenerator.Generate(Proto: TProtoFile; OutputStream: TStream; Encoding: TEncoding);
var
  SL: TStrings;
begin
  SL := TStringList.Create;
  try
    SL.WriteBOM := True;
    Generate(Proto, SL);
    SL.SaveToStream(OutputStream, Encoding);
  finally
    SL.Free;
  end;
end;

procedure TProtoBufGenerator.Generate(Proto: TProtoFile; const OutputDir: string; Encoding: TEncoding);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(IncludeTrailingPathDelimiter(OutputDir) + Proto.Name + '.pas', fmCreate);
  try
    Generate(Proto, FS, Encoding);
  finally
    FS.Free;
  end;
end;

procedure TProtoBufGenerator.GenerateImplementationSection(Proto: TProtoFile; SL: TStrings);
  procedure WriteInitialize(ProtoMsg: TProtoBufMessage; SL: TStrings);
  var
    Prop: TProtoBufProperty;
    DelphiProp: TDelphiProperty;
    i: Integer;
  begin
{$REGION 'procedure Initialize'}
    /// <remarks>
    /// Read InitializeProcName summary to get what it is.
    /// </remarks>
    SL.Add(Format('procedure T%s.%s;', [ProtoMsg.Name, InitializeProcName]));
    SL.Add('begin');
    SL.Add('  // We assume if there is a tag, all scalar fields are filled at least with default values');
    for i := 0 to ProtoMsg.Count - 1 do begin
      Prop := ProtoMsg[i];
      ParsePropType(Prop, Proto, DelphiProp);

      if DelphiProp.IsSimpleType then begin
        SL.Add(Format('  F%s%s := True;', [DelphiProp.PropertyName, HasValuePostfixName]));
        SL.Add(Format('  F%s := Default(%s);', [DelphiProp.PropertyName, DelphiProp.PropertyType]));
      end
      else if DelphiProp.isObject and not DelphiProp.IsList then begin
        /// <remarks>
        /// Read EmptyClassFunctionName summary to get what it is.
        /// </remarks>
        SL.Add(Format('  F%s := %s.%s;', //
          [DelphiProp.PropertyName, DelphiProp.PropertyType, EmptyClassFunctionName]));
      end;
    end;
    SL.Add('end;');

    SL.Add('');
{$ENDREGION}
{$REGION 'procedure OnZeroLength'}
    /// <remarks>
    /// Read OnZeroLengthProcName summary to get what it is.
    /// </remarks>
    SL.Add(Format('procedure T%s.%s;', [ProtoMsg.Name, OnZeroLengthProcName]));
    SL.Add('begin');
    SL.Add('  // If a length of an element = 0, then all fields are filled with default values');
    for i := 0 to ProtoMsg.Count - 1 do begin
      Prop := ProtoMsg[i];
      ParsePropType(Prop, Proto, DelphiProp);

      if DelphiProp.IsSimpleType then begin
        SL.Add(Format('  F%s%s := True;', [DelphiProp.PropertyName, HasValuePostfixName]));
        SL.Add(Format('  F%s := Default(%s);', [DelphiProp.PropertyName, DelphiProp.PropertyType]));
      end
      else if DelphiProp.isObject and not DelphiProp.IsList then begin
        SL.Add(Format('  %s.%s;', //
          [DelphiProp.PropertyName, OnZeroLengthProcName]));
      end;
    end;
    SL.Add('end;');

    SL.Add('');
{$ENDREGION}
{$REGION 'class function T%s.Empty'}
    SL.Add(Format('class function T%s.Empty: T%0:s;', [ProtoMsg.Name]));
    SL.Add('begin');

    for i := 0 to ProtoMsg.Count - 1 do begin
      Prop := ProtoMsg[i];
      ParsePropType(Prop, Proto, DelphiProp);

      if DelphiProp.isComplex and DelphiProp.isObject and not DelphiProp.IsList then begin
        SL.Add(Format('  Result.F%s := %s.Empty;', [DelphiProp.PropertyName, DelphiProp.PropertyType]));
      end
      else if DelphiProp.IsList then begin
        SL.Add(Format('  Result.F%s := nil;', [DelphiProp.PropertyName]));
      end
      else begin
        SL.Add(Format('  Result.F%s%s := False;', [DelphiProp.PropertyName, HasValuePostfixName]));
        SL.Add(Format('  Result.F%s := Default(%s);', [DelphiProp.PropertyName, DelphiProp.PropertyType]));
      end;

    end;
    SL.Add('end;');

    SL.Add('');
{$ENDREGION}
  end;

  procedure WriteLoadProc(ProtoMsg: TProtoBufMessage; SL: TStrings);
  var
    i, iInsertVarBlock, iInserttmpBufCreation, iBeginBlock: Integer;
    Prop: TProtoBufProperty;
    DelphiProp, OneOfDelphiProp: TDelphiProperty;
    bNeedtmpBuf: Boolean;
    sIndent: string;
  begin
    bNeedtmpBuf := False;
    SL.Add(Format
      ('function T%s.LoadSingleFieldFromBuf(ProtoBuf: TProtoBufInput; FieldNumber: Integer; WireType: Integer): Boolean;',
      [ProtoMsg.Name]));
    iInsertVarBlock := SL.Count;
    SL.Add('begin');
    SL.Add('  Result := inherited;');
    if ProtoMsg.Count = 0 then begin
      SL.Add('end;');
      Exit;
    end;
    SL.Add('  if Result then');
    SL.Add('    Exit;');
    SL.Add('  Result := True;');
    iInserttmpBufCreation := SL.Count;
    SL.Add('  case FieldNumber of');
    for i := 0 to ProtoMsg.Count - 1 do begin
      Prop := ProtoMsg[i];
      ParsePropType(Prop, Proto, DelphiProp);
      if Prop.PropKind = ptOneOf then
        Continue;
      SL.Add(Format('    %s:', [DelphiProp.tagName]));
      iBeginBlock := SL.Count;
      SL.Add('      begin');
      sIndent := StringOfChar(' ', 8); {4 for case + 4 for tag and begin/end}
      if not DelphiProp.IsList then begin
        if not DelphiProp.isComplex then
          SL.Add(Format('%s%s := ProtoBuf.read%s;', [sIndent, DelphiProp.PropertyName,
            GetProtoBufMethodForScalarType(Prop)]))
        else if not DelphiProp.isObject then
          SL.Add(Format('%s%s := %s(ProtoBuf.readEnum);', [sIndent, DelphiProp.PropertyName, DelphiProp.PropertyType]))
        else begin
          bNeedtmpBuf := True;
          SL.Add(Format('%stmpBuf := ProtoBuf.ReadSubProtoBufInput;', [sIndent]));
          SL.Add(Format('%sF%s.LoadFromBuf(tmpBuf);', [sIndent, DelphiProp.PropertyName]));
        end;
      end
      else begin
        if not DelphiProp.isComplex then begin
          if PropertyIsPrimitiveNumericPacked(Prop) then begin
            bNeedtmpBuf := True;
            SL.Add(Format('%sif WireType = WIRETYPE_LENGTH_DELIMITED then', [sIndent]));
            SL.Add(Format('%sbegin', [sIndent]));
            SL.Add(Format('%s  tmpBuf:=ProtoBuf.ReadSubProtoBufInput;', [sIndent]));
            SL.Add(Format('%s  while tmpBuf.getPos < tmpBuf.BufSize do', [sIndent]));
            SL.Add(Format('%s    F%s.Add(tmpBuf.read%s);', [sIndent, DelphiProp.PropertyName,
              GetProtoBufMethodForScalarType(Prop)]));
            SL.Add(Format('%send else', [sIndent]));
            SL.Add(Format('%s  F%s.Add(ProtoBuf.read%s);', [sIndent, DelphiProp.PropertyName,
              GetProtoBufMethodForScalarType(Prop)]));
          end
          else
            SL.Add(Format('%sF%s.Add(ProtoBuf.read%s);', [sIndent, DelphiProp.PropertyName,
              GetProtoBufMethodForScalarType(Prop)]));
        end
        else if not DelphiProp.isObject then begin
          if (Prop.PropOptions.Value['packed'] = 'true') then begin
            bNeedtmpBuf := True;
            SL.Add(Format('%sif WireType = WIRETYPE_LENGTH_DELIMITED then', [sIndent]));
            SL.Add(Format('%sbegin', [sIndent]));
            SL.Add(Format('%s  tmpBuf:=ProtoBuf.ReadSubProtoBufInput;', [sIndent]));
            SL.Add(Format('%s  while tmpBuf.getPos<tmpBuf.BufSize do', [sIndent]));
            SL.Add(Format('%s    F%s.Add(T%s(tmpBuf.readEnum));', [sIndent, DelphiProp.PropertyName, Prop.PropType]));
            SL.Add(Format('%send else', [sIndent]));
            SL.Add(Format('%s  F%s.Add(T%s(ProtoBuf.readEnum));', [sIndent, DelphiProp.PropertyName, Prop.PropType]));
          end
          else
            SL.Add(Format('%sF%s.Add(T%s(ProtoBuf.readEnum));', [sIndent, DelphiProp.PropertyName, Prop.PropType]));
        end
        else
          SL.Add(Format('%sF%s.AddFromBuf(ProtoBuf, fieldNumber);', [sIndent, DelphiProp.PropertyName]));
      end;
      if Prop.OneOfPropertyParent <> nil then begin
        ParsePropType(Prop.OneOfPropertyParent, Proto, OneOfDelphiProp);
        SL.Add(Format('%s%s:= %s_%s_%s;', [sIndent, OneOfDelphiProp.PropertyName, ProtoMsg.Name,
          OneOfDelphiProp.PropertyName, DelphiProp.PropertyName]));
      end;
      if SL.Count = iBeginBlock + 2 then begin
        // we added only begin and one extra line, so remove begin block and
        // remove 2 intending spaces from the one inserted line
        SL.Delete(iBeginBlock);
        SL[iBeginBlock] := Copy(SL[iBeginBlock], 3, MaxInt);
      end
      else
        SL.Add('      end;');
    end;
    SL.Add('  else');
    SL.Add('    Result := False;');
    SL.Add('  end;');
    if bNeedtmpBuf then begin
      SL.Insert(iInsertVarBlock, '  tmpBuf: TProtoBufInput;');
      SL.Insert(iInsertVarBlock, 'var');

      Inc(iInserttmpBufCreation, 2); // we just added two lines for the declaration
      SL.Insert(iInserttmpBufCreation, '  try');
      SL.Insert(iInserttmpBufCreation, '  tmpBuf:= nil;');
      for i := iInserttmpBufCreation + 2 to SL.Count - 1 do
        SL[i] := '  ' + SL[i];
      SL.Add('  finally');
      SL.Add('    tmpBuf.Free');
      SL.Add('  end;');
    end;
    SL.Add('end;');
    SL.Add('');
  end;

  procedure WriteGetterProcs(ProtoMsg: TProtoBufMessage; SL: TStrings);
  var
    i: Integer;
    Prop: TProtoBufProperty;
    DelphiProp: TDelphiProperty;
  begin
    for i := 0 to ProtoMsg.Count - 1 do begin
      Prop := ProtoMsg[i];
      ParsePropType(Prop, Proto, DelphiProp);

      if not DelphiProp.IsSimpleType then begin
        Continue;
      end;

      SL.Add(Format('function T%s.Get%s: Variant;', [ProtoMsg.Name, DelphiProp.PropertyName]));
      SL.Add('begin');
      SL.Add(Format('  if F%s%s then begin', [DelphiProp.PropertyName, HasValuePostfixName]));
      SL.Add(Format('    Result := F%s;', [DelphiProp.PropertyName]));
      SL.Add('  end');
      SL.Add('  else begin');
      SL.Add('    Result := Null;');
      SL.Add('  end;');
      SL.Add('end;');
      SL.Add('');
    end;
  end;

  procedure WriteSetterProcs(ProtoMsg: TProtoBufMessage; SL: TStrings);
  var
    i: Integer;
    Prop: TProtoBufProperty;
    DelphiProp: TDelphiProperty;
  begin
    for i := 0 to ProtoMsg.Count - 1 do begin
      Prop := ProtoMsg[i];
      ParsePropType(Prop, Proto, DelphiProp);

      if DelphiProp.readOnlyDelphiProperty or not DelphiProp.IsSimpleType then begin
        Continue;
      end;

      SL.Add(Format('procedure T%s.Set%s(const Value: Variant);', [ProtoMsg.Name, DelphiProp.PropertyName]));
      SL.Add('begin');
      SL.Add('  if VarIsNull(Value) then begin');
      SL.Add(Format('    F%s%s := False;', [DelphiProp.PropertyName, HasValuePostfixName]));
      SL.Add('  end');
      SL.Add('  else begin');
      SL.Add('    // Possible exception');
      SL.Add(Format('    F%s := Value;', [DelphiProp.PropertyName]));
      SL.Add(Format('    F%s%s := True;', [DelphiProp.PropertyName, HasValuePostfixName]));
      SL.Add('  end;');
      SL.Add('end;');
      SL.Add('');
    end;
  end;

  procedure WriteMessageToSL(ProtoMsg: TProtoBufMessage; SL: TStrings);
  begin
    if ProtoMsg.IsImported then begin
      Exit;
    end;

    SL.Add(Format('{T%s}', [ProtoMsg.Name]));
    SL.Add('');

    WriteInitialize(ProtoMsg, SL);
    WriteGetterProcs(ProtoMsg, SL);
    WriteSetterProcs(ProtoMsg, SL);
  end;

var
  i: Integer;
begin
  SL.Add('implementation');
  SL.Add('');

  for i := 0 to Proto.ProtoBufMessages.Count - 1 do begin
    WriteMessageToSL(Proto.ProtoBufMessages[i], SL);
  end;
  SL.Add('end.');
end;

procedure TProtoBufGenerator.GenerateInterfaceSection(Proto: TProtoFile; SL: TStrings);
var
  bNeedsGenericsCollection: Boolean;

  procedure WriteBeforeComments(AComments, SL: TStrings; const Indent: string = '  ');
  var
    i: Integer;
  begin
    for i := 0 to AComments.Count - 1 do
      SL.Add(Format('%s//%s', [Indent, AComments[i]]));
  end;

  procedure WriteEnumToSL(ProtoEnum: TProtoBufEnum; SL: TStrings);
  var
    i: Integer;
    s: string;
  begin
    if ProtoEnum.IsImported then
      Exit;

    WriteBeforeComments(ProtoEnum.Comments, SL);
    SL.Add(Format('  T%s = ( //', [ProtoEnum.Name]));
    for i := 0 to ProtoEnum.Count - 1 do begin
      if ProtoEnum[i].Comments.Count > 1 then
        WriteBeforeComments(ProtoEnum[i].Comments, SL, '    ');
      s := Format('    %s = %s%s //', [ProtoEnum[i].Name, ProtoEnum.GetEnumValueString(i),
        IfThen(i < (ProtoEnum.Count - 1), ',', '')]);
      if ProtoEnum[i].Comments.Count = 1 then
        s := Format('%s  //%s', [s, ProtoEnum[i].Comments[0]]);
      SL.Add(s);
    end;
    SL.Add('    );');
  end;

  procedure WriteMessageToSL(ProtoMsg: TProtoBufMessage; SL: TStrings);
  var
    i, j: Integer;
    Prop, OneOfProp: TProtoBufProperty;
    DelphiProp, OneOfDelphiProp: TDelphiProperty;
    s, sdefValue: string;
  begin
    if ProtoMsg.IsImported then
      Exit;

    WriteBeforeComments(ProtoMsg.Comments, SL);
    if ProtoMsg.ExtendOf = '' then
      s := 'AbstractProtoBufClass'
    else
      s := ProtoMsg.ExtendOf;
    SL.Add(Format('  T%s = class(T%s)', [ProtoMsg.Name, s]));
    // write tag constants and OneOfEnums, need to be first since
    // OneOfEnum is used for strict private field
    SL.Add('  public');
    for i := 0 to ProtoMsg.Count - 1 do begin
      Prop := ProtoMsg[i];
      ParsePropType(Prop, Proto, DelphiProp);
      if Prop.PropKind = ptOneOf then
        Continue;
      s := Format('    const %s = %d;', [DelphiProp.tagName, Prop.PropFieldNum]);
      SL.Add(s);
    end;
    // write oneOfEnums, using the tag constants we just wrote
    for i := 0 to ProtoMsg.Count - 1 do begin
      OneOfProp := ProtoMsg[i];
      if OneOfProp.PropKind <> ptOneOf then
        Continue;
      ParsePropType(OneOfProp, Proto, OneOfDelphiProp);
      SL.Add(Format('    type %s = (', [OneOfDelphiProp.PropertyType]));
      SL.Add(Format('      %s_%s_Nothing = 0,', [ProtoMsg.Name, OneOfDelphiProp.PropertyName]));
      for j := i + 1 to ProtoMsg.Count - 1 do begin
        Prop := ProtoMsg[j];
        if Prop.OneOfPropertyParent <> OneOfProp then
          Break;
        ParsePropType(Prop, Proto, DelphiProp);
        SL.Add(Format('      %s_%s_%s = %s,', [ProtoMsg.Name, OneOfDelphiProp.PropertyName, DelphiProp.PropertyName,
          DelphiProp.tagName]));
      end;
      // remove comma of last enum value
      s := SL[SL.Count - 1];
      SL[SL.Count - 1] := Copy(s, 1, Length(s) - 1);
      SL.Add('    );');
    end;
    SL.Add('  strict private');
    // field definitions
    for i := 0 to ProtoMsg.Count - 1 do begin
      Prop := ProtoMsg[i];
      ParsePropType(Prop, Proto, DelphiProp);
      SL.Add(Format('    F%s: %s;', [DelphiProp.PropertyName, DelphiProp.PropertyType]));
    end;
    SL.Add('');
    // property setters
    for i := 0 to ProtoMsg.Count - 1 do begin
      Prop := ProtoMsg[i];
      ParsePropType(Prop, Proto, DelphiProp);
      if DelphiProp.readOnlyDelphiProperty then
        Continue;
      if Prop.PropKind = ptOneOf then
        s := Format('    procedure Set%s(const Value: %s);', [DelphiProp.PropertyName, DelphiProp.PropertyType])
      else
        s := Format('    procedure Set%s(Tag: Integer; const Value: %s);',
          [DelphiProp.PropertyName, DelphiProp.PropertyType]);
      SL.Add(s);
    end;

    SL.Add('  strict protected');
    SL.Add('    function LoadSingleFieldFromBuf(ProtoBuf: TProtoBufInput; FieldNumber: Integer; WireType: Integer): Boolean; override;');
    SL.Add('    procedure SaveFieldsToBuf(ProtoBuf: TProtoBufOutput); override;');

    SL.Add('  public');
    if MsgNeedConstructor(ProtoMsg, Proto) then begin
      SL.Add('    constructor Create; override;');
      SL.Add('    destructor Destroy; override;');
      SL.Add('');
    end;
    for i := 0 to ProtoMsg.Count - 1 do begin
      Prop := ProtoMsg[i];
      ParsePropType(Prop, Proto, DelphiProp);
      // we need Generics.Collection if TList<> is used, but not for
      // TProtoBufClassList, which is defined in uAbstractProtoBufClasses
      if DelphiProp.IsList and (not DelphiProp.isObject) then
        bNeedsGenericsCollection := True;
      for j := 0 to Prop.Comments.Count - 1 do
        SL.Add('    //' + Prop.Comments[j]);
      if DelphiProp.readOnlyDelphiProperty then begin
        s := Format('    property %s: %s read F%s', [DelphiProp.PropertyName, DelphiProp.PropertyType,
          DelphiProp.PropertyName]);
      end
      else begin
        if Prop.PropKind = ptOneOf then begin
          s := Format('    property %s: %s read F%s write Set%s', [DelphiProp.PropertyName, DelphiProp.PropertyType,
            DelphiProp.PropertyName, DelphiProp.PropertyName]);
        end
        else begin
          s := Format('    property %s: %s index %s read F%s write Set%s',
            [DelphiProp.PropertyName, DelphiProp.PropertyType, DelphiProp.tagName, DelphiProp.PropertyName,
            DelphiProp.PropertyName]);
          if Prop.PropOptions.HasValue['default'] then begin
            sdefValue := Prop.PropOptions.Value['default'];
            if StartsStr('"', sdefValue) or ContainsStr(sdefValue, '.') or ContainsStr(sdefValue, 'e') then
              s := s + '; //';
            s := s + Format(' default %s', [ReQuoteStr(sdefValue)]);
          end;
        end;
      end;
      s := s + ';';
      SL.Add(s);
    end;
    SL.Add('  end;');
  end;

  procedure WriteMessageToSL_ClassesAsStructures(ProtoMsg: TProtoBufMessage; SL: TStrings);
  var
    i, j: Integer;
    Prop, OneOfProp: TProtoBufProperty;
    DelphiProp, OneOfDelphiProp: TDelphiProperty;
    s, sdefValue: string;

    FieldDefinitionsSectionIndex: Integer;
    PropertiesGettersSectionIndex: Integer;
    PropertiesSettersSectionIndex: Integer;
    PropertiesDefinitionsSectionIndex: Integer;
    FieldDefinitionsSectionName: string;
    PropertiesGettersSectionName: string;
    PropertiesSettersSectionName: string;
    PropertiesDefinitionsSectionName: string;
    PropertyType: string;
  const
    FieldDefinitionsSection = '  private // Fields definitions for %s';
    // Properties Getters and Setters in public sections for RTTI purposes
    // Default RTTI information generated for RTL/VCL/FMX classes is following
    // Fields - private, protected, public, published
    // Methods - public, published
    // Properties - public, published
    PropertiesGettersSection = '  public // Properties getters for %s';
    PropertiesSettersSection = '  public // Properties setters for %s';
    PropertiesDefinitionsSection = '  public // Properties definitions for %s';
  begin
    if ProtoMsg.IsImported then
      Exit;

    // Record comments
    WriteBeforeComments(ProtoMsg.Comments, SL);

    // Record header
    SL.Add(Format('  T%s = record', [ProtoMsg.Name]));

    // write oneOfEnums, using the tag constants we just wrote
    for i := 0 to ProtoMsg.Count - 1 do begin
      OneOfProp := ProtoMsg[i];
      if OneOfProp.PropKind <> ptOneOf then
        Continue;
      ParsePropType(OneOfProp, Proto, OneOfDelphiProp);
      SL.Add(Format('    type %s = (', [OneOfDelphiProp.PropertyType]));
      SL.Add(Format('      %s_%s_Nothing = 0,', [ProtoMsg.Name, OneOfDelphiProp.PropertyName]));
      for j := i + 1 to ProtoMsg.Count - 1 do begin
        Prop := ProtoMsg[j];
        if Prop.OneOfPropertyParent <> OneOfProp then
          Break;
        ParsePropType(Prop, Proto, DelphiProp);
        SL.Add(Format('      %s_%s_%s = %s,', [ProtoMsg.Name, OneOfDelphiProp.PropertyName, DelphiProp.PropertyName,
          DelphiProp.tagName]));
      end;
      // remove comma of last enum value
      s := SL[SL.Count - 1];
      SL[SL.Count - 1] := Copy(s, 1, Length(s) - 1);
      SL.Add('    );');
    end;

    FieldDefinitionsSectionName := Format(FieldDefinitionsSection, [ProtoMsg.Name]);
    PropertiesGettersSectionName := Format(PropertiesGettersSection, [ProtoMsg.Name]);
    PropertiesSettersSectionName := Format(PropertiesSettersSection, [ProtoMsg.Name]);
    PropertiesDefinitionsSectionName := Format(PropertiesDefinitionsSection, [ProtoMsg.Name]);

    // Field definitions
    SL.Add(FieldDefinitionsSectionName);
    // Properties getters
    SL.Add(PropertiesGettersSectionName);
    // Properties setters
    SL.Add(PropertiesSettersSectionName);
    // Properties definitions
    SL.Add(PropertiesDefinitionsSectionName);
    for i := ProtoMsg.Count - 1 downto 0 do begin // to insert last items last. For the aestetics!
      Prop := ProtoMsg[i];
      ParsePropType(Prop, Proto, DelphiProp);
      if DelphiProp.IsList then begin
        bNeedsGenericsCollection := True;
      end;

      // Field definitions
      FieldDefinitionsSectionIndex := SL.IndexOf(FieldDefinitionsSectionName) + 1;

      if DelphiProp.IsSimpleType then begin
        SL.Insert(FieldDefinitionsSectionIndex, //
          Format('    F%s%s: Boolean;', [DelphiProp.PropertyName, HasValuePostfixName]));
      end;

      SL.Insert(FieldDefinitionsSectionIndex, //
        Format('    [Serialize(%d)]'#13#10'    F%s: %s;', //
        [Prop.PropFieldNum, DelphiProp.PropertyName, DelphiProp.PropertyType]));

      PropertyType := IfThen(DelphiProp.IsSimpleType, 'Variant', DelphiProp.PropertyType);

      if DelphiProp.IsSimpleType then begin
        // Properties getters
        PropertiesGettersSectionIndex := SL.IndexOf(PropertiesGettersSectionName) + 1;

        SL.Insert(PropertiesGettersSectionIndex, //
          Format('    [IsGetterOrSetterFor(''F%s'')]'#13#10 + //
          '    function Get%0:s: %s;', [DelphiProp.PropertyName, PropertyType]));

        // Properties setters
        if not DelphiProp.readOnlyDelphiProperty then begin
          PropertiesSettersSectionIndex := SL.IndexOf(PropertiesSettersSectionName) + 1;

          SL.Insert(PropertiesSettersSectionIndex, //
            Format('    [IsGetterOrSetterFor(''F%s'')]'#13#10 + //
            '    procedure Set%0:s(const Value: %s);', [DelphiProp.PropertyName, PropertyType]));
        end;
      end;

      // Properties definitions
      PropertiesDefinitionsSectionIndex := SL.IndexOf(PropertiesDefinitionsSectionName) + 1;

      s := Format('    property %s: %s read %s', //
        [DelphiProp.PropertyName, PropertyType, //
        IfThen(DelphiProp.IsSimpleType, 'Get' + DelphiProp.PropertyName, 'F' + DelphiProp.PropertyName)]);

      if not DelphiProp.readOnlyDelphiProperty then begin
        s := s + Format(' write %s', //
          [IfThen(DelphiProp.IsSimpleType, 'Set' + DelphiProp.PropertyName, 'F' + DelphiProp.PropertyName)]);

        if (Prop.PropKind <> ptOneOf) and Prop.PropOptions.HasValue['default'] then begin
          sdefValue := Prop.PropOptions.Value['default'];
          if StartsStr('"', sdefValue) or ContainsStr(sdefValue, '.') or ContainsStr(sdefValue, 'e') then
            s := s + '; //';
          s := s + Format(' default %s', [ReQuoteStr(sdefValue)]);
        end;
      end;

      s := s + '; //';
      SL.Insert(PropertiesDefinitionsSectionIndex, s);
    end;

    SL.Add(Format('    procedure %s;', [InitializeProcName]));
    SL.Add(Format('    procedure %s;', [OnZeroLengthProcName]));
    SL.Add(Format('    class function Empty: T%s; static;', [ProtoMsg.Name]));
    SL.Add('  end;');
  end;

var
  i, iGenericsCollectionUses: Integer;
begin
  SL.Add(Format('unit %s;', [Proto.Name]));
  SL.Add('');
  SL.Add('interface');
  SL.Add('');
  SL.Add('// ***********************************');
  SL.Add(Format('// classes for %s.proto', [Proto.Name]));
  SL.Add('// generated by ProtoBufGenerator');
  SL.Add('// kami-soft 2016-2017');
  SL.Add('// ***********************************');
  SL.Add('// I am DanRom, who kinda borrowed this implementation, should I write here something? I dunno');
  SL.Add('// 2019');
  SL.Add('// ***********************************');
  SL.Add('');
  SL.Add('uses');
  iGenericsCollectionUses := SL.Count;
  bNeedsGenericsCollection := False;

  for i := 0 to Proto.Imports.Count - 1 do begin
    SL.Add('  ' + Proto.Imports[i] + ',');
  end;

  SL.Add('  Grijjy.ProtocolBuffers,');
  SL.Add('  System.Variants,');
  SL.Add('  System.SysUtils,');
  SL.Add('  System.Classes;');

  SL.Add('');
  SL.Add('type');
  // add all enums
  for i := 0 to Proto.ProtoBufEnums.Count - 1 do
    if not Proto.ProtoBufEnums[i].IsImported then begin
      WriteEnumToSL(Proto.ProtoBufEnums[i], SL);
      SL.Add('');
    end;

  // add all classes definitions
  for i := 0 to Proto.ProtoBufMessages.Count - 1 do
    if not Proto.ProtoBufMessages[i].IsImported then begin
      WriteMessageToSL_ClassesAsStructures(Proto.ProtoBufMessages[i], SL);
      SL.Add('');
    end;

  if bNeedsGenericsCollection then
    SL.Insert(iGenericsCollectionUses, '  Generics.Collections,');
end;

procedure TProtoBufGenerator.Generate(const InputFile, OutputDir: string; Encoding: TEncoding);
var
  Proto: TProtoFile;
  SL: TStringList;
  iPos: Integer;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(InputFile);
    Proto := TProtoFile.Create(nil);
    try
      Proto.FileName := InputFile;
      iPos := 1;
      Proto.ParseFromProto(SL.Text, iPos);
      Generate(Proto, OutputDir, Encoding);
    finally
      Proto.Free;
    end;
  finally
    SL.Free;
  end;
end;

{TDelphiProperty}

function TDelphiProperty.readOnlyDelphiProperty: Boolean;
begin
  Result := IsList;
end;

function TDelphiProperty.IsSimpleType: Boolean;
begin
  Result := not(isComplex and (isObject or IsList));
end;

function TDelphiProperty.tagName: string;
begin
  Result := 'tag_' + PropertyName;
end;

end.
