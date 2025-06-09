{-----------------------------------------------------------------------------
 Unit Name: XLSPUtils
 Author:    PyScripter
 Purpose: Support types and functions for LSP Client
-----------------------------------------------------------------------------}

unit XLSPUtils;

interface

uses
  Winapi.Windows,
  System.TypInfo,
  System.SysUtils,
  System.Rtti,
  System.JSON,
  System.JSON.Readers,
  System.JSON.Writers,
  System.JSON.Serializers,
  System.JSON.Converters,
  System.RegularExpressions;

type

{$SCOPEDENUMS ON}
  // Format procuces formatted (human readaable) output
  // IgnoreNil ignores object fields with nil value
  // IgnoreEmpty ignores other properties with empty values
  TSerializeOption = (Format, IgnoreNil, IgnoreEmpty);
  TSerializeOptions = set of TSerializeOption;

{$REGION 'Helper classes'}
  // (De)Serialization of Delphi objects and their members
  TJSONObjectHelper = class helper for TObject
  public
    function AsJSON(Options: TSerializeOptions = []): string;
    function AsJSONObject(Options: TSerializeOptions = []): TJSONObject;
    procedure FromJSON(const AJson: string); overload;
    procedure FromJSON(AJsonObject: TJSONObject); overload;
    procedure MemberFromJsonValue(const MemberName: string; Value: TJSONValue);
    function MemberAsJson(const MemberName: string; Options: TSerializeOptions = []): string;
  end;

{$ENDREGION 'Helper classes'}

{$REGION 'TSerializer'}
  // Wraps TJsonSerializer for ease of use.
  // Includes the handling of Variant fileds
  // Ensures proper handling of Raw Json fields
  // TODO: Add TSerializeOptions to Serialize
  TSerializer = class
    class function Serialize<T>(const AValue: T): string; overload;
    class function Deserialize<T>(const AJson: string): T; overload;
    class function Deserialize<T>(AJsonValue: TJSONValue): T; overload;
    class procedure Populate<T>(const AJson: string; var AValue: T); overload;
    class procedure Populate<T>(AJsonValue: TJSONValue; var AValue: T); overload;
  end;

{$ENDREGION 'TSerializer'}

{$REGION 'TSmartPtr'}

(*
  Minimalist SmartPointer implementation based on a blog post by Barry Kelly:
  http://blog.barrkel.com/2008/11/reference-counted-pointers-revisited.html,
  https://stackoverflow.com/questions/30153682/why-does-this-optimization-of-a-smartpointer-not-work
  Poor relative to Spring4D's Shared
*)
  TSmartPtr = record
  private type
    TObjectHandle<T: class> = class(TInterfacedObject, TFunc<T>)
    private
      FValue: T;
    protected
      function Invoke:  T;
    public
      constructor Create(AValue:  T);
      destructor Destroy;  override;
    end;
  public
    class function Make<T: class>(AValue: T): TFunc<T>; static;
  end;

  TSharedJsonObject = TFunc<TJSONObject>;

{$ENDREGION 'TSmartPtr'}

{$REGION 'Json Converters'}
  // Handles Variant fields
  TJsonVariantConverter = class(TJsonConverter)
  public
    function CanConvert(ATypeInfo: PTypeInfo): Boolean; override;
    function ReadJson(const AReader: TJsonReader; ATypeInfo: PTypeInfo;
      const AExistingValue: TValue; const ASerializer: TJsonSerializer): TValue; override;
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue;
      const ASerializer: TJsonSerializer); override;
  end;

  // Handles raw Json fields
  // Limitation: ReadJson requires a TJsonObjectReader
  TJsonRawConverter = class(TJsonConverter)
  public
    function CanConvert(ATypeInfo: PTypeInfo): Boolean; override;
    function ReadJson(const AReader: TJsonReader; ATypeInfo: PTypeInfo;
      const AExistingValue: TValue; const ASerializer: TJsonSerializer): TValue; override;
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue;
      const ASerializer: TJsonSerializer); override;
  end;

  // Writes empty values as null (string and TArray types). Not used for reading
  TJsonEmptyToNullConverter = class(TJsonConverter)
  public
    function CanRead: Boolean; override;
    function CanConvert(ATypeInfo: PTypeInfo): Boolean; override;
    function ReadJson(const AReader: TJsonReader; ATypeInfo: PTypeInfo;
      const AExistingValue: TValue; const ASerializer: TJsonSerializer): TValue; override;
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue;
      const ASerializer: TJsonSerializer); override;
  end;

  // Dictionary converter
  TJsonTypedStringDictionaryConverter<V> = class(TJsonStringDictionaryConverter<V>)
    function ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
      const ASerializer: TJsonSerializer): TValue; override;
  end;

  // BugFix for Delphi 11 or earlier
  {$IF CompilerVersion < 36}
  TJsonIntegerConverter  = class(TJsonConverter)
  public
    function CanWrite: Boolean; override;
    function CanConvert(ATypeInfo: PTypeInfo): Boolean; override;
    function ReadJson(const AReader: TJsonReader; ATypeInfo: PTypeInfo;
      const AExistingValue: TValue; const ASerializer: TJsonSerializer): TValue; override;
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue;
      const ASerializer: TJsonSerializer); override;
  end;
  {$ENDIF}

{$ENDREGION 'Converters'}


function CompiledRegEx(Expr: string; Options: TRegExOptions = [roNotEmpty];
  UCP: Boolean = True): TRegEx;

// Helper routine to create asynchronous pipes.  From Jcl JclSysUtils
function CreateAsyncPipe(var hReadPipe, hWritePipe: THandle;
  lpPipeAttributes: PSecurityAttributes; nSize: DWORD): BOOL;

var
  RttiContext: TRttiContext;

implementation

uses
  System.Variants,
  System.JSON.Types,
  System.Generics.Collections,
  System.RegularExpressionsAPI,
  System.RegularExpressionsCore;

resourcestring
  rsUnsupportedVariantType = 'Unsupported JSON token type for Variant: %s';

{$REGION 'TSmartPtr'}

constructor TSmartPtr.TObjectHandle<T>.Create(AValue:  T);
begin
  FValue  :=  AValue;
end;

destructor TSmartPtr.TObjectHandle<T>.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TSmartPtr.TObjectHandle<T>.Invoke: T;
begin
  Result  :=  FValue;
end;

class function TSmartPtr.Make<T>(AValue: T): TFunc<T>;
begin
  Result := TObjectHandle<T>.Create(AValue);
end;

function CompiledRegEx(Expr: string; Options: TRegExOptions = [roNotEmpty];
  UCP: Boolean = True): TRegEx;
begin
  Result := TRegEx.Create(Expr, Options);
  if UCP then
    Result.AddRawOptions(PCRE_UCP);
  Result.Study([preJIT]);
end;

{$ENDREGION 'TSmartPtr'}

var
  AsyncPipeCounter: Integer = 0;

function CreateAsyncPipe(var hReadPipe, hWritePipe: THandle;
  lpPipeAttributes: PSecurityAttributes; nSize: DWORD): BOOL;
var
  Error: DWORD;
  PipeReadHandle, PipeWriteHandle: THandle;
  PipeName: string;
begin
  Result := False;

  if (@hReadPipe = nil) or (@hWritePipe = nil) then
  begin
    SetLastError(ERROR_INVALID_PARAMETER);
    Exit;
  end;

  if nSize = 0 then
    nSize := 4096;

  // Unique name
  AtomicIncrement(AsyncPipeCounter);
  PipeName := Format('\\.\Pipe\AsyncAnonPipe.%.8x.%.8x.%.8x',
    [GetCurrentProcessId, GetCurrentThreadId, AsyncPipeCounter]);

  PipeReadHandle := CreateNamedPipe(PChar(PipeName), PIPE_ACCESS_INBOUND or FILE_FLAG_OVERLAPPED,
      PIPE_TYPE_BYTE or PIPE_WAIT, 1, nSize, nSize, 120 * 1000, lpPipeAttributes);
  if PipeReadHandle = INVALID_HANDLE_VALUE then
    Exit;

  PipeWriteHandle := CreateFile(PChar(PipeName), GENERIC_WRITE, 0, lpPipeAttributes, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL {or FILE_FLAG_OVERLAPPED}, 0);
  if PipeWriteHandle = INVALID_HANDLE_VALUE then
  begin
    Error := GetLastError;
    CloseHandle(PipeReadHandle);
    SetLastError(Error);
    Exit;
  end;

  hReadPipe := PipeReadHandle;
  hWritePipe := PipeWriteHandle;

  Result := True;
end;

{$REGION 'Helper classes'}

{ TJSONObjectHelper }

function TJSONObjectHelper.AsJSON(Options: TSerializeOptions): string;
var
  JsonSerializer: TJsonSerializer;
  JsonVariantConverter: TJsonVariantConverter;
  JsonObj: TJSONObject;
begin
  if Options - [TSerializeOption.Format] = [] then
  begin
    JsonSerializer := TSmartPtr.Make(TJsonSerializer.Create)();
    JsonVariantConverter := TSmartPtr.Make(TJsonVariantConverter.Create)();
    JsonSerializer.Converters.Add(JsonVariantConverter);
    if TSerializeOption.Format in Options then
      JsonSerializer.Formatting := TJsonFormatting.Indented;
    Result := JsonSerializer.Serialize(Self);
  end
  else
  begin
    // We have go via TJSONObject if we want to Ignore properties
    // based on their value.
    JsonObj := TSmartPtr.Make(Self.AsJSONObject(Options))();
    if TSerializeOption.Format in Options then
      Result := JsonObj.Format
    else
      Result := JsonObj.ToJSON;
  end;
end;

function TJSONObjectHelper.AsJSONObject(Options: TSerializeOptions): TJSONObject;

  procedure Process(Instance: Pointer; const Fields: TArray<TRttiField>;
    JsonObj: TJSONObject);
  var
    ElementType: TRttiType;
    RttiField: TRttiField;
    Value, ElemValue: TValue;
    ChildJson: TJSONValue;
    Index: Integer;
  begin
    if (Instance = nil) or (JsonObj = nil) then Exit;

    for RttiField in Fields do
    begin
      case RttiField.FieldType.TypeKind of
        tkArray, tkDynArray:
          begin
            ChildJson := JsonObj.Values[RttiField.Name];
            if ChildJson is TJSONArray then
            begin
              if RttiField.FieldType is TRttiArrayType then
                ElementType := TRttiArrayType(RttiField.FieldType).ElementType
              else if RttiField.FieldType is TRttiDynamicArrayType then
                ElementType := TRttiDynamicArrayType(RttiField.FieldType).ElementType
              else
                Continue; // should not happen
              if ElementType.TypeKind in
                [tkClass, tkRecord, tkMRecord, tkArray, tkDynArray]
              then
              begin
                Value := RttiField.GetValue(Instance);
                if Value.GetArrayLength <> TJSONArray(ChildJson).Count then
                  Continue;  // unexpected mismatch
                for Index := 0 to Value.GetArrayLength - 1 do
                begin
                  ElemValue := Value.GetArrayElement(Index);
                  if ElementType is TRttiInstanceType then
                    Process(ElemValue.AsObject, ElementType.GetFields,
                      TJSONArray(ChildJson)[Index] as TJSONObject)
                  else if ElementType is TRttiRecordType then
                    Process(ElemValue.GetReferenceToRawData, ElementType.GetFields,
                      TJSONArray(ChildJson)[Index] as TJSONObject);
                end;
              end;
            end;
          end;
        tkRecord, tkMRecord:
          begin
            ChildJson := JsonObj.Values[RttiField.Name];
            if ChildJson is TJSONObject then
            begin
              Value := RttiField.GetValue(Instance);
              // recursive call
              Process(Value.GetReferenceToRawData,
                RttiField.FieldType.GetFields, TJSONObject(ChildJson));
            end;
          end;
        tkClass:
          begin
            Value := RttiField.GetValue(Instance);
            if (TSerializeOption.IgnoreNil in Options) and Value.IsEmpty then
            begin
              // OK if RemovePair returns nil
              JsonObj.RemovePair(RttiField.Name).Free;
              Continue;
            end
            else if not Value.IsEmpty then
            begin
              ChildJson := JsonObj.Values[RttiField.Name];
              if ChildJson is TJSONObject then
                // recursive call
                Process(Value.AsObject,
                  RttiField.FieldType.GetFields, TJSONObject(ChildJson));
            end;
          end;
        tkUString:
          if TSerializeOption.IgnoreEmpty in Options then
          begin
            Value := RttiField.GetValue(Instance);
            if Value.AsString = '' then
              JsonObj.RemovePair(RttiField.Name).Free;
          end;
      else if TSerializeOption.IgnoreEmpty in Options then
        begin
          Value := RttiField.GetValue(Instance);
          if Value.IsEmpty then
            JsonObj.RemovePair(RttiField.Name).Free;
        end;
      end;
    end;
  end;

var
  RttiType: TRttiType;
  JsonSerializer: TJsonSerializer;
  JsonObjectWriter: TJsonObjectWriter;
  JsonVariantConverter: TJsonVariantConverter;
begin
  JsonSerializer := TSmartPtr.Make(TJsonSerializer.Create)();
  JsonVariantConverter := TSmartPtr.Make(TJsonVariantConverter.Create)();
  JsonSerializer.Converters.Add(JsonVariantConverter);
  JsonObjectWriter := TSmartPtr.Make(TJsonObjectWriter.Create(False))();
  JsonSerializer.Serialize(JsonObjectWriter, Self);
  Result := JsonObjectWriter.JSON as TJSONObject;

  // Finally process Ignore options
  if Options - [TSerializeOption.Format] <> [] then
  begin
     RttiType := RttiContext.GetType(Self.ClassType);
     Process(Self, RttiType.GetFields, Result);
  end;
end;

procedure TJSONObjectHelper.FromJSON(const AJson: string);
var
 JsonObj: TJSONObject;
begin
  // We have go via TJSONObject because otherwise the
  // TJsonRawConverter will fail.
  JsonObj := TSmartPtr.Make(TJSONValue.ParseJSONValue(
    TEncoding.UTF8.GetBytes(AJson), 0) as TJSONObject)();
  FromJSON(JsonObj);
end;

procedure TJSONObjectHelper.FromJSON(AJsonObject: TJSONObject);
var
  Reader: TJsonObjectReader;
  JsonSerializer: TJsonSerializer;
  JsonVariantConverter: TJsonVariantConverter;
  {$IF CompilerVersion < 36}
  JsonIntegerConverter: TJsonIntegerConverter;
  {$ENDIF}
begin
  if not Assigned(AJsonObject) then Exit;

  Reader := TSmartPtr.Make(TJsonObjectReader.Create(AJsonObject))();
  JsonSerializer := TSmartPtr.Make(TJsonSerializer.Create)();
  JsonVariantConverter := TSmartPtr.Make(TJsonVariantConverter.Create)();
  JsonSerializer.Converters.Add(JsonVariantConverter);
  {$IF CompilerVersion < 36}
  JsonIntegerConverter := TSmartPtr.Make(TJsonIntegerConverter.Create)();
  JsonSerializer.Converters.Add(JsonIntegerConverter);
  {$ENDIF}
  JsonSerializer.Populate(Reader, Self);
end;

procedure TJSONObjectHelper.MemberFromJsonValue(const MemberName: string;
  Value: TJSONValue);
var
  JsonObj: TJSONObject;
begin
  if Value = nil then Exit;

  Value.Owned := False;
  JsonObj := TJSONObject.Create;
  try
    JsonObj.AddPair(MemberName, Value);
    FromJSON(JsonObj);
  finally
    JsonObj.Free;
    Value.Owned := True;
  end;
end;

function TJSONObjectHelper.MemberAsJson(const MemberName: string; Options:
    TSerializeOptions = []): string;
var
  JsonObj: TJSONObject;
  JsonValue: TJSONValue;
begin
  JsonObj := Self.AsJSONObject(Options);
  try
    JsonValue := JsonObj.Values[MemberName];
    if Assigned(JsonValue) then
      Result := JsonValue.ToJSON
    else
      Result := 'null';
  finally
    JsonObj.Free;
  end;
end;

type
  // Fix for https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-3591
  TJSONObjectWriterHelper = class helper for TJSONObjectWriter
  public
    function FixedGetContainer: TJSONAncestor;
  end;

{ TJSONObjectWriterHelper }

function TJSONObjectWriterHelper.FixedGetContainer: TJSONAncestor;
begin
  with Self do
    begin
      if FContainerStack.Count > 0 then
        Result := FContainerStack.Peek
      else
        Result := nil;
    end;
end;

{$ENDREGION 'Helper classes'}

{$REGION 'TSerializer'}

class function TSerializer.Deserialize<T>(const AJson: string): T;
var
  JsonValue: TJSONValue;
begin
  if AJson = '' then Exit(Default(T));

  JsonValue := TSmartPtr.Make(TJSONValue.ParseJSONValue(
    TEncoding.UTF8.GetBytes(AJson), 0))();
  Result := TSerializer.Deserialize<T>(JsonValue);
end;

class function TSerializer.Deserialize<T>(AJsonValue:
  TJSONValue): T;
var
  Serializer: TJsonSerializer;
  Reader: TJsonObjectReader;
  JsonVariantConverter: TJsonVariantConverter;
  {$IF CompilerVersion < 36}
  JsonIntegerConverter: TJsonIntegerConverter;
  {$ENDIF}
begin
  if not Assigned(AJsonValue) then Exit(Default(T));

  Reader := TSmartPtr.Make(TJsonObjectReader.Create(AJsonValue))();
  Serializer := TSmartPtr.Make(TJsonSerializer.Create)();
  JsonVariantConverter := TSmartPtr.Make(TJsonVariantConverter.Create)();
  Serializer.Converters.Add(JsonVariantConverter);
  {$IF CompilerVersion < 36}
  JsonIntegerConverter := TSmartPtr.Make(TJsonIntegerConverter.Create)();
  Serializer.Converters.Add(JsonIntegerConverter);
  {$ENDIF}
  Result := Serializer.Deserialize<T>(Reader);
end;

class procedure TSerializer.Populate<T>(AJsonValue: TJSONValue; var AValue: T);
var
  Serializer: TJsonSerializer;
  Reader: TJsonObjectReader;
  JsonVariantConverter: TJsonVariantConverter;
  {$IF CompilerVersion < 36}
  JsonIntegerConverter: TJsonIntegerConverter;
  {$ENDIF}
begin
  if not Assigned(AJsonValue) then Exit;

  Reader := TSmartPtr.Make(TJsonObjectReader.Create(AJsonValue))();
  Serializer := TSmartPtr.Make(TJsonSerializer.Create)();
  JsonVariantConverter := TSmartPtr.Make(TJsonVariantConverter.Create)();
  Serializer.Converters.Add(JsonVariantConverter);
  {$IF CompilerVersion < 36}
  JsonIntegerConverter := TSmartPtr.Make(TJsonIntegerConverter.Create)();
  Serializer.Converters.Add(JsonIntegerConverter);
  {$ENDIF}
  Serializer.Populate<T>(Reader, AValue);
end;

class procedure TSerializer.Populate<T>(const AJson: string; var AValue: T);
var
  JsonValue: TJSONValue;
begin
  if AJson = '' then Exit;

  JsonValue := TSmartPtr.Make(TJSONValue.ParseJSONValue(
    TEncoding.UTF8.GetBytes(AJson), 0))();
  TSerializer.Populate<T>(JsonValue, AValue);
end;

class function TSerializer.Serialize<T>(const AValue: T): string;
var
  Serializer: TJsonSerializer;
  JsonVariantConverter: TJsonVariantConverter;
begin
  JsonVariantConverter := TSmartPtr.Make(TJsonVariantConverter.Create)();
  Serializer := TSmartPtr.Make(TJsonSerializer.Create)();
  Serializer.Converters.Add(JsonVariantConverter);
  Result := Serializer.Serialize(AValue);
end;

{$ENDREGION 'TSerializer'}

{$REGION 'Json Converters'}

{ TJsonVariantConverter }

function TJsonVariantConverter.CanConvert(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ATypeInfo = TypeInfo(Variant);
end;

function TJsonVariantConverter.ReadJson(const AReader: TJsonReader; ATypeInfo: PTypeInfo;
  const AExistingValue: TValue; const ASerializer: TJsonSerializer): TValue;
var
  LValue: Variant;
begin
  case AReader.TokenType of
    TJsonToken.Null:
      LValue := Null;
    TJsonToken.Integer:
      LValue := AReader.Value.AsInteger;
    TJsonToken.Float:
      LValue := AReader.Value.AsExtended;
    TJsonToken.String:
      LValue := AReader.Value.AsString;
    TJsonToken.Boolean:
      LValue := AReader.Value.AsBoolean;
    TJsonToken.Date:
      LValue := AReader.Value.AsType<TDateTime>;
  else
    raise EJsonReaderException.CreateResFmt(@rsUnsupportedVariantType,
      [TRttiEnumerationType.GetName<TJsonToken>(AReader.TokenType)]);
  end;
  Result := TValue.FromVariant(LValue);
end;

procedure TJsonVariantConverter.WriteJson(const AWriter: TJsonWriter; const AValue: TValue;
  const ASerializer: TJsonSerializer);
var
  LVariant: Variant;
begin
  LVariant := AValue.AsVariant;
  case VarType(LVariant) and varTypeMask of
    varEmpty, varNull:
      AWriter.WriteNull;
    varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64:
      AWriter.WriteValue(Integer(LVariant));
    varSingle, varDouble, varCurrency:
      AWriter.WriteValue(Double(LVariant));
    varString, varUString:
      AWriter.WriteValue(string(LVariant));
    varBoolean:
      AWriter.WriteValue(Boolean(LVariant));
    varDate:
      AWriter.WriteValue(TDateTime(LVariant));
    else
      raise EJsonWriterException.CreateFmt('Unsupported Variant type: %d', [VarType(LVariant)]);
  end;
end;

{ TJsonRawConverter }

function TJsonRawConverter.CanConvert(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ATypeInfo = TypeInfo(string);
end;

function TJsonRawConverter.ReadJson(const AReader: TJsonReader;
  ATypeInfo: PTypeInfo; const AExistingValue: TValue;
  const ASerializer: TJsonSerializer): TValue;
begin
  // Only works with TJsonObjectReader
  if not (AReader is TJsonObjectReader) then
    raise EJsonWriterException.Create('TJsonRawConverter requires a TJsonObjectReader');
  Assert(TJsonObjectReader(AReader).Current <> nil);
  if TJsonObjectReader(AReader).Current is TJSONNull then
    Result := ''
  else
    Result := TJsonObjectReader(AReader).Current.ToJSON;
  AReader.Skip;
end;

procedure TJsonRawConverter.WriteJson(const AWriter: TJsonWriter;
  const AValue: TValue; const ASerializer: TJsonSerializer);
var
  JsonPair: TJSONPair;
begin
  if AValue.AsString = '' then
    AWriter.WriteNull
  else if AWriter is TJsonTextWriter then
    AWriter.WriteRawValue(AValue.AsString)
  else if AWriter is TJsonObjectWriter then
  begin
    JsonPair :=  TJsonObjectWriter(AWriter).FixedGetContainer as TJsonPair;
    AWriter.WriteNull;  // to pop the JSONpair
    JsonPair.JsonValue := TJSONValue.ParseJSONValue(AValue.AsString);
  end
  else
    Assert(False, 'Incompatible Writer');
end;

{ TJsonEmptyToNullConverter }

function TJsonEmptyToNullConverter.CanConvert(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ATypeInfo.Kind in [tkString, tkDynArray];
end;

function TJsonEmptyToNullConverter.CanRead: Boolean;
begin
  Result := False;
end;

function TJsonEmptyToNullConverter.ReadJson(const AReader: TJsonReader;
  ATypeInfo: PTypeInfo; const AExistingValue: TValue;
  const ASerializer: TJsonSerializer): TValue;
begin
  raise EJsonReaderException.Create('TJsonEmptyToNullConverter does not support reading');
end;

procedure TJsonEmptyToNullConverter.WriteJson(const AWriter: TJsonWriter;
  const AValue: TValue; const ASerializer: TJsonSerializer);
begin
  if AValue.IsEmpty then
    AWriter.WriteNull
  else
    ASerializer.Serialize(AWriter, AValue);
end;

{ TTypedJsonStringDictionaryConverter<V> }

function TJsonTypedStringDictionaryConverter<V>.ReadJson(
  const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
  const ASerializer: TJsonSerializer): TValue;
var
  LObjectContract: TJsonObjectContract;
  LDict: TDictionary<string, V>;
  LExistingValue: TValue;
begin
  if (AReader.TokenType <> TJsonToken.Null) and AExistingValue.IsEmpty then
  begin
    LObjectContract := ASerializer.ContractResolver.ResolveContract(ATypeInf) as TJsonObjectContract;
    LDict := LObjectContract.DefaultCreator.Invoke([]).AsType<TDictionary<string, V>>;
    LExistingValue := LDict;
  end
  else
  begin
    LDict := nil;
    LExistingValue := AExistingValue;
  end;
  try
    Result := inherited ReadJson(AReader, ATypeInf, LExistingValue, ASerializer);
  except
    LDict.Free;
    raise;
  end;
end;

{ TJsonIntegerConverter }

{$IF CompilerVersion < 36}
function TJsonIntegerConverter.CanConvert(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ATypeInfo.Kind in [tkInteger, tkEnumeration];
end;

function TJsonIntegerConverter.CanWrite: Boolean;
begin
  Result := False;
end;

function TJsonIntegerConverter.ReadJson(const AReader: TJsonReader;
  ATypeInfo: PTypeInfo; const AExistingValue: TValue;
  const ASerializer: TJsonSerializer): TValue;
begin
  if AReader.TokenType = TJsonToken.Float then
    TValue.Make(Trunc(AReader.Value.AsExtended), AExistingValue.TypeInfo, Result)
  else
    Result := AReader.Value;
end;

procedure TJsonIntegerConverter.WriteJson(const AWriter: TJsonWriter;
  const AValue: TValue; const ASerializer: TJsonSerializer);
begin
  raise EJsonWriterException.Create('TJsonIntegerConverter does not support writing');
end;
{$ENDIF}

{$ENDREGION 'Json Converters'}


initialization
  // Initialize RTTI context and keep the Rtti Pool alive
  RttiContext := TRttiContext.Create;
  RttiContext.FindType('');
end.
