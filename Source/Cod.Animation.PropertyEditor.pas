(**************************************************************)
(*                 Codrut's Animation Library                 *)
(*                                                            *)
(*                                                            *)
(*                    Copyright (c) 2024                      *)
(*             Petculescu Codrut. Codrut Software             *)
(*                                                            *)
(*                https://www.codrutsoft.com/                 *)
(*       https://github.com/Codrax/Codrut-Animation-Lib/      *)
(*                                                            *)
(**************************************************************)

unit Cod.Animation.PropertyEditor;

interface
uses
  Classes,
  Types,
  Math,
  SysUtils,
  IOUtils,
  Vcl.Dialogs,
  Vcl.Controls,
  System.Generics.Collections,
  Cod.Animation.Component,
  Cod.Animation.Utils,

  // Property Editor
  Cod.Animation.PropEditDefine,
  DesignEditors,
  DesignIntf,
  RTTI,
  TypInfo;

type
  // Integer Property
  TIntegerAnimProperty = class(TPropertyEditorAnimExtended)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;

    procedure GetValues(Proc: TGetStrProc); override;
  end;

  // TAnimationPropertyName property
  TAnimationPropertyNameProperty = class(TPropertyEditorAnimExtended)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;

    procedure GetValues(Proc: TGetStrProc); override;

    function GetIsDefault: Boolean; override;

    // Custom
    procedure ScanObject(AObject: TObject; Proc: TGetStrProc; ATypes: TArray<TTypeKind>; Prefix: string);
  end;

implementation

const
  CREATE_TEXT = 'Create Integer Animation';

{ TFXIntegerAnimProperty }

function TIntegerAnimProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueEditable, paValueList, paSortList];
end;

function TIntegerAnimProperty.GetValue: string;
begin
  Result := GetordValue.ToString;
end;

procedure TIntegerAnimProperty.GetValues(Proc: TGetStrProc);
begin
  inherited;
  Proc(CREATE_TEXT);
end;

procedure TIntegerAnimProperty.SetValue(const Value: string);
const
  BASE = 'NewAnimation';
var
  Anim: TIntAnim;
  AName: string;
  I: integer;
begin
  if Value = CREATE_TEXT then
    begin
      Anim := TIntAnim.Create(Root); // main parent of the component

      I := 1;
      repeat
        AName := BASE + I.ToString;
        Inc(I);
      until Root.FindComponent(AName) = nil;

      Anim.Name := AName;

      // Data
      Anim.Component := Component;
      Anim.PropertyName := GetName;
      Anim.StartValue := GetOrdValue;
    end
  else
    SetOrdValue(Value.ToInteger)
end;

{ TAnimationPropertyNameProperty }

function TAnimationPropertyNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueEditable, paValueList, paSortList];
end;

function TAnimationPropertyNameProperty.GetIsDefault: Boolean;
begin
  Result := inherited;

  // Assigned
  if not Assigned(TAnimationController(Component).Component) then
    Exit;

  // Get exists
  Result := PropertyExists(TAnimationController(Component).Component, GetStrValue)
end;

function TAnimationPropertyNameProperty.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TAnimationPropertyNameProperty.GetValues(Proc: TGetStrProc);
var
  ATypes: TArray<TTypeKind>;
begin
  inherited;

  // Assigned
  if not Assigned(TAnimationController(Component).Component) then
    Exit;

  // Get integer type
  ATypes := [ tkInteger, tkInt64 ];

  // Enumerate
  ScanObject( TAnimationController(Component).Component, Proc, ATypes, '' );
end;

procedure TAnimationPropertyNameProperty.ScanObject(AObject: TObject; Proc: TGetStrProc;
  ATypes: TArray<TTypeKind>; Prefix: string);
begin
  // Enum properties
  EnumerateProperties(AObject, procedure(PropInfo: PPropInfo) begin
    // Add property of type
    if TArray.Contains<TTypeKind>(ATypes, PropInfo.PropType^.Kind) then
      Proc( Prefix + string(PropInfo.Name) )
    else
      // Scan sub-classess
      if PropInfo.PropType^^.Kind = tkClass then begin
        const Value = TObject(int64(GetPropValue(AObject, PropInfo)));

        // Start recurse object scan
        if Assigned(Value) then
          ScanObject(Value, Proc, ATypes, string(PropInfo.Name+'.'));
      end;
  end);
end;

procedure TAnimationPropertyNameProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

initialization
  { Initialize }
  RegisterPropertyEditor(TypeInfo(integer), nil, '', TIntegerAnimProperty);
  RegisterPropertyEditor(TypeInfo(TAnimationPropertyName), nil, '', TAnimationPropertyNameProperty);
end.
