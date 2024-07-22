unit Cod.Animation.PropertyEditor;

interface
  uses
    Classes,
    Types,
    Math,
    SysUtils,
    IOUtils,
    Vcl.Dialogs,
    Cod.Animation.Component,

    // Property Editor
    Cod.Animation.PropEditDefine,
    DesignEditors,
    DesignIntf,
    RTTI,
    TypInfo;

    type
      // Percent Property
      TFXIntegerAnimProperty = class(TPropertyEditorExtended)
      private
      public
        function GetAttributes: TPropertyAttributes; override;
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;

        procedure GetValues(Proc: TGetStrProc); override;
      end;

implementation

const
  CREATE_TEXT = 'Create Integer Animation';

{ TFXIntegerAnimProperty }

function TFXIntegerAnimProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueEditable, paValueList, paSortList];
end;

function TFXIntegerAnimProperty.GetValue: string;
begin
  Result := GetordValue.ToString;
end;

procedure TFXIntegerAnimProperty.GetValues(Proc: TGetStrProc);
begin
  inherited;
  Proc(CREATE_TEXT);
end;

procedure TFXIntegerAnimProperty.SetValue(const Value: string);
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

initialization
  { Initialize }
  RegisterPropertyEditor(TypeInfo(integer), nil, '', TFXIntegerAnimProperty);
end.
