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

unit Cod.Animation.Utils;

{$SCOPEDENUMS ON}

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Types, TypInfo;

type
  TAnimationPropertyName = type string;

  TPropEnumProc = reference to procedure(PropInfo: PPropInfo);

// Enum
procedure EnumerateProperties(AObject: TObject; EnumProc: TPropEnumProc);

// Exists
function PropertyExists(Instance: TObject; PropName: string): boolean; overload;
function GetPropertyType(Instance: TObject; PropName: string): TTypeKind; overload;

// Root
function GetRootInstance(Instance: TObject; var PropName: string): TObject;

// Get
function GetPropertyValue(Instance: TObject; PropName: string): Variant;

// Set
procedure SetPropertyValue(Instance: TObject; PropName: string; Value: Variant); overload;

implementation

procedure EnumerateProperties(AObject: TObject; EnumProc: TPropEnumProc);
var
  PropList: PPropList;
  PropCount, I: Integer;
begin
  if AObject = nil then
    Exit;

  // Get published properties count
  PropCount := GetTypeData(AObject.ClassInfo)^.PropCount;

  if PropCount > 0 then
  begin
    // Allocate memory for the property list
    GetMem(PropList, PropCount * SizeOf(Pointer));

    try
      // Retrieve the list of published properties
      GetPropList(AObject.ClassInfo, tkAny, PropList);

      // Enumerate through the properties
      for I := 0 to PropCount - 1 do
        EnumProc( PropList^[I] );
    finally
      FreeMem(PropList);
    end;
  end;
end;

function GetRootInstance(Instance: TObject; var PropName: string): TObject;
var
  Tree: TArray<string>;
begin
  Tree := PropName.Split(['.']);

  // Check root
  if Length(Tree) <= 1 then
    Exit(Instance);

  // Parse
  PropName := PropName.Remove(0, Length(Tree[0])+1);

  // Get upper level object
  var V: Variant;
  var EObject: TObject;

  V := GetPropValue(Instance, Tree[0]);
  EObject := TObject(int64(V));

  Result := GetRootInstance(EObject, PropName);
end;

function PropertyExists(Instance: TObject; PropName: string): boolean; overload;
var
  AProp: PPropInfo;
begin
  // Root
  Instance := GetRootInstance(Instance, PropName);

  // Get prop
  AProp := GetPropInfo(PTypeInfo(Instance.ClassInfo), PropName);

  // Compare
  Result := AProp <> nil;
end;

function GetPropertyType(Instance: TObject; PropName: string): TTypeKind; overload;
var
  AProp: PPropInfo;
  Info: PTypeInfo;
begin
  // Root
  Instance := GetRootInstance(Instance, PropName);

  // Work
  AProp := GetPropInfo(PTypeInfo(Instance.ClassInfo), PropName);

  Info := AProp^.PropType^;
  Result := Info.Kind;
end;

function GetPropertyValue(Instance: TObject; PropName: string): Variant;
begin
  // Root
  Instance := GetRootInstance(Instance, PropName);

  // Work
  Result := GetPropValue(Instance, PropName, false);
end;

procedure SetPropertyValue(Instance: TObject; PropName: string; Value: Variant); overload;
var
  AProp: PPropInfo;
begin
  // Root
  Instance := GetRootInstance(Instance, PropName);

  // Work
  AProp := GetPropInfo(PTypeInfo(Instance.ClassInfo), PropName);

  SetPropValue(Instance, AProp, Value);
end;

end.
