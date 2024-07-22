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

unit Cod.Animation.Register;

interface
  uses
    Classes,
    Cod.Animation.Component;

  procedure Register;

implementation

const
  CATEGORY_ANIM = 'Cod Animations';

procedure Register;
begin
  // Animations
  RegisterComponents( CATEGORY_ANIM, [TIntAnim, TFloatAnim] );
end;


end.
