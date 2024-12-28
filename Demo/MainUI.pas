unit MainUI;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Cod.Animation.Component,
  Vcl.NumberBox, Cod.Animation.Main, Vcl.ExtCtrls, Vcl.Samples.Gauges,
  Vcl.ComCtrls, Math;

type
  TForm1 = class(TForm)
    Label4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    IntAnim1: TIntAnim;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    NumberBox1: TNumberBox;
    Label6: TLabel;
    Label7: TLabel;
    CheckBox1: TCheckBox;
    Label8: TLabel;
    CheckBox2: TCheckBox;
    NumberBox2: TNumberBox;
    Label9: TLabel;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Label10: TLabel;
    ComboBox1: TComboBox;
    Shape1: TShape;
    Shape4: TShape;
    Panel1: TPanel;
    Label13: TLabel;
    Label14: TLabel;
    NumberBox3: TNumberBox;
    Label15: TLabel;
    Button5: TButton;
    Button6: TButton;
    Label5: TLabel;
    NewAnimation1: TIntAnim;
    Shape5: TShape;
    Shape6: TShape;
    Button7: TButton;
    Button8: TButton;
    Label16: TLabel;
    FloatAnim1: TFloatAnim;
    Gauge1: TGauge;
    Button9: TButton;
    Button10: TButton;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Gauge2: TGauge;
    Label17: TLabel;
    Shape2: TShape;
    Label11: TLabel;
    Label12: TLabel;
    Shape3: TShape;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure NumberBox1ChangeValue(Sender: TObject);
    procedure NumberBox2ChangeValue(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Button1EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure IntAnimStatChange(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Shape1EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure Shape4EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure NumberBox3ChangeValue(Sender: TObject);
    procedure Shape1StartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FloatAnim1Step(Sender: TObject; Step, TotalSteps: Integer);
    procedure Button10Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure NewAnimation1Step(Sender: TObject; Step, TotalSteps: Integer);
    procedure CheckBox6Click(Sender: TObject);
    procedure IntAnim1Step(Sender: TObject; Step, TotalSteps: Integer);
  private
    { Private declarations }
    procedure UpdateButtons;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button10Click(Sender: TObject);
begin
  IntAnim1.Paused := false;

  UpdateButtons;
end;

procedure TForm1.Button1EndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if TControl(Sender).Tag = 0 then
    Exit;
  (Sender as TWinControl).Left := X - (Sender as TWinControl).Width div 2;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  IntAnim1.Start;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  IntAnim1.Stop;

  UpdateButtons;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  IntAnim1.Restart;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  NewAnimation1.Start;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  NewAnimation1.Stop;

  UpdateButtons;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  FloatAnim1.Start;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  FloatAnim1.Stop;

  UpdateButtons;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  IntAnim1.Paused := true;

  UpdateButtons;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  IntAnim1.Inverse := TCheckBox(Sender).Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  IntAnim1.Loop := TCheckBox(Sender).Checked;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  IntAnim1.LoopInverse := TCheckBox(Sender).Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  IntAnim1.StartFromCurrent := TCheckBox(Sender).Checked;

  if not IntAnim1.StartFromCurrent then
    IntAnim1.StartValue := Shape1.Left;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  if NewAnimation1.Running then
    NewAnimation1.Stop;

  if TCheckbox(Sender).Checked then begin
    NewAnimation1.Component := Label15;
  end else begin
    NewAnimation1.Component := nil;
  end;
end;

procedure TForm1.CheckBox6Click(Sender: TObject);
begin
  NewAnimation1.Loop := TCheckBox(Sender).Checked;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  IntAnim1.Kind := TAnimationKind(TComboBox(Sender).ItemIndex);
end;

procedure TForm1.FloatAnim1Step(Sender: TObject; Step, TotalSteps: Integer);
begin
  // Update circle
  const PiVal = Step/TotalSteps * pi*2;
  const Radius = Shape5.Width div 2;

  const CenterP = Shape5.BoundsRect.CenterPoint;

  // Trigonometry
  const X = round(cos(PiVal) * Radius);
  const Y = round(sin(PiVal) * Radius);

  // Update position
  Shape6.Left := CenterP.X + X - Shape6.Width div 2;
  Shape6.Top := CenterP.Y + Y - Shape6.Height div 2;

  Gauge1.Progress := round(Step/TotalSteps * 100);
end;

procedure TForm1.FormDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Sender is TControl then begin
    (Source as TControl).Tag := 1;
    Accept := true;
  end;
end;

procedure TForm1.IntAnim1Step(Sender: TObject; Step, TotalSteps: Integer);
begin
  Gauge2.Progress := round((Step+1)/TotalSteps * 100);
end;

procedure TForm1.IntAnimStatChange(Sender: TObject);
begin
  UpdateButtons;
end;

function HSBtoColor(hue, sat, bri: Double): TColor;
var
  f, h: Double;
  u, p, q, t: Byte;
begin
  u := Trunc(bri * 255 + 0.5);
  if sat = 0 then
    Exit(rgb(u, u, u));

  h := (hue - Floor(hue)) * 6;
  f := h - Floor(h);
  p := Trunc(bri * (1 - sat) * 255 + 0.5);
  q := Trunc(bri * (1 - sat * f) * 255 + 0.5);
  t := Trunc(bri * (1 - sat * (1 - f)) * 255 + 0.5);

  case Trunc(h) of
    0:
      result := rgb(u, t, p);
    1:
      result := rgb(q, u, p);
    2:
      result := rgb(p, u, t);
    3:
      result := rgb(p, q, u);
    4:
      result := rgb(t, p, u);
    5:
      result := rgb(u, p, q);
  else
    result := clwhite;
  end;
end;

procedure TForm1.NewAnimation1Step(Sender: TObject; Step, TotalSteps: Integer);
begin
  if CheckBox5.Checked then
    Exit;

  // HUE mode
  Label15.Color := HSBtoColor(Step/TotalSteps, 1, 1);
end;

procedure TForm1.NumberBox1ChangeValue(Sender: TObject);
begin
  IntAnim1.Duration := TNumberBox(Sender).Value;
end;

procedure TForm1.NumberBox2ChangeValue(Sender: TObject);
begin
  IntAnim1.Delay := TNumberBox(Sender).Value;
end;

procedure TForm1.NumberBox3ChangeValue(Sender: TObject);
begin
  IntAnim1.Steps := TNumberBox(Sender).ValueInt;
end;

procedure TForm1.Shape1EndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if TControl(Sender).Tag = 0 then
    Exit;
  with (Sender as TControl) do begin
    Left := X - Width div 2;
    if Left < 0 then
      Left := 0;

    IntAnim1.StartValue := Left;
  end;
end;

procedure TForm1.Shape1StartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  TControl(Sender).Tag := 0;
end;

procedure TForm1.Shape4EndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if TControl(Sender).Tag = 0 then
    Exit;
  with (Sender as TControl) do begin
    Left := X - Width div 2;

    if Left >= Self.ClientWidth - Width then
      Left := Self.ClientWidth - Width;

    IntAnim1.EndValue := Left;
  end;
end;

procedure TForm1.UpdateButtons;
var
  Running: boolean;
begin
  // I
  Running := IntAnim1.Status <> TAnimationStatus.Stopped;

  Button2.Enabled := not Running;
  Button3.Enabled := Running;
  Button4.Enabled := Running;

  Button9.Enabled := Running;
  Button10.Enabled := IntAnim1.Paused;

  // I
  Running := NewAnimation1.Running;

  Button5.Enabled := not Running;
  Button6.Enabled := Running;

  // I
  Running := FloatAnim1.Running;

  Button7.Enabled := not Running;
  Button8.Enabled := Running;
end;

end.
