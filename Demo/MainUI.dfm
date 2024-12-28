object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Animation demo'
  ClientHeight = 486
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnDragOver = FormDragOver
  DesignSize = (
    784
    486)
  TextHeight = 15
  object Label4: TLabel
    Left = 8
    Top = 448
    Width = 292
    Height = 30
    Anchors = [akLeft, akBottom]
    Caption = 
      'Windows x64 - Codrut Software Animation Demo'#13#10'Copyright (c) 2024' +
      ' Codrut Software. All Rights Reserved'
    WordWrap = True
    ExplicitTop = 423
  end
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 357
    Height = 45
    Caption = 'Animation Library Demo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Segoe UI Semibold'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 24
    Top = 67
    Width = 256
    Height = 20
    Caption = 'Animation library by Petculescu Codrut'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 24
    Top = 103
    Width = 243
    Height = 15
    Caption = 'Movement animation (property name: Left)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI Bold'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 24
    Top = 203
    Width = 45
    Height = 15
    Caption = 'Controls'
  end
  object Label7: TLabel
    Left = 272
    Top = 203
    Width = 74
    Height = 15
    Caption = 'Duration (sec)'
  end
  object Label8: TLabel
    Left = 652
    Top = 203
    Width = 42
    Height = 15
    Caption = 'Settings'
  end
  object Label9: TLabel
    Left = 373
    Top = 203
    Width = 57
    Height = 15
    Caption = 'Delay (sec)'
  end
  object Label10: TLabel
    Left = 471
    Top = 203
    Width = 55
    Height = 15
    Caption = 'Anim kind'
  end
  object Shape1: TShape
    Left = 100
    Top = 124
    Width = 100
    Height = 30
    Cursor = crHandPoint
    Brush.Color = clLime
    DragMode = dmAutomatic
    Shape = stRoundRect
    OnEndDrag = Shape1EndDrag
    OnStartDrag = Shape1StartDrag
  end
  object Shape4: TShape
    Left = 565
    Top = 124
    Width = 100
    Height = 30
    Cursor = crHandPoint
    Brush.Color = clRed
    DragMode = dmAutomatic
    Shape = stRoundRect
    OnEndDrag = Shape4EndDrag
    OnStartDrag = Shape1StartDrag
  end
  object Label13: TLabel
    Left = 24
    Top = 296
    Width = 96
    Height = 15
    Caption = 'Other animations'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI Bold'
    Font.Style = []
    ParentFont = False
  end
  object Label14: TLabel
    Left = 562
    Top = 203
    Width = 82
    Height = 15
    Caption = 'Steps (override)'
  end
  object Label15: TLabel
    Left = 24
    Top = 320
    Width = 113
    Height = 36
    Alignment = taCenter
    AutoSize = False
    Caption = 'COLOR DEMO'
    Color = clMedGray
    ParentColor = False
    Transparent = False
    Layout = tlCenter
  end
  object Label5: TLabel
    Left = 24
    Top = 362
    Width = 45
    Height = 15
    Caption = 'Controls'
  end
  object Shape5: TShape
    Left = 298
    Top = 296
    Width = 105
    Height = 112
    Brush.Color = clYellow
    Shape = stCircle
  end
  object Shape6: TShape
    Left = 395
    Top = 344
    Width = 15
    Height = 15
    Brush.Color = clFuchsia
    Shape = stCircle
  end
  object Label16: TLabel
    Left = 436
    Top = 362
    Width = 45
    Height = 15
    Caption = 'Controls'
  end
  object Gauge1: TGauge
    Left = 436
    Top = 296
    Width = 50
    Height = 50
    BorderStyle = bsNone
    Color = clBtnFace
    Kind = gkPie
    ParentColor = False
    Progress = 0
  end
  object Gauge2: TGauge
    Left = 186
    Top = 255
    Width = 75
    Height = 24
    BorderStyle = bsNone
    Color = clBtnFace
    ParentColor = False
    Progress = 0
  end
  object Label17: TLabel
    Left = 525
    Top = 16
    Width = 251
    Height = 45
    Alignment = taRightJustify
    Caption = 
      'Hint: You can move the start and end position by dragging the co' +
      'lored rectangles.'
    WordWrap = True
  end
  object Shape2: TShape
    Left = 730
    Top = 71
    Width = 9
    Height = 9
    Brush.Color = clLime
    Shape = stRoundRect
  end
  object Label11: TLabel
    Left = 742
    Top = 67
    Width = 34
    Height = 15
    Caption = ' - start'
  end
  object Label12: TLabel
    Left = 742
    Top = 88
    Width = 31
    Height = 15
    Caption = ' - end'
  end
  object Shape3: TShape
    Left = 730
    Top = 91
    Width = 9
    Height = 9
    Brush.Color = clRed
    Shape = stRoundRect
  end
  object Panel1: TPanel
    Left = -8
    Top = 154
    Width = 785
    Height = 43
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = 'Panel1'
    Ctl3D = True
    ParentBackground = False
    ParentColor = True
    ParentCtl3D = False
    ShowCaption = False
    TabOrder = 11
    OnDragOver = FormDragOver
  end
  object Button1: TButton
    Left = 100
    Top = 159
    Width = 100
    Height = 36
    Caption = 'I will move'
    DragMode = dmAutomatic
    TabOrder = 0
    OnEndDrag = Button1EndDrag
    OnStartDrag = Shape1StartDrag
  end
  object Button2: TButton
    Left = 24
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 105
    Top = 223
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 186
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Restart'
    Enabled = False
    TabOrder = 3
    OnClick = Button4Click
  end
  object NumberBox1: TNumberBox
    Left = 272
    Top = 224
    Width = 85
    Height = 23
    Mode = nbmFloat
    MaxValue = 100.000000000000000000
    TabOrder = 4
    Value = 0.500000000000000000
    OnChangeValue = NumberBox1ChangeValue
  end
  object CheckBox1: TCheckBox
    Left = 652
    Top = 227
    Width = 125
    Height = 17
    Caption = 'Inverse'
    TabOrder = 5
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 652
    Top = 250
    Width = 125
    Height = 17
    Caption = 'Loop'
    TabOrder = 6
    OnClick = CheckBox2Click
  end
  object NumberBox2: TNumberBox
    Left = 373
    Top = 224
    Width = 85
    Height = 23
    Mode = nbmFloat
    MaxValue = 100.000000000000000000
    TabOrder = 7
    OnChangeValue = NumberBox2ChangeValue
  end
  object CheckBox3: TCheckBox
    Left = 652
    Top = 273
    Width = 125
    Height = 17
    Caption = 'Loop inverse'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = CheckBox3Click
  end
  object CheckBox4: TCheckBox
    Left = 652
    Top = 296
    Width = 125
    Height = 17
    Caption = 'Start from current'
    TabOrder = 9
    OnClick = CheckBox4Click
  end
  object ComboBox1: TComboBox
    Left = 468
    Top = 224
    Width = 75
    Height = 23
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 10
    Text = 'Linear'
    OnChange = ComboBox1Change
    Items.Strings = (
      'Exponential'
      'Linear'
      'Pulse'
      'Random'
      'ReverseExpo'
      'Sinus'
      'SinusArc'
      'Spring'
      'Wobbly')
  end
  object NumberBox3: TNumberBox
    Left = 562
    Top = 224
    Width = 63
    Height = 23
    MaxValue = 9999999.000000000000000000
    TabOrder = 12
    OnChangeValue = NumberBox3ChangeValue
  end
  object Button5: TButton
    Left = 24
    Top = 383
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 13
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 105
    Top = 383
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 14
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 436
    Top = 383
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 15
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 517
    Top = 383
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 16
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 24
    Top = 255
    Width = 75
    Height = 25
    Caption = 'Pause'
    Enabled = False
    TabOrder = 17
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 105
    Top = 254
    Width = 75
    Height = 25
    Caption = 'Unpause'
    Enabled = False
    TabOrder = 18
    OnClick = Button10Click
  end
  object CheckBox5: TCheckBox
    Left = 186
    Top = 387
    Width = 97
    Height = 17
    Caption = 'Control mode'
    Checked = True
    State = cbChecked
    TabOrder = 19
    OnClick = CheckBox5Click
  end
  object CheckBox6: TCheckBox
    Left = 186
    Top = 414
    Width = 97
    Height = 17
    Caption = 'Loop'
    Checked = True
    State = cbChecked
    TabOrder = 20
    OnClick = CheckBox6Click
  end
  object IntAnim1: TIntAnim
    Duration = 0.500000000000000000
    Kind = Linear
    LatencyAdjustments = True
    LoopInverse = True
    PropertyName = 'Left'
    Component = Button1
    OnStart = IntAnimStatChange
    OnFinish = IntAnimStatChange
    OnStep = IntAnim1Step
    StartValue = 100
    EndValue = 562
    CurrentValue = 0
    Left = 472
    Top = 32
  end
  object NewAnimation1: TIntAnim
    Duration = 1.000000000000000000
    Kind = Linear
    Loop = True
    LoopInverse = True
    PropertyName = 'Color'
    Component = Label15
    OnStart = IntAnimStatChange
    OnFinish = IntAnimStatChange
    OnStep = NewAnimation1Step
    StartValue = 0
    EndValue = 255
    CurrentValue = 0
    Left = 180
    Top = 304
  end
  object FloatAnim1: TFloatAnim
    Duration = 1.500000000000000000
    Kind = Linear
    Loop = True
    OnStart = IntAnimStatChange
    OnFinish = IntAnimStatChange
    OnStep = FloatAnim1Step
    EndValue = 360.000000000000000000
    Left = 656
    Top = 392
  end
end
