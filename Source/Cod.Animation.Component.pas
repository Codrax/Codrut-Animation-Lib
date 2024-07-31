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

unit Cod.Animation.Component;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, DateUtils,
    Vcl.Controls, Vcl.Dialogs, System.Math, TypInfo,
    Cod.Animation.Main, Cod.Animation.Utils;

  type
    // Notify Event
    TAniStepEvent = procedure(Sender: TObject; Step, TotalSteps: integer) of object;

    // Classes
    TAnimationController = class;

    TAnimationThread = class(TThread)
      private
        FAnimation: TAnimationController;
      public
        procedure Execute; override;
        constructor Create(AAnim: TAnimationController);
    end;

    TAnimationController = class(TComponent)
    private
      // Anim Const
      ValueKinds: set of TTypeKind;

      // Data
      FKind: TAnimationKind;
      FDelay: single;
      FDuration: Single;
      { Duration values are stored as single and are
        noted in seconds, they will be multiplied by 10^3
        to be used as miliseconds. }
      FInverse: boolean;

      // Runtime
      FSteps: integer;
      FStatus: TAnimationStatus;

      // Latency
      FLatencyAdjust: boolean;
      FLatencyCanSkipSteps: boolean;

      // Loop
      FLoop: boolean;
      FLoopInverse: boolean;
      FDelayLoop: boolean;

      // Property
      FStartFromCurrent: boolean;
      FPropertyName: string;

      // Notify event
      FOnStart,
      FOnFinish,
      FOnLoop: TNotifyEvent;
      FOnStep: TAniStepEvent;

      // Thread
      FThread: TAnimationThread;
      FComponent: TComponent;

      ComponentBased: boolean;

      // Animation Tick
      FStepValue: integer;
      FTotalStep: integer;
      FSleepStep: integer;

      // Getters
      function GetPaused: boolean;
      function GetRunning: boolean;

      // Setters
      procedure SetPaused(const Value: boolean);
      procedure SetSteps(const Value: integer);
      procedure SetDuration(const Value: single);
      procedure SetRunning(const Value: boolean);

      // Thread & System
      procedure CreateThread;
      function PropertyValid: boolean;

      procedure WaitDelay;
      procedure ExecuteAnimation; virtual;
      procedure DoStepValue; virtual;
      function CalculatePercent: single;

    published
      // Start
      procedure Start;
      procedure StartInverse;

      // Task
      procedure Stop;
      procedure Restart;

      // Properties
      property Delay: single read FDelay write FDelay;
      property Duration: single read FDuration write SetDuration;

      property Kind: TAnimationKind read FKind write FKind;
      property Inverse: boolean read FInverse write FInverse default false;

      property Steps: integer read FSteps write SetSteps default 0;

      // compensate for the time needed to execute the code
      property LatencyAdjustments: boolean read FLatencyAdjust write FLatencyAdjust default false;
      // determine wheather in order to compensate, skipping steps is permitted
      property LatencyCanSkipSteps: boolean read FLatencyCanSkipSteps write FLatencyCanSkipSteps default true;

      property StartFromCurrent: boolean read FStartFromCurrent write FStartFromCurrent default false;
      property Loop: boolean read FLoop write FLoop default false;
      property LoopInverse: boolean read FLoopInverse write FLoopInverse default false;
      property DelayLoop: boolean read FDelayLoop write FDelayLoop default false;

      property PropertyName: string read FPropertyName write FPropertyName;

      property Component: TComponent read FComponent write FComponent;

      // Status
      property Percent: single read CalculatePercent;

      // Notify
      property OnStart: TNotifyEvent read FOnStart write FOnStart;
      property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
      property OnLoop: TNotifyEvent read FOnLoop write FOnLoop;
      property OnStep: TAniStepEvent read FOnStep write FOnStep;

    public
      // Code Properties
      property Running: boolean read GetRunning write SetRunning;
      property Paused: boolean read GetPaused write SetPaused;
      property Status: TAnimationStatus read FStatus;

      // Constructors
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
    end;

    TIntAnim = class(TAnimationController)
    private
      // Values
      FStartValue: integer;
      FEndValue: integer;

      FDelta: integer;
      FCurrentValue: integer;

      procedure ExecuteAnimation; override;
      procedure DoStepValue; override;

    published
      // Properties
      property StartValue: integer read FStartValue write FStartValue;
      property EndValue: integer read FEndValue write FEndValue;

      // Status
      property CurrentValue: integer read FCurrentValue;

    public
      // Constructors
      constructor Create(AOwner: TComponent); override;
    end;

    TFloatAnim = class(TAnimationController)
    private
      // Values
      FStartValue: real;
      FEndValue: real;

      FDelta: real;
      FCurrentValue: real;

      procedure ExecuteAnimation; override;
      procedure DoStepValue; override;

    published
      // Properties
      property StartValue: real read FStartValue write FStartValue;
      property EndValue: real read FEndValue write FEndValue;

      // Status
      property CurrentValue: real read FCurrentValue;

    public
      // Constructors
      constructor Create(AOwner: TComponent); override;
    end;

implementation

{ TAnimationController }

function TAnimationController.CalculatePercent: single;
begin
  if not Running then
    Exit(1);

  Result := FStepValue / (FTotalStep-1);
end;

constructor TAnimationController.Create(AOwner: TComponent);
begin
  // Inherit
  inherited;

  // Animation Kind
  ValueKinds := [];

  // Defaults
  FLoop := false;
  FLoopInverse := false;
  FStartFromCurrent := false;
  FStatus := TAnimationStatus.Stopped;

  FKind := TAnimationKind.Linear;
  FSteps := 0;
  FDuration := 2;
  FDelay := 0;
  FInverse := false;

  FLatencyAdjust := false;
  FLatencyCanSkipSteps := true;
end;

procedure TAnimationController.CreateThread;
begin
  if FThread = nil then
    FThread := TAnimationThread.Create(Self)
  else
    begin
      if FThread.Finished then
        FThread.Free;

      FThread := TAnimationThread.Create(Self);
    end;
end;

destructor TAnimationController.Destroy;
begin
  // Free
  if (FThread <> nil) and FThread.Started and not FThread.Finished then
    begin
      FThread.Terminate;

      FThread.Free;
    end;

  // Inherit
  inherited;
end;

procedure TAnimationController.DoStepValue;
begin
  // nothing
end;

procedure TAnimationController.ExecuteAnimation;
label StartLoop;
var
  StartTime: TDateTime;
  SleepTime: cardinal;
begin
  // Sleep
  WaitDelay;

  // Notify
  if Assigned(FOnStart) then
    TThread.Synchronize(FThread, procedure
      begin
        FOnStart(Self);
      end);

  // Begin work
  StartLoop:
  StartTime := 0;
  FStepValue := 0;
  while FStepValue < FTotalStep do
    begin
      // Terminate
      if FThread.CheckTerminated then
        Exit;

      // Do step
      DoStepValue;

      // Compensate
      if FLatencyAdjust then
        StartTime := Now;

      // Notify
      if Assigned(FOnStep) then
        TThread.Synchronize(FThread, procedure
          begin
            FOnStep(Self, FStepValue, FTotalStep);
          end);

      // Sleep
      if FStepValue < FTotalStep-1 then begin
        SleepTime := FSleepStep;
        if FLatencyAdjust then begin
          const CodeLatency = MillisecondsBetween(Now, StartTime);
          SleepTime := Max(0, SleepTime-CodeLatency);

          if FLatencyCanSkipSteps and (CodeLatency >= FSleepStep*2) then begin
            FStepValue := FStepValue + (CodeLatency div FSleepStep - 1);

            // Ensure the final step will execute
            if FStepValue >= FTotalStep-1 then
              FStepValue := FTotalStep-2;
          end;
        end;
        Sleep(SleepTime);
      end;

      // Increase
      Inc(FStepValue);
    end;

  // Stopped
  if FThread.CheckTerminated then
    Exit;

  // Loop
  if Loop then
    begin
      if DelayLoop then
        WaitDelay;

      // Stopped
      if FThread.CheckTerminated then
        Exit;

      // Reset
      if FLoopInverse then
        Inverse := not Inverse;

      // Notify
      if Assigned(FOnLoop) then
        TThread.Synchronize(FThread, procedure
          begin
            FOnLoop(Self);
          end);

      goto StartLoop;
    end;

  // Done
  FStatus := TAnimationStatus.Stopped;

  // Notify
  if Assigned(FOnFinish) then
    TThread.Synchronize(FThread, procedure
      begin
        FOnFinish(Self);
      end);
end;

function TAnimationController.GetPaused: boolean;
begin
  Result := FStatus = TAnimationStatus.Paused;
end;

function TAnimationController.GetRunning: boolean;
begin
  Result := FStatus = TAnimationStatus.Running;
end;

function TAnimationController.PropertyValid: boolean;
var
  O: TObject;
  N: string;
begin
  // Valid
  if (PropertyName = '') or (FComponent = nil) then
    Exit(false);

  O := FComponent;
  N := PropertyName;
  try
    GetRootInstance(O, N);
  except
    Exit(false);
  end;

  Result := (GetPropertyType(FComponent, PropertyName) in ValueKinds);
end;

procedure TAnimationController.Restart;
begin
  if FStatus in [TAnimationStatus.Running, TAnimationStatus.Paused] then
    begin
      Stop;
      Start;
    end;
end;

procedure TAnimationController.SetDuration(const Value: single);
begin
  if Value*1000 > 1 then
    FDuration := Value;
end;

procedure TAnimationController.SetPaused(const Value: boolean);
begin
  if FStatus in [TAnimationStatus.Running, TAnimationStatus.Paused] then
    begin
      if GetPaused <> Value then
        // Suspend thread
        FThread.Suspended := Value;

      if Value then
        FStatus := TAnimationStatus.Paused
      else
        FStatus := TAnimationStatus.Running;
    end;
end;

procedure TAnimationController.SetRunning(const Value: boolean);
begin
  if Value <> Running then
    begin
      if Value then
        Start
      else
        Stop;
    end;
end;

procedure TAnimationController.SetSteps(const Value: integer);
begin
  if Value >= 0 then
    FSteps := Value;
end;

procedure TAnimationController.Start;
begin
  if FStatus = TAnimationStatus.Stopped then
    begin
      // Component Based
      ComponentBased := (Component <> nil);
      if not ComponentBased and StartFromCurrent then
        Exit;

      // Check valid
      if ComponentBased and not PropertyValid then
        Exit;

      // Build values
      FStepValue := 0;
      if Steps > 0 then
        begin
          FTotalStep := Max(Steps-1, 1); // Step 0 is considered
          FSleepStep := Max(trunc(Duration*1000 / FTotalStep), 1);
        end
      else
        begin
          FTotalStep := round(FDuration * 100);
          FSleepStep := 10;
        end;

      // Thread
      CreateThread;

      FStatus := TAnimationStatus.Running;
      FThread.Start;
    end;
end;

procedure TAnimationController.StartInverse;
begin
  if FStatus = TAnimationStatus.Stopped then
    begin
      FInverse := not FInverse;
      Start;
    end;
end;

procedure TAnimationController.Stop;
begin
  if FStatus in [TAnimationStatus.Running, TAnimationStatus.Paused] then
    begin
      FThread.Terminate;
      //FThread.WaitFor;

      FStatus := TAnimationStatus.Stopped;
    end;
end;

procedure TAnimationController.WaitDelay;
begin
  Sleep( trunc(FDelay * 1000) );
end;

{ TAnimationThread }

constructor TAnimationThread.Create(AAnim: TAnimationController);
begin
  inherited Create(true);
  FAnimation := AAnim;
end;

procedure TAnimationThread.Execute;
begin
  inherited;
  FAnimation.ExecuteAnimation;
end;

{ TIntAnimation }

constructor TIntAnim.Create(AOwner: TComponent);
begin
  inherited;
  ValueKinds := [tkInteger, tkInt64];
end;

procedure TIntAnim.DoStepValue;
var
  AStart, ADelta: integer;
begin
  inherited;
  // Inverse
  if Inverse then
    begin
      AStart := FEndValue;
      ADelta := -FDelta;
    end
  else
    begin
      AStart := FStartValue;
      ADelta := FDelta;
    end;

  // Calc
  FCurrentValue := AStart + trunc(CalculateAnimationValue(FKind, FStepValue, FTotalStep, ADelta));

  // Set
  if ComponentBased then
    TThread.Synchronize(nil, procedure
      begin
        SetPropertyValue(FComponent, PropertyName, FCurrentValue);
      end);
end;

procedure TIntAnim.ExecuteAnimation;
begin
  if StartFromCurrent then
    FStartValue := GetPropertyValue(FComponent, PropertyName);
  FDelta := FEndValue - FStartValue;

  // Start
  inherited;
end;

{ TFloatAnim }

constructor TFloatAnim.Create(AOwner: TComponent);
begin
  inherited;
  ValueKinds := [tkFloat];
end;

procedure TFloatAnim.DoStepValue;
var
  AStart, ADelta: real;
begin
  inherited;

  // Inverse
  if Inverse then
    begin
      AStart := FEndValue;
      ADelta := -FDelta;
    end
  else
    begin
      AStart := FStartValue;
      ADelta := FDelta;
    end;

  // Calc
  FCurrentValue := AStart + CalculateAnimationValue(FKind, FStepValue, FTotalStep, ADelta);

  // Set
  if ComponentBased then
    TThread.Synchronize(nil, procedure
      begin
        SetPropertyValue(FComponent, PropertyName, FCurrentValue);
      end);
end;

procedure TFloatAnim.ExecuteAnimation;
begin
  if StartFromCurrent then
    FStartValue := GetPropertyValue(FComponent, PropertyName);
  FDelta := FEndValue - FStartValue;

  // Start
  inherited;
end;

end.
