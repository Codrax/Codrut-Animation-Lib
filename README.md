# Codrut-Animation-Lib
Codruts Animation Library is a simple component-based animation library for Delphi VCL. It's similar to the official FMX counterpart.

### It can also support sub-classes!
If a property like `top` works, you can also use `margin.top` to change the sub-property of the class `margin`!
You can also use as many sub-classes as you wish. Like `Button1.Margin.Interior.Size.Left`!

### Component
| Image  | Description |
| ------------- | ------------- |
| ![Example](https://github.com/Codrax/Codrut-Animation-Lib/assets/68193064/c8ee78ec-75bc-4ae8-9f0b-48a0e0a6bbef)  | The animation control on the form  |
| ![Inspector](https://github.com/Codrax/Codrut-Animation-Lib/assets/68193064/1170d366-0a45-46d5-8c34-cd01b52cf747)  | To make the animation work, first select a component to animate, the animation, the start and end value and the property to change  |


## Start / stop animation
You can change the `Running` property, or use the `Start` or `Stop` procedures.

You may also pause the animation using the `Paused` property.

## Properties
| Property  | Type | Description |
| ------------- | ------------- | ------------- |
| Component | TComponent | The component of which the property will be changed (optional) |
| CurrentValue | {AnimationType} | The current animation value (Read-only) |
| Delay | Single | The time waited before the animation actually starts in seconds, also can be in-between loops |
| DelayLoop | Toggle to wait the Delay amout inbetween loops |
| Duration | Single | The animation duration |
| EndValue | {AnimationType} | The end value |
| Inverse | Boolean | Execute from End to Start |
| Kind | TAnimationKind | The animation function used |
| Loop | Boolean | Restart the animation on end |
| LoopInverse | Boolean | Toggle the inverse property on loop |
| Percent | Single | The percentage of the animation being finalised (Read-only) |
| PropertyName | String | The property to change. Can also access properties of sub-classes with the `.` separator. (optional) |
| StartFromCurrent | Boolean | Defines if the start value is determined by the current Component Value (Requires `component` and `PropertyName`) |
| StartValue | {AnimationType} | The value from which the animation starts. Can be overriden by `StartFromCurrent` |
| Steps | Integer | Set a custom steps value for the animation, instead of auto-calculating them. The less steps the greater the drawing performance |

## Animation types
- Linear
- Exponential
- Pulse
- Random
- ReverseExpo
- Sinus
- SinusAr
- Wobblyc
- Spring

Note: `Pulse` and `Wobbly` do not reach their end value, as they return back to their start value.

## Use without component
1) Assign the OnStep notify event from the Object Inspector.
2) Get the `CurrentValue` property from the Sender
3) Use the value as needed
Example:
```
  procedure TForm1.NewAnimation1Step(Sender: TObject; Step, TotalSteps: Integer);
  var
    Value: integer;
  begin
    Value := TIntAnim(Sender).CurrentValue;
  
    // now use the value
  end;
```

## Animation
![Animation](https://github.com/Codrax/Codrut-Animation-Lib/assets/68193064/5493e918-ca90-433c-b06d-6845fbc18f2b)
