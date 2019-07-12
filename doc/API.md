# XAda.Dispatching.TTS API

## `Set_Plan`

Set new TT plan to start at the end of the next mode change slot. This procedure can be invoked from any task. 

```ada
procedure Set_Plan
  (TTP : Time_Triggered_Plan_Access);
```

## `Wait_For_Activation`

TT works use this procedure to wait for their next assigned slot. 
The invoking task gets blocked until the next work slot with the proper `Work_Id` is reached. 
When task is released, the `When_Was_Released` result informs the caller task of the slot starting time.

If this procedure is invoked from within a TT slot it implicitly informs the TT scheduler that the current slot has been finished. 

```ada
procedure Wait_For_Activation
  (Work_Id           : TT_Work_Id;
   When_Was_Released : out Ada.Real_Time.Time);
```

## `Continue_Sliced`

TT works use this procedure to inform that the critical part of the current slot has been finished. 
It tranforms the current slot in a continuation slot. 

```ada
procedure Continue_Sliced;
```

## `Leave_TT_Level`

TT works use this procedure to inform the TT scheduler that there is no more work to do at TT priority level.

```ada
procedure Leave_TT_Level;
```

## `Get_Last_Plan_Release`

Returns the last time the first slot of the plan was released. 

```ada
function Get_Last_Plan_Release return Ada.Real_Time.Time;
```

## `Wait_For_Sync`

Priority-based tasks use this procedure to wait for their next assigned sync slot.
The invoking task gets blocked until the next sync slot with the proper `Sync_Id` is reached. 
If the sync slot is reached before the procedure is invoked, the caller tasks is not blocked and continues its execution. This behaviour is similar to use `delay until` with an absolute time in the past. 

When task is released, the `When_Was_Released` result informs the caller task of the slot starting time. 


If this procedure is invoked from within a TT slot it implicitly informs the TT scheduler that the current slot has been finished. 

```ada
procedure Wait_For_Sync
  (Sync_Id           : TT_Sync_Id;
   When_Was_Released : out Ada.Real_Time.Time);
```
