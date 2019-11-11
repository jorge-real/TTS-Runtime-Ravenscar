------------------------------------------------------------
--
--  GNAT RUN-TIME EXTENSIONS
--
--  XADA . DISPATCHING . TIME-TRIGGERED SCHEDULING
--
--  @file x-distts.ads / xada-dispatching-tts.ads
--
--  @package XAda.Dispatching.TTS (SPEC)
--
--  @author Jorge Real <jorge@disca.upv.es>
--  @author Sergio Saez <ssaez@disca.upv.es>
--
------------------------------------------------------------
pragma Profile (Ravenscar);

with Ada.Real_Time, System;
with Ada.Real_Time.Timing_Events;

generic

   Number_Of_Work_IDs : Positive;
   Number_Of_Sync_IDs : Positive := 1;
   TT_Priority        : System.Priority := System.Priority'Last;

package XAda.Dispatching.TTS is

   ---------------------
   --  Slot id types  --
   ---------------------

   --  TT tasks use a Work_Id of this type to identify themselves
   --  when they call the scheduler
   type TT_Work_Id is new Positive range 1 .. Number_Of_Work_IDs;

   --  ET tasks use a Sync_Id of this type to identify themselves
   --  when they call the scheduler
   type TT_Sync_Id is new Positive range 1 .. Number_Of_Sync_IDs;

   ------------------
   --  Slot types  --
   ------------------

   --  An abstract time slot in the TT plan
   type Time_Slot is abstract tagged record
      Slot_Size : Ada.Real_Time.Time_Span;
   end record;

   function Slot_Duration (S: in Time_Slot)
     return Ada.Real_Time.Time_Span is (S.Slot_Size);

   type Any_Time_Slot is access all Time_Slot'Class;

   -- An empty time slot
   type Empty_Slot is new Time_Slot with null record;
   type Any_Empty_Slot is access all Empty_Slot'Class;

   -- A mode change time slot
   type Mode_Change_Slot is new Time_Slot with null record;
   type Any_Mode_Change_Slot is access all Mode_Change_Slot'Class;

   -- A sync slot
   type Sync_Slot is new Time_Slot with
      record
         Sync_Id         : TT_Sync_Id;
      end record;
   type Any_Sync_Slot is access all Sync_Slot'Class;

   --  To represent the whole duration of the slot
   Full_Slot_Size : constant Ada.Real_Time.Time_Span;

   -- A work slot
   type Work_Slot is abstract new Time_Slot with
      record
         Work_Id         : TT_Work_Id;
         Work_Size       : Ada.Real_Time.Time_Span;
         Padding_Size    : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
         Is_Continuation : Boolean := False;

         -- Indicate if this slot is the first or last slot of a given job
         Is_Initial      : Boolean := True;
         Is_Final        : Boolean := True;
      end record;

   function Work_Duration (S: in Work_Slot)
     return Ada.Real_Time.Time_Span is (S.Work_Size);

   function Padding_Duration (S: in Work_Slot)
     return Ada.Real_Time.Time_Span is (S.Padding_Size);

   type Any_Work_Slot is access all Work_Slot'Class;

   -- A regular slot
   type Regular_Slot is new Work_Slot with null record;
   type Any_Regular_Slot is access all Regular_Slot'Class;

   -- An optional work slot
   type Optional_Slot is new Work_Slot with null record;
   type Any_Optional_Slot is access all Optional_Slot'Class;

   -------------------
   --  Event types  --
   -------------------

   type Overrun_Event is new Ada.Real_Time.Timing_Events.Timing_Event with
      record
         Slot : Any_Work_Slot;
      end record;

   ---------------------
   --  TT Plan types  --
   ---------------------

   --  Types representing/accessing TT plans
   type Time_Triggered_Plan        is array (Natural range <>) of Any_Time_Slot;
   type Time_Triggered_Plan_Access is access all Time_Triggered_Plan;

   ------------------------------
   --  TT Scheduler interface  --
   ------------------------------

   --  To represent the end of a mode change slot
   End_Of_MC_Slot : constant Ada.Real_Time.Time;

   --  Set new TT plan to start at the end of the next mode change slot
   procedure Set_Plan
     (TTP : Time_Triggered_Plan_Access;
      At_Time : Ada.Real_Time.Time := End_Of_MC_Slot);

   --  TT works use this procedure to wait for their next assigned slot
   --  The When_Was_Released result informs caller of slot starting time
   procedure Wait_For_Activation
     (Work_Id           : TT_Work_Id;
      When_Was_Released : out Ada.Real_Time.Time);

   --  TT works use this procedure to inform that the critical part
   --  of the current slot has been finished. It tranforms the current
   --  slot in a continuation slot
   procedure Continue_Sliced;

   --  TT works use this procedure to inform the TT scheduler that
   --   there is no more work to do at TT priority level
   procedure Leave_TT_Level;

   --  Returns the first time the first slot of the current plan was released.
   --   It is equivalent to an Epoch for the current plan.
   function Get_First_Plan_Release return Ada.Real_Time.Time;

   --  Returns the last time the first slot of the plan was released
   function Get_Last_Plan_Release return Ada.Real_Time.Time;

   --  ET works use this procedure to wait for their next asigned sync slot
   procedure Wait_For_Sync
     (Sync_Id           : TT_Sync_Id;
      When_Was_Released : out Ada.Real_Time.Time);

   --  Returns current slot
   function Get_Current_Slot return Any_Time_Slot;

   --  Sets the overrun handler for a given Work_Id
   procedure Set_Overrun_Handler
     (Work_Id : TT_Work_Id;
      Handler : Ada.Real_Time.Timing_Events.Timing_Event_Handler);

   --  Sets if a given Work Id is active or not. Deactivation can be postponed
   --   until the work is completed
   procedure Set_Work_Active_Status
     (Work_Id : TT_Work_Id;
      Active  : Boolean);

private
   Full_Slot_Size : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Last;
   End_Of_MC_Slot : constant Ada.Real_Time.Time := Ada.Real_Time.Time_Last;

   protected Time_Triggered_Scheduler
     with Priority => System.Interrupt_Priority'Last is

      --  Setting a new TT plan
      procedure Set_Plan
        (TTP : Time_Triggered_Plan_Access;
         At_Time : Ada.Real_Time.Time);

      --  Prepare work to wait for next activation
      procedure Prepare_For_Activation
        (Work_Id : TT_Work_Id);

      --  Transform current slot in a continuation slot
      procedure Continue_Sliced;

      --  Inform the scheduler that you have no more work as a TT task
      procedure Leave_TT_Level;

      --  Returns the first time the first slot of the plan was released
      function Get_First_Plan_Release return Ada.Real_Time.Time;

      --  Returns the last time the first slot of the plan was released
      function Get_Last_Plan_Release return Ada.Real_Time.Time;

      --  Prepare work to wait for next synchronization point
      procedure Prepare_For_Sync
        (Sync_Id : TT_Sync_Id);

      -- Returns current slot
      function Get_Current_Slot return Any_Time_Slot;

      --  Sets the overrun handler for a given Work_Id
      procedure Set_Overrun_Handler
        (Work_Id : TT_Work_Id;
         Handler : Ada.Real_Time.Timing_Events.Timing_Event_Handler);

      --  Sets if a given Work Id is active or not
      procedure Set_Work_Active_Status
        (Work_Id : TT_Work_Id;
         Active  : Boolean);

   private
      --  New slot timing event
      Next_Slot_Event : Ada.Real_Time.Timing_Events.Timing_Event;

      --  New slot handler procedure
      procedure Next_Slot_Handler
        (Event : in out Ada.Real_Time.Timing_Events.Timing_Event);

      --  This access object is the reason why the scheduler is declared
      --  in this private part, given that this is a generic package.
      --  It should be a constant, but a PO can't have constant components.
      Next_Slot_Handler_Access : Ada.Real_Time.Timing_Events.Timing_Event_Handler :=
        Next_Slot_Handler'Access;

      --  End of Work timing event
      End_Of_Work_Event : Ada.Real_Time.Timing_Events.Timing_Event;

      --  End of Work handler procedure
      procedure End_Of_Work_Handler
        (Event : in out Ada.Real_Time.Timing_Events.Timing_Event);

      --  This access object is the reason why the scheduler is declared
      --  in this private part, given that this is a generic package.
      --  It should be a constant, but a PO can't have constant components.
      End_Of_Work_Access : Ada.Real_Time.Timing_Events.Timing_Event_Handler :=
        End_Of_Work_Handler'Access;

      --  Hold timing event
      Hold_Event : Ada.Real_Time.Timing_Events.Timing_Event;

      --  Padding slot handler procedure
      procedure Hold_Handler
        (Event : in out Ada.Real_Time.Timing_Events.Timing_Event);

      --  This access object is the reason why the scheduler is declared
      --  in this private part, given that this is a generic package.
      --  It should be a constant, but a PO can't have constant components.
      Hold_Handler_Access : Ada.Real_Time.Timing_Events.Timing_Event_Handler :=
        Hold_Handler'Access;

      --  Procedure to enforce plan change
      procedure Change_Plan
        (At_Time : Ada.Real_Time.Time);

      --  Currently running plan and next plan to switch to, if any
      Current_Plan       : Time_Triggered_Plan_Access := null;
      Next_Plan          : Time_Triggered_Plan_Access := null;

      --  Index numbers of current and next slots in the plan
      Current_Slot_Index : Natural := 0;
      Next_Slot_Index    : Natural := 0;

      --  Start time of next slot
      Next_Slot_Release  : Ada.Real_Time.Time := Ada.Real_Time.Time_Last;

      --  End of current work slot
      End_Of_Work_Release : Ada.Real_Time.Time := Ada.Real_Time.Time_Last;

      --  Hold time for a work slot
      Hold_Release       : Ada.Real_Time.Time := Ada.Real_Time.Time_Last;

      --  Mode change next release
      Next_Mode_Release  : Ada.Real_Time.Time := Ada.Real_Time.Time_Last;

      --  Start time of the current plan
      Plan_Start_Pending : Boolean := True;
      First_Plan_Release : Ada.Real_Time.Time := Ada.Real_Time.Time_First;

      --  Start time of the first slot
      First_Slot_Release : Ada.Real_Time.Time := Ada.Real_Time.Time_First;

   end Time_Triggered_Scheduler;

end XAda.Dispatching.TTS;
