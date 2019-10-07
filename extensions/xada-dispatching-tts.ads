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
private with Ada.Real_Time.Timing_Events;

generic

   Number_Of_Work_IDs : Positive;
   Number_Of_Sync_IDs : Positive := 1;
   TT_Priority : System.Priority := System.Priority'Last;

package XAda.Dispatching.TTS is

   --  TT tasks use a Work_Id of this type to identify themselves
   --  when they call the scheduler
   type TT_Work_Id is new Positive range 1 .. Number_Of_Work_IDs;

   --  ET tasks use a Sync_Id of this type to identify themselves
   --  when they call the scheduler
   type TT_Sync_Id is new Positive range 1 .. Number_Of_Sync_IDs;

   --  An abstract time slot in the TT plan
   type Time_Slot is abstract tagged record
      Fixed_Slot_Duration : Ada.Real_Time.Time_Span;
   end record;

   function Slot_Duration (S: in Time_Slot)
     return Ada.Real_Time.Time_Span is (S.Fixed_Slot_Duration);



   -- An empty time slot
   type Empty_Slot is new Time_Slot with null record;
   type Any_Empty_Slot is access all Empty_Slot'Class;


   -- A mode change time slot
   type Mode_Change_Slot is new Time_Slot with null record;
   type Mode_Change_Slot_Access is access all Mode_Change_Slot'Class;

   -- A sync slot
   type Sync_Slot is new Time_Slot with
      record
         Sync_Id         : TT_Sync_Id;
      end record;
   type Sync_Slot_Access is access all Sync_Slot'Class;

   -- A work slot
   type Work_Slot is abstract new Time_Slot with
      record
         Work_Id         : TT_Work_Id;
         Is_Continuation : Boolean := False;
         Padding         : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
      end record;
   type Work_Slot_Access is access all Work_Slot'Class;

   -- A regular slot
   type Regular_Slot is new Work_Slot with null record;
   type Regular_Slot_Access is access all Regular_Slot'Class;

   -- An optional work slot
   type Optional_Slot is new Work_Slot with null record;
   type Optional_Slot_Access is access all Optional_Slot'Class;

   --  Types representing/accessing TT plans
   type Time_Triggered_Plan        is array (Natural range <>) of Time_Slot_Access;
   type Time_Triggered_Plan_Access is access all Time_Triggered_Plan;

   --  Set new TT plan to start at the end of the next mode change slot
   procedure Set_Plan
     (TTP : Time_Triggered_Plan_Access);

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

private

   protected Time_Triggered_Scheduler
     with Priority => System.Interrupt_Priority'Last is

      --  Setting a new TT plan
      procedure Set_Plan
        (TTP : Time_Triggered_Plan_Access);

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

   private
      --  New slot timing event
      NS_Event : Ada.Real_Time.Timing_Events.Timing_Event;

      --  New slot handler procedure
      procedure NS_Handler
        (Event : in out Ada.Real_Time.Timing_Events.Timing_Event);

      --  This access object is the reason why the scheduler is declared
      --  in this private part, given that this is a generic package.
      --  It should be a constant, but a PO can't have constant components.
      NS_Handler_Access : Ada.Real_Time.Timing_Events.Timing_Event_Handler :=
        NS_Handler'Access;

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

      --  Start time of the current plan
      Plan_Start_Pending : Boolean := True;
      First_Plan_Release : Ada.Real_Time.Time := Ada.Real_Time.Time_First;

      --  Start time of the first slot
      First_Slot_Release : Ada.Real_Time.Time := Ada.Real_Time.Time_First;

   end Time_Triggered_Scheduler;

end XAda.Dispatching.TTS;
