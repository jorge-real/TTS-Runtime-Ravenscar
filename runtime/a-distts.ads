------------------------------------------------------------
--
--  ADA . DISPATCHING . TIME-TRIGGERED SCHEDULING
--
--  @file a-distts.ads
--
--  @package Ada.Dispatching.TTS (SPEC)
--
--  @author Jorge Real <jorge@disca.upv.es>
--  @author Sergio Saez <ssaez@disca.upv.es>
--
------------------------------------------------------------

private with Ada.Real_Time.Timing_Events, System;

with Ada.Real_Time;

generic

   Number_Of_Work_IDs : Positive;

package Ada.Dispatching.TTS is

--     --  Work identifier types
--     type Any_Work_Id is new Integer range Integer'First .. Number_Of_Work_IDs;
--     subtype Special_Work_Id is Any_Work_Id range Any_Work_Id'First .. 0;
--     subtype Regular_Work_Id is Any_Work_Id range 1 .. Any_Work_Id'Last;

   --  Slot types
   type Any_Slot_Id is new Integer range Integer'First .. Number_Of_Work_IDs;
   subtype Special_Slot_Id is Any_Slot_Id range Any_Slot_Id'First .. 0;
   subtype Regular_Slot_Id is Any_SLot_Id range 1 .. Any_Slot_Id'Last;

   --  The Work_ID of a TT task is a positive integer and a regular slot is
   --  represented by a positive as well. The following declaration is for
   --  clarity when using the Work_ID as parameters to the package-provided
   --  subprograms
   subtype Regular_Work_Id is Regular_Slot_Id;



   --  Special IDs
   Empty_Slot       : constant Special_Slot_Id;
   Mode_Change_Slot : constant Special_Slot_Id;

   --  A time slot in the TT plan
   type Time_Slot is record
      Slot_Duration   : Ada.Real_Time.Time_Span;
      Slot_Id         : Any_Slot_Id;
      Is_Continuation : Boolean := False;
      Is_Optional     : Boolean := False;
   end record;

   --  Types representing/accessing TT plans
   type Time_Triggered_Plan        is array (Natural range <>) of Time_Slot;
   type Time_Triggered_Plan_Access is access all Time_Triggered_Plan;

   --  Set new TT plan to start at the end of the next mode change slot
   procedure Set_Plan
     (TTP       : Time_Triggered_Plan_Access);

   --  TT works use this procedure to wait for their next assigned slot
   --  The When_Was_Released result informs caller of slot starting time
   procedure Wait_For_Activation
     (Work_Id : Regular_Work_Id;
      When_Was_Released : out Ada.Real_Time.Time);

   --  TT works use this procedure to inform that the critical part
   --  of the current slot has been finished. It tranforms the current
   --  slot in a continuation slot
   procedure Continue_Sliced;

   --  TT works use this procedure to inform the TT scheduler that
   --   there is no more work to do at TT priority level
   procedure Leave_TT_Level
     (Work_Id : Regular_Work_Id);

   --  TT works use this procedure to inform the TT scheduler that
   --   the next slot is not required. It can be used from PB-level.
   --  If it is used at TT-level, it implies a Leave_TT_Level action
   procedure Skip_Next_Slot
     (Work_Id : Regular_Work_Id);

private

   Empty_Slot       : constant Special_Slot_Id :=  0;
   Mode_Change_Slot : constant Special_Slot_Id := -1;

   protected Time_Triggered_Scheduler
     with Priority => System.Interrupt_Priority'Last is

      --  Setting a new TT plan
      procedure Set_Plan
        (TTP     : Time_Triggered_Plan_Access);

      --  Prepare work to wait for next activation
      procedure Prepare_For_Activation
        (Work_Id : Regular_Work_Id);

      --  Transform current slot in a continuation slot
      procedure Continue_Sliced;

      --  Inform the scheduler that you have no more work as a TT task
      procedure Leave_TT_Level
        (Work_Id : Regular_Work_Id);

      --  Inform the scheduler that the next slot is not required
      procedure Skip_Next_Slot
        (Work_Id : Regular_Work_Id;
         Must_Leave : out Boolean);

   private
      --  New slot timing event
      NS_Event           : Ada.Real_Time.Timing_Events.Timing_Event;

      --  New slot handler procedure
      procedure NS_Handler
        (Event : in out Ada.Real_Time.Timing_Events.Timing_Event);

      --  This access object is the reason why the scheduler is declared
      --  in this private part, given that this is a generioc package.
      --  It should be a constant, but a PO can't have constant components.
      NS_Handler_Access  : Ada.Real_Time.Timing_Events.Timing_Event_Handler :=
        NS_Handler'Access;

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

   end Time_Triggered_Scheduler;

end Ada.Dispatching.TTS;
