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

with Ada.Real_Time;
private with Ada.Real_Time.Timing_Events, System;

generic

   Number_Of_Work_IDs : Positive;

package XAda.Dispatching.TTS is

   --  Slot types
   type Kind_Of_Slot is (TT_Work_Slot, Empty_Slot, Mode_Change_Slot);

   --  TT tasks use a Work_Id of this type to identify themselves
   --  when they call the scheduler
   type TT_Work_Id is new Positive range 1 .. Number_Of_Work_IDs;

   --  A time slot in the TT plan. We use a variant record because different
   --  kinds of slots require different types of information
   type Time_Slot (Kind : Kind_Of_Slot := TT_Work_Slot) is record
      Slot_Duration : Ada.Real_Time.Time_Span;
      case Kind is
         when TT_Work_Slot =>
            Work_Id         : TT_Work_Id;
            Is_Continuation : Boolean := False;
            Is_Optional     : Boolean := False;
         when others =>
            null;
      end case;
   end record;

   --  Types representing/accessing TT plans
   type Time_Triggered_Plan        is array (Natural range <>) of Time_Slot;
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

   --  TT works use this procedure to inform the TT scheduler that
   --   the next slot is not required. It can be used from PB-level.
   --  If it is used at TT-level, it implies a Leave_TT_Level action
   procedure Skip_Next_Slot
     (Work_Id : TT_Work_Id);

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

      --  Inform the scheduler that the next slot is not required
      procedure Skip_Next_Slot
        (Work_Id    : TT_Work_Id;
         Must_Leave : out Boolean);

   private
      --  New slot timing event
      NS_Event : Ada.Real_Time.Timing_Events.Timing_Event;

      --  New slot handler procedure
      procedure NS_Handler
        (Event : in out Ada.Real_Time.Timing_Events.Timing_Event);

      --  This access object is the reason why the scheduler is declared
      --  in this private part, given that this is a generioc package.
      --  It should be a constant, but a PO can't have constant components.
      NS_Handler_Access : Ada.Real_Time.Timing_Events.Timing_Event_Handler :=
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

end XAda.Dispatching.TTS;
