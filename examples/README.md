# Examples using the TT_Utilities library

-------------
TTS_EXAMPLE_A
-------------
This example uses 5 TT tasks:

 - Wk1: A simple TT task that sets Var_1, a global, volatile variable, to zero
  
 - Wk3: Same as Wk1 resp. Var_2
  
 - Wk2: A IMs-F task. The I part checks that Var_1 is set to zero; the Ms part adds 1 to Var_1 300_000 times; and the F part displays the value of Var_1
  
 - Wk4: Same as Wk2 resp. Var_2
  
 - Wk5: An I-F task. The I part displays the values of Var_1 and Var_2; the F part simply displays a message
  
  
The plan is stored in the array TT_Plan.

The Main5 procedure is the main subprogram.
The project file tts_ravenscar.gpr is configured for this example.
Tested on a modified vefrsion of the ravenscar-full runtime for the STM32F4 Discovery board. 
We just changed the clock resolution to make it 10 us (see s-bbbosu.ads in the "runtime" directory)

File "TTS Example A.pdf" gives a sketched representation of the example system and the plan.
