WITH Hashlab;
USE Hashlab;
WITH Ada.Text_IO;
USE Ada.Text_IO;
WITH Ada.Integer_Text_IO;
USE Ada.Integer_Text_IO;

PROCEDURE Hash_Test IS

BEGIN


   --C Option
   Put_Line("------------------ 'C' Option -------------------");
   New_Line;
   Put_Line(
    "------------------Linear Probe w/ Lab Function @ 55%/85% load-----------------------");
   New_Line;
   Hashlab.Hash_In_Main("Words200D16.txt",128,Linear,Given,0.55);
   Theoretical_Probe_Calc(128,70,Linear);
   Hashlab.Hash_In_Main("Words200D16.txt",128,Linear,Given,0.85);
   Theoretical_Probe_Calc(128,109,Linear);
   New_Line;
   Put_Line(
     "------------------Random Probe w/ Lab Function @ 55%/85% load-----------------------");
   New_Line;
   Hashlab.Hash_In_Main("Words200D16.txt",128,Random,Given,0.55);
   Theoretical_Probe_Calc(128,70,Random);
   Hashlab.Hash_In_Main("Words200D16.txt",128,Random,Given,0.85);
   Theoretical_Probe_Calc(128,109,Random);
   New_Line;
   Put_Line(
     "------------------Linear Probe w/ My Function @ 55%/85% load-----------------------");
   New_Line;
   Hashlab.Hash_In_Main("Words200D16.txt",128,Linear,Mine,0.55);
   Theoretical_Probe_Calc(128,70,Linear);
   Hashlab.Hash_In_Main("Words200D16.txt",128,Linear,Mine,0.85);
   Theoretical_Probe_Calc(128,109,Linear);
   New_Line;
   Put_Line(
     "------------------Random Probe w/ My Function @ 55%/85% load-----------------------");
   New_Line;
   Hashlab.Hash_In_Main("Words200D16.txt",128,Random,Mine,0.55);
   Theoretical_Probe_Calc(128,70,Random);
   Hashlab.Hash_In_Main("Words200D16.txt",128,Random,Mine,0.85);
   Theoretical_Probe_Calc(128,109,Random);


   --B Option
   Put_Line("------------------ 'B' Option -------------------");
   New_Line;
   Put_Line(
      "------------------Linear Probe w/ Lab Function @ 55%/85% load-----------------------");
   New_Line;
   Hashlab.Hash_In_File("Words200D16.txt","HashOutput.txt",128,Linear,Given,0.55);
   Theoretical_Probe_Calc(128,70,Linear);
   Hashlab.Hash_In_File("Words200D16.txt","HashOutput.txt",128,Linear,
      Given,0.85);
   Theoretical_Probe_Calc(128,109,Linear);
   New_Line;
   Put_Line(
       "------------------Random Probe w/ Lab Function @ 55%/85% load-----------------------");
    New_Line;
    Hashlab.Hash_In_File("Words200D16.txt","HashOutput.txt",128,Random,
      Given,0.55);
    Theoretical_Probe_Calc(128,70,Random);
    Hashlab.Hash_In_File("Words200D16.txt","HashOutput.txt",128,Random,
       Given,0.85);
   Theoretical_Probe_Calc(128,109,Random);
   New_Line;
   Put_Line(
      "------------------Linear Probe w/ My Function @ 55%/85% load-----------------------");
   New_Line;
   Hashlab.Hash_In_File("Words200D16.txt","HashOutput.txt",128,Linear,
      Mine,0.55);
   Theoretical_Probe_Calc(128,70,Linear);
    Hashlab.Hash_In_File("Words200D16.txt","HashOutput.txt",128,Linear,
      Mine,0.85);
   Theoretical_Probe_Calc(128,109,Linear);
    New_Line;
    Put_Line(
       "------------------Random Probe w/ My Function @ 55%/85% load-----------------------");
    New_Line;
    Hashlab.Hash_In_File("Words200D16.txt","HashOutput.txt",128,Random,
       Mine,0.55);
    New_Line;
    Theoretical_Probe_Calc(128,70,Random);
    New_Line;
    Hashlab.Hash_In_File("Words200D16.txt","HashOutput.txt",128,Random,
       Mine,0.85);
    Theoretical_Probe_Calc(128,109,Random);


   --A Option
   --Both B and C options, compare and explain the results
END Hash_Test;
