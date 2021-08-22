WITH Ada.Unchecked_Conversion, Ada.Text_IO; USE Ada.Text_IO;
with Direct_IO;
Package HashLab IS

   SUBTYPE Input_Items is String(1..18);
   SUBTYPE Hash_Items IS String(1..16);
   SUBTYPE Slice IS String (1..2);
   SUBTYPE OutString IS String(1..25);
   SUBTYPE HashSlice is String(1..6);
   TYPE SmallInt IS mod 2**16;
   TYPE TinyInt is mod 2**8;

   --Record for Hashing
   --Values = Key, 16 character string
   --Hash_Addr , self explanatory
   --ProbeNum, Number of probes to insert into table
   TYPE Hash_Entry IS RECORD
      Values : Hash_Items;
      Hash_Addr: Integer;
      ProbeNum : Integer := 1;
   END RECORD;

   PACKAGE Hash_Output_IO IS NEW Direct_IO(Input_Items);
   PACKAGE Hash_File_IO IS NEW Direct_IO(Hash_Entry);
   USE Hash_Output_IO; USE Hash_File_IO;

   --Initialize an array for hashing in main memory
   TYPE Hash_Table IS ARRAY(Integer RANGE <>) OF Hash_Entry;

   TYPE Probe_Method IS (Linear, Random);
   TYPE Hash_Function IS (Mine, Given);
   TYPE Hash_Location is (Main, FromFile);

   PACKAGE FloatIO IS NEW Ada.Text_IO.Float_IO(Float); USE FloatIO;


   FUNCTION Tiny_To_Small IS NEW
      Ada.Unchecked_Conversion(TinyInt,SmallInt);
   FUNCTION Small_To_Int IS NEW
      Ada.Unchecked_Conversion(SmallInt,Integer);
   FUNCTION Char_To_Tiny IS NEW
      Ada.Unchecked_Conversion(Character,TinyInt);
   FUNCTION Slice_To_Small IS NEW
      Ada.Unchecked_Conversion(Slice,SmallInt);


   --Functions to get hash address for appropriate function
   --GivenKey corresponds to lab defined hash function
   --MyKey corresponds to my own defined hash function
   FUNCTION GetGivenKey(Item: Hash_Items) RETURN Integer;
   FUNCTION GetMyKey (Item: Hash_Items) RETURN Integer;
   PROCEDURE MyFloatPut (X : Float);
   --Perform hashing in main memory
   PROCEDURE Hash_In_Main(InFile: String; Table_Size: Integer; Probe: Probe_Method;
      Hashing_Type: Hash_Function; Fill_Percent: Float);
   --Perform hashing in relative file
   PROCEDURE Hash_In_File(InFile: String; Out_File: String; Table_Size: Integer; Probe: Probe_Method;
      Hashing_Type: Hash_Function; Fill_Percent: Float);

   PROCEDURE Theoretical_Probe_Calc (Table_Size: Integer; Num_Keys: Integer; Probe: Probe_Method);

   --Probe Stats for Hash_in_File
   PROCEDURE Probe_Stats(InFile: Hash_Output_IO.File_Type; Start_Bound: Integer; End_Bound: Integer;
      Table_Size: Integer; Probe: Probe_Method; Hash_FN: Hash_Function;
      Location: Hash_Location; StoreFile : Hash_File_IO.File_Type);
   --Probe Stats for Hash_In_Main
   PROCEDURE Probe_Stats(InFile: String;My_Table: Hash_Table; Start_Bound: Integer; End_Bound: Integer;
      Table_Size: Integer; Probe: Probe_Method; Hash_FN: Hash_Function;
      Location: Hash_Location);

Print_File  : Hash_File_IO.File_Type;
END HashLab;
