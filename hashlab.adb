WITH Ada.Integer_Text_IO; USE Ada.Integer_Text_IO;
WITH Ada.Numerics.Elementary_Functions; USE Ada.Numerics.Elementary_Functions;
with URandInt;
PACKAGE BODY Hashlab IS


   PROCEDURE MyFloatPut (X : Float) IS
   BEGIN
      FloatIO.Put(X,2,2,0);
   END;


   FUNCTION GetGivenKey (Item:Hash_Items) RETURN Integer IS
      --SmallInt is a 16-bit integer
      --TinyInt is an 8-bit integer
      temp : SmallInt;
      HA : Integer;
      Temp2 : TinyInt;
   BEGIN
      Temp:= abs(Slice_To_Small(Item(1..2)))/3;
      Temp:=  Temp + abs(Slice_To_Small(Item(15..16)))/3;
      Temp:= Temp / 2048;
      Temp2 := ABS(Char_To_Tiny(Item(8)));
      Temp := Tiny_To_Small(Temp2) + Temp;
      Temp := Temp mod (128 + 1);
      HA:= Small_To_Int(Temp);
      return HA;
   END GetGivenKey;

FUNCTION GetMyKey (Item:Hash_Items) RETURN Integer IS
      --SmallInt = 16bit Integer
      temp : SmallInt;
      HA : Integer;
   Temp2 : SmallInt;
   --Utilizes a simple code with weights, plus appropriate shifting to
   --achieve a fairly good distribution across a hash table
   BEGIN
      Temp:= abs(Slice_To_Small(Item(1..2)));
      Temp:=  Temp + ABS(Slice_To_Small(Item(15..16)))*2;
      Temp:= Temp * 8;
      Temp:= Temp / 2;
      Temp2 := ABS(Slice_To_Small(Item(7..8))*3);
      Temp := Temp2 + Temp;
      Temp:= Temp * 8;
      Temp:= Temp / 4;
      Temp := Temp mod (128 + 1);
      HA:= Small_To_Int(Temp);

      return HA;

   END GetMyKey;




  --Overloaded Probe_Stats just for Hash_In_Main
  PROCEDURE Probe_Stats(InFile: String; My_Table: Hash_Table; Start_Bound: Integer; End_Bound: Integer;
      Table_Size: Integer; Probe: Probe_Method; Hash_FN: Hash_Function;
        Location: Hash_Location) IS


      NumKeyInChunk : Float := 30.0;
      Min_Probes: Integer := 1;
      Max_Probes: Integer := 1;
     Avg_Probes: Float := 0.0;
     Sum_Probes: Integer := 0;
      File: Hash_Output_IO.File_Type;
      Temp : Input_Items;
     Search_Record : Hash_Entry;

   BEGIN
      Open(File, In_File, InFile);
      FOR I IN Start_Bound..End_Bound LOOP
         DECLARE
            Values : Input_Items;
            Offset : Integer := 0;
            N : Float := Log(Base => 2.0, X => Float(Table_Size));

            PACKAGE RandomOffset IS NEW URandInt(Integer(N));
            USE RandomOffset;
         BEGIN
            --Read Keys into a Search Record
            Read(File, Temp, Hash_Output_IO.Count(i));
            Search_Record.Values := Temp(1..16);
            --Get Initial Hash Address for key
               IF Hash_FN = Given THEN Search_Record.Hash_Addr := GetGivenKey(Search_Record.Values);
            ELSE Search_Record.Hash_Addr := GetMyKey(Search_Record.Values);
            END IF;

            Search_Record.ProbeNum := 1;

            --Search for key in HashTable
            WHILE Search_Record.Values /= My_Table(Search_Record.Hash_Addr).Values LOOP
            Search_Record.ProbeNum := Search_Record.ProbeNum + 1;

            --Update offset if Random Probe or set to 1 for linear
               IF Probe = Linear THEN Offset := 1;
               ELSE Offset := UniqueRandInteger;
               END IF;

               -- Given is HashFN from lab assignment
               -- Mine is my created hash function
               IF Hash_FN = Given and Probe = Linear then
                  Search_Record.Hash_Addr := Search_Record.Hash_Addr + Offset;
               ELSIF Hash_FN = Given AND Probe = Random THEN
                  Search_Record.Hash_Addr := GetGivenKey(Search_Record.Values) + Offset;
               ELSIF Hash_FN = Mine AND Probe = Linear THEN
                  Search_Record.Hash_Addr := Search_Record.Hash_Addr + Offset;
               ELSIF Hash_FN = Mine and Probe = Random then
                  Search_Record.Hash_Addr := GetMyKey(Search_Record.Values) + Offset;
               END IF;

               --Handle Hash Address going out of table bounds
               --to wrap around and start back at the top
               IF Search_Record.Hash_Addr > 128 and Probe = Linear
                  THEN Search_Record.Hash_Addr :=  Search_Record.Hash_Addr - 128;
               END IF;
               IF Search_Record.Hash_Addr > 128 AND Probe = Random
                     THEN Search_Record.Hash_Addr :=  Search_Record.Hash_Addr mod 128;
               end if;

            END LOOP;

            --Update Min/Max and Average
            IF Search_Record.ProbeNum < Min_Probes
               THEN Min_Probes := Search_Record.ProbeNum;
            END IF;
            IF Search_Record.ProbeNum > Max_Probes
               THEN Max_Probes := Search_Record.ProbeNum;
            END IF;

            Sum_Probes := Search_Record.ProbeNum + Sum_Probes;
            Avg_Probes := Float(Sum_Probes) /NumKeyInChunk ;

         END;
      END LOOP;

      Put_Line("Minimum Num. Probes:" & " " & Integer'Image(Min_Probes));
      Put_Line("Maximum Num. Probes:" & " " & Integer'Image(Max_Probes));
      Put_Line("Average Num. Probes:" & " " );
      MyFloatPut(Avg_Probes); new_line;
      Put_Line("End of Probe Statistics.");
      Close(File);
   end Probe_Stats;


   --Probe_Stats Function just for Hash in File
   PROCEDURE Probe_Stats(inFile: Hash_Output_IO.File_Type; Start_Bound: Integer; End_Bound: Integer;
      Table_Size: Integer; Probe: Probe_Method; Hash_FN: Hash_Function;
      Location: Hash_Location; StoreFile : Hash_File_IO.File_Type) IS


      Min_Probes: Integer := 500;
      Max_Probes: Integer := 1;
      Avg_Probes: Float := 0.0;
      Sum_Probes: Integer := 0;
      Temp : Input_Items;
      Search_Record : Hash_Entry;
      NumKeyInChunk: Float := 30.0;

   BEGIN
      FOR I IN Start_Bound..End_Bound LOOP
         DECLARE
            N : Float := Log(Base => 2.0, X => Float(Table_Size));
            TempHE: Hash_Entry;
            Offset : Integer := 0;
            PACKAGE RandomOffset IS NEW URandInt(Integer(N));
            USE RandomOffset;
         BEGIN
            --Read keys from file
            Read(inFile,Temp,Hash_Output_IO.Count(I));
            Search_Record.Values := Temp(1..16);
            --Get original hash address
            IF Hash_FN = Given THEN Search_Record.Hash_Addr := GetGivenKey(Search_Record.Values);
            ELSE Search_Record.Hash_Addr := GetMyKey(Search_Record.Values);
            END IF;

            --Initialize for first probe of new key search
            Search_Record.ProbeNum := 1;
            LOOP
            --Nested IF to get the next hash address to check then handle address overflow
            IF Hash_FN = Given and Probe = Linear then
                  Search_Record.Hash_Addr := Search_Record.Hash_Addr + Offset;
               ELSIF Hash_FN = Given AND Probe = Random THEN
                  Search_Record.Hash_Addr := GetGivenKey(Search_Record.Values) + Offset;
               ELSIF Hash_FN = Mine AND Probe = Linear THEN
                  Search_Record.Hash_Addr := Search_Record.Hash_Addr + Offset;
               ELSIF Hash_FN = Mine and Probe = Random then
                  Search_Record.Hash_Addr := GetMyKey(Search_Record.Values) + Offset;
               END IF;

               IF Search_Record.Hash_Addr > 128 and Probe = Linear
                  THEN Search_Record.Hash_Addr :=  Search_Record.Hash_Addr - 128;
               END IF;
               IF Search_Record.Hash_Addr > 128 AND Probe = Random
                     THEN Search_Record.Hash_Addr :=  Search_Record.Hash_Addr mod 128;
               end if;

           --Read key in storage file, stop if found, if not: update offset and probe count
           Hash_File_IO.Read(StoreFile, TempHE, Hash_File_IO.Count(Search_Record.Hash_Addr));
                  Exit when Search_Record.Values = TempHE.Values;

              IF Probe = Linear THEN Offset :=  1;
                  ELSE Offset := UniqueRandInteger;
              END IF;

                  Search_Record.ProbeNum := Search_Record.ProbeNum + 1;

            END LOOP;
            --Update Min/Max and Average
            IF Search_Record.ProbeNum < Min_Probes
               THEN Min_Probes := Search_Record.ProbeNum;
            END IF;
            IF Search_Record.ProbeNum > Max_Probes
               THEN Max_Probes := Search_Record.ProbeNum;
            END IF;

            Sum_Probes := Search_Record.ProbeNum + Sum_Probes;
            Avg_Probes := Float(Sum_Probes) /NumKeyInChunk ;

         END;
      END LOOP;

      --Print output for ease of viewing results

      Put_Line("Minimum Num. Probes:" & " " & Integer'Image(Min_Probes));
      Put_Line("Maximum Num. Probes:" & " " & Integer'Image(Max_Probes));
      Put_Line("Average Num. Probes:" & " " );
      MyFloatPut(Avg_Probes); new_line;
      Put_Line("End of Probe Statistics.");
   end Probe_Stats;



   --Create the hash table in main memory and hash entries until fill percent reached
   PROCEDURE Hash_In_Main(InFile: String; Table_Size: Integer;
         Probe: Probe_Method; Hashing_Type: Hash_Function; Fill_Percent: Float) IS

      Location : Hash_Location := Main;
      File: Hash_Output_IO.File_Type;
      FillSize : Integer := Integer(Float'Floor(Float(Table_Size) * Fill_Percent));

   BEGIN
      Open(File, In_File, InFile);
      Reset(File);
      DECLARE
         --table initalize with all blanks
         Blank_Entry: Hash_Entry :=(Values => "                ", Hash_Addr => 0, ProbeNum => 0);
         Main_Table: Hash_Table(0..Table_Size) := (others => Blank_Entry);
      BEGIN
         FOR I IN 1..FillSize LOOP
            DECLARE
               Hash_Record : Hash_Entry;
               Temp : Input_Items;
               Offset: Integer := 0;
               N : Float := Log(Base => 2.0, X => Float(Table_Size));
               --Value of N is TableSize = 2^N , for a table of 128, n=7
               PACKAGE RandomOffset IS NEW URandInt(Integer(N));
               USE RandomOffset;
            BEGIN
               Read(File,Temp,Hash_Output_IO.Count(I)); --read from file
               Hash_Record.Values := Temp(1..16); --store into record

               IF Hashing_Type = Given THEN Hash_Record.Hash_Addr := GetGivenKey(Hash_Record.Values);
               ELSE Hash_Record.Hash_Addr:= GetMyKey(Hash_Record.Values);
               END IF;

                  --Search for an empty space in the table
               WHILE Main_Table(Hash_Record.Hash_Addr).Values /= Blank_Entry.Values LOOP

                  --Update offset if necessary
                  IF Probe = Linear THEN Offset := Offset + 1;
                  ELSE Offset := UniqueRandInteger;
                  END IF;
                  -- Update probe number and check for hash address overflow table index
                  Hash_Record.ProbeNum := Hash_Record.ProbeNum + 1;
                  IF Hashing_Type = Given then
                     Hash_Record.Hash_Addr := GetGivenKey(Hash_Record.Values) + Offset;
                  ELSE
                     Hash_Record.Hash_Addr := GetMyKey(Hash_Record.Values) + Offset;
                  END IF;
                  IF Hash_Record.Hash_Addr  > 128
                  THEN Hash_Record.Hash_Addr :=  Hash_Record.Hash_Addr  - 128;
                  END IF;

               END LOOP;

               Main_Table(Hash_Record.Hash_Addr) := Hash_Record;


               end;
         end loop;
         --Loop for printing values from Hash Table
         FOR I IN 1..Table_Size LOOP
            IF Main_Table(I).Values /= Blank_Entry.Values THEN
               Put(Integer'Image(I) & " : " & Main_Table(I).Values & " ");
               IF Hashing_Type = Given THEN
                  Put("Original Location:" & " " & Integer'Image(GetGivenKey(Main_Table(I).Values)) & " ");
               ELSE
                  Put("Original Location:" & " " & Integer'Image(GetMyKey(Main_Table(I).Values)) & " ");
                  end if;
               Put("Probes:" & " " & Integer'Image(Main_Table(I).ProbeNum)); New_Line;
            ELSE Put_Line(Integer'Image(I) & " :  " & "NULL");
            END IF;

         END LOOP;

         Close(File);
         Put_Line("Probe Statistics for 1st 30 keys"); new_line;
         Probe_Stats("Words200D16.txt",Main_Table, 1, 30, Table_Size, Probe, Hashing_Type, Location);
         Put_Line("Probe Statistics for last 30 keys"); New_Line;
         Probe_Stats("Words200D16.txt",Main_Table, FillSize-29, FillSize, Table_Size, Probe, Hashing_Type, Location);
      END;
      End Hash_In_Main;

   --Calcuate theoretical probe values and show to user
   PROCEDURE Theoretical_Probe_Calc(Table_Size: Integer; Num_Keys: Integer; Probe: Probe_Method) IS
      alpha, E : Float;
   BEGIN
      alpha := (Float(Num_Keys)) / (Float(Table_Size));

      IF Probe = Linear THEN E:= (1.0 - alpha / 2.0) / (1.0 - Alpha);
      ELSE E:= -(1.0 / Alpha) * (Log(1.0 - Alpha));
      END IF;

      Put_Line("----------Theoretical Probe Calculation----------");
      Put_Line("Number of Keys:" & " " & Integer'Image(Num_Keys));
      Put("Table load:" & " ");
      MyFloatPut(Alpha);
      New_Line;
      Put("Calculated value of E, expected number of probes to locate key:");
      MyFloatPut(E);
      New_Line;
      Put_Line("--------------------");
   END Theoretical_Probe_Calc;




   PROCEDURE Hash_In_File(InFile: String; Out_File: String; Table_Size: Integer; Probe: Probe_Method;
         Hashing_Type: Hash_Function; Fill_Percent: Float) IS

      Location : Hash_Location := FromFile;
      Input: Hash_Output_IO.File_Type;
      StoreFile: Hash_File_IO.File_Type;
      FillSize : Integer := Integer(Float'Floor(Float(Table_Size) * Fill_Percent));
      TempHE : Hash_Entry;

   BEGIN
      Open(Input, In_File, InFile);
      DECLARE
         --Initialize a blank record
         Blank_Entry: Hash_Entry :=(Values => "                ", Hash_Addr => 0, ProbeNum => 0);
      BEGIN
         --Create Storage File where hashing table will be located,
         --And initialize all records in file to be empty.
         Create(StoreFile, InOut_File, Out_File);
         FOR I IN 1..Table_Size LOOP
            Hash_File_IO.Write(StoreFile, Blank_Entry, Hash_File_IO.Count(I));
         END LOOP;

         --Start filling storage file with hash records
         FOR I IN 1..FillSize LOOP
            DECLARE
               Hash_Record : Hash_Entry;
               Temp : Input_Items;
               Offset: Integer := 0;
               N : Float := Log(Base => 2.0, X => Float(Table_Size));
               --Value of N is TableSize = 2^N , for a table of 128, n=7
               PACKAGE RandomOffset IS NEW URandInt(Integer(N));
               USE RandomOffset;

            BEGIN
               Hash_Output_IO.Read(Input, Temp, Hash_Output_IO.Count(I));
               Hash_Record.Values := Temp(1..16);

               IF Hashing_Type = Given THEN Hash_Record.Hash_Addr := GetGivenKey(Hash_Record.Values);
               ELSE Hash_Record.Hash_Addr:= GetMyKey(Hash_Record.Values);
               END IF;

               --Set new hash address to look at for a collision
               --Offset initially 0 to look at original hash location
                  LOOP
                  IF Hashing_Type = Given then
                     Hash_Record.Hash_Addr := GetGivenKey(Hash_Record.Values) + Offset;
                  ELSE
                     Hash_Record.Hash_Addr := GetMyKey(Hash_Record.Values) + Offset;
                  END IF;
                  IF Hash_Record.Hash_Addr  > 128
                  THEN Hash_Record.Hash_Addr :=  Hash_Record.Hash_Addr  - 128;
                  END IF;

                  --Read until a space is located
                  Hash_File_IO.Read(StoreFile, TempHE, Hash_File_IO.Count(Hash_Record.Hash_Addr));
                  Exit when TempHE = Blank_Entry;

                  --Update offset if necessary
                  IF Probe = Linear THEN Offset := Offset + 1;
                  ELSE Offset := UniqueRandInteger;
                  END IF;

                  --Update probe count
                  Hash_Record.ProbeNum := Hash_Record.ProbeNum + 1;


               END LOOP;
              Hash_File_IO.Write(StoreFile, Hash_Record, Hash_File_IO.Count(Hash_Record.Hash_Addr));
            END;
         END LOOP;

         --Loop to print out contents in storage file for ease of viewing
         FOR I IN 1..Table_Size LOOP
            Hash_File_IO.Read(StoreFile, TempHE, Hash_File_IO.Count(I));

            IF TempHE /= Blank_Entry THEN
               Put(Integer'Image(I) & " : " & TempHE.Values & " ");

               IF Hashing_Type = Given THEN
               Put("Original Location:" & " " & Integer'Image(GetGivenKey(TempHE.Values)) & " ");
               ELSE
               Put("Original Location:" & " " & Integer'Image(GetMyKey(TempHE.Values)) & " ");
               end if;

               Put("Probes:" & " " & Integer'Image(TempHE.ProbeNum)); New_Line;
            ELSE Put_Line(Integer'Image(I) & " :  " & "NULL");
            END IF;

         --Get probe statistics then close both input and storage files
         end loop;
         Put_Line("Probe Statistics for 1st 30 keys"); new_line;
         Probe_Stats(Input, 1, 30, Table_Size, Probe, Hashing_Type, Location, StoreFile);
         Put_Line("Probe Statistics for last 30 keys"); new_line;
         Probe_Stats(Input, FillSize-29, FillSize, Table_Size, Probe, Hashing_Type, Location, StoreFile);
         Close(StoreFile);
      END;
      Close(Input);
   end Hash_In_File;

end Hashlab;
