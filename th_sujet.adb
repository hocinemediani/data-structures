with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with TH;

procedure TH_Sujet is

   arrayLength : CONSTANT Integer := 11;

   package hashTableSujet is
      new TH (
         nodeKey => Unbounded_String, 
         nodeValue => Integer,
         lengthArray => arrayLength
         );

   use hashTableSujet;


   function "+" (Item : in String) return Unbounded_String
		renames To_Unbounded_String;


   HashTable : hashMap;
   Keys : CONSTANT array (0 .. 6) of Unbounded_String
      := (+"un", +"deux", +"trois", +"quatre", +"cinq", +"vingt-et-un", +"quatre-vingt-dix-neuf");

begin

   InitialiseHashTable (HashTable, arrayLength);

   Register (HashTable, Keys (0), 1);
   Register (HashTable, Keys (1), 2);
   Register (HashTable, Keys (2), 3);
   Register (HashTable, Keys (3), 4);
   Register (HashTable, Keys (4), 5);
   Register (HashTable, Keys (5), 21);
   Register (HashTable, Keys (6), 99);

   New_Line;

   Put_Line ("Displaying the fully initiated hash table :");
   DisplayHashTable (HashTable);

   Delete (HashTable, +"deux");
   Delete (HashTable, +"vingt-et-un");
   Delete (HashTable, +"trois");

   Put_Line ("Displaying the hash table after deleting 'deux', 'vingt-et-un', and 'trois' :");
   DisplayHashTable (HashTable);

   DestroyHashTable (HashTable);

   Put_Line ("Displaying the hash table after it has been destructed :");
   DisplayHashTable (HashTable);

   Put_Line ("Test completed.");

end TH_Sujet;