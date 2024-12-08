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


   function Avec_Guillemets (S: Unbounded_String) return String is
	begin
		return '"' & To_String (S) & '"';
	end;


   HashTable : hashMap;
   Keys : CONSTANT array (0 .. 6) of Unbounded_String
      := (+"un", +"deux", +"trois", +"quatre", +"cinq", +"vingt-et-un", +"quatre-vingt-dix-neuf");


   procedure DisplayKey (Key : in Unbounded_String) is
   begin
      Put (Avec_Guillemets (Key));
      Put (" : ");
   end DisplayKey;


   procedure DisplayValue (Value : in Integer) is
   begin
      Put (Value'Image);
   end DisplayValue;


   procedure DisplayHashTable2 is
      new DisplayHashTable (DisplayKey, DisplayValue);


   function HashKey (HashTable : in hashMap; Key : in Unbounded_String) return Integer is
   begin
      return (length (Key)) mod (GetSize(HashTable));
   end HashKey;

begin

   InitialiseHashTable (HashTable, arrayLength);

   Register (HashTable, Keys (0), HashKey (HashTable, Keys (0)), 1);
   Register (HashTable, Keys (1), HashKey (HashTable, Keys (1)), 2);
   Register (HashTable, Keys (2), HashKey (HashTable, Keys (2)), 3);
   Register (HashTable, Keys (3), HashKey (HashTable, Keys (3)), 4);
   Register (HashTable, Keys (4), HashKey (HashTable, Keys (4)), 5);
   Register (HashTable, Keys (5), HashKey (HashTable, Keys (5)), 21);
   Register (HashTable, Keys (6), HashKey (HashTable, Keys (6)), 99);

   New_Line;

   Put_Line ("Displaying the fully initiated hash table :");
   DisplayHashTable2 (HashTable);

   Delete (HashTable, +"deux", HashKey (HashTable, +"deux"));
   Delete (HashTable, +"vingt-et-un", HashKey (HashTable, +"vingt-et-un"));
   Delete (HashTable, +"trois", HashKey (HashTable, +"trois"));

   Put_Line ("Displaying the hash table after deleting 'deux', 'vingt-et-un', and 'trois' :");
   DisplayHashTable2 (HashTable);

   DestroyHashTable (HashTable);

   Put_Line ("Displaying the hash table after it has been destructed :");
   DisplayHashTable2 (HashTable);

   Put_Line ("Test completed.");

end TH_Sujet;