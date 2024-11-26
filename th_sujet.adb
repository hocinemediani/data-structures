with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with TH;

procedure th_sujet is

   package hashTableSujet is
      new TH (nodeKey => Unbounded_String, nodeValue => Integer);

   use hashTableSujet;


   function "+" (Item : in String) return Unbounded_String
		renames To_Unbounded_String;

   HashTable : hashMap;

   Key1 : CONSTANT Unbounded_String := +"un";
   Key2 : CONSTANT Unbounded_String := +"cinq";
   Value1 : CONSTANT Integer := 42;
   Value2 : CONSTANT Integer := 51;

begin
   -- Initialize the hash table
   InitialiseHashTable (HashTable, 2);

   -- Register entries
   Register (HashTable, Key1, Value1);
   Register (HashTable, Key2, Value2);

   -- Display the hash table
   DisplayHashTable (HashTable);

   -- Retrieve a value
   declare
      Retrieved_Value : Integer;
   begin
      Retrieved_Value := ValueOf (HashTable, Key1);
      Put_Line ("Retrieved Value: " & Integer'Image(Retrieved_Value));
   end;

   -- Delete an entry
   Delete (HashTable, Key1);

   -- Display again after deletion
   DisplayHashTable (HashTable);

   -- Destroy the hash table
   DestroyHashTable (HashTable);

   Put_Line ("Test completed.");

end th_sujet;
