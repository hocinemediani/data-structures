with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with TH;

procedure th_sujet is

   package hashTableSujet is
      new TH (
         nodeKey => Unbounded_String, 
         nodeValue => Integer
         );

   use hashTableSujet;


   function "+" (Item : in String) return Unbounded_String
		renames To_Unbounded_String;

   HashTable : hashMap;

   Key1 : CONSTANT Unbounded_String := +"un";
   Key2 : CONSTANT Unbounded_String := +"cinq";
   Key3 : CONSTANT Unbounded_String := +"de";
   Key4 : CONSTANT Unbounded_String := +"bu";
   Key5 : CONSTANT Unbounded_String := +"ap";
   Key6 : CONSTANT Unbounded_String := +"apadadadadadadada";
   Value1 : CONSTANT Integer := 42;
   Value2 : CONSTANT Integer := 51;
   Value3 : CONSTANT Integer := 51;
   Value4 : CONSTANT Integer := 888;
   Value5 : CONSTANT Integer := 45699;
   Value6 : CONSTANT Integer := 1111;

begin

   InitialiseHashTable (HashTable, 11);

   Register (HashTable, Key1, Value1);
   Register (HashTable, Key2, Value2);
   Register (HashTable, Key3, Value3);
   Register (HashTable, Key4, Value4);
   Register (HashTable, Key5, Value5);
   Register (HashTable, Key6, Value6);

   DisplayHashTable (HashTable);

   Delete (HashTable, Key1);

   DisplayHashTable (HashTable);

   Delete (HashTable, Key4);

   DisplayHashTable(HashTable);
   
   DestroyHashTable (HashTable);

   Put_Line ("Test completed.");

end th_sujet;
