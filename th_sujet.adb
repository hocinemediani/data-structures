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
   arrayLength : CONSTANT Integer := 11;
   Keys : CONSTANT array (0 .. 6) of Unbounded_String
      := (+"un", +"deux", +"trois", +"quatre", +"cinq", +"vingt-et-un", +"quatre-vingt-dix-neuf");

begin

   InitialiseHashTable (HashTable, arrayLength);

   Register (HashTable, Keys (0), 1);
   Register (HashTable, +"ddddddddddd", 50);
   Register (HashTable, +"ddddddddezd", 78);
   Register (HashTable, +"dddddazdddd", 485);
   Register (HashTable, Keys (1), 2);
   Register (HashTable, Keys (2), 3);
   Register (HashTable, Keys (3), 4);
   Register (HashTable, Keys (4), 5);
   Register (HashTable, Keys (5), 21);
   Register (HashTable, Keys (6), 99);

   DisplayHashTable (HashTable);

   -- Problèmes à regler :
      -- Les clefs ne sont pas bien enregistrés (pour une hashedKey donnée, toutes les entrées sont égales).
      -- Reprendre la fonction DestroyHashMap et vérifier avec valgrind la présence ou non de memory leaks.
      -- Reprendre les autres fonction en supprimant les redondances et en profitant de la structure des hashTables pour optimiser le code.

   Delete (HashTable, +"cinq");
   Delete (HashTable, +"vingt-et-un");
   Delete (HashTable, +"trois");

   DisplayHashTable (HashTable);

   DestroyHashTable (HashTable);

   DisplayHashTable (HashTable);

   Put_Line ("Test completed.");

end th_sujet;