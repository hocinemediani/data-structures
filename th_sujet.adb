with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with TH;


procedure th_sujet is
   
   arrayLength : Integer := 11;
   
   package hashTableSujet is
      new TH(nodeKey => Unbounded_String, nodeValue => Integer, arrayLength => Integer);

   use hashTableSujet;

   HashTable : hashMap;
   entryNodeArray : nodeArray;
   
   begin
    Put("ok");
    
end th_sujet;
