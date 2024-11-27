with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body TH is

   procedure Free is
      new Ada.Unchecked_Deallocation (Object => entryNode, Name => entryNodePointer);


   procedure InitialiseHashTable (HashTable : in out hashMap; Length : in Integer) is
   begin
      HashTable.size := 0;
      HashTable.length := Length;
      -- Initializing each node to be null.
      for i in 0 .. (HashTable.length - 1) loop
         HashTable.entryNodeArray (i) := null;
      end loop;
   end InitialiseHashTable;


   procedure DestroyHashTable (HashTable : in out hashMap) is

   current, previous : entryNodePointer;

   begin
      -- Exploring the nodes.
      for i in 0..10 loop
         current := HashTable.entryNodeArray (i);
         if current /= null then
            -- If there are conflicts with the hash key.
            if current.next /= null then
               previous := current;
               current := current.next;
               while current.next /= null loop
                  Free (previous);
                  previous := current;
                  current := current.next;
               end loop;
               Free (previous);
               Free (current);
            end if;
         end if;
         HashTable.entryNodeArray (i) := null;
      end loop;
      HashTable.size := 0;
    end DestroyHashTable;


    function IsEmpty (HashTable : in hashMap) return Boolean is
    begin
        -- Size is the actual numbers of entry in the hash map.
        return HashTable.size = 0;
    end IsEmpty;


    function GetSize (HashTable : in hashMap) return Integer is
    begin
        return HashTable.size;
    end GetSize;


   procedure Register (HashTable : in out hashMap; Key : in Unbounded_String; Value : in Integer) is

   current, previous : entryNodePointer;
   hashedKey : CONSTANT Integer := To_String (Key)'Length mod HashTable.length;

   begin
      previous := null;
      current := HashTable.entryNodeArray (hashedKey);
      -- If there is no existing occurence with the said hash key.
      if current /= null then
         while current /= null loop
            if current.key = Key then
               current.value := Value;
               return;
            end if;
            previous := current;
            current := current.next;
         end loop;
      end if;
      declare
         newNode : CONSTANT entryNodePointer := new entryNode' (key => Key, value => Value, next => null);
      begin
         current := newNode;
         HashTable.size := HashTable.size + 1;
         if previous /= null then      
            previous.next := current;
            return;
         end if;
         HashTable.entryNodeArray (hashedKey) := current;
      end;
    end Register;


   procedure Delete (HashTable : in out hashMap; Key : in Unbounded_String) is

   current : entryNodePointer;
   hashedKey : CONSTANT Integer := To_String (Key)'Length mod HashTable.length;

   begin
      current := HashTable.entryNodeArray (hashedKey);
      if current = null then
         return;
      end if;
      if current.key = Key then
         if current.next = null then
            HashTable.entryNodeArray (hashedKey) := null;
         else
            HashTable.entryNodeArray (hashedKey) := current.next;
         end if;
         Free (current);
         return;
      end if;
         current := current.next;
         while current /= null loop
            if current.key = Key then
               if current.next = null then
                  HashTable.entryNodeArray (hashedKey) := null;
               else
                  HashTable.entryNodeArray (hashedKey) := current;
               end if;
               Free (current);
            end if;
            current := current.next;
         end loop;
      raise Cle_Absente_Exception;
    end Delete;


    function IsIn (HashTable : in hashMap; Key : in Unbounded_String) return Boolean is
    
    current : entryNodePointer;
    hashedKey : CONSTANT Integer := To_String (Key)'Length mod HashTable.length;
    
    begin
        current := HashTable.EntryNodeArray (hashedKey);
        if current.key = Key then
            return True;
        elsif current.next /= null then
            current := current.next;
            while current /= null loop
                if current.key = Key then
                    return True;
                end if;
                current := current.next;
            end loop;
        end if;
        return False;
    end IsIn;


    function ValueOf (HashTable : in hashMap; Key : in Unbounded_String) return Integer is

    current : entryNodePointer;
    hashedKey : CONSTANT Integer := To_String (Key)'Length mod HashTable.length;

    begin
        current := HashTable.entryNodeArray (hashedKey);
        if current.key = Key then
            return current.value;
        elsif current.next /= null then
            current := current.next;
            while current /= null loop
                if current.key = Key then
                    return current.value;
                end if;
                current := current.next;
            end loop;
        end if;
        raise Cle_Absente_Exception;
    end ValueOf;


    procedure Display (Key : in Unbounded_String; Value : in Integer) is
    begin
        Put("-->["); Put ('"'); Put (To_String (Key)); Put ('"'); Put (" : "); Put (Value, 1); Put("]");
    end Display;


    procedure DisplayHashTable (HashTable : in hashMap) is

    current : entryNodePointer;
        
    begin
        for i in 0 .. (HashTable.length - 1) loop
            current := HashTable.entryNodeArray (i);
            Put (i, 1); Put (" : ");
            if current /= null then
                Display (HashTable.entryNodeArray (i).key, HashTable.entryNodeArray (i).value);
                while current.next /= null loop
                    Display (HashTable.entryNodeArray (i).key, HashTable.entryNodeArray (i).value);
                    current := current.next;
                end loop;
            end if;
            Put_Line("--E");
        end loop;
        New_Line;
        Put ("Le nombre d'entree est de : "); Put (HashTable.size, 1); New_Line;
    end DisplayHashTable;

end TH;
