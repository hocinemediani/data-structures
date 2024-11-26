with Ada.Text_IO;            use Ada.Text_IO;
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
        for i in 1..HashTable.entryNodeArray'Length loop
                HashTable.entryNodeArray (i) := null;
        end loop;
    end InitialiseHashTable;


    procedure DestroyHashTable (HashTable : in out hashMap) is

    current, previous : entryNodePointer;

    begin
        -- Exploring the nodes.
        for i in 1..HashTable.entryNodeArray'Length loop
            current := HashTable.entryNodeArray (i);
            -- If there isn't conflict with the hash key.
            if current.next = null then
                Free (current);
            -- If there is conflict with the hash key.
            elsif current.next /= null then
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
        end loop;
        HashTable.size := 0;
    end DestroyHashTable;


    function IsEmpty (HashTable : in hashMap) return Boolean is
    begin
        -- Size is the actual numbers of entry in the hash map.
        return HashTable.size = 0;
    end IsEmpty;


    function GetSize (HashTable : in hashMap) return nodeValue is
    begin
        return HashTable.size;
    end GetSize;


    procedure Register (HashTable : in out hashMap; Key : in nodeKey; Value : in nodeValue) is

    current : entryNodePointer;
    hashedKey : nodeValue := Key'Length mod HashTable.length;

    begin
        current := HashTable.entryNodeArray (hashedKey);
        -- If there is no existing occurence with the said hash key.
        if HashTable.entryNodeArray (HashedKey).value = null then
            current.key := Key;
            current.value := Value;
            HashTable.size := HashTable.size + 1;
        else
            -- If the key is already mapped, we update the value.
            if current.key = Key then
                current.value := Value;
                return;
            end if;
            -- Exploring the different nodes until we find the last one.
            while current.next /= null loop
                current := current.next;
            end loop;
            declare
                -- Creating the new node.
                newNode : CONSTANT entryNodePointer	:= new entryNode' (key => Key, value => Value, next => null);
            begin
                current.next := newNode;
                HashTable.size := HashTable.size + 1;
            end;
        end if;
    end Register;


    procedure Delete (HashTable : in out hashMap; Key : in nodeKey) is

    current : entryNodePointer;
    hashedKey : nodeValue := Key'Length mod HashTable.length;

    begin
        current := HashTable.entryNodeArray (hashedKey);
        if current.key = Key then
            if current.next = null then
                HashTable.entryNodeArray (hashedKey) := null;
            else
                HashTable.entryNodeArray (hashedKey) := current;
            end if;
            Free (current);
        else
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
            end loop;
        end if;
        raise Cle_Absente_Exception;
    end Delete;


    function IsIn (HashTable : in hashMap; Key : in nodeKey) return Boolean is
    
    current : entryNodePointer;
    hashedKey : nodeValue := Key'Length mod HashTable.length;
    
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


    function ValueOf (HashTable : in hashMap; Key : in nodeKey) return nodeValue is

    current : entryNodePointer;
    hashedKey : nodeValue := Key'Length mod HashTable.length;

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


    procedure Display (Key : in nodeKey; Value : in nodeValue) is
    begin
        Put ("-->["); Put ('"''Image); Put (Key); Put ('"''Image); Put (" : "); Put (Value'Image); Put("]");
    end Display;


    procedure DisplayHashTable (HashTable : in hashMap) is

    current : entryNodePointer;
        
    begin
        for i in 1..HashTable.entryNodeArray'Length loop
            current := HashTable.entryNodeArray (i);
            Put (i, 1); Put (" : ");
            if current /= null then
                Display (current.key, current.value);
                while current.next /= null loop
                    Display (current.key, current.value);
                    current := current.next;
                end loop;
            end if;
            Put ("--E");
        end loop;
    end DisplayHashTable;

end TH;
