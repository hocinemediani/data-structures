with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body TH is

    procedure Free is
        new Ada.Unchecked_Deallocation (Object => entryNode, Name => entryNodePointer);


    procedure InitialiseHashTable (HashTable : out hashMap; Length : in Integer; EntryNodeArray : out nodeArray) is

    -- Type used to initialize "nodeArrays" in th.ads.
    type arrayLength is range 1..Length;
    current : entryNodePointer;

    begin
        HashTable.size := 0;
        HashTable.length := Length;
        -- Initializing each node to be null.
        for i in 1..Length loop
            declare
                newNode : CONSTANT entryNodePointer := new entryNode' (key => null, value => null, next => null, hashedKey => null);
            begin
                current := newNode;
                -- Keeping track of pointors in an array.
                EntryNodeArray (i) := current;
            end;
        end loop;
    end InitialiseHashTable;


    procedure DestroyHashTable (HashTable : in out hashMap; EntryNodeArray : in nodeArray) is

    current, previous : entryNodePointer;

    begin
        -- Exploring the nodes.
        for i in 1..HashTable.length loop
            current := EntryNodeArray (i);
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


    function GetSize (HashTable : in hashMap) return Integer is
    begin
        return HashTable.size;
    end GetSize;


    procedure Register (HashTable : in out HashTable; EntryNodeArray : in out nodeArray; Key : in String; Value : in Integer) is

    current : entryNodePointer;
    hashedKey : Integer := Key'Length mod HashTable.length;

    begin
        current := EntryNodeArray (hashedKey);
        -- If there is no existing occurence with the said hash key.
        if EntryNodeArray (HashedKey).value = null then
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


    procedure Delete (HashTable : in out hashMap; EntryNodeArray : in out nodeArray; Key : in String) is

    current : entryNodePointer;
    hashedKey : Integer := Key'Length mod HashTable.length;

    begin
        current := EntryNodeArray (hashedKey);
        if current.key = Key then
            if current.next = null then
                EntryNodeArray (hashedKey) := null;
            else
                EntryNodeArray (hashedKey) := current;
            end if;
            Free (current);
        else
            current := current.next;
            while current /= null loop
                if current.key = Key then
                    if current.next = null then
                        EntryNodeArray (hashedKey) := null;
                    else
                        EntryNodeArray (hashedKey) := current;
                    end if;
                    Free (current);
                end if;
            end loop;
        end if;
        raise Cle_Absente_Exception;
    end Delete;


    function IsIn (HashTable : in hashMap; EntryNodeArray : in EntryNodeArray; Key : in String) return Boolean is
    
    current : entryNodePointer;
    hashedKey : Integer := Key'Length mod HashTable.length;
    
    begin
        current := EntryNodeArray (hashedKey);
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


    function ValueOf (HashTable : in hashMap; EntryNodeArray : in nodeArray; Key : in String) return Integer is

    current : entryNodePointer;
    hashedKey : Integer := Key'Length mod HashTable.length;

    begin
        current := EntryNodeArray (hashedKey);
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


    procedure Display (HashTable : in hashMap; EntryNodeArray : in nodeArray) is

    current : entryNodePointer;
        
    begin
        for i in 1..HashTable.length loop
            current := EntryNodeArray (i);
            Put(i);
            if current.next = null then
                Put("-->[" + '"' + current.key + '"' + " : " + current.value + "]");
            elsif current.next /= null then
                while current /= null loop
                    Put("-->[" + '"' + current.key + '"' + " : " + current.value + "]");
                end loop;
            end if;
            Put("--E");
        end loop;
    end Display;

end TH;