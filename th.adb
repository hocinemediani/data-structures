with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body TH is

    procedure Free is
        new Ada.Unchecked_Deallocation (Object => entryNode, Name => entryNodePointer);


    procedure InitialiseHashMap (HashMap : out hashMap; Length : in Integer; NodeArray : out nodeArray) is

    -- Type used to initialize "nodeArrays" in th.ads.
    type arrayLength is range 1..Length;
    current : entryNodePointer;

    begin
        HashMap.size := 0;
        HashMap.length := Length;
        -- Initializing each node to be null.
        for i in 1..Length loop
            declare
                newNode : CONSTANT entryNodePointer := new entryNode' (key => null, value => null, next => null, hashedKey => null);
            begin
                current := newNode;
                -- Keeping track of pointors in an array.
                NodeArray (i) := current;
            end;
        end loop;
    end InitialiseHashMap;


    procedure DestroyHashMap (HashMap : in out hashMap; NodeArray : in nodeArray) is

    current, previous : entryNodePointer;

    begin
        -- Exploring the nodes.
        for i in 1..HashMap.length loop
            current := NodeArray (i);
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
        HashMap.size := 0;
    end DestroyHashMap;


    function IsEmpty (HashMap : in hashMap) return Boolean is
    begin
        -- Size is the actual numbers of entry in the hash map.
        return HashMap.size = 0;
    end IsEmpty;


    function GetSize (HashMap : in hashMap) return Integer is
    begin
        return HashMap.size;
    end GetSize;


    procedure Register (HashMap : in out HashMap; NodeArray : in out nodeArray; Key : in String; Value : in Integer) is

    current : entryNodePointer;
    hashedKey : Integer := Key'Length mod HashMap.length;

    begin
        current := NodeArray (hashedKey);
        -- If there is no existing occurence with the said hash key.
        if NodeArray (HashedKey).value = null then
            current.key := Key;
            current.value := Value;
            HashMap.size := HashMap.size + 1;
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
                HashMap.size := HashMap.size + 1;
            end;
        end if;
    end Register;


    procedure Delete (HashMap : in out hashMap; NodeArray : in out nodeArray; Key : in String) is

    current : entryNodePointer;
    hashedKey : Integer := Key'Length mod HashMap.length;

    begin
        current := NodeArray (hashedKey);
        if current.key = Key then
            if current.next = null then
                NodeArray (hashedKey) := null;
            else
                NodeArray (hashedKey) := current;
            end if;
            Free (current);
        else
            current := current.next;
            while current /= null loop
                if current.key = Key then
                    if current.next = null then
                        NodeArray (hashedKey) := null;
                    else
                        NodeArray (hashedKey) := current;
                    end if;
                    Free (current);
                end if;
            end loop;
        end if;
        raise Cle_Absente_Exception;
    end Delete;


    function IsIn (HashMap : in hashMap; NodeArray : in NodeArray; Key : in String) return Boolean is
    
    current : entryNodePointer;
    hashedKey : Integer := Key'Length mod HashMap.length;
    
    begin
        current := NodeArray (hashedKey);
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


    function ValueOf (HashMap : in hashMap; NodeArray : in nodeArray; Key : in String) return Integer is

    current : entryNodePointer;
    hashedKey : Integer := Key'Length mod HashMap.length;

    begin
        current := NodeArray (hashedKey);
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


    procedure Display (HashMap : in hashMap; NodeArray : in nodeArray) is

    current : entryNodePointer;
        
    begin
        for i in 1..HashMap.length loop
            current := NodeArray (i);
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