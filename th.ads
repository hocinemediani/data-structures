generic
    type nodeKey is private;
    type nodeValue is private;
    type arrayLength is private;

package TH is

    type entryNode is private;
    type hashMap is private;
    type nodeArray is private;


    -- Initialize an empty hash map.
    procedure InitialiseHashTable (HashTable : out hashMap; Length : in Integer; EntryNodeArray : out nodeArray) with
        Post => IsEmpty (HashTable);


    -- Destroy the hash map.
    procedure DestroyHashTable (HashTable : in out hashMap; EntryNodeArray : in nodeArray);


    -- Check if a hash map is empty.
    function IsEmpty (HashTable : in hashMap) return Boolean;


    -- Get the number of elements in a hash map.
    function GetSize (HashTable : in hashMap) return Integer with
        Post => GetSize'Result >= 0
            and (GetSize'Result = 0) = IsEmpty (HashTable);


    -- Registers a new value associated to a key or update it.
    procedure Register (HashTable : in out hashMap; EntryNodeArray : in out nodeArray; Key : in String; Value : in Integer) with
        Post => IsIn(HashTable, EntryNodeArray, Key) and (ValueOf (HashTable, EntryNodeArray, Key) = Value)
            and (not (IsIn (HashTable, EntryNodeArray, Key)'Old) or GetSize (HashTable) = GetSize (HashTable)'Old)
            and (IsIn (HashTable, EntryNodeArray, Key)'Old or GetSize (HashTable) = GetSize (HashTable)'Old + 1);


    -- Deletes a node in the hash map with the exception Cle_Absente_Exception.
    procedure Delete (HashTable : in out hashMap; EntryNodeArray : in out nodeArray; Key : in String) with
        Post => GetSize (HashTable) = GetSize (HashTable)'Old - 1
            and not IsIn (HashTable, EntryNodeArray, Key);


    -- Check if a key is in the hash map.
    function IsIn (HashTable : in hashMap; EntryNodeArray : in nodeArray; Key : in String) return Boolean;


    -- Get the value associated to a key with the exception Cle_Absente_Exception.
    function ValueOf (HashTable : in hashMap; EntryNodeArray : in nodeArray; Key : in String) return Integer;


    -- Display the hash map.
    procedure Display (HashTable : in hashMap; EntryNodeArray : in nodeArray);


private

    type entryNodePointer is access entryNode;

    type entryNode is record
        key : nodeKey;
        value : nodeValue;
        -- Only used if two or more nodes have the same hashed key.
        next : entryNodePointer;
    end record;

    type hashMap is record
        -- The actual used size of the hash map.
        size : Integer;
        -- The total size of the hash map (used for hashing too).
        length : Integer;
    end record;

    type nodeArray is
        array(1 .. arrayLength) of entryNodePointer;

end TH;
