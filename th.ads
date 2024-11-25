package TH is

    type entryNode is private;
    type hashMap is private;
    type nodeArray is private;


    -- Initialize an empty hash map.
    procedure InitialiseHashMap (HashMap : out hashMap; Length : in Integer; NodeArray : out nodeArray) with
        Post => IsEmpty (HashMap);


    -- Destroy the hash map.
    procedure DestroyHashMap (HashMap : in out hashMap; NodeArray : in nodeArray);


    -- Check if a hash map is empty.
    function IsEmpty (HashMap : in hashMap) return Boolean;


    -- Get the number of elements in a hash map.
    function GetSize (HashMap : in hashMap) return Integer with
        Post => GetSize'Result >= 0
            and (Taille'Result = 0) = IsEmpty (HashMap);


    -- Registers a new value associated to a key or update it.
    procedure Register (HashMap : in out HashMap; NodeArray : in out nodeArray; Key : in String; Value : in Integer) with
        Post => IsIn(HashMap, Key) and (ValueOf (HashMap, Key) = Key)
            and (not (IsIn (HashMap, Key)'Old) or GetSize (HashMap) = GetSize (HashMap)'Old)
            and (IsIn (HashMap, Key)'Old or GetSize (HashMap) = GetSize (HashMap)'Old + 1);


    -- Deletes a node in the hash map with the exception Cle_Absente_Exception.
    procedure Delete (HashMap : in out hashMap; NodeArray : in out nodeArray; Key : in String) with
        Post => GetSize (HashMap) = GetSize (HashMap)'Old - 1
            and not IsIn (HashMap, Key);


    -- Check if a key is in the hash map.
    function IsIn (Hashmap : in hashMap; NodeArray : in nodeArray; Key : in String) return Boolean;


    -- Get the value associated to a key with the exception Cle_Absente_Exception.
    function ValueOf (HashMap : in hashMap; NodeArray : in nodeArray; Key : in String) return Integer;


    -- Display the hash map.
    procedure Display (HashMap : in hashMap; NodeArray : in nodeArray);


private

    type entryNodePointer is access entryNode;

    type entryNode is record
        key : String;
        value : Integer;
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
        array(arrayLength) of entryNodePointer;

end TH;