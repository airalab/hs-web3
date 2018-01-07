pragma solidity ^0.4.13;

contract ComplexStorage {
    uint public uintVal;
    int public intVal;
    bool public boolVal;
    int224 public int224Val;
    bool[2] public boolVectorVal;
    int[] public intListVal;
    string public stringVal;
    bytes16 public bytes16Val;
    bytes2[4][] public bytes2VectorListVal;

    event ValsSet(uint a, int b, bool c, int224 d, bool[2] e, int[] f, string g, bytes16 h, bytes2[4][] i);
    
    function setValues(uint _uintVal, int _intVal, bool _boolVal, int224 _int224Val, bool[2] _boolVectorVal, int[] _intListVal, string _stringVal, bytes16 _bytes16Val, bytes2[4][] _bytes2VectorListVal) public {
         uintVal =           _uintVal;
         intVal =            _intVal;
         boolVal =           _boolVal;
         int224Val =         _int224Val;
         boolVectorVal =     _boolVectorVal;
         intListVal =        _intListVal;
         stringVal   =       _stringVal;
         bytes16Val   =      _bytes16Val;
         bytes2VectorListVal = _bytes2VectorListVal;
         
         ValsSet(_uintVal, _intVal, _boolVal, _int224Val, _boolVectorVal, _intListVal, _stringVal, _bytes16Val, _bytes2VectorListVal);
    }
    
}
