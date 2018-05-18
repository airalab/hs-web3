pragma solidity ^0.4.15;

contract SimpleStorage {
    uint public count;
    
    event _CountSet(uint _count);
    
    function setCount(uint _count) {
        count = _count;
        _CountSet(_count);
    }
}
