pragma solidity ^0.4.2;

contract TestContract {
    event Action1(address indexed client, uint indexed s);
    event Action2(string str, uint indexed i);
    
    function runA1() {
        Action1(msg.sender, 10);
    }
    function runA2(string _s, uint _x) {
        Action2(_s, _x);
    }
}
