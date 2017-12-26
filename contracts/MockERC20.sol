pragma solidity ^0.4.13;

contract MockERC20 {

    event Transfer(address indexed to, address indexed from, uint amount);

    function transfer(address to, uint amount) {
        Transfer(to, msg.sender, amount);
    }
}
