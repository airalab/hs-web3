pragma solidity ^0.4.22;

contract Linearization {

  event E1(address sender, uint amount);
  event E2(string tag, bytes4 uuid);
  event E3(address txOrigin, uint blockNumber);
  event E4(bytes4 sig, bytes32 blockHash);

  function e12() public {
    emit E1(msg.sender, 0);
    emit E2("hello", 0xdeadbeef);
  }

  function e21() public {
    emit E2("hello", 0xdeadbeef);
    emit E1(msg.sender, 0);
  }

  function e1() public {
    emit E1(msg.sender, 0);
  }

  function e2() public {
    emit E2("hello", 0xdeadbeef);
  }

  function e3() public {
    emit E3(tx.origin, block.number);
  }

  function e4() public {
    emit E4(msg.sig, blockhash(block.number));
  }
}