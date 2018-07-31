pragma solidity ^0.4.22;

contract Registry {


    event A(bytes32 indexed listingHash);
    event B(address indexed sender, bytes32 listingHash);
    event C(address dog, bytes32 cat);

}
