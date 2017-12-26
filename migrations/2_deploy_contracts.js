var SimpleStorage = artifacts.require("./SimpleStorage.sol");
var ComplexStorage = artifacts.require("./ComplexStorage.sol");
var MockERC20 = artifacts.require("./MockERC20.sol");

module.exports = function(deployer) {
  deployer.deploy(SimpleStorage);
  deployer.deploy(ComplexStorage);
  deployer.deploy(MockERC20);
};
