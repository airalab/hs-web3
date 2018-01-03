var SimpleStorage = artifacts.require("./SimpleStorage.sol");
var ComplexStorage = artifacts.require("./ComplexStorage.sol");

module.exports = function(deployer) {
  deployer.deploy(SimpleStorage);
  deployer.deploy(ComplexStorage);
};
