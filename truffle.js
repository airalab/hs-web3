module.exports = {
  networks: {
    development: {
      host: "localhost",
      port: 8545,
      network_id: "*" // Match any network id
    },
    rinkeby: {
        host: "geth-rinkeby-deploy.foam.svc.cluster.local",
        port: 8545,
        network_id: "*" // Match any network id
    },
    ephemeralnet: { // required for CI testing
        host: "EPH_POD_NAME.geth-pods.jenkins.svc.cluster.local",
        port: 8545,
        network_id: "*"
    }
  }
};
