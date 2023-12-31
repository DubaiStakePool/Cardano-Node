# Configure topology files for block-producing and relay nodes.

Before you start your nodes, you need to prepare the topology files.

#### Configure the block-producing node

Make the __block-producing__ node to "talk" only to __YOUR__ relay node. Do not forget to configure your firewall also:

    nano topology.json

    {
      "Producers": [
        {
          "addr": "<RELAY IP ADDRESS>",
          "port": <PORT>,
          "valency": 1
        }
      ]
    }

#### Configure the relay node:

Make your __relay node__ `talk` to your __block-producing__ node and __other relays__ in the network by editing the `topology.json` file:


    nano topology.json

    {
      "Producers": [
        {
          "addr": "<BLOCK-PRODUCING IP ADDRESS>",
          "port": <PORT>,
          "valency": 1
        },
        {
          "addr": "<IP ADDRESS>",
          "port": <PORT>,
          "valency": 1
        },
        {
          "addr": "<IP ADDRESS>",
          "port": <PORT>,
          "valency": 1
        }
      ]
    }

**Note**: If you want to connect to IPv4 and IPv6 relays, you must either not specify host addresses when starting `cardano-node` or make sure to specify
both an IPv4 and IPv6 host address.

Please see [Understanding configuration files](../getting-started/understanding-config-files.md) to learn about P2P topology.
