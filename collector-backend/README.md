# Collector-backend

To run a collector-backend:
```console
$> cargo run
```

The collector backend supports a number of options. Use `--help` flag to access the full list.

### Adjusting validation
The collector backend tries to do some validation of the data received from the collectors. These checks can be set using either command line arguments or environment variables.
For a description of all the arguments run:

```console
$> cargo run -- --help
```

But some of the settings require a bit more explaination:

#### Limiting total data size
Setting `--valid-content-length` is a limit on the total byte size of the data received from a node, meaning it should also accommodate for everything in the data set. If we change something like the allowed node name length or the valid node peers count, this should probably be adjusted as well.

#### Comparing block heights against the average
Data where the best block height or finalized block height is too far from the current average will be rejected.

Adjusting what is considered 'too far' can be done for each check using `--valid-additional-best-block-height` and`--valid-additional-finalized-block-height`.

Comparing with averages only makes sense when the collector-backend have enough data points, the minimum number of required data points can be adjusted using `--validate-against-average-at`.

For the average to better withstand outliers, it is calculated from a percentage of the nodes, where the leftout nodes are the highest and lowest data values.
The percentage can be adjusted using `--percentage-used-for-averages` and must be an integer between 1 and 100.

Example: Say, we set the percentage to 60, with 20 nodes running, then new data would be compared to the average of 12 nodes, leaving out the nodes with the 3 lowest values and the 3 highest values.
