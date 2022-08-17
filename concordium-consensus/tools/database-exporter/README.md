# database-exporter

This tool exports the node database as a collection of files that contain blocks which can be
imported using the node's out-of-band catchup feature. The exported block file collection is
accompanied by an index file listing block files and the range of blocks contained in each block
file.

The index file is consumed by the use of the node's `--download-blocks-from`
option. `concordium-node` will retrieve all block files listed in the index file and pass the blocks
to consensus, which will import each serialized block in sequence. If any block fails to be
imported, the state will remain as-is and the node will have to catch-up using P2P after starting.

## Available commands

`stack run database-exporter -- check --exportpath FILENAME` determines if FILENAME is a well-formed
	block file. This will try to read the exported file showing the hash and block slot for each
	block read from the file. Note that `database-exporter` will not check if the blocks are
	properly chained in sequence, neither will it verify any other chain integrity rule.


`database-exporter export --dbpath DBPATH --exportpath EXPORTPATH --chunksize NUM` exports the node
	database located at DBPATH to a directory located at EXPORTPATH. Each block file created in that
	directory has no more than NUM blocks in it. Each serialized block is prepended by its length in
	bytes. Genesis blocks will not be exported. The last exported block file will also contain
	finalization records finalizing all blocks after the newest block which contained a finalization
	record.

## Migration from earlier versions

A version of `database-exporter` shipped with `concordium-node` <=4.3.0 did not implement the
`--chunksize` option in its `export` command. It exported the node database as a single file. If you
would like still to export the node database as a single file, run `database-exporter` with a very
large `--chunksize` argument, for example, 1000000000000.

After upgrading to `database-exporter` shipped with `concordium-node` >4.3.0, use the `--chunksize`
option to every invocation of `database-exporter export`, as it is required. A good first choice of
the `--chunksize` argument is 10000, which is close to the number of blocks produced on the mainnet
daily. At the current mainnet maximum block size of 3 MiB, the resulting block files will not exceed
30 GiB in size.

