# database-exporter

This tool exports the node database as a collection of files that contain blocks which can be
imported using the node's out-of-band catchup feature. The exported block file collection is
accompanied by an index file listing block files and the range of blocks contained in each block
file. The index file is consumed by the use of the node's `--download-blocks-from` option.

## Available commands

`database-exporter check --exportpath FILENAME` determines if FILENAME is a well-formed block file.

`database-exporter export --dbpath DBPATH --exportpath EXPORTPATH --chunksize NUM` exports the node
	database located at DBPATH to a directory located at EXPORTPATH. Each block file created in that
	directory has no more than NUM blocks in it.

## Migration from earlier versions

A version of `database-exporter` shipped with `concordium-node` <=4.3.0 did not implement the
`--chunksize` option in its `export` command. It exported the node database as a single file. If you
would like still to export the node database as a single file, run `database-exporter` with a very
large `--chunksize` argument, for example, 1000000000000.

After upgrading to `database-exporter` shipped with `concordium-node` >4.3.0, use the `--chunksize`
option to every invocation of `database-exporter export`, as it is non-optional. A good first choice
of the `--chunksize` argument is 10000, which exports block files with sizes on the order of tens of
MiB.

