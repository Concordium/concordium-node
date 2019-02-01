# Requirements to run #

## Changes to "working directory" ##

Due to the way the oak interpreter works at the moment the workdir (with its
contents) directory needs to be placed at the root directory of where the baker
*process* is run. Otherwise the baker will not work. Note that for each contract
call the "current working directory" of the whole *process* will be temporarily
set to $ROOT/workdir. So if any of the rest of the rust client relies on the
"current directory" not changing then we can probably change this behaviour, but
then the interpreter will pollute the $ROOT directory with temporary files.

## Set ELM_HOME ##

The interpreter will create a global cache in $ELM_HOME .
If ELM_HOME is not set then, at least on linux, it will default to $HOME/.elm .
