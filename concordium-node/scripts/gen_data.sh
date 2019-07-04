#!/usr/bin/env bash
for i in `seq 0 99`;
do
	echo $i
	dd if=/dev/urandom of=test-$i bs=1 count=1024 > /dev/null 2>&1
done
