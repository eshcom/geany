#!/bin/sh

#~ invalid
v11=$(find -name
		test.sh)

#~ invalid
v12=$(find -name \ 
		test.sh)

#~ valid
v13=$(find -name \
		test.sh)

echo success: $v13

#~ invalid
v21=`find -name 
		test.sh`

#~ invalid
v22=`find -name \ 
		test.sh`

#~ valid
v23=`find -name \
		test.sh`

echo success: $v23
