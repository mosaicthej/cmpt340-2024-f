#!/bin/bash

# Run the sbt test command in the background and pipe the output
sbt test -mem 65536 | sed -u \
    -e 's/[0-9]\{2\}:[0-9]\{2\}:[0-9]\{2\}\.[0-9]\{3\} \[\TestSystem-akka\.actor\.default-dispatcher-[0-9]*\] //g' \
    -e 's/fibActor\.FibActor - //g' \
    -e 's/akka:\/\/TestSystem\/user\///g' \
    -e 's/\(\$[a-z]*\)/\o033[34m\1\o033[0m/g' \
    -e 's/\(Req(\([0-9]*\))\)/\o033[33m\1\o033[0m/g' \
    -e 's/\(Res(\([0-9]*\))\)/\o033[32m\1\o033[0m/g' \
    -e 's/\(stackDepth [0-9]*\)/\o033[35m\1\o033[0m/g' \
    > ./testout.txt &
SBT_PID=$!

sleep 5

JAVA_PID=$(pgrep -P $SBT_PID java)
if [ ! -z "$JAVA_PID" ]; then
    echo "Renicing Java process ID $JAVA_PID"
    renice -n 19 -p $JAVA_PID
else
    echo "Java process not found under sbt PID $SBT_PID"
    echo "Renicing all Java processes..."
    renice -n 19 -p $(pgrep java)
fi

wait $SBT_PID

# Extract lines with "expected" to a separate file
grep "expected" ./testout.txt > ./testres.txt

