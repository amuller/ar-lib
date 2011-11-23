#!/bin/sh

rm pom.xml

lein jar, pom

FILE=`ls *.jar`
echo "$FILE"

mvn install:install-file -DpomFile=pom.xml -Dfile=$FILE
