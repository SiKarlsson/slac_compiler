#!/bin/bash
ERROR=()
for f in ./testprograms_public_typer/lab5/valid/*.slac
do
	echo "--------------------------"
	echo "COMPILING $f"
	sbt "run $f --symid"
	if [ $? == 1 ] 
		then
		ERROR+=($f)
	fi
done

echo "--------------------------------"
echo "Files that crashed the compiler: "
printf '%s\n' "${ERROR[@]}"
