#!/bin/bash
ERROR=()
COMPILES=()
RUNS=()
for f in ./testprograms_public_typer/lab5/valid/*.slac
do
	echo "--------------------------"
	echo "COMPILING $f"
	sbt "run $f -d test_dir/$(basename $f)"
	if [ $? == 1 ]
		then
			ERROR+=($f)
	else
			echo "RUNNING test_dir/$(basename $f)"
			cd "test_dir/$(basename $f)";
			java Main
			if [ $? == 0 ]
				then
					RUNS+=($f)
				else
					COMPILES+=($f)
			fi
			cd ../..
	fi
done

echo "--------------------------------"
echo "Files that crashed the compiler: "
printf '%s\n' "${ERROR[@]}"
echo "Files that compile (but doesn't run): "
printf '%s\n' "${COMPILES[@]}"
echo "Files that run: "
printf '%s\n' "${RUNS[@]}"
