#!/bin/bash
# Run all stack jobs at once.
# The executable will run too if '-e' option is provided.

output_file="runall.log"
job_description="Running build, test (with coverage), bench (with output) and haddock.\n"
printf "${job_description}Outputs will be written to: ${output_file}\n\n"

# run tests and create coverage report (check the log file for location of it
# and also to see the test results)
test_str="--test --coverage"

# run benchmarks and output report to bench.html file
benchmark_args="\"--output bench.html\""
bench_str="--bench --benchmark-arguments ${benchmark_args}"

# create documentation (check the log for location and coverage info)
haddock_str="--haddock"

# clean some files before running to avoid errors
rm kithara-bench.tix

run_str="stack build ${test_str} ${bench_str} ${haddock_str} &> ${output_file}"
printf "Running: ${run_str}\n\n"
eval $run_str
printf "Check results in: ${output_file}\n\n"

flag=false
while getopts 'e' to_exec
do
    case $to_exec in
        e) flag=true ;;
    esac
done

if $flag; then
    eval "stack exec kithara-exe"
fi
