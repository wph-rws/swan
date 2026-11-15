#!/bin/bash
# Time the fork and upstream, each built with Intel ifx and with gfortran,
# on Voordelta: one thread (serial builds) and four threads (OpenMP builds).
# Run it from the root of the repository, after build.sh:
#
#    bash benchmarks/intel/time.sh
#
# The gfortran builds are the usual ones: build/ and
# build-modernization-openmp/ for the fork, .upstream/build_O3_gfortran16.2.0
# and .upstream/build_O3_gfortran16_omp for upstream. The builds run in turn,
# so that a busy spell of the machine hits all of them. Under WSL the CPU
# numbers do not say which kind of core a run gets, so only the comparison
# between builds counts.
#
# Settings through the environment: REPS (5), B (the directory of the ifx
# builds), OUT (where the runs and times.csv go).
set -u
ROOT=$(pwd)
REPS=${REPS:-5}
B=${B:-$ROOT/build-intel}
OUT=${OUT:-$B/timing}
EX=$ROOT/examples/voordelta
CSV=$OUT/times.csv
mkdir -p "$OUT"
echo "kind,build,rep,seconds,exit,sha_hs" > "$CSV"
echo "load before: $(cut -d' ' -f1-3 /proc/loadavg)"

run() {  # kind, label, program, threads, CPUs, repetition
   local d=$OUT/$1_$2
   rm -rf "$d"; mkdir -p "$d"
   cp "$EX/voordelta.swn" "$d/INPUT"; cp "$EX/voordelta.dep" "$d/"
   local t0 t1 rc
   t0=$(date +%s.%N)
   ( cd "$d" && OMP_NUM_THREADS=$4 OMP_PLACES=cores OMP_PROC_BIND=close taskset -c "$5" "$3" > stdout.log 2>&1 )
   rc=$?
   t1=$(date +%s.%N)
   echo "$1,$2,$6,$(echo "$t1 - $t0" | bc),$rc,$(sha256sum "$d/voordelta_hs.blk" | cut -c1-12)" >> "$CSV"
}
for r in $(seq "$REPS"); do
   run serial fork_gfortran  "$ROOT/build/bin/swan.exe" 1 10 "$r"
   run serial fork_ifx       "$B/fork_serial/bin/swan.exe" 1 10 "$r"
   run serial up_O3_ifx      "$B/up_O3_serial/bin/swan.exe" 1 10 "$r"
   run serial up_default_ifx "$B/up_default_serial/bin/swan.exe" 1 10 "$r"
   run serial up_O3_gfortran "$ROOT/.upstream/build_O3_gfortran16.2.0/bin/swan.exe" 1 10 "$r"
done
for r in $(seq "$REPS"); do
   run omp4 fork_gfortran  "$ROOT/build-modernization-openmp/bin/swan.exe" 4 10,12,14,16 "$r"
   run omp4 fork_ifx       "$B/fork_omp/bin/swan.exe" 4 10,12,14,16 "$r"
   run omp4 up_O3_ifx      "$B/up_O3_omp/bin/swan.exe" 4 10,12,14,16 "$r"
   run omp4 up_default_ifx "$B/up_default_omp/bin/swan.exe" 4 10,12,14,16 "$r"
   run omp4 up_O3_gfortran "$ROOT/.upstream/build_O3_gfortran16_omp/bin/swan.exe" 4 10,12,14,16 "$r"
done
echo "load after: $(cut -d' ' -f1-3 /proc/loadavg)"
python3 - "$CSV" <<'EOF'
import collections
import csv
import statistics
import sys

times = collections.defaultdict(list)
fields = collections.defaultdict(set)
failed = 0
for row in csv.DictReader(open(sys.argv[1])):
    key = (row["kind"], row["build"])
    times[key].append(float(row["seconds"]))
    fields[key].add(row["sha_hs"])
    failed += row["exit"] != "0"
for key, values in times.items():
    values.sort()
    print("%-7s %-16s median %6.2f s  (%.2f to %.2f)  Hsig file %s"
          % (key[0], key[1], statistics.median(values), values[0], values[-1], ",".join(sorted(fields[key]))))
print("runs with a non-zero exit:", failed)
EOF
