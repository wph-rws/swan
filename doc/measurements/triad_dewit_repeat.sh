#!/bin/bash
# Repeat examples/nonlinear_interactions/triad/triad_biph_dewit.swn N times
# with one SWAN executable at a given number of OpenMP threads, and print the
# md5 of the table of each run (the known open issue "A deck that is not
# reproducible with OpenMP" in doc/bug-fixes-vs-tu-delft-41.51.md).
#
#   doc/measurements/triad_dewit_repeat.sh EXE THREADS N WORKDIR [CPUS]
#
# Each run goes to WORKDIR/t<THREADS>-<i>. CPUS (default 12-15) is passed to
# taskset; with four CPUs and eight threads the threads share the CPUs. The
# line printed per run is: threads, run, the first 8 hex digits of the md5
# of triad_biph_dewit.tbl, and the number of iterations. Compare the md5
# with that of a serial run.
set -eu
if [ $# -lt 4 ]; then
   sed -n '2,14p' "$0"; exit 2
fi
exe=$(realpath "$1"); threads=$2; n=$3; work=$4; cpus=${5:-12-15}
deck=$(dirname "$(realpath "$0")")/../../examples/nonlinear_interactions/triad
for i in $(seq 1 "$n"); do
   d=$work/t$threads-$i
   rm -rf "$d"; mkdir -p "$d"
   cp "$deck/triad_biph_dewit.swn" "$d/INPUT"
   cp "$deck/bottom.bot" "$d/"
   (cd "$d" && OMP_NUM_THREADS=$threads taskset -c "$cpus" "$exe" > console.log 2>&1)
   echo "t$threads $i $(md5sum < "$d/triad_biph_dewit.tbl" | cut -c1-8) iterations=$(grep -c 'sweep 1' "$d/PRINT")"
done
