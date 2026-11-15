#!/bin/bash
# Build the fork and upstream SWAN with Intel ifx, serial and OpenMP, for the
# timing in time.sh. Run it from the root of the repository:
#
#    bash benchmarks/intel/build.sh
#
# It expects the Intel environment (source /opt/intel/oneapi/setvars.sh, so
# that `ifx` is found) and the upstream source in .upstream/src (commit
# 43e9bbb). The builds go to build-intel/, which git ignores. Never more than
# six compile jobs at once.
set -u
ROOT=$(pwd)
B=${B:-$ROOT/build-intel}
UP=$B/upstreamsrc
mkdir -p "$B/logs"
command -v ifx > /dev/null || { echo "ifx not found: source the Intel environment first"; exit 1; }

# Upstream's build writes next to its source, so it gets a copy. The path
# must not hold a hyphen below the copy: upstream's switch.pl loops on one.
rm -rf "$UP"
mkdir -p "$UP"
rsync -a --exclude .git "$ROOT/.upstream/src/" "$UP/"

build() {  # name, source, extra cmake arguments...
   local name=$1 src=$2
   shift 2
   rm -rf "$B/$name"
   if ! cmake -S "$src" -B "$B/$name" -DCMAKE_Fortran_COMPILER=ifx "$@" > "$B/logs/$name.config.log" 2>&1; then
      echo "CONFIGURE FAILED: $name"; tail -20 "$B/logs/$name.config.log"; return 1
   fi
   if ! cmake --build "$B/$name" -j 6 > "$B/logs/$name.build.log" 2>&1; then
      echo "BUILD FAILED: $name"; grep -n -i -B2 -A6 'error' "$B/logs/$name.build.log" | head -60; return 1
   fi
   echo "built $name"
}

# The fork: Release is -O3, and its CMake adds -fp-model precise for ifx.
build fork_serial "$ROOT" -DCMAKE_BUILD_TYPE=Release
build fork_omp    "$ROOT" -DCMAKE_BUILD_TYPE=Release -DOPENMP=ON
# Upstream forces the build type None and its own flags. Its default with
# Intel is -O2; CMAKE_Fortran_FLAGS_NONE comes after those flags, so -O3
# there wins.
build up_default_serial "$UP"
build up_default_omp    "$UP" -DOPENMP=ON
build up_O3_serial      "$UP" -DCMAKE_Fortran_FLAGS_NONE=-O3
build up_O3_omp         "$UP" -DCMAKE_Fortran_FLAGS_NONE=-O3 -DOPENMP=ON
