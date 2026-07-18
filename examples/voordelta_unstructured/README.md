# Unstructured multicore Voordelta example

This case is the unstructured counterpart of `examples/voordelta`. It uses the
same 65 x 70 km RD New domain, bathymetry, synthetic wind and western boundary
spectrum, but computes on a generated triangular mesh. The default 200 m mesh
contains **114,426 vertices and 227,500 triangles**.

The size is deliberate: it makes initialization costs such as constructing and
sorting the sweep-direction vertex lists measurable. A pre-optimization build
with an O(N²) vertex sort can therefore spend a long time before the first
iteration. Use a 500 m mesh for a quicker functional smoke-test.

## OpenMP on one multicore machine

Build a separate OpenMP executable from the repository root:

```sh
cmake -S . -B build-openmp -G Ninja \
      -DCMAKE_Fortran_COMPILER=gfortran -DOPENMP=ON
cmake --build build-openmp
```

Generate the default mesh and run on eight cores:

```sh
python3 examples/voordelta_unstructured/run.py --cores 8
```

For a quicker end-to-end test (18,471 vertices, matching the regular example's
500 m point density):

```sh
python3 examples/voordelta_unstructured/run.py \
        --spacing 500 --regenerate-mesh --cores 8
```

The runner sets `OMP_NUM_THREADS`, `OMP_PLACES=cores` and
`OMP_PROC_BIND=spread`. Existing values for the latter two are respected.

## MPI

An MPI run over an unstructured mesh also requires METIS so SWAN can partition
the mesh. With MPI and METIS development packages installed:

```sh
cmake -S . -B build-mpi -G Ninja \
      -DCMAKE_Fortran_COMPILER=mpifort -DMPI=ON -DMETIS=ON
cmake --build build-mpi
python3 examples/voordelta_unstructured/run.py \
        --parallel mpi --cores 8
```

Use `--mpi-launcher mpirun` if that is the launcher name on the system. MPI and
OpenMP are separate SWAN build variants; this project does not enable them
together.

## Mesh and boundary setup

`generate_mesh.py` writes Triangle-compatible `.node` and `.ele` files. It
alternates cell diagonals and writes every triangle counterclockwise. Boundary
marker 1 denotes the complete western offshore boundary used by
`BOUNDSPEC SIDE 1`; marker 2 denotes the remaining outer boundary. These
marker-based boundary conditions are suitable for parallel unstructured runs,
where `SEGMENT` boundary input is not supported.

Generate a mesh without starting SWAN with:

```sh
python3 examples/voordelta_unstructured/run.py --mesh-only
```

The spacing must divide both the 65 km and 70 km domain extents exactly. The
generated Triangle files and numerical results are ignored by Git.

## Vertex-sort benchmark

`benchmark_sort.f90` compares the former `MINLOC` selection sort with the
tie-compatible O(N log N) segment-tree sort on all four sweep projections of
the generated mesh:

```sh
gfortran -O -ffree-line-length-none \
  examples/voordelta_unstructured/benchmark_sort.f90 \
  -o /tmp/swan-sort-benchmark

/tmp/swan-sort-benchmark \
  examples/voordelta_unstructured/voordelta_unstructured.node
```

The benchmark also verifies that both algorithms produce exactly the same
vertex order, including vertices with equal projected distances.

## Results

Results are collected under `examples/voordelta_unstructured/results/`:

- `voordelta_hs.blk` and `voordelta_depth.blk`: unstructured results
  interpolated to the same 500 m output frame as the regular example;
- `voordelta_sites.tbl`: values at the three reference sites;
- `voordelta_unstructured.prt` and `.erf`: diagnostics;
- `voordelta_hs.png` and `voordelta_depth.png`: maps, when NumPy and Matplotlib
  are available.

The bathymetry provenance and model limitations are documented in
`examples/voordelta/README.md`. In particular, this remains a realistic
demonstration and performance case, not a calibrated operational model.

See the official SWAN documentation for the
[unstructured computational-grid syntax](https://swanmodel.sourceforge.io/online_doc/swanuse/node25.html)
and [marker-based spectral boundary conditions](https://swanmodel.sourceforge.io/online_doc/swanuse/node27.html).
