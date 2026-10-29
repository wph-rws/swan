---
project: SWAN
summary: Source map of the SWAN fork, for reading and for AI agents.
src_dir: ../src
output_dir: ../build-ford
exclude_dir: ../src/hcat
exclude: swan_esmf_coupling_backend_enabled.f90
         swan_netcdf_backend_disabled.f90
         swan_block_coloring_backend_enabled.f90
         swan_metis_backend_enabled.f90
         swan_mpi_backend_disabled.f90
         swan_mpi_lifecycle_backend_coherent.f90
         swan_sweep_exchange_backend_jacobi.f90
         swan_file_open_backend_dec.f90
         swan_mpi_mpif_wrapper.f90
         swan_front_scheduling_backend_fixed.f90
         swan_matlab_v4_backend.f90
         swan_matlab_v4_bytes.f90
         agioncmd.f90
extensions: f90
fixed_length_limit: false
graph: true
graph_maxdepth: 4
graph_maxnodes: 60
search: false
display: public
         protected
         private
source: true
incl_src: false
quiet: true
---

Several modules in `src/` exist in two versions (a backend that is
switched on and one that is switched off, or two schedules), and only one
of each is built. This map follows the MPI build with netCDF: the enabled
MPI and netCDF backends, the standard file opening, the wavefront sweep
exchange and the graph front scheduling. Run it from the repository root
with `ford doc/ford.md`; the pages appear in `build-ford/`.
