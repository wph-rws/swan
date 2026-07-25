module swan_metis_interface
! Explicit METIS binding through ISO_C_BINDING/BIND(C).
!
! The claim that the two METIS calls can "never" get a Fortran interface
! is wrong: a C binding is an explicit interface. This module
! replaces the two `external :: METIS_*` declarations (the last 2
! implicit-interface warnings) with checked BIND(C) interfaces.
!
! Derivation (not the Fortran kind number): the installed metis.h reports
! IDXTYPEWIDTH=32 and REALTYPEWIDTH=32 (libmetis-dev 5.1.0, /usr/include/metis.h;
! idx_t=int32_t, real_t=float). The interoperable argument types are therefore
! integer(c_int32_t) and real(c_float); the return code is a C int (also with
! 64-bit indices). With METIS=ON, CMake checks that header and library
! come from the same installation and that the widths are 32/32; a 64-bit
! METIS installation fails loudly at configure time (no silent ABI break) and is
! listed as "not done" in the support matrix until the interface has been
! derived again and tested with a 64-bit index library. Without METIS there is
! no METIS dependency (the module is only used where METIS is enabled).
   use iso_c_binding, only: c_float, c_int, c_int32_t
   implicit none(type, external)
   private

   public :: metis_idx_width, metis_real_width
   public :: metis_set_default_options, metis_part_graph_kway

   integer, parameter :: metis_idx_width = 32
   integer, parameter :: metis_real_width = 32

   interface
      function metis_c_set_default_options(options) bind(C, name="METIS_SetDefaultOptions")
         import :: c_int, c_int32_t
         implicit none
         integer(c_int32_t), intent(inout) :: options(*)
         integer(c_int) :: metis_c_set_default_options
      end function metis_c_set_default_options

      function metis_c_part_graph_kway(nvtxs, ncon, xadj, adjncy, vwgt, vsize, &
                                       adjwgt, nparts, tpwgts, ubvec, options, &
                                       edgecut, part) bind(C, name="METIS_PartGraphKway")
         import :: c_float, c_int, c_int32_t
         implicit none
         integer(c_int32_t), intent(in) :: nvtxs, ncon
         integer(c_int32_t), intent(in) :: xadj(*), adjncy(*)
         integer(c_int32_t), intent(in) :: vwgt(*), vsize(*), adjwgt(*)
         integer(c_int32_t), intent(in) :: nparts
         real(c_float), intent(in) :: tpwgts(*), ubvec(*)
         integer(c_int32_t), intent(in) :: options(*)
         integer(c_int32_t), intent(out) :: edgecut
         integer(c_int32_t), intent(inout) :: part(*)
         integer(c_int) :: metis_c_part_graph_kway
      end function metis_c_part_graph_kway
   end interface

contains

   function metis_set_default_options(options) result(ierr)
      ! Thin checked wrapper: range check + explicit interface.
      ! Successful linking alone does not prove a matching ABI; that is why the
      ! options size and the 0-based METIS window are checked here.
      integer(c_int32_t), intent(inout) :: options(0:)
      integer(c_int) :: ierr

      if (size(options) < 40) error stop "METIS options window smaller than 40"
      ierr = metis_c_set_default_options(options)
   end function metis_set_default_options

   function metis_part_graph_kway(nvtxs, ncon, xadj, adjncy, vwgt, vsize, &
                                  adjwgt, nparts, tpwgts, ubvec, options, &
                                  edgecut, part) result(ierr)
      integer(c_int32_t), intent(in) :: nvtxs, ncon
      integer(c_int32_t), intent(in) :: xadj(*), adjncy(*)
      integer(c_int32_t), intent(in) :: vwgt(*), vsize(*), adjwgt(*)
      integer(c_int32_t), intent(in) :: nparts
      real(c_float), intent(in) :: tpwgts(*), ubvec(*)
      integer(c_int32_t), intent(in) :: options(*)
      integer(c_int32_t), intent(out) :: edgecut
      integer(c_int32_t), intent(inout) :: part(*)
      integer(c_int) :: ierr

      ! Index range: METIS numbers from 0; the Fortran side converts
      ! to 1-based afterwards (ipown+1). Negative counts are always an error.
      if (nvtxs <= 0 .or. ncon <= 0 .or. nparts <= 0) &
         error stop "METIS partition counts must be positive"
      ierr = metis_c_part_graph_kway(nvtxs, ncon, xadj, adjncy, vwgt, vsize, &
                                     adjwgt, nparts, tpwgts, ubvec, options, &
                                     edgecut, part)
   end function metis_part_graph_kway

end module swan_metis_interface
