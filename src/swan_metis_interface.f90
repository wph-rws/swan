module swan_metis_interface
! Expliciete METIS-koppeling via ISO_C_BINDING/BIND(C).
!
! De bewering dat de twee METIS-aanroepen "nooit" een Fortran-interface kunnen
! krijgen is onjuist: een C-koppeling ís een expliciete interface. Dit modulet
! vervangt de twee `external :: METIS_*`-declaraties (de laatste 2
! implicit-interface-waarschuwingen) door gecontroleerde BIND(C)-interfaces.
!
! Afleiding (niet het Fortran-kindnummer): de geïnstalleerde metis.h meldt
! IDXTYPEWIDTH=32 en REALTYPEWIDTH=32 (libmetis-dev 5.1.0, /usr/include/metis.h;
! idx_t=int32_t, real_t=float). Daarom zijn de interoperabele argumenttypes
! integer(c_int32_t) en real(c_float); de retourcode is C int (ook bij
! 64-bit-indexen). CMake controleert bij METIS=ON dat header en bibliotheek
! uit dezelfde installatie komen en dat de breedtes 32/32 zijn; een 64-bit
! METIS-installatie faalt luid bij configure (geen stille ABI-breuk) en staat
! als "niet uitgevoerd" in de supportmatrix tot de interface opnieuw is
! afgeleid en met 64-bit-indexbibliotheek is getest. Zonder METIS ontstaat
! geen METIS-afhankelijkheid (module wordt alleen gebruikt waar METIS aan staat).
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
      ! Dunne gecontroleerde wrapper: bereikcontrole + expliciete interface.
      ! Alleen succesvol linken bewijst geen passende ABI; daarom wordt hier
      ! de optiegrootte en het 0-gebaseerde METIS-venster gecontroleerd.
      integer(c_int32_t), intent(inout) :: options(0:)
      integer(c_int) :: ierr

      if (size(options) < 40) error stop "METIS optionsvenster kleiner dan 40"
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

      ! Indexbereik: METIS nummer 0-gebaseerd; de Fortran-kant converteert
      ! na afloop naar 1-gebaseerd (ipown+1). Negatieve aantallen zijn altijd fout.
      if (nvtxs <= 0 .or. ncon <= 0 .or. nparts <= 0) &
         error stop "METIS partitie-aantallen moeten positief zijn"
      ierr = metis_c_part_graph_kway(nvtxs, ncon, xadj, adjncy, vwgt, vsize, &
                                     adjwgt, nparts, tpwgts, ubvec, options, &
                                     edgecut, part)
   end function metis_part_graph_kway

end module swan_metis_interface
