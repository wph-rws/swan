module swan_metis_partition_backend
   use SwanParallel, only: SwanCollBpntlist, SwanDecomposition, &
      SwanUvExchgR, ipown, irbuf, isbuf, ivrecv, ivsend, nvrecv, nvsend, &
      rbuf, rrqst, sbuf, srqst, vres, vsubcm
   implicit none(type, external)
   private

   logical, parameter, public :: metis_enabled = .true.
   public :: metis_collect_boundary_points, metis_copy_ownership
   public :: metis_decompose, metis_exchange_real, metis_release
   public :: metis_vertex_is_resident

contains

   subroutine metis_decompose(logcom)
      logical, intent(inout) :: logcom(7)

      call SwanDecomposition(logcom)
   end subroutine metis_decompose

   subroutine metis_collect_boundary_points()
      call SwanCollBpntlist
   end subroutine metis_collect_boundary_points

   subroutine metis_exchange_real(field)
      real, intent(inout) :: field(:)

      call SwanUvExchgR(field)
   end subroutine metis_exchange_real

   subroutine metis_copy_ownership(ownership)
      real, allocatable, intent(inout) :: ownership(:)

      ownership = real(ipown)
   end subroutine metis_copy_ownership

   logical function metis_vertex_is_resident(vertex)
      integer, intent(in) :: vertex

      metis_vertex_is_resident = vres(vertex)
   end function metis_vertex_is_resident

   subroutine metis_release()
      if (allocated(ipown)) deallocate(ipown)
      if (allocated(vres)) deallocate(vres)
      if (allocated(vsubcm)) deallocate(vsubcm)
      if (allocated(nvrecv)) deallocate(nvrecv)
      if (allocated(nvsend)) deallocate(nvsend)
      if (allocated(ivrecv)) deallocate(ivrecv)
      if (allocated(ivsend)) deallocate(ivsend)
      if (allocated(rrqst)) deallocate(rrqst)
      if (allocated(srqst)) deallocate(srqst)
      if (allocated(irbuf)) deallocate(irbuf)
      if (allocated(isbuf)) deallocate(isbuf)
      if (allocated(rbuf)) deallocate(rbuf)
      if (allocated(sbuf)) deallocate(sbuf)
   end subroutine metis_release

end module swan_metis_partition_backend
