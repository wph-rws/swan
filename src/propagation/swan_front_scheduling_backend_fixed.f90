module swan_front_scheduling_backend
   use swan_diagnostics_level, only: ITEST
   use swan_io_units, only: PRINTF
   implicit none(type, external)
   private

   logical, parameter, public :: fixed_front_scheduling_enabled = .true.
   integer, public :: nfront
   integer, allocatable, save, public :: fronts(:), fronte(:)
   integer, allocatable, save, public :: flist(:,:), fptr(:,:)
   integer, pointer, public :: scheduled_vertices(:,:) => null()
   public :: build_front_schedule, clear_front_schedule
   public :: front_count, front_bounds

contains

   subroutine build_front_schedule(vlist)
      integer, target, intent(in) :: vlist(:,:)
      integer, parameter :: nvth = 10
      integer :: ifront, nth, nvf, nverts
!$    integer, external :: omp_get_max_threads

      nverts = size(vlist,1)
      scheduled_vertices => vlist
      nth = 1
!$    nth = omp_get_max_threads()
      nvf = nvth * nth
      nfront = min(nverts,max(100,ceiling(real(nverts)/real(nvf))))

      if (.not.allocated(fronts)) allocate(fronts(nfront))
      if (.not.allocated(fronte)) allocate(fronte(nfront))

      nvf = (nverts+nfront-1)/nfront
      if (ITEST >= 40) write (PRINTF, &
         "(' Number of fronts = ',i4,' and number of vertices per front = ',i6)") &
         nfront, nvf

      do ifront = 1, nfront
         fronts(ifront) = 1 + (ifront-1)*nvf
         fronte(ifront) = min(nverts, ifront*nvf)
      end do
   end subroutine build_front_schedule

   integer function front_count(sweep_direction)
      integer, intent(in) :: sweep_direction

      front_count = nfront + 0*sweep_direction
   end function front_count

   subroutine front_bounds(front, sweep_direction, first_vertex, last_vertex)
      integer, intent(in) :: front, sweep_direction
      integer, intent(out) :: first_vertex, last_vertex

      first_vertex = fronts(front) + 0*sweep_direction
      last_vertex = fronte(front)
   end subroutine front_bounds

   subroutine clear_front_schedule()
      if (allocated(fronts)) deallocate(fronts)
      if (allocated(fronte)) deallocate(fronte)
      if (allocated(flist)) deallocate(flist)
      if (allocated(fptr)) deallocate(fptr)
      nullify(scheduled_vertices)
      nfront = 0
   end subroutine clear_front_schedule
end module swan_front_scheduling_backend
