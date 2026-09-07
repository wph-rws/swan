module swan_front_scheduling_backend
   use swan_diagnostics_level, only: ITEST
   use swan_io_units, only: PRINTF
   use swan_service_interfaces, only: MSGERR
   use SwanGridobjects, only: CELLID, CELLV1, CELLV2, CELLV3, celltype, &
      gridobject, verttype
   implicit none(type, external)
   private

   logical, parameter, public :: fixed_front_scheduling_enabled = .false.
   integer, allocatable, save, target, public :: flist(:,:)
   integer, allocatable, save, public :: fptr(:,:), nfront(:)
   integer, allocatable, save, public :: fronts(:), fronte(:)
   integer, pointer, public :: scheduled_vertices(:,:) => null()
   public :: build_front_schedule, clear_front_schedule
   public :: front_count, front_bounds

contains

   subroutine build_front_schedule(vlist)
      integer, intent(in) :: vlist(:,:)
      integer, parameter :: nlpf = 1
      integer :: icell, ifront, istat, j, jc, k, l, lmax, m, maxfr
      integer :: nlevel, nverts, nsweep, swpdir
      integer :: v(3), vu(2)
      integer, allocatable :: fcount(:), fid(:,:), fill(:), level(:), pos(:)
      type(celltype), pointer :: cell(:)
      type(verttype), pointer :: vert(:)

      nverts = size(vlist,1)
      nsweep = size(vlist,2)
      vert => gridobject%vert_grid
      cell => gridobject%cell_grid

      allocate(nfront(nsweep), pos(nverts), level(nverts), fid(nverts,nsweep))
      do swpdir = 1, nsweep
         pos = 0
         do j = 1, nverts
            k = vlist(j,swpdir)
            pos(k) = j
         end do

         level = 0
         do j = 1, nverts
            k = vlist(j,swpdir)
            lmax = 0
            do jc = 1, vert(k)%noc
               icell = vert(k)%cell(jc)%atti(CELLID)
               v(1) = cell(icell)%atti(CELLV1)
               v(2) = cell(icell)%atti(CELLV2)
               v(3) = cell(icell)%atti(CELLV3)
               do l = 1, 3
                  if (v(l) == k) then
                     vu(1) = v(mod(l  ,3)+1)
                     vu(2) = v(mod(l+1,3)+1)
                     exit
                  end if
               end do
               m = vu(1)
               if (pos(m) < pos(k)) lmax = max(lmax,level(m))
            end do
            level(k) = lmax + 1
         end do

         nlevel = maxval(level)
         nfront(swpdir) = ceiling(real(nlevel)/real(nlpf))
         if (ITEST >= 40) then
            if (nlpf == 1) then
               write(PRINTF,"(' sweepnr= ',i2,': number of graph levels = ',i8)") &
                  swpdir, nlevel
            else
               write(PRINTF,"(' sweepnr= ',i2,': number of graph levels = ',i8, &
                  & ' and number of fronts = ',i8)") swpdir, nlevel, nfront(swpdir)
            end if
         end if
         do j = 1, nverts
            fid(j,swpdir) = (level(j)-1)/nlpf + 1
         end do
      end do

      maxfr = maxval(nfront)
      allocate(fptr(maxfr+1,nsweep))
      do swpdir = 1, nsweep
         allocate(fcount(nfront(swpdir)))
         fcount = 0
         do j = 1, nverts
            ifront = fid(j,swpdir)
            fcount(ifront) = fcount(ifront) + 1
         end do
         fptr(1,swpdir) = 1
         do ifront = 1, nfront(swpdir)
            fptr(ifront+1,swpdir) = fptr(ifront,swpdir) + fcount(ifront)
         end do
         deallocate(fcount)
      end do

      istat = 0
      if (.not.allocated(flist)) allocate(flist(nverts,nsweep),stat=istat)
      if (istat /= 0) then
         call MSGERR(4,'Allocation problem in SwanVertlist: array flist ')
         return
      end if
      flist = 0
      scheduled_vertices => flist
      do swpdir = 1, nsweep
         allocate(fill(nfront(swpdir)))
         fill = fptr(1:nfront(swpdir),swpdir)
         do j = 1, nverts
            k = vlist(j,swpdir)
            ifront = fid(k,swpdir)
            l = fill(ifront)
            flist(l,swpdir) = k
            fill(ifront) = fill(ifront) + 1
         end do
         deallocate(fill)
      end do
      deallocate(fid,level,pos)
   end subroutine build_front_schedule

   integer function front_count(sweep_direction)
      integer, intent(in) :: sweep_direction

      front_count = nfront(sweep_direction)
   end function front_count

   subroutine front_bounds(front, sweep_direction, first_vertex, last_vertex)
      integer, intent(in) :: front, sweep_direction
      integer, intent(out) :: first_vertex, last_vertex

      first_vertex = fptr(front,sweep_direction)
      last_vertex = fptr(front+1,sweep_direction)-1
   end subroutine front_bounds

   subroutine clear_front_schedule()
      if (allocated(flist)) deallocate(flist)
      if (allocated(fptr)) deallocate(fptr)
      if (allocated(nfront)) deallocate(nfront)
      if (allocated(fronts)) deallocate(fronts)
      if (allocated(fronte)) deallocate(fronte)
      nullify(scheduled_vertices)
   end subroutine clear_front_schedule
end module swan_front_scheduling_backend
