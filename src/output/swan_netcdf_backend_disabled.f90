module swan_netcdf_output_backend
   use swan_computational_grid, only: MCGRD, MXC, MYC
   use swan_global_grid, only: MXCGL, MYCGL
   use swan_spectral_grid, only: MDC, MSC
   implicit none(type, external)
   private

   logical, parameter, public :: netcdf_enabled = .false.
   character(len=40), public :: STNAMES(171,2) = ''
   public :: stnames_init
   public :: swn_outnc_appendblock, swn_outnc_close_on_end
   public :: swn_outnc_colspc, swn_outnc_openblockfile, swn_outnc_spec
   public :: is_netcdf_filename, netcdf_block_file_needs_open

contains

   pure logical function is_netcdf_filename(filename)
      character(len=*), intent(in) :: filename

      ! Keep the filename syntactically part of the fixed interface while
      ! preserving the historical non-netCDF interpretation of .nc files.
      is_netcdf_filename = netcdf_enabled .and. &
         (index(filename, '.NC') /= 0 .or. index(filename, '.nc') /= 0)
   end function is_netcdf_filename

   pure logical function netcdf_block_file_needs_open(irq)
      integer, intent(in) :: irq

      netcdf_block_file_needs_open = netcdf_enabled .and. irq > 0
   end function netcdf_block_file_needs_open

   subroutine stnames_init()
      STNAMES = ''
   end subroutine stnames_init

   subroutine swn_outnc_spec(rtype, oqi, oqr, mip, voqr, voq, ac2, &
      spcsig, spcdir, dep2, kgrpnt, cross, ionod)
      character(len=*), intent(in) :: rtype
      integer, intent(in) :: mip
      integer, intent(in) :: voqr(*), kgrpnt(MXC,MYC), ionod(*)
      integer, intent(inout) :: oqi(4)
      real(kind=kind(0.0d0)), intent(in) :: oqr(2)
      real, intent(in) :: voq(mip,*), ac2(MDC,MSC,MCGRD)
      real, intent(in) :: spcsig(MSC), spcdir(MDC,6), dep2(MCGRD)
      logical, intent(in) :: cross(1:4,1:mip)

      if (netcdf_enabled) write (*,*) rtype, oqi(1), oqr(1), mip, &
         voqr(1), voq(1,1), ac2(1,1,1), spcsig(1), spcdir(1,1), &
         dep2(1), kgrpnt(1,1), cross(1,1), ionod(1)
      call backend_unavailable()
   end subroutine swn_outnc_spec

   subroutine swn_outnc_colspc(rtype, oqi, oqr, mip, kgrpgl)
      character(len=*), intent(in) :: rtype
      integer, intent(inout) :: oqi(4)
      real(kind=kind(0.0d0)), intent(in) :: oqr(2)
      integer, intent(in) :: mip, kgrpgl(MXCGL,MYCGL)

      if (netcdf_enabled) write (*,*) rtype, oqi(1), oqr(1), mip, &
         kgrpgl(1,1)
      call backend_unavailable()
   end subroutine swn_outnc_colspc

   subroutine swn_outnc_appendblock(myk, mxk, ivtype, nref, irq, data, &
      excv, col)
      integer, intent(in) :: myk, mxk, ivtype, nref, irq, col
      real, intent(in) :: data(mxk*myk), excv

      if (netcdf_enabled) write (*,*) myk, mxk, ivtype, nref, irq, &
         data(1), excv, col
      call backend_unavailable()
   end subroutine swn_outnc_appendblock

   subroutine swn_outnc_close_on_end(nref, irq)
      integer, intent(inout) :: nref
      integer, optional, intent(in) :: irq

      if (netcdf_enabled) then
         if (present(irq)) write (*,*) nref, irq
         if (.not.present(irq)) write (*,*) nref
      end if
      call backend_unavailable()
   end subroutine swn_outnc_close_on_end

   subroutine swn_outnc_openblockfile(ncfile, myk, mxk, ovlnam, xgrdgl, &
      ygrdgl, oqi, oqr, ivtyp, irq)
      character(len=80), intent(in) :: ncfile
      integer, intent(in) :: myk, mxk, irq
      integer, intent(in) :: oqi(:), ivtyp(:)
      real(kind=kind(0.0d0)), intent(in) :: oqr(:)
      character(len=40), intent(in) :: ovlnam(:)
      real, intent(in) :: xgrdgl(mxk,myk), ygrdgl(mxk,myk)

      if (netcdf_enabled) write (*,*) ncfile, myk, mxk, ovlnam(1), &
         xgrdgl(1,1), ygrdgl(1,1), oqi(1), oqr(1), ivtyp(1), irq
      call backend_unavailable()
   end subroutine swn_outnc_openblockfile

   subroutine backend_unavailable()
      error stop 'netCDF output backend called while NETCDF is disabled'
   end subroutine backend_unavailable

end module swan_netcdf_output_backend
