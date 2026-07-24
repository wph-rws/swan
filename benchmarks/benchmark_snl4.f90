module snl4_bench_dirmajor

!  Direction-major prototype of the SWSNL2 compute core.
!
!  The production routine keeps its quadruplet workspace frequency-major
!  as UE/SA1/SA2/SFNL(MSC4MI:MSC4MA, MDC4MI:MDC4MA), while every other
!  spectral array in SWAN -- AC2, IMATRA, IMATDA, PLNL4S, MEMNL4 -- is
!  direction-major. That mismatch forces a transpose on the way in and a
!  strided gather on the way out. This module holds the same arithmetic
!  with the workspace turned around, so the two layouts can be timed and
!  compared bit for bit before any production source is touched.
!
!  Every expression keeps the operand order of the production routine, so
!  the two variants are expected to agree to the last bit.

   implicit none
   private
   public :: swsnl2_dirmajor

contains

   subroutine swsnl2_dirmajor(iddlow, iddtop, wwint, wwawg, ue, sa1, isstop, &
                              sa2, spcsig, snlc1, dal1, dal2, dal3, sfnl,    &
                              dep2, ac2, kmespc, redc0, redc1, imatda,       &
                              imatra, fachfr, idcmin, idcmax,                &
                              mdc, msc, mcgrd, mreds, kcgrd1, pquad, af11, pi)

      integer, intent(in) :: iddlow, iddtop, isstop
      integer, intent(in) :: mdc, msc, mcgrd, mreds, kcgrd1
      integer, intent(in) :: wwint(*)
      integer, intent(in) :: idcmin(msc), idcmax(msc)
      real, intent(in) :: wwawg(*), spcsig(msc), pquad(*), pi
      real, intent(in) :: snlc1, dal1, dal2, dal3, kmespc, fachfr
      real, intent(in) :: dep2(mcgrd), ac2(mdc, msc, mcgrd)
      real, intent(in) :: af11(:)
      real, intent(inout) :: ue(:,:), sa1(:,:), sa2(:,:), sfnl(:,:)
      real, intent(inout) :: imatda(mdc, msc), imatra(mdc, msc)
      real, intent(inout) :: redc0(mdc, msc, mreds), redc1(mdc, msc, mreds)

      integer :: idp, idp1, idm, idm1, isp, isp1, ism, ism1
      integer :: islow, ishgh, isclw, ischg, idlow, idhgh
      integer :: msc4mi, mdc4mi
      integer :: is, id, id0, i, j, iddum, iiid, idclow, idchgh, run, jd
      integer :: ioff, joff
      logical :: percir
      real :: awg1, awg2, awg3, awg4, awg5, awg6, awg7, awg8
      real :: x, x2, cons, jacobi, snlcs1, snlcs2, snlcs3
      real :: e00, ep1, em1, ep2, em2, factor, sa1a, sa1b, sa2a, sa2b, sigpi

      idp   = wwint(1)
      idp1  = wwint(2)
      idm   = wwint(3)
      idm1  = wwint(4)
      isp   = wwint(5)
      isp1  = wwint(6)
      ism   = wwint(7)
      ism1  = wwint(8)
      islow = wwint(9)
      ishgh = wwint(10)
      isclw = wwint(11)
      ischg = wwint(12)
      idlow = wwint(13)
      idhgh = wwint(14)
      msc4mi = wwint(15)
      mdc4mi = wwint(17)

      !  ue(jd, is) addresses direction mdc4mi+jd-1 and frequency msc4mi+is-1
      joff = 1 - mdc4mi
      ioff = 1 - msc4mi

      awg1 = wwawg(1)
      awg2 = wwawg(2)
      awg3 = wwawg(3)
      awg4 = wwawg(4)
      awg5 = wwawg(5)
      awg6 = wwawg(6)
      awg7 = wwawg(7)
      awg8 = wwawg(8)

      snlcs1 = pquad(3)
      snlcs2 = pquad(4)
      snlcs3 = pquad(5)
      x      = max(0.75 * dep2(kcgrd1) * kmespc, 0.5)
      x2     = max(-1.e15, snlcs3*x)
      cons   = snlc1 * (1. + snlcs1/x * (1.-snlcs2*x) * exp(x2))
      jacobi = 2. * pi

      percir = .false.
      if (iddlow == 1 .and. iddtop == mdc) then
         idclow = 1
         idchgh = mdc
         iiid   = 0
         percir = .true.
      else
         iiid   = max(idm1, idp1)
         idclow = idlow
         idchgh = idhgh
      end if

      !  Zero only the rows below the lowest discrete bin, as production does.
      do is = msc4mi, 0
         do iddum = idlow - iiid, idhgh + iiid
            ue(iddum+joff, is+ioff) = 0.
         end do
      end do
      do is = msc4mi, 0
         do id = idlow, idhgh
            sa1(id+joff, is+ioff) = 0.
            sa2(id+joff, is+ioff) = 0.
         end do
      end do

      !  Auxiliary spectrum. The direction wrap is hoisted out of the inner
      !  loop by walking contiguous runs, so both sides stay unit stride.
      iddum = idlow - iiid
      do while (iddum <= idhgh + iiid)
         id0 = modulo(iddum - 1, mdc) + 1
         run = min(mdc - id0 + 1, idhgh + iiid - iddum + 1)
         do is = 1, msc
            do j = 0, run - 1
               ue(iddum+j+joff, is+ioff) = ac2(id0+j, is, kcgrd1) * spcsig(is) * jacobi
            end do
         end do
         iddum = iddum + run
      end do

      !  High-frequency tail. The recurrence is in frequency, which is now the
      !  outer loop, so the direction loop vectorizes.
      do is = msc + 1, ishgh
         do id = idlow - iiid, idhgh + iiid
            ue(id+joff, is+ioff) = ue(id+joff, is-1+ioff) * fachfr
         end do
      end do

      !  Energy at the interacting bins.
      do is = isclw, ischg
         do id = idclow, idchgh
            e00 =       ue(id      +joff, is     +ioff)
            ep1 = awg1 * ue(id+idp1+joff, is+isp1+ioff) +  &
                  awg2 * ue(id+idp +joff, is+isp1+ioff) +  &
                  awg3 * ue(id+idp1+joff, is+isp +ioff) +  &
                  awg4 * ue(id+idp +joff, is+isp +ioff)
            em1 = awg5 * ue(id-idm1+joff, is+ism1+ioff) +  &
                  awg6 * ue(id-idm +joff, is+ism1+ioff) +  &
                  awg7 * ue(id-idm1+joff, is+ism +ioff) +  &
                  awg8 * ue(id-idm +joff, is+ism +ioff)

            ep2 = awg1 * ue(id-idp1+joff, is+isp1+ioff) +  &
                  awg2 * ue(id-idp +joff, is+isp1+ioff) +  &
                  awg3 * ue(id-idp1+joff, is+isp +ioff) +  &
                  awg4 * ue(id-idp +joff, is+isp +ioff)
            em2 = awg5 * ue(id+idm1+joff, is+ism1+ioff) +  &
                  awg6 * ue(id+idm +joff, is+ism1+ioff) +  &
                  awg7 * ue(id+idm1+joff, is+ism +ioff) +  &
                  awg8 * ue(id+idm +joff, is+ism +ioff)

            factor = cons * af11(is-msc4mi+1) * e00

            sa1a = e00 * (ep1*dal1 + em1*dal2) * pquad(2)
            sa1b = sa1a - ep1*em1*dal3 * pquad(2)
            sa2a = e00 * (ep2*dal1 + em2*dal2) * pquad(2)
            sa2b = sa2a - ep2*em2*dal3 * pquad(2)

            sa1(id+joff, is+ioff) = factor * sa1b
            sa2(id+joff, is+ioff) = factor * sa2b
         end do
      end do

      if (percir) then
         do is = isclw, ischg
            do id = 1, idhgh - mdc
               sa1(mdc+id+joff, is+ioff) = sa1(id+joff, is+ioff)
               sa2(mdc+id+joff, is+ioff) = sa2(id+joff, is+ioff)
               sa1(1-id +joff, is+ioff)  = sa1(mdc+1-id+joff, is+ioff)
               sa2(1-id +joff, is+ioff)  = sa2(mdc+1-id+joff, is+ioff)
            end do
         end do
      end if

      !  Put the source term together. The inner loop now runs along the
      !  contiguous direction axis of sa1/sa2 instead of across it.
      do i = 1, isstop
         do j = idcmin(i), idcmax(i)
            id = mod(j - 1 + mdc, mdc) + 1
            sfnl(id+joff, i+ioff) =                                                            &
                 - 2. * (sa1(j+joff, i+ioff) + sa2(j+joff, i+ioff))                            &
                 + awg1 * (sa1(j-idp1+joff, i-isp1+ioff) + sa2(j+idp1+joff, i-isp1+ioff))      &
                 + awg2 * (sa1(j-idp +joff, i-isp1+ioff) + sa2(j+idp +joff, i-isp1+ioff))      &
                 + awg3 * (sa1(j-idp1+joff, i-isp +ioff) + sa2(j+idp1+joff, i-isp +ioff))      &
                 + awg4 * (sa1(j-idp +joff, i-isp +ioff) + sa2(j+idp +joff, i-isp +ioff))      &
                 + awg5 * (sa1(j+idm1+joff, i-ism1+ioff) + sa2(j-idm1+joff, i-ism1+ioff))      &
                 + awg6 * (sa1(j+idm +joff, i-ism1+ioff) + sa2(j-idm +joff, i-ism1+ioff))      &
                 + awg7 * (sa1(j+idm1+joff, i-ism +ioff) + sa2(j-idm1+joff, i-ism +ioff))      &
                 + awg8 * (sa1(j+idm +joff, i-ism +ioff) + sa2(j-idm +joff, i-ism +ioff))
         end do
      end do

      !  Patankar update. sfnl is now read along the same axis that imatra,
      !  imatda, redc0 and redc1 already use.
      do i = 1, isstop
         sigpi = spcsig(i) * jacobi
         do j = idcmin(i), idcmax(i)
            id = mod(j - 1 + mdc, mdc) + 1
            jd = id + joff
            if (sfnl(jd, i+ioff) > 0.) then
               imatra(id,i)   = imatra(id,i)   + sfnl(jd, i+ioff) / sigpi
               redc0(id,i,1)  = redc0(id,i,1)  + sfnl(jd, i+ioff) / sigpi
            else
               imatda(id,i)   = imatda(id,i)   - sfnl(jd, i+ioff) /  &
                                max(1.e-18, ac2(id,i,kcgrd1)*sigpi)
               redc1(id,i,1)  = redc1(id,i,1)  + sfnl(jd, i+ioff) /  &
                                max(1.e-18, ac2(id,i,kcgrd1)*sigpi)
            end if
         end do
      end do

   end subroutine swsnl2_dirmajor

end module snl4_bench_dirmajor


program benchmark_snl4

!  Harness for the quadruplet workspace layout question.
!
!  Drives the production SWSNL2 and the direction-major prototype over the
!  same input, checks that they agree bit for bit, and reports the median
!  time of each. Run it before changing any production source: if the
!  prototype does not win here, the layout change is not worth making.
!
!  usage: benchmark_snl4 [REPETITIONS] [SECTOR]
!
!  Defaults reproduce the regular Voordelta case as observed under gdb:
!  MSC=25, MDC=36, IQUAD=2, sweep sector IDDLOW=1..IDDTOP=9.

   use, intrinsic :: iso_fortran_env, only: int64, real64
   use SWCOMM3
   use SWCOMM4
   use OCPCOMM4
   use M_SNL4
   use snl4_bench_dirmajor, only: swsnl2_dirmajor

   implicit none

   integer, parameter :: nrounds = 5

   character(len=32) :: argument
   integer :: repetitions, sector, mgrid, ipoint, pollute_kb, npollute, p
   real, allocatable :: pollute(:)
   real :: sink
   integer :: wwint(18), i, is, id, r
   integer, allocatable :: idcmin(:), idcmax(:)
   integer :: iddlow, iddtop, isstop
   integer(int64) :: started, finished, rate
   real :: wwawg(8), wwswg(8)
   real :: xis, snlc1, dal1, dal2, dal3, kmespc, fachfr
   real :: flow, fhigh, frint, sig, theta, thpeak, spread
   real, allocatable :: spcsig(:), dep2(:), ac2(:,:,:)
   real, allocatable :: ue(:,:), sa1(:,:), sa2(:,:), sfnl(:,:)
   real, allocatable :: ued(:,:), sa1d(:,:), sa2d(:,:), sfnld(:,:)
   real, allocatable :: imatda(:,:), imatra(:,:)
   real, allocatable :: imatda_d(:,:), imatra_d(:,:)
   real, allocatable :: redc0(:,:,:), redc1(:,:,:)
   real, allocatable :: redc0_d(:,:,:), redc1_d(:,:,:)
   real, allocatable :: plnl4s(:,:,:)
   real(real64) :: t_prod(nrounds), t_dirm(nrounds)
   integer :: mismatch

   repetitions = 200000
   sector = 9
   mgrid = 9076
   if (command_argument_count() >= 1) then
      call get_command_argument(1, argument)
      read(argument, *) repetitions
   end if
   if (command_argument_count() >= 2) then
      call get_command_argument(2, argument)
      read(argument, *) sector
   end if
   if (command_argument_count() >= 3) then
      call get_command_argument(3, argument)
      read(argument, *) mgrid
   end if
   pollute_kb = 0
   if (command_argument_count() >= 4) then
      call get_command_argument(4, argument)
      read(argument, *) pollute_kb
   end if
   if (repetitions < 1) error stop 'REPETITIONS must be positive'
   if (sector < 1) error stop 'SECTOR must be positive'
   if (mgrid < 2) error stop 'GRIDPOINTS must be at least 2'
   if (pollute_kb < 0) error stop 'POLLUTE_KB must not be negative'

   !  Between calls the production sweep runs the rest of SOURCE, which evicts
   !  the quadruplet workspace. Touching a scratch buffer of POLLUTE_KB
   !  reproduces that pressure; 0 keeps the workspace artificially hot.
   npollute = max(1, pollute_kb*256)
   allocate(pollute(npollute))
   pollute = 1.0
   sink = 0.

   !  ---- module state, matching the observed Voordelta run ----

   MSC   = 25
   MDC   = 36
   MCGRD = mgrid
   PI    = 4.*atan(1.)
   GRAV  = 9.81
   DDIR  = 2.*PI/real(MDC)

   IQUAD = 2
   MDIA  = 1
   PQUAD(1) = 0.25
   PQUAD(2) = 3.E7
   PQUAD(3) = 5.5
   PQUAD(4) = 0.833
   PQUAD(5) = -1.25

   ITEST  = 0
   ITRACE = 0
   LTRACE = .false.
   PRINTF = 6
   PRTEST = 6
   TESTFL = .false.
   NPTST  = 1
   IPTST  = 1

   ICMAX = 3
   KCGRD = 1
   KCGRD(1) = 2

   iddlow = 1
   iddtop = sector
   isstop = MSC

   !  ---- logarithmic frequency grid, 0.04 to 1.0 Hz ----

   allocate(spcsig(MSC))
   flow  = 0.04
   fhigh = 1.0
   frint = log(fhigh/flow) / real(MSC-1)
   do is = 1, MSC
      spcsig(is) = 2.*PI*flow*exp(frint*real(is-1))
   end do

   !  ---- interaction coefficients; also sets MSC4MI..MDC4MA and AF11 ----

   call FAC4WW(xis, snlc1, dal1, dal2, dal3, spcsig, wwint, wwawg, wwswg)
   call RANGE4(wwint, iddlow, iddtop)

   write(*,'(a,i0,a,i0,a,i0,a,i0)')                     &
      'workspace  MSC4MI:MSC4MA = ', MSC4MI, ':', MSC4MA,  &
      '   MDC4MI:MDC4MA = ', MDC4MI, ':', MDC4MA
   write(*,'(a,i0,a,i0,a,i0,a,i0,a,i0,a,i0)')           &
      'ranges     ISCLW:ISCHG = ', wwint(11), ':', wwint(12), &
      '   IDLOW:IDHGH = ', wwint(13), ':', wwint(14),          &
      '   sector = ', iddlow, ':', iddtop

   !  ---- spectra and work arrays ----

   allocate(dep2(MCGRD), ac2(MDC, MSC, MCGRD))
   allocate(idcmin(MSC), idcmax(MSC))
   allocate(ue(MSC4MI:MSC4MA, MDC4MI:MDC4MA))
   allocate(sa1(MSC4MI:MSC4MA, MDC4MI:MDC4MA))
   allocate(sa2(MSC4MI:MSC4MA, MDC4MI:MDC4MA))
   allocate(sfnl(MSC4MI:MSC4MA, MDC4MI:MDC4MA))
   allocate(ued(MDC4MI:MDC4MA, MSC4MI:MSC4MA))
   allocate(sa1d(MDC4MI:MDC4MA, MSC4MI:MSC4MA))
   allocate(sa2d(MDC4MI:MDC4MA, MSC4MI:MSC4MA))
   allocate(sfnld(MDC4MI:MDC4MA, MSC4MI:MSC4MA))
   allocate(imatda(MDC, MSC), imatra(MDC, MSC))
   allocate(imatda_d(MDC, MSC), imatra_d(MDC, MSC))
   allocate(redc0(MDC, MSC, MREDS), redc1(MDC, MSC, MREDS))
   allocate(redc0_d(MDC, MSC, MREDS), redc1_d(MDC, MSC, MREDS))
   allocate(plnl4s(MDC, MSC, NPTST))

   dep2 = 20.0
   kmespc = 0.242337763
   fachfr = 0.584803402

   !  JONSWAP-like shape with a cos^2 directional spread, in action density.
   !  One template is built and then scaled across the grid, so that AC2 has
   !  its production footprint (MCGRD spectra) without a costly setup.
   thpeak = 0.
   do is = 1, MSC
      sig = spcsig(is)
      do id = 1, MDC
         theta  = (real(id) - 0.5)*DDIR - PI
         spread = max(cos(theta - thpeak), 0.)**2
         ac2(id, is, 1) = 1.0e-2 * spread *                                 &
                          exp(-1.25*(0.6/(sig/(2.*PI)))**4) /               &
                          (sig**5) * exp(-0.05*real(is))
      end do
   end do
   do i = 2, MCGRD
      ac2(:,:,i) = ac2(:,:,1) * (0.5 + 0.5*real(i)/real(MCGRD))
   end do

   do is = 1, MSC
      idcmin(is) = iddlow
      idcmax(is) = iddtop
   end do

   ue = 0.; sa1 = 0.; sa2 = 0.; sfnl = 0.
   ued = 0.; sa1d = 0.; sa2d = 0.; sfnld = 0.
   plnl4s = 0.

   !  ---- correctness: one call of each, then compare bit for bit ----

   imatra = 0.; imatda = 0.; redc0 = 0.; redc1 = 0.
   call SWSNL2(iddlow, iddtop, wwint, wwawg, ue, sa1, isstop, sa2, spcsig,  &
               snlc1, dal1, dal2, dal3, sfnl, dep2, ac2, kmespc,            &
               redc0, redc1, imatda, imatra, fachfr, plnl4s, idcmin, idcmax)

   imatra_d = 0.; imatda_d = 0.; redc0_d = 0.; redc1_d = 0.
   call swsnl2_dirmajor(iddlow, iddtop, wwint, wwawg, ued, sa1d, isstop,    &
                        sa2d, spcsig, snlc1, dal1, dal2, dal3, sfnld,       &
                        dep2, ac2, kmespc, redc0_d, redc1_d, imatda_d,      &
                        imatra_d, fachfr, idcmin, idcmax,                   &
                        MDC, MSC, MCGRD, MREDS, KCGRD(1), PQUAD, AF11, PI)

   !  Exact comparison is deliberate. Both variants evaluate the same
   !  expressions in the same operand order on the same inputs, so anything
   !  other than bit equality means the port changed the arithmetic.
   mismatch = count(imatra /= imatra_d) + count(imatda /= imatda_d)  &
            + count(redc0 /= redc0_d)   + count(redc1 /= redc1_d)
   if (mismatch /= 0) then
      write(*,'(a,i0,a)') 'FAIL: ', mismatch, ' bit mismatches between layouts'
      write(*,'(a,es14.6)') '  max |imatra diff| = ', maxval(abs(imatra-imatra_d))
      write(*,'(a,es14.6)') '  max |imatda diff| = ', maxval(abs(imatda-imatda_d))
      error stop 'layout prototype is not bit-identical'
   end if
   write(*,'(a)') 'bit-identical: yes'

   !  ---- timing, alternating the two variants across rounds ----

   !  KCGRD(1) marches through the grid exactly as the sweep does, so the AC2
   !  reads stream through the full MCGRD-sized array instead of sitting in L1.

   do r = 1, nrounds
      imatra = 0.; imatda = 0.; redc0 = 0.; redc1 = 0.
      call system_clock(started, rate)
      do i = 1, repetitions
         KCGRD(1) = 2 + mod(i, MCGRD-1)
         call SWSNL2(iddlow, iddtop, wwint, wwawg, ue, sa1, isstop, sa2,    &
                     spcsig, snlc1, dal1, dal2, dal3, sfnl, dep2, ac2,      &
                     kmespc, redc0, redc1, imatda, imatra, fachfr, plnl4s,  &
                     idcmin, idcmax)
         if (pollute_kb > 0) then
            do p = 1, npollute, 16
               sink = sink + pollute(p)
            end do
         end if
      end do
      call system_clock(finished)
      t_prod(r) = real(finished-started, real64) / real(rate, real64)

      imatra_d = 0.; imatda_d = 0.; redc0_d = 0.; redc1_d = 0.
      call system_clock(started, rate)
      do i = 1, repetitions
         ipoint = 2 + mod(i, MCGRD-1)
         call swsnl2_dirmajor(iddlow, iddtop, wwint, wwawg, ued, sa1d,      &
                              isstop, sa2d, spcsig, snlc1, dal1, dal2,      &
                              dal3, sfnld, dep2, ac2, kmespc, redc0_d,      &
                              redc1_d, imatda_d, imatra_d, fachfr,          &
                              idcmin, idcmax,                               &
                              MDC, MSC, MCGRD, MREDS, ipoint, PQUAD,        &
                              AF11, PI)
         if (pollute_kb > 0) then
            do p = 1, npollute, 16
               sink = sink + pollute(p)
            end do
         end if
      end do
      call system_clock(finished)
      t_dirm(r) = real(finished-started, real64) / real(rate, real64)
   end do

   call report('production (freq-major)', t_prod)
   call report('prototype  (dir-major) ', t_dirm)
   write(*,'(a,f6.3,a)') 'speed-up ', median(t_prod)/median(t_dirm), 'x'
   write(*,'(a,f0.3,a,f0.3,a)') 'per call   ',                              &
      1.0e6_real64*median(t_prod)/real(repetitions,real64),                 &
      ' us production, ',                                                   &
      1.0e6_real64*median(t_dirm)/real(repetitions,real64), ' us prototype'

   !  The scratch buffer is touched identically in both arms, so it inflates
   !  both times and compresses the ratio. The absolute saving per call is the
   !  quantity that survives, and it is what scales to the full model run.
   write(*,'(a,f0.4,a)') 'saved      ',                                     &
      1.0e6_real64*(median(t_prod)-median(t_dirm))/real(repetitions,real64),&
      ' us per call'
   if (pollute_kb > 0) write(*,'(a,es10.3)') 'sink (ignore) ', sink

contains

   subroutine report(label, times)
      character(len=*), intent(in) :: label
      real(real64), intent(in) :: times(:)
      write(*,'(a,a,f0.4,a,f0.4,a)') label, '  median ', median(times),  &
         ' s   min ', minval(times), ' s'
   end subroutine report

   function median(values) result(m)
      real(real64), intent(in) :: values(:)
      real(real64) :: m, sorted(size(values)), swap
      integer :: a, b
      sorted = values
      do a = 1, size(sorted)-1
         do b = a+1, size(sorted)
            if (sorted(b) < sorted(a)) then
               swap = sorted(a); sorted(a) = sorted(b); sorted(b) = swap
            end if
         end do
      end do
      m = sorted((size(sorted)+1)/2)
   end function median

end program benchmark_snl4
