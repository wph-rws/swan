! Clear contracts of the run-state owners, one check per owner.
!
! Each check fills the state an owner holds, asserts that IS_CLEAR notices,
! clears it, and clears again to show the clear is idempotent (SWCLME may run
! after an input or setup error that already released part of the state).
! These are the predicates swan_library%verify_owners_clear relies on. They
! were thirteen near-identical programs; one program keeps every check and
! its message, at the cost of stopping at the first failing owner.
program test_state_clear
   implicit none(type, external)

   call check_obstacle_state()
   call check_boundary_state()
   call check_output_state()
   call check_general_array_state()
   call check_input_fields()
   call check_vegetation_layers()
   call check_global_grid()
   call check_parallel_storage()
   call check_grid_data()
   call check_computation_data()
   call check_iem_state()
   call check_bragg_state()
   call check_qcm_state()

contains

   subroutine check_obstacle_state()
      use M_OBSTA, only: OBSTDAT, FOBSTAC, OBSTDONE, &
         CLEAR_OBSTACLE_STATE, OBSTACLE_STATE_IS_CLEAR

      call CLEAR_OBSTACLE_STATE()
      call require(OBSTACLE_STATE_IS_CLEAR(), 'initial state is not clear')

      OBSTDONE = .TRUE.
      allocate(FOBSTAC%TRCF1D(3), FOBSTAC%TRCF2D(2,2))
      allocate(FOBSTAC%IGFRQD(4), FOBSTAC%XCRP(2), FOBSTAC%YCRP(2))
      allocate(FOBSTAC%NEXTOBST)
      allocate(FOBSTAC%NEXTOBST%TRCF1D(1))
      allocate(FOBSTAC%NEXTOBST%XCRP(2), FOBSTAC%NEXTOBST%YCRP(2))
      allocate(FOBSTAC%NEXTOBST%NEXTOBST)
      allocate(FOBSTAC%NEXTOBST%NEXTOBST%IGFRQD(2))

      call require(.not.OBSTACLE_STATE_IS_CLEAR(), &
         'filled state was reported as clear')
      call CLEAR_OBSTACLE_STATE()
      call require(OBSTACLE_STATE_IS_CLEAR(), 'clear left obstacle state behind')

      ! The owner contract is deliberately idempotent: SWCLME may be called after
      ! an input or setup error that already released part of the state.
      call CLEAR_OBSTACLE_STATE()
      call require(OBSTACLE_STATE_IS_CLEAR(), 'second clear changed empty state')

      print *, 'obstacle state clear contract passes'
   end subroutine check_obstacle_state

   subroutine check_boundary_state()
      use M_BNDSPEC, only: ALOBND, FBNDFIL, FBS, FBGP, CUBGP, &
         LBFILS, LBS, LBGP, CLEAR_BOUNDARY_STATE, &
         BOUNDARY_STATE_IS_CLEAR

      call CLEAR_BOUNDARY_STATE()
      call require(BOUNDARY_STATE_IS_CLEAR(), 'initial state is not clear')

      ALOBND = .TRUE.
      LBFILS = .TRUE.
      LBS = .TRUE.
      LBGP = .TRUE.
      allocate(FBNDFIL%BSPLOC(2), FBNDFIL%BSPDIR(2), FBNDFIL%BSPFRQ(2))
      allocate(FBNDFIL%NEXTBSPC)
      allocate(FBNDFIL%NEXTBSPC%BSPLOC(1))
      allocate(FBS%NEXTBS)
      allocate(FBS%NEXTBS%NEXTBS)
      allocate(FBGP%NEXTBGP)
      allocate(FBGP%NEXTBGP%NEXTBGP)
      CUBGP => FBGP%NEXTBGP%NEXTBGP

      call require(.not.BOUNDARY_STATE_IS_CLEAR(), &
         'filled state was reported as clear')
      call CLEAR_BOUNDARY_STATE()
      call require(BOUNDARY_STATE_IS_CLEAR(), 'clear left boundary state behind')
      call CLEAR_BOUNDARY_STATE()
      call require(BOUNDARY_STATE_IS_CLEAR(), 'second clear changed empty state')

      print *, 'boundary state clear contract passes'
   end subroutine check_boundary_state

   subroutine check_output_state()
      use OUTP_DATA, only: FOPS, FORQ, COPS, LOPS, LORQ, LCOMPGRD, NREOQ, &
         OUTP_FILES, CLEAR_OUTPUT_STATE, OUTPUT_STATE_IS_CLEAR

      call CLEAR_OUTPUT_STATE()
      call require(OUTPUT_STATE_IS_CLEAR(), 'initial state is not clear')

      LOPS = .TRUE.
      LORQ = .TRUE.
      LCOMPGRD = .TRUE.
      NREOQ = 1
      OUTP_FILES(1) = 'output.tbl'
      allocate(FOPS%XP(2), FOPS%YP(2), FOPS%XQ(2), FOPS%YQ(2))
      allocate(FOPS%NEXTOPS)
      allocate(FOPS%NEXTOPS%XP(1))
      allocate(FORQ%IVTYP(2), FORQ%FAC(2))
      allocate(FORQ%NEXTORQ)
      allocate(FORQ%NEXTORQ%IVTYP(1))
      COPS => FOPS

      call require(.not.OUTPUT_STATE_IS_CLEAR(), &
         'filled state was reported as clear')
      call CLEAR_OUTPUT_STATE()
      call require(OUTPUT_STATE_IS_CLEAR(), 'clear left output state behind')
      call CLEAR_OUTPUT_STATE()
      call require(OUTPUT_STATE_IS_CLEAR(), 'second clear changed empty state')

      print *, 'output state clear contract passes'
   end subroutine check_output_state

   subroutine check_general_array_state()
      use M_GENARR, only: KGRPNT, KGRBND, XYTST, AC2, XCGRID, YCGRID, &
         SPCSIG, SPCDIR, &
         CLEAR_GENERAL_ARRAY_STATE, GENERAL_ARRAY_STATE_IS_CLEAR

      call CLEAR_GENERAL_ARRAY_STATE()
      call require(GENERAL_ARRAY_STATE_IS_CLEAR(), 'initial state is not clear')

      allocate(KGRPNT(2,2), KGRBND(2), XYTST(2))
      allocate(AC2(2,2,2), XCGRID(2,2), YCGRID(2,2))
      allocate(SPCSIG(2), SPCDIR(2,2))

      call require(.not.GENERAL_ARRAY_STATE_IS_CLEAR(), &
         'filled state was reported as clear')
      call CLEAR_GENERAL_ARRAY_STATE()
      call require(GENERAL_ARRAY_STATE_IS_CLEAR(), 'clear left array state behind')
      call CLEAR_GENERAL_ARRAY_STATE()
      call require(GENERAL_ARRAY_STATE_IS_CLEAR(), 'second clear changed empty state')

      print *, 'general array state clear contract passes'
   end subroutine check_general_array_state

   subroutine check_input_fields()
      use swan_input_fields, only: DEPTH, WLEVL, FRIC, UXB, UYB, WXI, WYI, &
         ASTDF, MUDLF, NPLAF, TURBF, AICEF, HICEF, HSSF, TSSF, DSSF, &
         CLEAR_INPUT_FIELDS, INPUT_FIELDS_ARE_CLEAR
      use M_GENARR, only: ENSURE_FIELD_SIZE

      call CLEAR_INPUT_FIELDS()
      call require(INPUT_FIELDS_ARE_CLEAR(), 'initial state is not clear')

      allocate(DEPTH(2), WLEVL(2), FRIC(2), UXB(2), UYB(2), WXI(2), WYI(2))
      allocate(ASTDF(2), MUDLF(2), NPLAF(2), TURBF(2))
      allocate(AICEF(2), HICEF(2), HSSF(2), TSSF(2), DSSF(2))

      call require(.not.INPUT_FIELDS_ARE_CLEAR(), &
         'filled state was reported as clear')

      ! A field can be read again after its input grid has changed.  The old
      ! allocate-once guard retained the first extent and let the subsequent
      ! copy write past it; pin the resize primitive used by SREDEP directly.
      call ENSURE_FIELD_SIZE(DEPTH, 5)
      call require(allocated(DEPTH), 'resized depth field is not allocated')
      call require(size(DEPTH) == 5, 'depth field retained its old extent')

      call CLEAR_INPUT_FIELDS()
      call require(INPUT_FIELDS_ARE_CLEAR(), 'clear left input fields behind')
      call CLEAR_INPUT_FIELDS()
      call require(INPUT_FIELDS_ARE_CLEAR(), 'second clear changed empty state')

      print *, 'input field resize and clear contracts pass'
   end subroutine check_input_fields

   subroutine check_vegetation_layers()
      use swan_vegetation_layers, only: LAYH, VEGDIL, VEGDRL, VEGNSL, &
         CLEAR_VEGETATION_LAYERS, VEGETATION_LAYERS_ARE_CLEAR

      call CLEAR_VEGETATION_LAYERS()
      call require(VEGETATION_LAYERS_ARE_CLEAR(), 'initial state is not clear')

      allocate(LAYH(2), VEGDIL(2), VEGDRL(2), VEGNSL(2))

      call require(.not.VEGETATION_LAYERS_ARE_CLEAR(), &
         'filled state was reported as clear')
      call CLEAR_VEGETATION_LAYERS()
      call require(VEGETATION_LAYERS_ARE_CLEAR(), 'clear left vegetation behind')
      call CLEAR_VEGETATION_LAYERS()
      call require(VEGETATION_LAYERS_ARE_CLEAR(), 'second clear changed empty state')

      print *, 'vegetation layers clear contract passes'
   end subroutine check_vegetation_layers

   subroutine check_global_grid()
      use swan_global_grid, only: KGRPGL, KGRBGL, XGRDGL, YGRDGL, &
         CLEAR_GLOBAL_GRID, GLOBAL_GRID_IS_CLEAR

      call CLEAR_GLOBAL_GRID()
      call require(GLOBAL_GRID_IS_CLEAR(), 'initial state is not clear')

      allocate(KGRPGL(2,2), KGRBGL(2), XGRDGL(2,2), YGRDGL(2,2))

      call require(.not.GLOBAL_GRID_IS_CLEAR(), &
         'filled state was reported as clear')
      call CLEAR_GLOBAL_GRID()
      call require(GLOBAL_GRID_IS_CLEAR(), 'clear left global grid behind')
      call CLEAR_GLOBAL_GRID()
      call require(GLOBAL_GRID_IS_CLEAR(), 'second clear changed empty state')

      print *, 'global grid clear contract passes'
   end subroutine check_global_grid

   subroutine check_parallel_storage()
      use M_PARALL, only: IBLKAD, CLEAR_PARALLEL_STORAGE, &
         PARALLEL_STORAGE_IS_CLEAR

      call CLEAR_PARALLEL_STORAGE()
      call require(PARALLEL_STORAGE_IS_CLEAR(), 'initial state is not clear')

      allocate(IBLKAD(4))

      call require(.not.PARALLEL_STORAGE_IS_CLEAR(), &
         'filled state was reported as clear')
      call CLEAR_PARALLEL_STORAGE()
      call require(PARALLEL_STORAGE_IS_CLEAR(), 'clear left storage behind')
      call CLEAR_PARALLEL_STORAGE()
      call require(PARALLEL_STORAGE_IS_CLEAR(), 'second clear changed empty state')

      print *, 'parallel storage clear contract passes'
   end subroutine check_parallel_storage

   subroutine check_grid_data()
      use SwanGriddata, only: xcugrd, ycugrd, xcugrdgl, ycugrdgl, ivertg, &
         vmark, CLEAR_GRID_DATA, GRID_DATA_IS_CLEAR

      call CLEAR_GRID_DATA()
      call require(GRID_DATA_IS_CLEAR(), 'initial state is not clear')

      allocate(xcugrd(2), ycugrd(2), xcugrdgl(2), ycugrdgl(2))
      allocate(ivertg(2), vmark(2))

      call require(.not.GRID_DATA_IS_CLEAR(), &
         'filled state was reported as clear')
      call CLEAR_GRID_DATA()
      call require(GRID_DATA_IS_CLEAR(), 'clear left grid data behind')
      call CLEAR_GRID_DATA()
      call require(GRID_DATA_IS_CLEAR(), 'second clear changed empty state')

      print *, 'grid data clear contract passes'
   end subroutine check_grid_data

   subroutine check_computation_data()
      use SwanCompdata, only: vlist, blist, bvertg, bmark, &
         CLEAR_COMPUTATION_DATA, COMPUTATION_DATA_IS_CLEAR

      call CLEAR_COMPUTATION_DATA()
      call require(COMPUTATION_DATA_IS_CLEAR(), 'initial state is not clear')

      allocate(vlist(2,2), blist(2,2), bvertg(2,2), bmark(2,2))

      call require(.not.COMPUTATION_DATA_IS_CLEAR(), &
         'filled state was reported as clear')
      call CLEAR_COMPUTATION_DATA()
      call require(COMPUTATION_DATA_IS_CLEAR(), 'clear left computation data behind')
      call CLEAR_COMPUTATION_DATA()
      call require(COMPUTATION_DATA_IS_CLEAR(), 'second clear changed empty state')

      print *, 'computation data clear contract passes'
   end subroutine check_computation_data

   subroutine check_iem_state()
      use SwanIEM, only: iss, iwt, itt, freq, E0, Ebig, &
         CLEAR_IEM_STATE, IEM_STATE_IS_CLEAR

      call CLEAR_IEM_STATE()
      call require(IEM_STATE_IS_CLEAR(), 'initial state is not clear')

      allocate(iss(2), iwt(2), itt(2), freq(2), E0(2,2), Ebig(2,2,2))

      call require(.not.IEM_STATE_IS_CLEAR(), &
         'filled state was reported as clear')
      call CLEAR_IEM_STATE()
      call require(IEM_STATE_IS_CLEAR(), 'clear left IEM state behind')
      call CLEAR_IEM_STATE()
      call require(IEM_STATE_IS_CLEAR(), 'second clear changed empty state')

      print *, 'IEM state clear contract passes'
   end subroutine check_iem_state

   subroutine check_bragg_state()
      use SwanBraggScat, only: fb, fbdxy, &
         CLEAR_BRAGG_STATE, BRAGG_STATE_IS_CLEAR

      call CLEAR_BRAGG_STATE()
      call require(BRAGG_STATE_IS_CLEAR(), 'initial state is not clear')

      allocate(fb(2,2,2), fbdxy(2,2,2,2))

      call require(.not.BRAGG_STATE_IS_CLEAR(), &
         'filled state was reported as clear')
      call CLEAR_BRAGG_STATE()
      call require(BRAGG_STATE_IS_CLEAR(), 'clear left Bragg state behind')
      call CLEAR_BRAGG_STATE()
      call require(BRAGG_STATE_IS_CLEAR(), 'second clear changed empty state')

      print *, 'Bragg state clear contract passes'
   end subroutine check_bragg_state

   subroutine check_qcm_state()
      use SwanQCM, only: kx, ky, xpsc, xpd, ypd, kxd, kyd, disbk0, disbk1, &
         CLEAR_QCM_STATE, QCM_STATE_IS_CLEAR

      call CLEAR_QCM_STATE()
      call require(QCM_STATE_IS_CLEAR(), 'initial state is not clear')

      allocate(kx(2), ky(2), xpsc(2), xpd(2), ypd(2), kxd(2), kyd(2))
      allocate(disbk0(2), disbk1(2))

      call require(.not.QCM_STATE_IS_CLEAR(), &
         'filled state was reported as clear')
      call CLEAR_QCM_STATE()
      call require(QCM_STATE_IS_CLEAR(), 'clear left QCM state behind')
      call CLEAR_QCM_STATE()
      call require(QCM_STATE_IS_CLEAR(), 'second clear changed empty state')

      print *, 'QCM state clear contract passes'
   end subroutine check_qcm_state

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not.condition) error stop message
   end subroutine require
end program test_state_clear
