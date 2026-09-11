module swan_library
! Contract and complete serial route.
!
! Target shape (in part): initialize / compute / write_output / finalize, each with
! explicit configuration, state and error result. Configuration is immutable
! after reading and kept apart from evolving state; hot kernels
! will later get only the arrays they need (not this object).
!
! This step fixes the contract and one complete serial route from
! reading input to result, with an idempotent finalize, also after a failed
! initialisation. A library call returns errors to the host (no
! STOP, no process-wide chdir). The executable uses the same route; deck
! and output contracts are unchanged. This route enforces the existing
! run-state ownership contract in finalize (read only): surviving
! state after a successful compute is an error, not a silent success. The
! state behind SWMAIN is otherwise still global (later steps
! migrate it per subsystem); two instances existing at once and overlapping
! execution have not been proven yet and are explicitly not claimed.
   use swan_service_interfaces, only: stpnow
   use swan_driver, only: swmain
   use M_OBSTA, only: OBSTACLE_STATE_IS_CLEAR
   use M_BNDSPEC, only: BOUNDARY_STATE_IS_CLEAR
   use OUTP_DATA, only: OUTPUT_STATE_IS_CLEAR
   use M_GENARR, only: GENERAL_ARRAY_STATE_IS_CLEAR
   use swan_input_fields, only: INPUT_FIELDS_ARE_CLEAR
   use swan_vegetation_layers, only: VEGETATION_LAYERS_ARE_CLEAR
   use swan_global_grid, only: GLOBAL_GRID_IS_CLEAR
   use M_PARALL, only: PARALLEL_STORAGE_IS_CLEAR
   use SwanGriddata, only: GRID_DATA_IS_CLEAR
   use SwanCompdata, only: COMPUTATION_DATA_IS_CLEAR
   use SwanIEM, only: IEM_STATE_IS_CLEAR
   use SwanBraggScat, only: BRAGG_STATE_IS_CLEAR
   use SwanQCM, only: QCM_STATE_IS_CLEAR
   implicit none(type, external)
   private

   public :: swan_config_t, swan_state_t, swan_result_t
   public :: swan_initialize, swan_compute, swan_write_output, swan_finalize
   public :: swan_result_ok, swan_result_error
   public :: SWAN_LIBRARY_OK, SWAN_LIBRARY_ERROR

   integer, parameter :: SWAN_LIBRARY_OK = 0
   integer, parameter :: SWAN_LIBRARY_ERROR = 1

   type :: swan_config_t
      ! Immutable after initialize (to be filled only through swan_initialize).
      character(len=256) :: input_file = "INPUT"
      logical :: validated = .false.
   end type swan_config_t

   type :: swan_state_t
      type(swan_config_t) :: config
      logical :: is_initialized = .false.
      logical :: is_computed = .false.
      integer :: run_count = 0
   end type swan_state_t

   type :: swan_result_t
      integer :: code = SWAN_LIBRARY_OK
      character(len=256) :: message = ""
   end type swan_result_t

contains

   function swan_result_ok() result(res)
      type(swan_result_t) :: res
      res%code = SWAN_LIBRARY_OK
      res%message = ""
   end function swan_result_ok

   function swan_result_error(message) result(res)
      character(len=*), intent(in) :: message
      type(swan_result_t) :: res
      res%code = SWAN_LIBRARY_ERROR
      res%message = message
   end function swan_result_error

   function swan_initialize(config, state) result(res)
      ! Validate the configuration without computing. Idempotent: repeated
      ! initialisation with the same input is not an error.
      type(swan_config_t), intent(in) :: config
      type(swan_state_t), intent(inout) :: state
      type(swan_result_t) :: res
      logical :: exists

      if (len_trim(config%input_file) == 0) then
         res = swan_result_error("empty input file name")
         return
      end if
      inquire (file=trim(config%input_file), exist=exists)
      if (.not. exists) then
         state%is_initialized = .false.
         res = swan_result_error("input file missing: "//trim(config%input_file))
         return
      end if
      state%config = config
      state%config%validated = .true.
      state%is_initialized = .true.
      state%is_computed = .false.
      res = swan_result_ok()
   end function swan_initialize

   function swan_compute(state) result(res)
      ! Complete computation route (for now through SWMAIN; phase split follows).
      type(swan_state_t), intent(inout) :: state
      type(swan_result_t) :: res

      if (.not. state%is_initialized) then
         res = swan_result_error("compute without a successful initialize")
         return
      end if
      if (stpnow()) then
         res = swan_result_error("compute blocked: stop flag is already set")
         return
      end if
      call swmain()
      state%run_count = state%run_count + 1
      if (stpnow()) then
         res = swan_result_error("SWMAIN ended with the stop flag set (see PRINT/Errfile)")
         return
      end if
      state%is_computed = .true.
      res = swan_result_ok()
   end function swan_compute

   function swan_write_output(state) result(res)
      ! Output contract: SWMAIN writes itself (PRINT/norm_end/tables/blocks).
      ! This step checks that contract explicitly instead of assuming it.
      type(swan_state_t), intent(in) :: state
      type(swan_result_t) :: res
      logical :: finished

      if (.not. state%is_computed) then
         res = swan_result_error("write_output without a successful compute")
         return
      end if
      inquire (file="norm_end", exist=finished)
      if (.not. finished) then
         res = swan_result_error("norm_end missing after compute")
         return
      end if
      res = swan_result_ok()
   end function swan_write_output

   function swan_finalize(state) result(res)
      ! Idempotent clean-up, also after a failed initialisation. SWMAIN cleans up
      ! through SWCLME/owners; finalize marks the library state as closed
      ! and is safe to call repeatedly.
      !
      ! After a successful compute, finalize verifies the
      ! ownership contract (read only, no second mutation path): surviving
      ! state is an error for the host, not a silent success. After an
      ! unsuccessful compute/initialisation it only resets (safe and
      ! idempotent), because an aborted run can legitimately leave
      ! partial state behind that the next initialize/compute
      ! rebuilds through SWINIT/SWREAD.
      type(swan_state_t), intent(inout) :: state
      type(swan_result_t) :: res

      if (state%is_computed) then
         res = verify_owners_clear()
         if (res%code /= SWAN_LIBRARY_OK) then
            state%is_initialized = .false.
            state%is_computed = .false.
            state%config%validated = .false.
            return
         end if
      end if
      state%is_initialized = .false.
      state%is_computed = .false.
      state%config%validated = .false.
      res = swan_result_ok()
   end function swan_finalize

   function verify_owners_clear() result(res)
      ! Read-only check of the run-state ownership contract
      ! (doc/run-state-ownership.md); same predicates as the A-B-A tests.
      type(swan_result_t) :: res

      if (.not. OBSTACLE_STATE_IS_CLEAR()) then
         res = swan_result_error("ownership survives run: obstacle state")
         return
      end if
      if (.not. BOUNDARY_STATE_IS_CLEAR()) then
         res = swan_result_error("ownership survives run: boundary state")
         return
      end if
      if (.not. OUTPUT_STATE_IS_CLEAR()) then
         res = swan_result_error("ownership survives run: output state")
         return
      end if
      if (.not. GENERAL_ARRAY_STATE_IS_CLEAR()) then
         res = swan_result_error("ownership survives run: general array state")
         return
      end if
      if (.not. INPUT_FIELDS_ARE_CLEAR()) then
         res = swan_result_error("ownership survives run: input fields")
         return
      end if
      if (.not. VEGETATION_LAYERS_ARE_CLEAR()) then
         res = swan_result_error("ownership survives run: vegetation layers")
         return
      end if
      if (.not. GLOBAL_GRID_IS_CLEAR()) then
         res = swan_result_error("ownership survives run: global grid")
         return
      end if
      if (.not. PARALLEL_STORAGE_IS_CLEAR()) then
         res = swan_result_error("ownership survives run: parallel storage")
         return
      end if
      if (.not. GRID_DATA_IS_CLEAR()) then
         res = swan_result_error("ownership survives run: grid data")
         return
      end if
      if (.not. COMPUTATION_DATA_IS_CLEAR()) then
         res = swan_result_error("ownership survives run: computation data")
         return
      end if
      if (.not. IEM_STATE_IS_CLEAR()) then
         res = swan_result_error("ownership survives run: IEM state")
         return
      end if
      if (.not. BRAGG_STATE_IS_CLEAR()) then
         res = swan_result_error("ownership survives run: Bragg state")
         return
      end if
      if (.not. QCM_STATE_IS_CLEAR()) then
         res = swan_result_error("ownership survives run: QCM state")
         return
      end if
      res = swan_result_ok()
   end function verify_owners_clear

end module swan_library
