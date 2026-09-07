module swan_library
! Contract en volledige seriële route.
!
! Doelvorm (deel): initialize / compute / write_output / finalize, elk met
! expliciete configuratie, toestand en foutresultaat. Configuratie is na
! inlezen onveranderlijk en gescheiden van evoluerende toestand; hete kernels
! krijgen later alleen benodigde arrays (niet dit object).
!
! Deze stap legt het contract en één volledige seriële route vast van
! inlezen tot resultaat met idempotent finalize, ook na mislukte
! initialisatie. Een bibliotheekaanroep retourneert fouten aan de host (geen
! STOP, geen procesbrede chdir). De executable gebruikt dezelfde route; deck-
! en uitvoercontracten zijn ongewijzigd. Deze route bindt het bestaande
! run-state-ownership-contract in finalize (alleen lezen): overlevende
! toestand na een geslaagde compute is een fout, geen stil succes. De
! toestand achter SWMAIN is verder nog globaal (volgende stappen
! migreren per subsysteem); twee tegelijk bestaande instanties en overlappende
! uitvoering zijn nog niet bewezen en uitdrukkelijk niet geclaimd.
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
      ! Na initialize onveranderlijk (alleen via swan_initialize te vullen).
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
      ! Valideer configuratie zonder te rekenen. Idempotent: herhaald
      ! initialiseren met dezelfde invoer is geen fout.
      type(swan_config_t), intent(in) :: config
      type(swan_state_t), intent(inout) :: state
      type(swan_result_t) :: res
      logical :: exists

      if (len_trim(config%input_file) == 0) then
         res = swan_result_error("lege invoerbestandsnaam")
         return
      end if
      inquire (file=trim(config%input_file), exist=exists)
      if (.not. exists) then
         state%is_initialized = .false.
         res = swan_result_error("invoerbestand ontbreekt: "//trim(config%input_file))
         return
      end if
      state%config = config
      state%config%validated = .true.
      state%is_initialized = .true.
      state%is_computed = .false.
      res = swan_result_ok()
   end function swan_initialize

   function swan_compute(state) result(res)
      ! Volledige rekenroute (voorlopig via SWMAIN; fasesplitsing volgt).
      type(swan_state_t), intent(inout) :: state
      type(swan_result_t) :: res

      if (.not. state%is_initialized) then
         res = swan_result_error("compute zonder geslaagde initialize")
         return
      end if
      if (stpnow()) then
         res = swan_result_error("compute geblokkeerd: stopvlag staat al aan")
         return
      end if
      call swmain()
      state%run_count = state%run_count + 1
      if (stpnow()) then
         res = swan_result_error("SWMAIN eindigde met stopvlag (zie PRINT/Errfile)")
         return
      end if
      state%is_computed = .true.
      res = swan_result_ok()
   end function swan_compute

   function swan_write_output(state) result(res)
      ! Uitvoercontract: SWMAIN schrijft zelf (PRINT/norm_end/tabellen/blokken).
      ! Deze stap bewaakt dat contract expliciet i.p.v. het te veronderstellen.
      type(swan_state_t), intent(in) :: state
      type(swan_result_t) :: res
      logical :: finished

      if (.not. state%is_computed) then
         res = swan_result_error("write_output zonder geslaagde compute")
         return
      end if
      inquire (file="norm_end", exist=finished)
      if (.not. finished) then
         res = swan_result_error("norm_end ontbreekt na compute")
         return
      end if
      res = swan_result_ok()
   end function swan_write_output

   function swan_finalize(state) result(res)
      ! Idempotent opruiming, ook na mislukte initialisatie. SWMAIN ruimt via
      ! SWCLME/owners op; finalize markeert de bibliotheektoestand als gesloten
      ! en is veilig herhaald aan te roepen.
      !
      ! Na een geslaagde compute verifieert finalize het
      ! eigendomscontract (alleen lezen, geen tweede mutatiepad): overlevende
      ! toestand is een fout aan de host, geen stil succes. Na een niet
      ! geslaagde compute/initialisatie wordt alleen teruggezet (veilig en
      ! idempotent), omdat een afgebroken run legitiem gedeeltelijke toestand
      ! kan achterlaten die de volgende initialize/compute via SWINIT/SWREAD
      ! opnieuw opbouwt.
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
      ! Lees-only bewaking van het run-state-ownership-contract
      ! (doc/run-state-ownership.md); zelfde predicaten als de A-B-A-tests.
      type(swan_result_t) :: res

      if (.not. OBSTACLE_STATE_IS_CLEAR()) then
         res = swan_result_error("eigendom overleeft run: obstacle state")
         return
      end if
      if (.not. BOUNDARY_STATE_IS_CLEAR()) then
         res = swan_result_error("eigendom overleeft run: boundary state")
         return
      end if
      if (.not. OUTPUT_STATE_IS_CLEAR()) then
         res = swan_result_error("eigendom overleeft run: output state")
         return
      end if
      if (.not. GENERAL_ARRAY_STATE_IS_CLEAR()) then
         res = swan_result_error("eigendom overleeft run: general array state")
         return
      end if
      if (.not. INPUT_FIELDS_ARE_CLEAR()) then
         res = swan_result_error("eigendom overleeft run: input fields")
         return
      end if
      if (.not. VEGETATION_LAYERS_ARE_CLEAR()) then
         res = swan_result_error("eigendom overleeft run: vegetation layers")
         return
      end if
      if (.not. GLOBAL_GRID_IS_CLEAR()) then
         res = swan_result_error("eigendom overleeft run: global grid")
         return
      end if
      if (.not. PARALLEL_STORAGE_IS_CLEAR()) then
         res = swan_result_error("eigendom overleeft run: parallel storage")
         return
      end if
      if (.not. GRID_DATA_IS_CLEAR()) then
         res = swan_result_error("eigendom overleeft run: grid data")
         return
      end if
      if (.not. COMPUTATION_DATA_IS_CLEAR()) then
         res = swan_result_error("eigendom overleeft run: computation data")
         return
      end if
      if (.not. IEM_STATE_IS_CLEAR()) then
         res = swan_result_error("eigendom overleeft run: IEM state")
         return
      end if
      if (.not. BRAGG_STATE_IS_CLEAR()) then
         res = swan_result_error("eigendom overleeft run: Bragg state")
         return
      end if
      if (.not. QCM_STATE_IS_CLEAR()) then
         res = swan_result_error("eigendom overleeft run: QCM state")
         return
      end if
      res = swan_result_ok()
   end function verify_owners_clear

end module swan_library
