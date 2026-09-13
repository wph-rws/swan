program test_surfbeat_sector
   use SwanIEM, only: SwanIEMsrfbeat, CLEAR_IEM_STATE, E0, Ebig, freq, &
      iss, iwt, itt, ntf, nff, nif, dfiem
   use swan_computational_grid, only: MXC, MYC, MCGRD, DX
   use swan_spectral_grid, only: MDC, MSC, DDIR, FRINTF, FULCIR
   use swan_math_constants, only: PI, PI2
   use swan_physical_settings, only: GRAV, DEPMIN
   use swan_physics_selection, only: PWIND, PSURF
   use swan_diagnostics_level, only: LTRACE
   implicit none(type, external)
   real, allocatable :: directions(:,:), ac(:,:,:)
   real :: hs(4), depth(4), sigma(2), energy(2), angle(2), first
   integer :: grid(3,1), form, id, incident
   GRAV = 9.81
   DEPMIN = .01
   LTRACE = .false.
   PWIND(17) = 1025.
   PSURF(2) = .73
   MXC = 3
   MYC = 1
   MCGRD = 4
   DX = 100.
   MSC = 2
   FRINTF = .1
   DDIR = PI/12.
   grid(:,1) = [2,3,4]
   depth = 20.
   hs = 1.
   sigma = [.6, 1.]
   do form = 1, 2
      call CLEAR_IEM_STATE()
      FULCIR = form == 1
      MDC = 24
      first = .5*DDIR
      incident = 1
      if (.not.FULCIR) then
         MDC = 6
         first = -2.5*DDIR
         incident = 4
      endif
      allocate(directions(MDC,6), ac(MDC,2,4))
      directions = 0.
      do id = 1, MDC
         directions(id,1) = first + (id-1)*DDIR
         directions(id,2) = cos(directions(id,1))
         directions(id,3) = sin(directions(id,1))
      enddo
      ac = 0.
      ac(incident,:,:) = .1
      nff = 2
      nif = 2
      ntf = 8
      dfiem = .1/PI2
      allocate(freq(2), iss(2), iwt(2), itt(2), E0(MDC,2))
      freq = sigma
      iss = 1
      iwt = [1,2]
      itt = incident
      E0 = 0.
      E0(incident,:) = 1.
      call SwanIEMsrfbeat(hs, ac, depth, directions, sigma, grid)
      energy(form) = sum(Ebig(:,:,2))
      if (.not.energy(form) > 0.) error stop 'no bound wave energy in direction test'
      angle(form) = sum(sum(Ebig(:,:,2),dim=2)*directions(:,1))/energy(form)
      if (abs(angle(form)-.5*DDIR) > .001) error stop 'bound wave stored in wrong direction'
      deallocate(directions, ac)
   enddo
   if (abs(energy(1)-energy(2)) > 1.e-5*energy(1)) error stop 'sector changed bound wave energy'
   call CLEAR_IEM_STATE()
end program test_surfbeat_sector
