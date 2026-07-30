program test_output_interfaces
   use swan_output_writers, only: SWTABP
   implicit none(type, external)

   integer :: oqi(4), ivtyp(1), voqr(1), ionod(1)
   real :: voq(1,1)
   real(kind=kind(0.0d0)) :: oqr(2)

   oqi = (/0, 1, 1, 1/)
   oqr = -1.0d0
   ivtyp = 10
   voqr = 1
   ionod = 1
   voq = 0.0

   if (.false.) then
      call SWTABP('TABD', oqi, oqr, ivtyp, 'POINTS  ', 1, voqr, voq, ionod)
      call SWTABP('TABD', oqi, ivtyp, 'POINTS  ', 1, voqr, voq, ionod)
   end if
end program test_output_interfaces
