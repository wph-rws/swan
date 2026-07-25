module swan_physical_settings
!
!     The physical constants of the run: gravity, density, the water level and
!     the thresholds that keep the physics well-behaved.
!
!     Almost all of these come from one command, `SET`, and are read-only after
!     it. GRAV and DEPMIN are wanted by eighteen of SWCOMM3's importers each --
!     more than any other symbol left in it -- because nearly every source term
!     needs one or both.
!
!     BNAUT and DNORTH belong here too: together they say how a direction in
!     the user's input maps onto the model's own convention, which is as much a
!     property of the problem as the density of the water.
!
   implicit none(type, external)
   private

   public :: GRAV, RHO, DEPMIN, WLEV
   public :: DNORTH, BNAUT
   public :: CDCAP, USCAP, HSRERR
   public :: CASTD, ICEWIND, PWTAIL

!     GRAV   : acceleration due to gravity
!     RHO    : density of the water
!     DEPMIN : threshold depth, kept above zero to prevent divisions by it
!     WLEV   : water level
   real :: GRAV, RHO
   real :: DEPMIN, WLEV

!     DNORTH : direction of North with respect to the x-axis of the user's
!              coordinate system
!     BNAUT  : whether directions are nautical rather than Cartesian
   real :: DNORTH
   logical :: BNAUT

!     CDCAP  : maximum drag coefficient
!     USCAP  : maximum friction velocity
!     HSRERR : relative error in Hs accepted when checking a boundary condition
   real :: CDCAP, USCAP, HSRERR

!     CASTD   : constant air-sea temperature difference
!     ICEWIND : factor controlling how much wind input passes through ice cover
   real :: CASTD, ICEWIND

!     PWTAIL : the powers describing the tail of the spectrum, and the
!              integration factors derived from them. Entry 1 is set by
!              `SET [pwtail]`; the rest follow from it and from the frequency
!              resolution.
   real :: PWTAIL(10)
end module swan_physical_settings
