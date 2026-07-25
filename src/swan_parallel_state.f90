MODULE swan_parallel_state
!
!     Process identity of the running SWAN instance.
!
!     These few flags used to live in M_PARALL, but the diagnostic services
!     (MSGERR, STRACE) need them while M_PARALL itself calls MSGERR. Holding
!     them in a dependency-free module breaks that cycle, so the services can
!     be real module procedures instead of external symbols. M_PARALL uses and
!     re-exports this module, so its own users are unaffected.
!
   IMPLICIT NONE
   PUBLIC

!     MASTER    : rank of the master process
   INTEGER, PARAMETER :: MASTER = 1

!     INODE     : rank of the present process
!     NPROC     : number of processes in the parallel run
!     IAMMASTER : whether this process is the master
!     PARLL     : whether this is a parallel (distributed-memory) run
   INTEGER :: INODE, NPROC
   LOGICAL :: IAMMASTER, PARLL

END MODULE swan_parallel_state
