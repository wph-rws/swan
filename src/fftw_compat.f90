module swan_fftw_compat
  use, intrinsic :: iso_c_binding
  implicit none(type, external)

  include 'fftw3.f03'

  integer, parameter :: max_cached_plans = 8

  type :: fftw_plan_pair
    integer :: l = 0
    integer :: m = 0
    integer(c_int) :: alignment = -1_c_int
    type(c_ptr) :: aligned_forward = c_null_ptr
    type(c_ptr) :: aligned_backward = c_null_ptr
    type(c_ptr) :: unaligned_forward = c_null_ptr
    type(c_ptr) :: unaligned_backward = c_null_ptr
  end type fftw_plan_pair

  type(fftw_plan_pair), save :: plan_cache(max_cached_plans)

  interface
    subroutine cfft2i(l, m, wsave, lensav, ier)
      import :: c_double
      integer, intent(in) :: l, m, lensav
      real(c_double), intent(out) :: wsave(lensav)
      integer, intent(out) :: ier
    end subroutine cfft2i

    subroutine cfft2f(ldim, l, m, c, wsave, lensav, work, lenwrk, ier)
      import :: c_double, c_double_complex
      integer, intent(in) :: ldim, l, m, lensav, lenwrk
      complex(c_double_complex), intent(inout) :: c(ldim,m)
      real(c_double), intent(in) :: wsave(lensav), work(lenwrk)
      integer, intent(out) :: ier
    end subroutine cfft2f

    subroutine cfft2b(ldim, l, m, c, wsave, lensav, work, lenwrk, ier)
      import :: c_double, c_double_complex
      integer, intent(in) :: ldim, l, m, lensav, lenwrk
      complex(c_double_complex), intent(inout) :: c(ldim,m)
      real(c_double), intent(in) :: wsave(lensav), work(lenwrk)
      integer, intent(out) :: ier
    end subroutine cfft2b
  end interface

contains

  integer(c_int) function array_alignment(values) result(alignment)
    complex(c_double_complex), contiguous, intent(inout), target :: values(:,:)
    real(c_double), pointer :: storage(:)

    call c_f_pointer(c_loc(values(1,1)), storage, [2*size(values)])
    alignment = fftw_alignment_of(storage)
  end function array_alignment

  integer function cached_plan_index(l, m) result(index)
    integer, intent(in) :: l, m
    integer :: i

    index = 0
    do i = 1, max_cached_plans
      if (plan_cache(i)%l == l .and. plan_cache(i)%m == m) then
        index = i
        return
      end if
    end do
  end function cached_plan_index

  subroutine ensure_plans(l, m, ier)
    integer, intent(in) :: l, m
    integer, intent(out) :: ier

    complex(c_double_complex), allocatable, target :: scratch(:,:)
    integer :: i, slot
    integer(c_int) :: alignment
    type(c_ptr) :: aligned_backward, aligned_forward, &
                   unaligned_backward, unaligned_forward

    ier = 0
    if (l < 1 .or. m < 1) then
      ier = 20
      return
    end if

!$omp critical(swan_fftw_planner)
    if (cached_plan_index(l, m) == 0) then
      slot = 0
      do i = 1, max_cached_plans
        if (plan_cache(i)%l == 0) then
          slot = i
          exit
        end if
      end do

      if (slot == 0) then
        ier = 20
      else
        allocate(scratch(l,m))
        alignment = array_alignment(scratch)

        ! FFTW uses C row-major dimensions. Reversing M and L maps the
        ! contiguous Fortran array C(L,M) to the same two-dimensional data.
        aligned_forward = fftw_plan_dft_2d(int(m,c_int), int(l,c_int), &
                                           scratch, scratch, FFTW_FORWARD, FFTW_MEASURE)
        aligned_backward = fftw_plan_dft_2d(int(m,c_int), int(l,c_int), &
                                            scratch, scratch, FFTW_BACKWARD, FFTW_MEASURE)
        unaligned_forward = fftw_plan_dft_2d(int(m,c_int), int(l,c_int), &
                                             scratch, scratch, FFTW_FORWARD, &
                                             ior(FFTW_ESTIMATE, FFTW_UNALIGNED))
        unaligned_backward = fftw_plan_dft_2d(int(m,c_int), int(l,c_int), &
                                              scratch, scratch, FFTW_BACKWARD, &
                                              ior(FFTW_ESTIMATE, FFTW_UNALIGNED))

        if (.not. c_associated(aligned_forward) .or. &
            .not. c_associated(aligned_backward) .or. &
            .not. c_associated(unaligned_forward) .or. &
            .not. c_associated(unaligned_backward)) then
          if (c_associated(aligned_forward)) call fftw_destroy_plan(aligned_forward)
          if (c_associated(aligned_backward)) call fftw_destroy_plan(aligned_backward)
          if (c_associated(unaligned_forward)) call fftw_destroy_plan(unaligned_forward)
          if (c_associated(unaligned_backward)) call fftw_destroy_plan(unaligned_backward)
          ier = 20
        else
          plan_cache(slot)%l = l
          plan_cache(slot)%m = m
          plan_cache(slot)%alignment = alignment
          plan_cache(slot)%aligned_forward = aligned_forward
          plan_cache(slot)%aligned_backward = aligned_backward
          plan_cache(slot)%unaligned_forward = unaligned_forward
          plan_cache(slot)%unaligned_backward = unaligned_backward
        end if

        deallocate(scratch)
      end if
    end if
!$omp end critical(swan_fftw_planner)
  end subroutine ensure_plans

  subroutine get_plan(l, m, forward, alignment, plan, ier)
    integer, intent(in) :: l, m
    logical, intent(in) :: forward
    integer(c_int), intent(in) :: alignment
    type(c_ptr), intent(out) :: plan
    integer, intent(out) :: ier
    integer :: index

    ier = 0
!$omp critical(swan_fftw_planner)
    index = cached_plan_index(l, m)
    if (index == 0) then
      ier = 20
      plan = c_null_ptr
    elseif (alignment == plan_cache(index)%alignment) then
      if (forward) then
        plan = plan_cache(index)%aligned_forward
      else
        plan = plan_cache(index)%aligned_backward
      end if
    elseif (forward) then
      plan = plan_cache(index)%unaligned_forward
    else
      plan = plan_cache(index)%unaligned_backward
    end if
!$omp end critical(swan_fftw_planner)
  end subroutine get_plan

  subroutine execute_transform(ldim, l, m, c, forward, ier)
    integer, intent(in) :: ldim, l, m
    complex(c_double_complex), intent(inout), target :: c(ldim,m)
    logical, intent(in) :: forward
    integer, intent(out) :: ier

    complex(c_double_complex), allocatable, target :: packed(:,:)
    integer(c_int) :: alignment
    real(c_double) :: scale
    type(c_ptr) :: plan

    call ensure_plans(l, m, ier)
    if (ier /= 0) return

    if (ldim == l) then
      alignment = array_alignment(c)
      call get_plan(l, m, forward, alignment, plan, ier)
      if (ier /= 0) return
      call fftw_execute_dft(plan, c, c)
      if (forward) then
        scale = 1.0_c_double / real(l*m, c_double)
        c = c * scale
      end if
    else
      allocate(packed(l,m))
      packed = c(1:l,:)
      alignment = array_alignment(packed)
      call get_plan(l, m, forward, alignment, plan, ier)
      if (ier /= 0) then
        deallocate(packed)
        return
      end if
      call fftw_execute_dft(plan, packed, packed)
      if (forward) then
        scale = 1.0_c_double / real(l*m, c_double)
        packed = packed * scale
      end if
      c(1:l,:) = packed
      deallocate(packed)
    end if
  end subroutine execute_transform

end module swan_fftw_compat

subroutine cfft2i(l, m, wsave, lensav, ier)
  use, intrinsic :: iso_c_binding, only: c_double
  use swan_fftw_compat, only: ensure_plans
  implicit none(type, external)

  integer, intent(in) :: l, m, lensav
  real(c_double), intent(out) :: wsave(lensav)
  integer, intent(out) :: ier
  integer :: minimum_lensav

  ier = 0
  minimum_lensav = 2*l + int(log(real(l,c_double))/log(2.0_c_double)) + &
                   2*m + int(log(real(m,c_double))/log(2.0_c_double)) + 8
  if (lensav < minimum_lensav) then
    ier = 2
    return
  end if

  wsave = 0.0_c_double
  call ensure_plans(l, m, ier)
end subroutine cfft2i

subroutine cfft2f(ldim, l, m, c, wsave, lensav, work, lenwrk, ier)
  use, intrinsic :: iso_c_binding, only: c_double, c_double_complex
  use swan_fftw_compat, only: execute_transform
  implicit none(type, external)

  integer, intent(in) :: ldim, l, m, lensav, lenwrk
  complex(c_double_complex), intent(inout) :: c(ldim,m)
  real(c_double), intent(in) :: wsave(lensav), work(lenwrk)
  integer, intent(out) :: ier
  integer :: minimum_lensav

  ier = 0
  if (ldim < l) then
    ier = 5
    return
  end if

  minimum_lensav = 2*l + int(log(real(l,c_double))/log(2.0_c_double)) + &
                   2*m + int(log(real(m,c_double))/log(2.0_c_double)) + 8
  if (lensav < minimum_lensav) then
    ier = 2
    return
  end if
  if (lenwrk < 2*l*m) then
    ier = 3
    return
  end if

  call execute_transform(ldim, l, m, c, .true., ier)
end subroutine cfft2f

subroutine cfft2b(ldim, l, m, c, wsave, lensav, work, lenwrk, ier)
  use, intrinsic :: iso_c_binding, only: c_double, c_double_complex
  use swan_fftw_compat, only: execute_transform
  implicit none(type, external)

  integer, intent(in) :: ldim, l, m, lensav, lenwrk
  complex(c_double_complex), intent(inout) :: c(ldim,m)
  real(c_double), intent(in) :: wsave(lensav), work(lenwrk)
  integer, intent(out) :: ier
  integer :: minimum_lensav

  ier = 0
  if (ldim < l) then
    ier = 5
    return
  end if

  minimum_lensav = 2*l + int(log(real(l,c_double))/log(2.0_c_double)) + &
                   2*m + int(log(real(m,c_double))/log(2.0_c_double)) + 8
  if (lensav < minimum_lensav) then
    ier = 2
    return
  end if
  if (lenwrk < 2*l*m) then
    ier = 3
    return
  end if

  call execute_transform(ldim, l, m, c, .false., ier)
end subroutine cfft2b
