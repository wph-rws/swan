module swan_fftw_compat
  use, intrinsic :: iso_c_binding
  implicit none

  include 'fftw3.f03'

  integer, parameter :: max_cached_plans = 8

  type :: fftw_plan_pair
    integer :: l = 0
    integer :: m = 0
    type(c_ptr) :: forward = c_null_ptr
    type(c_ptr) :: backward = c_null_ptr
  end type fftw_plan_pair

  type(fftw_plan_pair), save :: plan_cache(max_cached_plans)

contains

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

    complex(c_double_complex), allocatable :: scratch(:,:)
    integer :: i, slot
    integer(c_int) :: flags
    type(c_ptr) :: backward_plan, forward_plan

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
        flags = ior(FFTW_ESTIMATE, FFTW_UNALIGNED)

        ! FFTW uses C row-major dimensions. Reversing M and L maps the
        ! contiguous Fortran array C(L,M) to the same two-dimensional data.
        forward_plan = fftw_plan_dft_2d(int(m,c_int), int(l,c_int), &
                                        scratch, scratch, FFTW_FORWARD, flags)
        backward_plan = fftw_plan_dft_2d(int(m,c_int), int(l,c_int), &
                                         scratch, scratch, FFTW_BACKWARD, flags)

        if (.not. c_associated(forward_plan) .or. &
            .not. c_associated(backward_plan)) then
          if (c_associated(forward_plan)) call fftw_destroy_plan(forward_plan)
          if (c_associated(backward_plan)) call fftw_destroy_plan(backward_plan)
          ier = 20
        else
          plan_cache(slot)%l = l
          plan_cache(slot)%m = m
          plan_cache(slot)%forward = forward_plan
          plan_cache(slot)%backward = backward_plan
        end if

        deallocate(scratch)
      end if
    end if
!$omp end critical(swan_fftw_planner)
  end subroutine ensure_plans

  subroutine execute_transform(ldim, l, m, c, forward, ier)
    integer, intent(in) :: ldim, l, m
    complex(c_double_complex), intent(inout) :: c(ldim,m)
    logical, intent(in) :: forward
    integer, intent(out) :: ier

    complex(c_double_complex), allocatable :: packed(:,:)
    integer :: index
    real(c_double) :: scale
    type(c_ptr) :: plan

    call ensure_plans(l, m, ier)
    if (ier /= 0) return

!$omp critical(swan_fftw_planner)
    index = cached_plan_index(l, m)
    if (index == 0) then
      ier = 20
      plan = c_null_ptr
    elseif (forward) then
      plan = plan_cache(index)%forward
    else
      plan = plan_cache(index)%backward
    end if
!$omp end critical(swan_fftw_planner)
    if (ier /= 0) return

    if (ldim == l) then
      call fftw_execute_dft(plan, c, c)
      if (forward) then
        scale = 1.0_c_double / real(l*m, c_double)
        c = c * scale
      end if
    else
      allocate(packed(l,m))
      packed = c(1:l,:)
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
  implicit none

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
  implicit none

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
  implicit none

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
