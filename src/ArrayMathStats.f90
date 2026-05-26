module arraymath_stats
  use, intrinsic :: ieee_arithmetic
  implicit none
  private

  public :: nansum, nanavg, nanstd, nanmax, nanmin

contains

  function nansum(arr) result(aval)
    real, intent(in) :: arr(:)
    real :: aval
    integer :: i, nn

    aval = 0.0
    nn = 0
    do i = 1, size(arr)
      if (.not. ieee_is_nan(arr(i))) then
        aval = aval + arr(i)
        nn = nn + 1
      end if
    end do
    if (nn == 0) aval = ieee_value(aval, ieee_quiet_nan)
  end function nansum

  function nanavg(arr) result(aval)
    real, intent(in) :: arr(:)
    real :: aval
    integer :: i, nn

    aval = 0.0
    nn = 0
    do i = 1, size(arr)
      if (.not. ieee_is_nan(arr(i))) then
        aval = aval + arr(i)
        nn = nn + 1
      end if
    end do
    if (nn > 0) then
      aval = aval / nn
    else
      aval = ieee_value(aval, ieee_quiet_nan)
    end if
  end function nanavg

  function nanstd(arr) result(aval)
    real, intent(in) :: arr(:)
    real :: aval, avg
    integer :: i, nn

    avg = nanavg(arr)
    if (ieee_is_nan(avg)) then
      aval = ieee_value(aval, ieee_quiet_nan)
      return
    end if

    aval = 0.0
    nn = 0
    do i = 1, size(arr)
      if (.not. ieee_is_nan(arr(i))) then
        aval = aval + (arr(i) - avg) ** 2
        nn = nn + 1
      end if
    end do
    if (nn > 1) then
      aval = sqrt(aval / nn)
    else
      aval = ieee_value(aval, ieee_quiet_nan)
    end if
  end function nanstd

  function nanmax(arr) result(aval)
    real, intent(in) :: arr(:)
    real :: aval
    integer :: i, nn

    aval = 0.0
    nn = 0
    do i = 1, size(arr)
      if (.not. ieee_is_nan(arr(i))) then
        if (nn == 0 .or. arr(i) > aval) aval = arr(i)
        nn = nn + 1
      end if
    end do
    if (nn == 0) aval = ieee_value(aval, ieee_quiet_nan)
  end function nanmax

  function nanmin(arr) result(aval)
    real, intent(in) :: arr(:)
    real :: aval
    integer :: i, nn

    aval = 0.0
    nn = 0
    do i = 1, size(arr)
      if (.not. ieee_is_nan(arr(i))) then
        if (nn == 0 .or. arr(i) < aval) aval = arr(i)
        nn = nn + 1
      end if
    end do
    if (nn == 0) aval = ieee_value(aval, ieee_quiet_nan)
  end function nanmin

end module arraymath_stats
