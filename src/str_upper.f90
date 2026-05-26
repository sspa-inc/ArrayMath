subroutine to_upper(str)
! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May 2012)
! Original author: Clive Page

     implicit none

     character(len=*), intent(inout)  :: str
     
     integer :: i,j,cA,cZ
	 cA = iachar("a")
	 cZ = iachar("z")
     do i = 1, len_trim(str)
          j = iachar(str(i:i))
          if (j>=cA  .and. j<=cZ ) then
               str(i:i) = achar(iachar(str(i:i))-32)
          end if
     end do
end subroutine to_upper

subroutine to_lower(str)
! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May 2012)
! Original author: Clive Page

     implicit none

     character(len=*), intent(inout)  :: str
     
     integer :: i,j,cA,cZ
	 cA = iachar("A")
	 cZ = iachar("Z")
     do i = 1, len_trim(str)
          j = iachar(str(i:i))
          if (j>=cA .and. j<=cZ ) then
               str(i:i) = achar(iachar(str(i:i))+32)
          end if
     end do
end subroutine to_lower