
Subroutine  Check_NLDN(numStrike,xlon,ylat,time,strike,lquality)
!
!  Author: Ming Hu, ESRL/GSD
!  
!  First written: 01/02/2008.
!
!  IN:
!     numStrike
!     xlon
!     ylat
!     time
!     strike
!     
!  out:
!     lquality
!
  IMPLICIT NONE

  INTEGER ::   numStrike

  REAL ::   xlon(numStrike)
  REAL ::   ylat(numStrike)
  real ::   time(numStrike)
  real ::   strike(numStrike)

  integer   ::   lquality(numStrike)

  integer   :: i,j

!
!   check duplicate
!
  DO i=2,numStrike
    DO j=1,i-1
      if(abs(xlon(i) - xlon(j)) < 0.001 .and.   &
         abs(ylat(i) - ylat(j)) < 0.001 .and.   &
         abs(time(i) - time(j)) < 0.001 ) then
        lquality(i)=1
      endif
    ENDDO
  enddo

!
!  check if flash qc number > 0
!
  DO i=1,numStrike
      if(strike(i) > 0.1 ) then
        lquality(i)=1
      endif
  enddo

end subroutine Check_NLDN

