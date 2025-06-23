SUBROUTINE set_wrf_debug_level ( level )
  USE module_wrf_error
  IMPLICIT NONE
  INTEGER , INTENT(IN) :: level
  wrf_debug_level = level
  RETURN
END SUBROUTINE set_wrf_debug_level
SUBROUTINE get_wrf_debug_level ( level )
  USE module_wrf_error
  IMPLICIT NONE
  INTEGER , INTENT(OUT) :: level
  level = wrf_debug_level
  RETURN
END SUBROUTINE get_wrf_debug_level
SUBROUTINE wrf_debug( level , str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER , INTENT (IN) :: level
  INTEGER :: debug_level
  CHARACTER (LEN=256) :: time_str
  CHARACTER (LEN=256) :: grid_str
  CHARACTER (LEN=512) :: out_str
  if(silence/=0) return
  CALL get_wrf_debug_level( debug_level )
  IF ( level .LE. debug_level ) THEN
  CALL wrf_message( str )
  ENDIF
  RETURN
END SUBROUTINE wrf_debug
