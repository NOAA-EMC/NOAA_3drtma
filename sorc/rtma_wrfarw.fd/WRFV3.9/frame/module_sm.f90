MODULE module_sm
   INTEGER , EXTERNAL :: omp_get_num_threads , &
                         omp_get_max_threads , &
                         omp_get_thread_num , &
                         omp_get_num_procs
   LOGICAL , EXTERNAL :: omp_in_parallel
CONTAINS
   SUBROUTINE omp_info
      IMPLICIT NONE
      PRINT '(/A,/,A,/,A,I2/)','omp_get_num_threads:', &
                              'Number of threads currently in the team executing', &
                              'the parallel region = ',omp_get_num_threads()
      PRINT '(A,/,A,/,A,I2/)', 'omp_get_max_threads:', &
                              'Maximum value that can be returned by the',&
                              'omp_get_num_threads function = ',omp_get_max_threads()
      PRINT '(A,/,A,/,A,I2/)', 'omp_get_thread_num:', &
                              'Returns the thread number, within the team, between', &
                              '0 and omp_get_num_threads-1, inclusive = ',omp_get_thread_num()
      PRINT '(A,/,A,/,A,I2/)', 'omp_get_num_procs:', &
                              'Returns the number of processors that are available', &
                              'to the program = ',omp_get_num_procs()
      PRINT '(A,/,A,/,A,L7/)','omp_in_parallel:', &
                              'Returns .TRUE. if called with the dynamic extent of a region', &
                              'executing in parallel, and otherwise .FALSE. = ',omp_in_parallel()
   END SUBROUTINE omp_info
   SUBROUTINE init_module_sm
   END SUBROUTINE init_module_sm
END MODULE module_sm
