list(APPEND src_obslist 
  get_ob_lists3d.f90
  gridspecs.f90
  mod_rll_trans.f90
  prdgen.f90
  weather_report_status_list.f90
  latlon_to_grid.f
  w3fb06_8.f
  w3fb08_8.f
  w3fb11_8.f)

add_executable(obslist.exe ${src_obslist})
target_link_libraries(obslist.exe PRIVATE w3emc::w3emc_4
                                    PRIVATE w3nco::w3nco_4
                                    PRIVATE g2::g2_4
                                    PRIVATE g2tmpl::g2tmpl
                                    PRIVATE bacio::bacio_4
                                    PRIVATE wgrib2::wgrib2
                                    PRIVATE ip::ip_4)

if(OpenMP_Fortran_FOUND)
  target_link_libraries(obslist.exe PRIVATE OpenMP::OpenMP_Fortran)
endif()

install(
  TARGETS obslist.exe
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS
  "-fp-model strict -convert big_endian -traceback -free -integer-size 64 -real-size 64")
   set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
   set(CMAKE_Fortran_FLAGS_DEBUG "-O0")
endif()
