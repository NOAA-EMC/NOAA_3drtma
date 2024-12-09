set(CMAKE_C_COMPILER "/opt/cray/pe/craype/2.7.10/bin/cc")
set(CMAKE_C_COMPILER_ARG1 "")
set(CMAKE_C_COMPILER_ID "Intel")
set(CMAKE_C_COMPILER_VERSION "19.1.3.20200925")
set(CMAKE_C_COMPILER_VERSION_INTERNAL "")
set(CMAKE_C_COMPILER_WRAPPER "")
set(CMAKE_C_STANDARD_COMPUTED_DEFAULT "11")
set(CMAKE_C_COMPILE_FEATURES "c_std_90;c_function_prototypes;c_std_99;c_restrict;c_variadic_macros;c_std_11;c_static_assert")
set(CMAKE_C90_COMPILE_FEATURES "c_std_90;c_function_prototypes")
set(CMAKE_C99_COMPILE_FEATURES "c_std_99;c_restrict;c_variadic_macros")
set(CMAKE_C11_COMPILE_FEATURES "c_std_11;c_static_assert")

set(CMAKE_C_PLATFORM_ID "Linux")
set(CMAKE_C_SIMULATE_ID "GNU")
set(CMAKE_C_COMPILER_FRONTEND_VARIANT "")
set(CMAKE_C_SIMULATE_VERSION "7.5.0")




set(CMAKE_AR "/usr/bin/ar")
set(CMAKE_C_COMPILER_AR "")
set(CMAKE_RANLIB "/usr/bin/ranlib")
set(CMAKE_C_COMPILER_RANLIB "")
set(CMAKE_LINKER "/usr/bin/ld")
set(CMAKE_MT "")
set(CMAKE_COMPILER_IS_GNUCC )
set(CMAKE_C_COMPILER_LOADED 1)
set(CMAKE_C_COMPILER_WORKS TRUE)
set(CMAKE_C_ABI_COMPILED TRUE)
set(CMAKE_COMPILER_IS_MINGW )
set(CMAKE_COMPILER_IS_CYGWIN )
if(CMAKE_COMPILER_IS_CYGWIN)
  set(CYGWIN 1)
  set(UNIX 1)
endif()

set(CMAKE_C_COMPILER_ENV_VAR "CC")

if(CMAKE_COMPILER_IS_MINGW)
  set(MINGW 1)
endif()
set(CMAKE_C_COMPILER_ID_RUN 1)
set(CMAKE_C_SOURCE_FILE_EXTENSIONS c;m)
set(CMAKE_C_IGNORE_EXTENSIONS h;H;o;O;obj;OBJ;def;DEF;rc;RC)
set(CMAKE_C_LINKER_PREFERENCE 10)

# Save compiler ABI information.
set(CMAKE_C_SIZEOF_DATA_PTR "8")
set(CMAKE_C_COMPILER_ABI "ELF")
set(CMAKE_C_LIBRARY_ARCHITECTURE "")

if(CMAKE_C_SIZEOF_DATA_PTR)
  set(CMAKE_SIZEOF_VOID_P "${CMAKE_C_SIZEOF_DATA_PTR}")
endif()

if(CMAKE_C_COMPILER_ABI)
  set(CMAKE_INTERNAL_PLATFORM_ABI "${CMAKE_C_COMPILER_ABI}")
endif()

if(CMAKE_C_LIBRARY_ARCHITECTURE)
  set(CMAKE_LIBRARY_ARCHITECTURE "")
endif()

set(CMAKE_C_CL_SHOWINCLUDES_PREFIX "")
if(CMAKE_C_CL_SHOWINCLUDES_PREFIX)
  set(CMAKE_CL_SHOWINCLUDES_PREFIX "${CMAKE_C_CL_SHOWINCLUDES_PREFIX}")
endif()





set(CMAKE_C_IMPLICIT_INCLUDE_DIRECTORIES "/opt/cray/pe/mpich/8.1.7/ofi/intel/19.0/include;/pe/intel/compilers_and_libraries_2020.4.304/linux/pstl/include;/apps/prod/hpc-stack/intel-19.1.3.304/cray-mpich-8.1.4/esmf/7_1_0r/include;/apps/prod/hpc-stack/intel-19.1.3.304/cray-mpich-8.1.4/netcdf/4.7.4/include;/apps/prod/hpc-stack/intel-19.1.3.304/cray-mpich-8.1.4/hdf5/1.10.6/include;/pe/intel/compilers_and_libraries_2020.4.304/linux/ipp/include;/pe/intel/compilers_and_libraries_2020.4.304/linux/mkl/include;/pe/intel/compilers_and_libraries_2020.4.304/linux/pstl/stdlib;/pe/intel/compilers_and_libraries_2020.4.304/linux/tbb/include;/apps/spack/libjpeg/9c/intel/19.1.3.304/jkr3isi257ktoouprwaxcn4twtye747z/include;/apps/prod/xmlparse/2.0.0/intel/19.1.3.304/xmlparse-2.0.0/include;/apps/spack/libpng/1.6.37/intel/19.1.3.304/4ohkronuhlyherusoszzrmur5ewvlwzh/include;/apps/prod/nco/4.7.9/intel/19.1.3.304/include;/apps/spack/zlib/1.2.11/intel/19.1.3.304/hjotqkckeoyt6j6tibalwzrlfljcjtdh/include;/apps/spack/jasper/2.0.25/intel/19.1.3.304/sjib74krrorkyczqpqah4tvewmlnqdx4/include;/pe/intel/compilers_and_libraries_2020.4.304/linux/compiler/include/intel64;/pe/intel/compilers_and_libraries_2020.4.304/linux/compiler/include/icc;/pe/intel/compilers_and_libraries_2020.4.304/linux/compiler/include;/usr/lib64/gcc/x86_64-suse-linux/7/include;/usr/lib64/gcc/x86_64-suse-linux/7/include-fixed;/usr/include")
set(CMAKE_C_IMPLICIT_LINK_LIBRARIES "mpi_intel;imf;m;ifcoremt;ifport;pthread;imf;svml;irng;m;ipgo;decimal;cilkrts;stdc++;gcc;gcc_s;irc;svml;c;gcc;gcc_s;irc_s;dl;c")
set(CMAKE_C_IMPLICIT_LINK_DIRECTORIES "/opt/cray/pe/mpich/8.1.7/ofi/intel/19.0/lib;/apps/spack/libjpeg/9c/intel/19.1.3.304/jkr3isi257ktoouprwaxcn4twtye747z/lib;/apps/prod/xmlparse/2.0.0/intel/19.1.3.304/xmlparse-2.0.0/lib;/apps/spack/libpng/1.6.37/intel/19.1.3.304/4ohkronuhlyherusoszzrmur5ewvlwzh/lib;/apps/prod/hpc-stack/intel-19.1.3.304/cray-mpich-8.1.4/esmf/7_1_0r/lib;/apps/prod/nco/4.7.9/intel/19.1.3.304/lib64;/apps/spack/zlib/1.2.11/intel/19.1.3.304/hjotqkckeoyt6j6tibalwzrlfljcjtdh/lib;/apps/spack/jasper/2.0.25/intel/19.1.3.304/sjib74krrorkyczqpqah4tvewmlnqdx4/lib64;/pe/intel/compilers_and_libraries_2020.4.304/linux/ipp/lib/intel64;/pe/intel/compilers_and_libraries_2020.4.304/linux/compiler/lib/intel64_lin;/pe/intel/compilers_and_libraries_2020.4.304/linux/mkl/lib/intel64_lin;/pe/intel/compilers_and_libraries_2020.4.304/linux/tbb/lib/intel64/gcc4.8;/usr/lib64/gcc/x86_64-suse-linux/7;/usr/lib64;/lib64;/usr/x86_64-suse-linux/lib;/lib;/usr/lib")
set(CMAKE_C_IMPLICIT_LINK_FRAMEWORK_DIRECTORIES "")
