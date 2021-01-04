## build bufr lib and wrfio lib

This repo is required becuase NCEPLIBS does not release bufr lib and wrfio lib at this moment. In the future, when NCEPLIBS includes these two libraries, this repo will not be needed.

You can build these two libraries as follows:

```
git clone https://github.com/comgsi/GSILIBS
cd GSILIBS
mkdir build; cd build
cmake -DBUILD_CORELIBS=ON ..
make -j8
```

The compiled libraries will be under build/lib
