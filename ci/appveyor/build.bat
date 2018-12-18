@echo on
rem Matrix-driven Appveyor CI build script
rem Currently only supports MSYS2 builds.
rem See https://www.appveyor.com/docs/installed-software#mingw-msys-cygwin
rem Required vars:
rem    JOB
rem    MSYS2_ROOT

SETLOCAL ENABLEEXTENSIONS
cd %APPVEYOR_BUILD_FOLDER%

rem Set the paths appropriately

call "C:\Program Files (x86)\Microsoft Visual Studio %VS_VERSION%\VC\vcvarsall.bat" %PLATFORM%
SET PATH=%MSYS2_ROOT%\mingw64\bin;%MSYS2_ROOT%\usr\bin;%PATH%

GOTO JOB_%JOB%

:JOB_build
rem Run job: build
bash -lc "cd %BUILD_PATH% && make KIND=test local-build"

GOTO BUILD_DONE

:BUILD_DONE
rem Finished build phase
