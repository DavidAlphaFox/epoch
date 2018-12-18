@echo on
rem Matrix-driven Appveyor CI test script
rem Currently only supports MSYS2 builds.
rem See https://www.appveyor.com/docs/installed-software#mingw-msys-cygwin
rem Required vars:
rem    TEST
rem    MSYS2_ROOT

SETLOCAL ENABLEEXTENSIONS
cd %APPVEYOR_BUILD_FOLDER%

rem Set the paths appropriately

call "C:\Program Files (x86)\Microsoft Visual Studio %VS_VERSION%\VC\vcvarsall.bat" %PLATFORM%
SET PATH=%MSYS2_ROOT%\mingw64\bin;%MSYS2_ROOT%\usr\bin;%PATH%

GOTO TEST_%TEST%

:TEST_ct
rem Run test: ct
bash -lc "cd %BUILD_PATH% && epmd -daemon && make ct"

GOTO TEST_DONE

:TEST_eunit
rem Run test: eunit
bash -lc "cd %BUILD_PATH% && epmd -daemon && make eunit"

GOTO TEST_DONE

:TEST_DONE
rem Finished test phase
