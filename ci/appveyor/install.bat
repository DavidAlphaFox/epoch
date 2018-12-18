@echo on
rem Matrix-driven Appveyor CI install script
rem Currently only supports MSYS2 builds.
rem See https://www.appveyor.com/docs/installed-software#mingw-msys-cygwin
rem Required vars:
rem    ERTS_VERSION
rem    OTP_VERSION
rem    MSYS2_ROOT

SETLOCAL ENABLEEXTENSIONS
cd %APPVEYOR_BUILD_FOLDER%

rem Set the paths appropriately

call "C:\Program Files (x86)\Microsoft Visual Studio %VS_VERSION%\VC\vcvarsall.bat" %PLATFORM%
SET PATH=%MSYS2_ROOT%\mingw64\bin;%MSYS2_ROOT%\usr\bin;%PATH%

rem Upgrade the MSYS2 platform

bash -lc "pacman --noconfirm --needed -Sy pacman"
bash -lc "pacman --noconfirm -Su"

rem Install required tools

bash -lc "pacman --noconfirm --needed -S base-devel cmake curl gcc gcc-fortran git make patch"
bash -lc "pacman --noconfirm --needed -S mingw-w64-x86_64-SDL mingw-w64-x86_64-libsodium mingw-w64-x86_64-ntldd-git mingw-w64-x86_64-yasm"
bash -lc "pacman --noconfirm --needed -S mingw-w64-x86_64-gcc mingw-w64-x86_64-gcc-ada mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-gcc-objc"

rem Ensure Erlang/OTP %OTP_VERSION% is installed

IF EXIST "%OTP_PATH%%ERTS_VERSION%\bin\" GOTO OTPINSTALLED
SET OTP_PACKAGE=otp_win64_%OTP_VERSION%.exe
PowerShell -Command "Invoke-WebRequest http://erlang.org/download/%OTP_PACKAGE% -OutFile %TMP%\%OTP_PACKAGE%"
START "" /WAIT "%TMP%\%OTP_PACKAGE%" /S
:OTPINSTALLED

rem Set up msys2 env variables

COPY ci\appveyor\env_build.sh %MSYS2_ROOT%\etc\profile.d\env_build.sh

rem Finished installation phase
