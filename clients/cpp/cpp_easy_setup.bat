
setlocal

set OS=win64
set ARCH=x64
set ARCHIVE_EXTENSION=zip
set CMAKE_BIN_DIR=bin
set CMAKE=cmake.exe
set CTEST=ctest.exe
set CMAKE_GENERATOR_OPTIONS=-G "Visual Studio 15 2017 Win64"

set CMAKE_VERSION=3.19.3
set CMAKE_EXPECTED_DIR=cmake-%CMAKE_VERSION%-%OS%-%ARCH%
set CMAKE_EXPECTED_ARCHIVE_FILE=%CMAKE_EXPECTED_DIR%.%ARCHIVE_EXTENSION%
set CMAKE_ARCHIVE_URL="http://github.com/Kitware/CMake/releases/download/v%CMAKE_VERSION%/%CMAKE_EXPECTED_ARCHIVE_FILE%"
set CMAKE_HOME=cmake-%OS%-%ARCH%

set BUILD_DIR=build
if not exist %BUILD_DIR% (
    mkdir %BUILD_DIR%
)
pushd %BUILD_DIR%

set CMAKE_BUILD_DIR=cmake
if not exist %CMAKE_BUILD_DIR% (
    mkdir %CMAKE_BUILD_DIR%
)
pushd %CMAKE_BUILD_DIR%

if not exist %CMAKE_EXPECTED_ARCHIVE_FILE% (
	powershell -command "Invoke-WebRequest %CMAKE_ARCHIVE_URL% -OutFile %CMAKE_EXPECTED_ARCHIVE_FILE%"
    powershell -command "Expand-Archive -Force '%~dp0\%BUILD_DIR%\%CMAKE_BUILD_DIR%\%CMAKE_EXPECTED_ARCHIVE_FILE%' '%~dp0\%BUILD_DIR%\%CMAKE_BUILD_DIR%'"
	powershell -command "Rename-Item %CMAKE_EXPECTED_DIR% %CMAKE_HOME%"
)

pushd ..

set CMAKE_BIN_PATH=%CMAKE_BUILD_DIR%\%CMAKE_HOME%\%CMAKE_BIN_DIR%

%CMAKE_BIN_PATH%\%CMAKE% %CMAKE_GENERATOR_OPTIONS% -S .. -B .
%CMAKE_BIN_PATH%\%CMAKE% --build . --config Debug
%CMAKE_BIN_PATH%\%CTEST% --output-on-failure -C Debug

popd

popd

popd

