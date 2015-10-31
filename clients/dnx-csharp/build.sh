@echo off
echo ===============================
echo -     Xtrem Carpaccio!        -
echo ===============================

echo ========    RESTORE DEPENDENCIES   =============
dnu commands install Microsoft.Dnx.Watcher
dnu restore

echo ========    BUILD SOURCES   =============
cd src/xcarpaccio/
dnu build
cd ../..
echo ========    BUILD TESTS   =============
cd test/xcarpaccio.tests/
dnu build
echo ========    RUN TESTS   =============
dnx test
cd ../../
