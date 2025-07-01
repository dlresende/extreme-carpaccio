# Fetch googletest

CPMAddPackage(
  NAME googletest
  GITHUB_REPOSITORY google/googletest
  GIT_TAG release-1.11.0
  VERSION 1.11.0
  OPTIONS "INSTALL_GTEST OFF" "gtest_force_shared_crt" "BUILD_GMOCK ON"
)
