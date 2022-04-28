# Fetch Boost

CPMAddPackage(
   NAME orphisboost
   GITHUB_REPOSITORY Orphis/boost-cmake
   GIT_TAG 7f97a08b64bd5d2e53e932ddf80c40544cf45edf
   VERSION 1.71.0
   OPTIONS "BOOST_LOCALE_ENABLE_ICU_BACKEND OFF"
)
