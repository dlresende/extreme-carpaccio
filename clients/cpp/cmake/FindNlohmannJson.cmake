# Fetch nlohmann_json

CPMFindPackage(
  NAME nlohmann_json
  VERSION 3.6.1
  # the git repo is incredibly large, so we download the archived include directory
  URL https://github.com/nlohmann/json/releases/download/v3.6.1/include.zip
  URL_HASH SHA256=69cc88207ce91347ea530b227ff0776db82dcb8de6704e1a3d74f4841bc651cf
)

if (nlohmann_json_ADDED)
  add_library(nlohmann_json INTERFACE IMPORTED)
  target_include_directories(nlohmann_json INTERFACE ${nlohmann_json_SOURCE_DIR})
endif()

# This is the latest package provided on CPM repo, but does not seem to work

#CPMAddPackage(
#  NAME nlohmann_json
#  VERSION 3.9.1
#  OPTIONS
#    "JSON_BuildTests OFF"
#)