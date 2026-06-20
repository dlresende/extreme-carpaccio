#!/usr/bin/env sh

set -eu

case $(uname -s) in
Darwin)
    os="macos"
    arch="universal"
    archive_extension="tar.gz"
    cmake_bin_dir="CMake.app/Contents/bin"
    cmake="cmake"
    ctest="ctest"
    cmake_generator_options="-G \"Xcode\""
    ;;
Linux)
    os="Linux"
    arch="x86_64"
    archive_extension="tar.gz"
    cmake_bin_dir="bin"
    cmake="cmake"
    ctest="ctest"
    cmake_generator_options="-G \"Unix Makefiles\""
    ;;
MINGW64_NT-*)
    os="win64"
    arch="x64"
    archive_extension="zip"
    cmake_bin_dir="bin"
    cmake="cmake.exe"
    ctest="ctest.exe"
    cmake_generator_options="-G \"Visual Studio 15 2017 Win64\""
    ;;
*)
    echo "os $(uname -s) is currently not supported."
    exit 1
    ;;
esac

base_dir=$(dirname -- "$0")
cd "${base_dir}"

cmake_version="3.19.3"
cmake_expected_dir="cmake-${cmake_version}-${os}-${arch}"
cmake_expected_archive_file="${cmake_expected_dir}.${archive_extension}"
cmake_archive_url="http://github.com/Kitware/CMake/releases/download/v${cmake_version}/${cmake_expected_archive_file}"
cmake_home="cmake-${os}-${arch}"

build_dir="build"
mkdir -p "${build_dir}"
cd "${build_dir}"

cmake_build_dir="cmake"
mkdir -p "${cmake_build_dir}"
cd "${cmake_build_dir}"

if ! [ -f "${cmake_expected_archive_file}" ]
then
  echo "-- Downloading ${cmake_expected_archive_file}"
  curl -# -L "${cmake_archive_url}" -o "${cmake_expected_archive_file}"

  echo "-- Extracting cmake ${cmake_version}"
  case "${archive_extension}" in
  zip)
    unzip -q -o "${cmake_expected_archive_file}"
    ;;
  tar.gz)
    tar zxf "${cmake_expected_archive_file}"
    ;;
  *)
    echo "Archive format ${archive_extension} is currently not supported."
    exit 1
    ;;
  esac

  [ -d "${cmake_home}" ] && rm -Rf "${cmake_home}"
  mv "${cmake_expected_dir}" "${cmake_home}"
fi

cd ..

cmake_bin_path="${cmake_build_dir}/${cmake_home}/${cmake_bin_dir}"

eval ${cmake_bin_path}/${cmake} "${cmake_generator_options}" -S .. -B .
${cmake_bin_path}/${cmake} --build . --config Debug
${cmake_bin_path}/${ctest} --output-on-failure -C Debug
