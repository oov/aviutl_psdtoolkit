#!/usr/bin/env bash
set -eu

CUR_DIR="${PWD}"
cd "$(dirname "${BASH_SOURCE:-$0}")"

mkdir -p build/tools
cd build/tools

if [ ! -e "setup-llvm-mingw.bash" ]; then
  curl -sOL https://raw.githubusercontent.com/oov/ovbase/471d0270eb097a62b6f5edb88cec74164d9ca224/setup-llvm-mingw.bash
fi
. setup-llvm-mingw.bash --dir $PWD

cd ..

REBUILD=0
SKIP_TESTS=0
CREATE_DOCS=0
CREATE_ZIP=0
CMAKE_BUILD_TYPE=Release
ARCHS="i686 x86_64"
while [[ $# -gt 0 ]]; do
  case $1 in
    -d|--debug)
      CMAKE_BUILD_TYPE=Debug
      shift
      ;;
    -a|--arch)
      ARCHS="$2"
      shift 2
      ;;
    -r|--rebuild)
      REBUILD=1
      shift
      ;;
    -s|--skip-tests)
      SKIP_TESTS=1
      shift
      ;;
    -z|--zip)
      CREATE_ZIP=1
      CREATE_DOCS=1
      shift
      ;;
    -*|--*)
      echo "Unknown option $1"
      exit 1
      ;;
    *)
      shift
      ;;
  esac
done

for arch in $ARCHS; do
  builddir="${PWD}/${CMAKE_BUILD_TYPE}/${arch}"
  if [ "${REBUILD}" -eq 1 ] || [ ! -e "${builddir}/CMakeCache.txt" ]; then
    rm -rf "${builddir}"
    cmake -S .. -B "${builddir}" --preset ${arch} \
      -DCMAKE_BUILD_TYPE="${CMAKE_BUILD_TYPE}" \
      -DCMAKE_TOOLCHAIN_FILE="src/c/3rd/ovbase/cmake/llvm-mingw.cmake" \
      -DCMAKE_C_COMPILER="${arch}-w64-mingw32-clang"
  fi
  cmake --build "${builddir}"
  if [ "${SKIP_TESTS}" -eq 0 ]; then
    ctest --test-dir "${builddir}" --output-on-failure --output-junit testlog.xml
  fi
done

if [ "${CREATE_ZIP}" -eq 1 ]; then
  curdir="${PWD}"
  distdir="${curdir}/${CMAKE_BUILD_TYPE}/dist"
  rm -rf "${distdir}"
  mkdir -p "${distdir}"
  for target in package package_en; do
    builddir="${PWD}/${CMAKE_BUILD_TYPE}/${target}"
    if [ "${REBUILD}" -eq 1 ] || [ ! -e "${builddir}/CMakeCache.txt" ]; then
      rm -rf "${builddir}"
      cmake -S .. -B "${builddir}" --preset ${target}
    fi
    cmake --build "${builddir}"
    cp -r "${PWD}/${CMAKE_BUILD_TYPE}/i686/bin/"* "${builddir}/bin"
    cp -r "${PWD}/${CMAKE_BUILD_TYPE}/x86_64/bin/"* "${builddir}/bin"

    cd "${builddir}/bin"
    cmake -E tar cf "${distdir}/${CMAKE_BUILD_TYPE}_${target}.zip" --format=zip .
    cd "${curdir}"
  done
fi

cd "${CUR_DIR}"
