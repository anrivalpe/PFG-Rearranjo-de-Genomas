cmake_minimum_required(VERSION 3.3)

project(dist)

option(CXX "enable C++ compilation" ON)
enable_language(CXX)

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR})
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release")
endif()
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS ON)

set(CMAKE_CXX_STANDARD 11)
file(GLOB MISC misc/*.hpp misc/*.cpp)
file(GLOB GRIMM GRIMM/*.h GRIMM/*.c)
file(GLOB DIST distance_algorithms/*.hpp distance_algorithms/*.cpp distance_algorithms/perm/*.c distance_algorithms/perm/*.h distance_algorithms/perm/*/*.c distance_algorithms/perm/*/*.h)
file(GLOB EXTER external/*.hpp external/*.cpp)
file(GLOB CYCLE cycle/*.hpp cycle/*.cpp)
file(GLOB HEUR heur/*.hpp heur/*.cpp)

set(CMAKE_CXX_STANDARD 11)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
        set(CXX_FILESYSTEM_LIBRARIES "stdc++fs")
endif()

#################################################################################################
# dist
#################################################################################################

find_package(OpenMP REQUIRED)
add_executable(${CMAKE_PROJECT_NAME} main_dist.cpp ${MISC} ${DIST} ${EXTER} ${CYCLE} ${GRIMM})
target_compile_options(dist PRIVATE -Wall PRIVATE "${OpenMP_CXX_FLAGS}")
target_link_libraries(dist PUBLIC ${CXX_FILESYSTEM_LIBRARIES} PRIVATE "${OpenMP_CXX_FLAGS}")

#################################################################################################
# decomposition
#################################################################################################

find_package(OpenMP REQUIRED)
add_executable(dec main_dec.cpp ${MISC} ${CYCLE} ${HEUR} ${GRIMM})
target_compile_options(dec PRIVATE -Wall PRIVATE "${OpenMP_CXX_FLAGS}")
target_link_libraries(dec PUBLIC ${CXX_FILESYSTEM_LIBRARIES} PRIVATE "${OpenMP_CXX_FLAGS}")

#################################################################################################


if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_CURRENT_SOURCE_DIR})
    include(FeatureSummary)
    feature_summary(WHAT ALL)
endif()
