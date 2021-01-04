cmake_minimum_required(VERSION 3.14)

project(CMM)

SET(GCC_FPERMISSIVE_FLAGS "-fpermissive")

file(MAKE_DIRECTORY generated)

if(NOT WIN32)
find_package(FLEX)
find_package(BISON)

flex_target(cmmflex token.l ${CMAKE_CURRENT_BINARY_DIR}/generated/token.cpp)

bison_target(cmmbison
    grammar.y ${CMAKE_BINARY_DIR}/generated/grammar.y.cpp
    DEFINES_FILE ${CMAKE_BINARY_DIR}/generated/grammar.tab.h
)
else()
    set(FLEX_cmmflex_OUTPUTS ${CMAKE_CURRENT_SOURCE_DIR}/generated/token.cpp)
    set(BISON_cmmbison_OUTPUTS ${CMAKE_CURRENT_SOURCE_DIR}/generated/grammar.y.cpp)
endif()
SET(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} ${GCC_FPERMISSIVE_FLAGS}")

add_definitions(${GCC_FPERMISSIVE_FLAGS})

add_executable(CMM
        types.h types.cpp main.cpp
    ${FLEX_cmmflex_OUTPUTS} ${BISON_cmmbison_OUTPUTS}
        ast.cpp)


if(NOT WIN32)
    target_link_libraries(CMM fl)
endif()
target_include_directories(CMM PRIVATE ${CMAKE_SOURCE_DIR})
