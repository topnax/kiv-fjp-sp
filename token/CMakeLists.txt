cmake_minimum_required(VERSION 3.14)

project(CMM)

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

add_executable(CMM
        types.h types.cpp main.cpp
    ${FLEX_cmmflex_OUTPUTS} ${BISON_cmmbison_OUTPUTS}
)

if(NOT WIN32)
    target_link_libraries(CMM fl)
endif()
target_include_directories(CMM PRIVATE ${CMAKE_SOURCE_DIR})
