cmake_minimum_required(VERSION 3.16)

project(CMM)

file(MAKE_DIRECTORY generated)

find_package(FLEX)
find_package(BISON)

flex_target(cmmflex token.l ${CMAKE_CURRENT_BINARY_DIR}/generated/token.c)

bison_target(cmmbison
    grammar.y ${CMAKE_BINARY_DIR}/generated/grammar.y.c
    DEFINES_FILE ${CMAKE_BINARY_DIR}/generated/grammar.tab.h
)

add_executable(CMM
    main.c
    types.h types.c
    ${FLEX_cmmflex_OUTPUTS} ${BISON_cmmbison_OUTPUTS}
)

target_link_libraries(CMM fl)
target_include_directories(CMM PRIVATE ${CMAKE_SOURCE_DIR})
