include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# Support plugins.
set(LLVM_NO_DEAD_STRIP 1)
macro(add_llvm_java_executable name)
    add_llvm_executable( ${name} ${ARGN} )
    set_target_properties(${name} PROPERTIES FOLDER "LLVM Java executables")
endmacro(add_llvm_java_executable)

add_subdirectory(lib)
add_subdirectory(runtime)
add_subdirectory(tools)