set( LLVM_LINK_COMPONENTS
        ${LLVM_TARGETS_TO_BUILD}
        BitWriter
        Core
        Support
        )

add_llvm_java_executable(class2llvm class2llvm.cpp)
target_link_libraries(class2llvm
        JavaCompiler
        )