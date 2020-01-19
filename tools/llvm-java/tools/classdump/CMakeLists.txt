set( LLVM_LINK_COMPONENTS
        ${LLVM_TARGETS_TO_BUILD}
        Core
        Support
        )

add_llvm_java_executable(classdump classdump.cpp)
target_link_libraries(classdump
        JavaCompiler)