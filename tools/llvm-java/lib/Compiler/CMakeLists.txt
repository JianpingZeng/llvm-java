set( LLVM_LINK_COMPONENTS
        ${LLVM_TARGETS_TO_BUILD}
        BitWriter
        Core
        Support
        )

add_llvm_library(JavaCompiler
        Compiler.cpp
        Locals.cpp
        OperandStack.cpp
        Resolver.cpp
        VMClass.cpp
        VMField.cpp
        VMMethod.cpp

        DEPENDS
        ${codegen_deps}

        LINK_LIBS
        JavaCompiler
        JavaClassfile
        )