#include <llvm/Java/jni.h>

/* For now we cast a java/lang/Class reference to a class record. When
 * we get proper java/lang/Class representation this will be a field
 * access. */
#define GET_CLASS_RECORD(clazz) ((struct llvm_java_class_record*) clazz)
#define GET_CLASS(classRecord) ((jclass) classRecord)

const JNIEnv llvm_java_JNIEnv;

struct llvm_java_object_base;
struct llvm_java_object_header;
struct llvm_java_class_record;
struct llvm_java_typeinfo;

struct llvm_java_object_header {
  /* gc info, hash info, locking */
    int dummy;
};

struct llvm_java_object_base {
  struct llvm_java_object_header header;
  struct llvm_java_class_record* classRecord;
};

struct llvm_java_typeinfo {
  /* The name of this class */
  const char* name;

  /* The number of super classes to java.lang.Object. */
  jint depth;

  /* The super class records up to java.lang.Object. */
  struct llvm_java_class_record** superclasses;

  /* If an interface its interface index, otherwise the last interface
   * index implemented by this class. */
  jint interfaceIndex;

  /* The interface class records this class implements. */
  struct llvm_java_class_record** interfaces;

  /* The component class record if this is an array class, null
   * otherwise. */
  struct llvm_java_class_record* component;

  /* If an array the size of its elements, otherwise 0 for classes, -1
   * for interfaces and -2 for primitive classes. */
  jint elementSize;

  /* A null terminated array of strings describing the member fields
   * of this class. A field description is the concatenation of its
   * name with its descriptor. */
  const char** fieldDescriptors;

  /* An array of offsets to fields. This is indexed the same way as
   * the field descriptor array. */
  unsigned* fieldOffsets;

  /* A null terminated array of strings describing the static fields
   * of this class. A field description is the concatenation of its
   * name with its descriptor. */
  const char** staticFieldDescriptors;

  /* An array of pointers to static fields. This is indexed the
   * same way as the static field descriptor array. */
  void** staticFields;

  /* A null terminated array of strings describing the dynamic methods
     of this class. A method descriptor is the concatenation of its
     name with its descriptor. */
  const char** methodDescriptors;

  /* An array of pointers to bridge functions. This is indexed the
   * same way as the dynamic method descriptor array. */
  void** methodBridges;

  /* A null terminated array of strings describing the static methods
     of this class. A method descriptor is the concatenation of its
     name with its descriptor. */
  const char** staticMethodDescriptors;

  /* An array of pointers to bridge functions. This is indexed the
   * same way as the static method descriptor array. */
  void** staticMethodBridges;
};

struct llvm_java_class_record {
  struct llvm_java_typeinfo typeinfo;
};

#define HANDLE_TYPE(TYPE) \
  struct llvm_java_##TYPE##array { \
    struct llvm_java_object_base object_base; \
    jint length; \
    j##TYPE data[0]; \
  };
#include "llvm/Java/types.def"

struct llvm_java_class_record* llvm_java_find_class_record(const char* name);
struct llvm_java_class_record* llvm_java_get_class_record(jobject obj);
void llvm_java_set_class_record(jobject obj, struct llvm_java_class_record* cr);
jboolean llvm_java_is_instance_of(jobject obj,
                                  struct llvm_java_class_record* cr);
struct llvm_java_class_record* llvm_java_get_superclass_record(
  struct llvm_java_class_record* cr);
jboolean llvm_java_is_assignable_from(struct llvm_java_class_record* cr,
                                      struct llvm_java_class_record* from);
jint llvm_java_throw(jthrowable obj);
