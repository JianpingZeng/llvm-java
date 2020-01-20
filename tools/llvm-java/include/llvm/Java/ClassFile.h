//===-- ClassFile.h - ClassFile support library -----------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the ClassFile and subordinate
// classes, which represent a parsed class file.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_JAVA_CLASSFILE_H
#define LLVM_JAVA_CLASSFILE_H

#include <iosfwd>
#include <stdexcept>
#include <vector>

#include <llvm/Support/DataTypes.h>

namespace llvm { namespace Java {

  // Forward declarations
  class Attribute;
  class ConstantValueAttribute;
  class CodeAttribute;
  class ExceptionsAttribute;
  class Constant;
  class ConstantClass;
  class ConstantFieldRef;
  class ConstantInterfaceMethodRef;
  class ConstantMemberRef;
  class ConstantMethodRef;
  class ConstantNameAndType;
  class ConstantUtf8;
  class ClassFile;
  class Field;
  class Method;

  enum AccessFlag {
    ACC_PUBLIC       = 0x0001,
    ACC_PRIVATE      = 0x0002,
    ACC_PROTECTED    = 0x0004,
    ACC_STATIC       = 0x0008,
    ACC_FINAL        = 0x0010,
    ACC_SUPER        = 0x0020,
    ACC_SYNCHRONIZED = 0x0020,
    ACC_VOLATILE     = 0x0040,
    ACC_TRANSIENT    = 0x0080,
    ACC_NATIVE       = 0x0100,
    ACC_INTERFACE    = 0x0200,
    ACC_ABSTRACT     = 0x0400,
    ACC_STRICT       = 0x0800,
  };

  typedef std::vector<Constant*> ConstantPool;
  typedef std::vector<Field*> Fields;
  typedef std::vector<Method*> Methods;
  typedef std::vector<Attribute*> Attributes;

  Attribute* getAttribute( Attributes& attrs,
                           std::string& name);

  class ClassFile {
    static  ClassFile* readClassFile(std::istream& is);
    static std::vector<std::string> getClassPath();
    static std::string getFileForClass( std::string& classname);

  public:
    static  ClassFile* get( std::string& classname);

    ~ClassFile();

    uint16_t getMinorVersion()  { return minorV_; }
    uint16_t getMajorVersion()  { return majorV_; }

    unsigned getNumConstants()  { return cPool_.size(); }
    Constant* getConstant(unsigned index)  { return cPool_[index]; }
    ConstantClass* getConstantClass(unsigned index) ;
    ConstantMemberRef* getConstantMemberRef(unsigned index) ;
    ConstantFieldRef* getConstantFieldRef(unsigned index) ;
    ConstantMethodRef* getConstantMethodRef(unsigned index) ;
    ConstantInterfaceMethodRef*
    getConstantInterfaceMethodRef(unsigned index) ;
    ConstantNameAndType* getConstantNameAndType(unsigned index) ;
    ConstantUtf8* getConstantUtf8(unsigned index) ;

    bool isPublic()  { return accessFlags_ & ACC_PUBLIC; }
    bool isFinal()  { return accessFlags_ & ACC_FINAL; }
    bool isSuper()  { return accessFlags_ & ACC_SUPER; }
    bool isInterface()  { return accessFlags_ & ACC_INTERFACE; }
    bool isAbstract()  { return accessFlags_ & ACC_ABSTRACT; }

    unsigned getThisClassIndex()  { return thisClassIdx_; }
    ConstantClass* getThisClass()  {
      return getConstantClass(thisClassIdx_);
    }
    unsigned getSuperClassIndex()  { return superClassIdx_; }
    ConstantClass* getSuperClass()  {
      return superClassIdx_ ? getConstantClass(superClassIdx_) : NULL;
    }

    unsigned getNumInterfaces()  { return interfaces_.size(); }
    unsigned getInterfaceIndex(unsigned i)  { return interfaces_[i]; }
    ConstantClass* getInterface(unsigned i)  {
      return getConstantClass(getInterfaceIndex(i));
    }

     Fields& getFields()  { return fields_; }

     Methods& getMethods()  { return methods_; }

     Attributes& getAttributes()  { return attributes_; }

    bool isNativeMethodOverloaded( Method& method) ;

    std::ostream& dump(std::ostream& os) ;

  private:
    uint16_t majorV_;
    uint16_t minorV_;
    ConstantPool cPool_;
    uint16_t accessFlags_;
    uint16_t thisClassIdx_;
    uint16_t superClassIdx_;
    std::vector<uint16_t> interfaces_;
    Fields fields_;
    Methods methods_;
    Attributes attributes_;

    ClassFile(std::istream& is);
  };

  inline std::ostream& operator<<(std::ostream& os,  ClassFile& c) {
    return c.dump(os);
  }

  class Constant {
  public:
    enum Tag {
      CLASS = 7,
      FIELD_REF = 9,
      METHOD_REF = 10,
      INTERFACE_METHOD_REF = 11,
      STRING = 8,
      INTEGER = 3,
      FLOAT = 4,
      LONG = 5,
      DOUBLE = 6,
      NAME_AND_TYPE = 12,
      UTF8 = 1
    };

    static Constant* readConstant( ClassFile* cf, std::istream& is);

    virtual bool isSingleSlot() { return true; }
    bool isDoubleSlot() { return !isSingleSlot(); }
     ClassFile* getParent() { return parent_; }

    virtual ~Constant();

    virtual std::ostream& dump(std::ostream& os)  = 0;
    Constant::Tag getKind() const { return kind; }

  protected:
    ClassFile* parent_;
    Constant::Tag kind;
    Constant(ClassFile* cf, Constant::Tag kind_)
        : parent_(cf), kind(kind_) { }
  };

  inline std::ostream& operator<<(std::ostream& os,  Constant& c) {
    return c.dump(os);
  }

  class ConstantClass : public Constant {
    uint16_t nameIdx_;
  public:
    ConstantClass( ClassFile* cf, std::istream& is);
    unsigned getNameIndex()  { return nameIdx_; }
    ConstantUtf8* getName()  {
      return parent_->getConstantUtf8(nameIdx_);
    }
    std::ostream& dump(std::ostream& os) ;
    static bool classof(const Constant *C) {
      return C->getKind() == Constant::CLASS;
    }
  };

  class ConstantMemberRef : public Constant {
  protected:
    uint16_t classIdx_;
    uint16_t nameAndTypeIdx_;
    ConstantMemberRef( ClassFile* cf, std::istream& is, Constant::Tag kind);

  public:
    unsigned getClassIndex()  { return classIdx_; }
    ConstantClass* getClass()  {
      return parent_->getConstantClass(classIdx_);
    }
    unsigned getNameAndTypeIndex()  { return nameAndTypeIdx_; }
    ConstantNameAndType* getNameAndType()  {
      return parent_->getConstantNameAndType(nameAndTypeIdx_);
    }
  };

  class ConstantFieldRef : public ConstantMemberRef {
  public:
    ConstantFieldRef( ClassFile* cf, std::istream& is)
      : ConstantMemberRef(cf, is, Constant::FIELD_REF) { }
    std::ostream& dump(std::ostream& os);

    static bool classof(const Constant *C) {
      return C->getKind() == Constant::FIELD_REF;
    }
  };

  class ConstantMethodRef : public ConstantMemberRef {
  public:
    ConstantMethodRef( ClassFile* cf, std::istream& is)
      : ConstantMemberRef(cf, is, Constant::FIELD_REF) { }
    std::ostream& dump(std::ostream& os);
    static bool classof(const Constant *C) {
      return C->getKind() == Constant::FIELD_REF;
    }
  };

  class ConstantInterfaceMethodRef : public ConstantMemberRef {
  public:
    ConstantInterfaceMethodRef( ClassFile* cf, std::istream& is)
      : ConstantMemberRef(cf, is, Constant::INTERFACE_METHOD_REF) { }
    std::ostream& dump(std::ostream& os);
    static bool classof(const Constant *C) {
      return C->getKind() == Constant::INTERFACE_METHOD_REF;
    }
  };

  class ConstantString : public Constant {
    uint16_t stringIdx_;
  public:
    ConstantString( ClassFile* cf, std::istream& is);
    unsigned getStringIndex()  { return stringIdx_; }
    ConstantUtf8* getValue()  {
      return parent_->getConstantUtf8(stringIdx_);
    }
    std::ostream& dump(std::ostream& os);
    static bool classof(const Constant *C) {
      return C->getKind() == Constant::STRING;
    }
  };

  class ConstantInteger : public Constant {
    int32_t value_;
  public:
    ConstantInteger( ClassFile* cf, std::istream& is);
    int32_t getValue()  { return value_; }
    std::ostream& dump(std::ostream& os);
    static bool classof(const Constant *C) {
      return C->getKind() == Constant::INTEGER;
    }
  };

  class ConstantFloat : public Constant {
    float value_;
  public:
    ConstantFloat( ClassFile* cf, std::istream& is);
    float getValue()  { return value_; }
    std::ostream& dump(std::ostream& os);
    static bool classof(const Constant *C) {
      return C->getKind() == Constant::FLOAT;
    }
  };

  class ConstantLong : public Constant {
    int64_t value_;
  public:
    ConstantLong( ClassFile* cf, std::istream& is);
    virtual bool isSingleSlot() { return false; }
    int64_t getValue()  { return value_; }
    std::ostream& dump(std::ostream& os) ;
    static bool classof(const Constant *C) {
      return C->getKind() == Constant::LONG;
    }
  };

  class ConstantDouble : public Constant {
    double value_;
  public:
    ConstantDouble( ClassFile* cf, std::istream& is);
    virtual bool isSingleSlot() { return false; }
    double getValue()  { return value_; }
    std::ostream& dump(std::ostream& os);
    static bool classof(const Constant *C) {
      return C->getKind() == Constant::DOUBLE;
    }
  };

  class ConstantNameAndType : public Constant {
    uint16_t nameIdx_;
    uint16_t descriptorIdx_;
  public:
    ConstantNameAndType( ClassFile* cf, std::istream& is);
    unsigned getNameIndex()  { return nameIdx_; }
    ConstantUtf8* getName()  {
      return parent_->getConstantUtf8(nameIdx_);
    }
    unsigned getDescriptorIndex()  { return descriptorIdx_; }
    ConstantUtf8* getDescriptor()  {
      return parent_->getConstantUtf8(descriptorIdx_);
    }
    std::ostream& dump(std::ostream& os);
    static bool classof(const Constant *C) {
      return C->getKind() == Constant::NAME_AND_TYPE;
    }
  };

  class ConstantUtf8 : public Constant {
    std::string utf8_;
  public:
    ConstantUtf8( ClassFile* cf, std::istream& is);
     std::string& str()  { return utf8_; }

    std::ostream& dump(std::ostream& os);
    static bool classof(const Constant *C) {
      return C->getKind() == Constant::UTF8;
    }
  };

  class Member {
  protected:
     ClassFile* parent_;
    uint16_t accessFlags_;
    uint16_t nameIdx_;
    uint16_t descriptorIdx_;
    Attributes attributes_;

    Member( ClassFile* parent, std::istream& is);
    ~Member();

public:
    bool isPublic()  { return accessFlags_ & ACC_PUBLIC; }
    bool isPrivate()  { return accessFlags_ & ACC_PRIVATE; }
    bool isProtected()  { return accessFlags_ & ACC_PROTECTED; }
    bool isStatic()  { return accessFlags_ & ACC_STATIC; }
    bool isFinal()  { return accessFlags_ & ACC_FINAL; }

     ClassFile* getParent()  { return parent_; }
    unsigned getNameIndex()  { return nameIdx_; }
    ConstantUtf8* getName()  { return parent_->getConstantUtf8(nameIdx_); }
    unsigned getDescriptorIndex()  { return descriptorIdx_; }
    ConstantUtf8* getDescriptor()  {
      return parent_->getConstantUtf8(descriptorIdx_);
    }
     Attributes& getAttributes()  { return attributes_; }
  };

  class Field : public Member {
  private:
    Field( ClassFile* parent, std::istream& is);

  public:
    static Field* readField( ClassFile* parent, std::istream& is) {
      return new Field(parent, is);
    }

    ~Field();

    bool isVolatile()  { return accessFlags_ & ACC_VOLATILE; }
    bool isTransient()  { return accessFlags_ & ACC_TRANSIENT; }

    ConstantValueAttribute* getConstantValueAttribute() ;

    std::ostream& dump(std::ostream& os) ;
  };

  inline std::ostream& operator<<(std::ostream& os,  Field& f) {
    return f.dump(os);
  }

  class Method : public Member {
    Method( ClassFile* parent, std::istream& is);

  public:
    static Method* readMethod( ClassFile* parent, std::istream& is) {
      return new Method(parent, is);
    }

    ~Method();

    bool isSynchronized()  { return accessFlags_ & ACC_SYNCHRONIZED; }
    bool isNative()  { return accessFlags_ & ACC_NATIVE; }
    bool isAbstract()  { return accessFlags_ & ACC_ABSTRACT; }
    bool isStrict()  { return accessFlags_ & ACC_STRICT; }

    CodeAttribute* getCodeAttribute() ;
    ExceptionsAttribute* getExceptionsAttribute() ;

    std::ostream& dump(std::ostream& os) ;
  };

  inline std::ostream& operator<<(std::ostream& os,  Method& m) {
    return m.dump(os);
  }

  class Attribute {
  protected:
     ClassFile* parent_;
    uint16_t nameIdx_;

    Attribute( ClassFile* cf, uint16_t nameIdx, std::istream& is);

  public:
    static Attribute* readAttribute( ClassFile* cf, std::istream& is);

    virtual ~Attribute();

    unsigned getNameIndex()  { return nameIdx_; }
    ConstantUtf8* getName()  { return parent_->getConstantUtf8(nameIdx_); }

    virtual std::ostream& dump(std::ostream& os) ;

    static  std::string CONSTANT_VALUE;
    static  std::string CODE;
    static  std::string EXCEPTIONS;
    static  std::string INNER_CLASSES;
    static  std::string SYNTHETIC;
    static  std::string SOURCE_FILE;
    static  std::string LINE_NUMBER_TABLE;
    static  std::string LOCAL_VARIABLE_TABLE;
    static  std::string DEPRECATED;
  };

  inline std::ostream& operator<<(std::ostream& os,  Attribute& a) {
    return a.dump(os);
  }

  class ConstantValueAttribute : public Attribute {
    uint16_t valueIdx_;
  public:
    ConstantValueAttribute( ClassFile* cf,
                           uint16_t nameIdx,
                           std::istream& is);

    unsigned getValueIndex()  { return valueIdx_; }
    Constant* getValue()  { return parent_->getConstant(valueIdx_); }

    std::ostream& dump(std::ostream& os) ;
  };

  class CodeAttribute : public Attribute {
  public:
    class Exception {
       ClassFile* parent_;
      uint16_t startPc_;
      uint16_t endPc_;
      uint16_t handlerPc_;
      uint16_t catchTypeIdx_;

    public:
      Exception( ClassFile* cf, std::istream& is);

      uint16_t getStartPc()  { return startPc_; }
      uint16_t getEndPc()  { return endPc_; }
      uint16_t getHandlerPc()  { return handlerPc_; }
      uint16_t getCatchTypeIndex()  { return catchTypeIdx_; }
      ConstantClass* getCatchType()  {
        return catchTypeIdx_ ? NULL : parent_->getConstantClass(catchTypeIdx_);
      }

      std::ostream& dump(std::ostream& os) ;
    };

    typedef std::vector<Exception*> Exceptions;

  private:
    uint16_t maxStack_;
    uint16_t maxLocals_;
    uint32_t codeSize_;
    uint8_t* code_;
    Exceptions exceptions_;
    Attributes attributes_;

  public:
    CodeAttribute( ClassFile* cf, uint16_t nameIdx, std::istream& is);
    ~CodeAttribute();
    uint16_t getMaxStack()  { return maxStack_; }
    uint16_t getMaxLocals()  { return maxLocals_; }
     uint8_t* getCode()  { return code_; }
    uint32_t getCodeSize()  { return codeSize_; }
     Exceptions& getExceptions()  { return exceptions_; }
     Attributes& getAttributes()  { return attributes_; }

    std::ostream& dump(std::ostream& os) ;
  };

  inline std::ostream& operator<<(std::ostream& os,
                                   CodeAttribute::Exception& e) {
    return e.dump(os);
  }

  class ExceptionsAttribute : public Attribute {
  private:
    uint16_t nameIdx_;
    std::vector<uint16_t> exceptions_;

  public:
    ExceptionsAttribute( ClassFile* cf,
                        uint16_t nameIdx,
                        std::istream& is);

    unsigned getNumExceptions()  { return exceptions_.size(); }
    unsigned getExceptionIndex(unsigned i)  { return exceptions_[i]; }
    ConstantClass* getException(unsigned i)  {
      return parent_->getConstantClass(getExceptionIndex(i));
    }

    std::ostream& dump(std::ostream& os) ;
  };
} } // namespace llvm::Java

#endif//LLVM_JAVA_CLASSFILE_H
