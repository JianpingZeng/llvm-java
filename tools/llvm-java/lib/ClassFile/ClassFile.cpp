//===-- ClassFile.cpp - ClassFile class -----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of the ClassFile library. It
// is used by the LLVM Java frontend to parse Java Class files.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "classfile"

#include <llvm/Java/ClassFile.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/FileSystem.h>

#include <algorithm>
#include <cassert>
#include <fstream>
#include <functional>
#include <iostream>
#include <iterator>
#include <map>

using namespace llvm::Java;

namespace {

  using namespace llvm;

  static cl::opt<std::string>
  ClassPath("cp",
            cl::desc("A : separated list of directories"),
            cl::value_desc("class search path"),
            cl::init(getenv("CLASSPATH") ? getenv("CLASSPATH") : ""));
  static cl::alias
  ClassPathA("classpath",
             cl::desc("Alias for -cp"),
             cl::aliasopt(ClassPath));

}

//===----------------------------------------------------------------------===//
// Internal utility functions
namespace {

  uint8_t readU1(std::istream& is) {
    char val;
    if (!is.get(val))
      llvm_unreachable("unexpected end of input");
    return val;
  }

  uint16_t readU2(std::istream& is) {
    uint16_t val = readU1(is);
    return (val << 8) | readU1(is);
  }

  uint32_t readU4(std::istream& is) {
    uint32_t val = readU2(is);
    return (val << 16) | readU2(is);
  }

  uint64_t readU8(std::istream& is) {
    uint64_t hi = readU4(is), lo = readU4(is);
    return hi << 32 | lo;
  }

  float int2float(uint32_t v) {
    union { uint32_t in; float out; } tmp;
    tmp.in = v;
    return tmp.out;
  }

  double long2double(uint64_t v) {
    union { uint64_t in; double out; } tmp;
    tmp.in = v;
    return tmp.out;
  }

  void readFields(Fields& f,  ClassFile* parent, std::istream& is)
  {
    assert(f.empty() && "Should not call with a non-empty fields vector");
    uint16_t count = readU2(is);
    f.reserve(count);
    while(count--)
      f.push_back(Field::readField(parent, is));
  }

  void readMethods(Methods& m,  ClassFile* parent, std::istream& is)
  {
    assert(m.empty() && "Should not call with a non-empty methods vector");
    uint16_t count = readU2(is);
    m.reserve(count);
    while(count--)
      m.push_back(Method::readMethod(parent, is));
  }

  void readAttributes(Attributes& a,  ClassFile* cf, std::istream& is)
  {
    assert(a.empty() &&
           "Should not call with a non-empty attributes vector");
    uint16_t count = readU2(is);
    a.reserve(count);
    while(count--)
      a.push_back(Attribute::readAttribute(cf, is));
  }

}

//===----------------------------------------------------------------------===//
// ClassFile implementation
 ClassFile* ClassFile::readClassFile(std::istream& is)
{
  if (readU1(is) != 0xCA) llvm_unreachable("bad magic");
  if (readU1(is) != 0xFE) llvm_unreachable("bad magic");
  if (readU1(is) != 0xBA) llvm_unreachable("bad magic");
  if (readU1(is) != 0xBE) llvm_unreachable("bad magic");

  return new ClassFile(is);
}

std::vector<StringRef> ClassFile::getClassPath()
{
  DEBUG(std::cerr << "CLASSPATH=" << ClassPath << '\n');

  std::vector<StringRef> result;
  StringRef path;
  int b = 0, e = 0;
  do {
    e = ClassPath.find(':', b);
    // FIXME: Currently we only support flat class file reading. When
    // jar files are supported this chech has to change to not require
    // that each CLASSPATH component is a directory.
    path = ClassPath.substr(b, e - b);
    if (!path.empty() && sys::fs::exists(path) && sys::fs::is_directory(path)) {
      result.push_back(path);
      DEBUG(std::cerr << "Adding: " << path.str() << " to CLASSPATH\n");
    }
    b = e + 1;
  } while (e != std::string::npos);

  return result;
}

StringRef ClassFile::getFileForClass( std::string& classname)
{
  static  std::vector<StringRef> classpath = getClassPath();
  DEBUG(std::cerr << "Looking up class: " << classname << '\n');

  std::string clazz = classname;
  // replace '.' with '/'
  for (unsigned i = 0, e = clazz.size(); i != e; ++i)
    if (clazz[i] == '.')
      clazz[i] = '/';
  clazz += ".class";

  for (unsigned i = 0, e = classpath.size(); i != e; ++i) {
    StringRef path = classpath[i];
    assert(sys::fs::is_directory(path) && "CLASSPATH element not a directory!");
    StringRef filename = (path + "/" + clazz).str();
    DEBUG(std::cerr << "Trying file: " << filename.str() << '\n');
    if (sys::fs::exists(filename))
      return filename;
  }

  llvm_unreachable(("Class " + classname + " not found").c_str());
}

 ClassFile* ClassFile::get( std::string& classname)
{
  typedef std::map<std::string,  ClassFile*> Name2ClassMap;
  static Name2ClassMap n2cMap_;

  Name2ClassMap::iterator it = n2cMap_.lower_bound(classname);

  if (it == n2cMap_.end() || it->first != classname) {
    std::ifstream in(getFileForClass(classname));
    it = n2cMap_.insert(it, std::make_pair(classname, readClassFile(in)));
  }

  return it->second;
}

ClassFile::ClassFile(std::istream& is)
{
  minorV_ = readU2(is);
  majorV_ = readU2(is);
  // Read constant pool.
  uint16_t count = readU2(is);
  cPool_.reserve(count);
  cPool_.push_back(NULL);
  while (cPool_.size() < count) {
    cPool_.push_back(Constant::readConstant(this, is));
    if (cPool_.back()->isDoubleSlot())
      cPool_.push_back(NULL);
  }
  accessFlags_ = readU2(is);
  thisClassIdx_ = readU2(is);
  superClassIdx_ = readU2(is);
  count = readU2(is);
  interfaces_.reserve(count);
  while (count--)
    interfaces_.push_back(readU2(is));

  readFields(fields_, this, is);
  readMethods(methods_, this, is);
  readAttributes(attributes_, this, is);
}

ConstantClass* ClassFile::getConstantClass(unsigned index) 
{
  /*assert(dynamic_cast<ConstantClass*>(getConstant(index)) &&
         "Constant is not a ConstantClass!");*/
  return static_cast<ConstantClass*>(getConstant(index));
}

ConstantMemberRef* ClassFile::getConstantMemberRef(unsigned index) 
{
  /*assert(dynamic_cast<ConstantMemberRef*>(getConstant(index)) &&
         "Constant is not a ConstantMemberRef!");*/
  return static_cast<ConstantMemberRef*>(getConstant(index));
}

ConstantFieldRef* ClassFile::getConstantFieldRef(unsigned index) 
{
  /*assert(dynamic_cast<ConstantFieldRef*>(getConstant(index)) &&
         "Constant is not a ConstantFieldRef!");*/
  return static_cast<ConstantFieldRef*>(getConstant(index));
}

ConstantMethodRef* ClassFile::getConstantMethodRef(unsigned index) 
{
  /*assert(dynamic_cast<ConstantMethodRef*>(getConstant(index)) &&
         "Constant is not a ConstantMethodRef!");*/
  return static_cast<ConstantMethodRef*>(getConstant(index));
}

ConstantInterfaceMethodRef*
ClassFile::getConstantInterfaceMethodRef(unsigned index) 
{
  /*assert(dynamic_cast<ConstantInterfaceMethodRef*>(getConstant(index)) &&
         "Constant is not a ConstantInterfaceMethodRef!");*/
  return static_cast<ConstantInterfaceMethodRef*>(getConstant(index));
}

ConstantNameAndType* ClassFile::getConstantNameAndType(unsigned index) 
{
  /*assert(dynamic_cast<ConstantNameAndType*>(getConstant(index)) &&
         "Constant is not a ConstantNameAndType!");*/
  return static_cast<ConstantNameAndType*>(getConstant(index));
}

ConstantUtf8* ClassFile::getConstantUtf8(unsigned index) 
{
  /*assert(dynamic_cast<ConstantUtf8*>(getConstant(index)) &&
         "Constant is not a ConstantUtf8!");*/
  return static_cast<ConstantUtf8*>(getConstant(index));
}

bool ClassFile::isNativeMethodOverloaded( Method& method) 
{
  unsigned count = 0;
  for (Methods::const_iterator i = methods_.begin(), e = methods_.end();
       i != e; ++i)
    if ((*i)->isNative() && (*i)->getName() == method.getName())
      ++count;

  return count > 1;
}

ClassFile::~ClassFile()
{
  for_each(cPool_.begin(), cPool_.end(), deleter<Constant>);
  for_each(fields_.begin(), fields_.end(), deleter<Field>);
  for_each(methods_.begin(), methods_.end(), deleter<Method>);
  for_each(attributes_.begin(), attributes_.end(), deleter<Attribute>);
}

std::ostream& ClassFile::dump(std::ostream& os) 
{
  os << "Minor version: " << getMinorVersion() << '\n'
     << "Major version: " << getMajorVersion() << "\n\n"
     << "class " << *getThisClass();
  if (getSuperClass())
    os  << " (" << *getSuperClass() << ")\n";

  os << "Flags:";
  if (isPublic()) os << " public";
  if (isFinal()) os << " final";
  if (isSuper()) os << " super";
  if (isInterface()) os << " interface";
  if (isAbstract()) os << " abstract";

//   dumpCollection(interfaces_, "Interface", os);
//   dumpCollection(fields_, "Field", os);
//   dumpCollection(methods_, "Method", os);
//   dumpCollection(attributes_, "Attribute", os);

  return os;
}

//===----------------------------------------------------------------------===//
// Utility functions
Attribute* llvm::Java::getAttribute( Attributes& attrs,
                                     std::string& name) {
  for (unsigned i = 0, e = attrs.size(); i != e; ++i)
    if (attrs[i]->getName()->str() == name)
      return attrs[i];

  return NULL;
}

//===----------------------------------------------------------------------===//
// Constant implementation
Constant* Constant::readConstant( ClassFile* cf, std::istream& is)
{
  Constant::Tag tag = static_cast<Constant::Tag>(readU1(is));
  switch (tag) {
  case Constant::CLASS:
    return new ConstantClass(cf, is);
  case Constant::FIELD_REF:
    return new ConstantFieldRef(cf, is);
  case Constant::METHOD_REF:
    return new ConstantMethodRef(cf, is);
  case Constant::INTERFACE_METHOD_REF:
    return new ConstantInterfaceMethodRef(cf, is);
  case Constant::STRING:
    return new ConstantString(cf, is);
  case Constant::INTEGER:
    return new ConstantInteger(cf, is);
  case Constant::FLOAT:
    return new ConstantFloat(cf, is);
  case Constant::LONG:
    return new ConstantLong(cf, is);
  case Constant::DOUBLE:
    return new ConstantDouble(cf, is);
  case Constant::NAME_AND_TYPE:
    return new ConstantNameAndType(cf, is);
  case Constant::UTF8:
    return new ConstantUtf8(cf, is);
  default:
    assert(0 && "Unknown constant tag");
  }

  return NULL;
}

Constant::~Constant()
{

}

ConstantClass::ConstantClass( ClassFile* cf, std::istream& is)
  : Constant(cf, Constant::CLASS),
    nameIdx_(readU2(is))
{

}

std::ostream& ConstantClass::dump(std::ostream& os) 
{
  return os << "class (Name=" << nameIdx_ << ')';
}

ConstantMemberRef::ConstantMemberRef(ClassFile* cf, std::istream& is, Constant::Tag kind)
  : Constant(cf, kind),
    classIdx_(readU2(is)),
    nameAndTypeIdx_(readU2(is))
{

}

std::ostream& ConstantFieldRef::dump(std::ostream& os) 
{
  return os << "fieldRef (class=" << classIdx_
            << ", nameAndType=" << nameAndTypeIdx_ << ')';
}

std::ostream& ConstantMethodRef::dump(std::ostream& os) 
{
  return os << "methodRef (class=" << classIdx_
            << ", nameAndType=" << nameAndTypeIdx_ << ')';
}

std::ostream& ConstantInterfaceMethodRef::dump(std::ostream& os) 
{
  return os << "interfaceMethodRef (class=" << classIdx_
            << ", nameAndType=" << nameAndTypeIdx_ << ')';
}

ConstantString::ConstantString( ClassFile* cf, std::istream& is)
  : Constant(cf, Constant::STRING),
    stringIdx_(readU2(is))
{

}

std::ostream& ConstantString::dump(std::ostream& os) 
{
  return os << "string (utf8=" << stringIdx_ << ')';
}

ConstantInteger::ConstantInteger( ClassFile* cf, std::istream& is)
  : Constant(cf, Constant::INTEGER),
    value_(static_cast<int32_t>(readU4(is)))
{

}

std::ostream& ConstantInteger::dump(std::ostream& os) 
{
  return os << "int " << value_;
}

ConstantFloat::ConstantFloat( ClassFile* cf, std::istream& is)
  : Constant(cf, FLOAT),
    value_(int2float(readU4(is)))
{

}

std::ostream& ConstantFloat::dump(std::ostream& os) 
{
  return os << "float " << value_;
}

ConstantLong::ConstantLong( ClassFile* cf, std::istream& is)
  : Constant(cf, LONG),
    value_(static_cast<int64_t>(readU8(is)))
{

}

std::ostream& ConstantLong::dump(std::ostream& os) 
{
  return os << "long " << value_ << 'l';
}

ConstantDouble::ConstantDouble( ClassFile* cf, std::istream& is)
  : Constant(cf, DOUBLE),
    value_(long2double(readU8(is)))
{

}

std::ostream& ConstantDouble::dump(std::ostream& os) 
{
  return os << "double " << value_;
}

ConstantNameAndType::ConstantNameAndType( ClassFile* cf, std::istream& is)
  : Constant(cf, Constant::NAME_AND_TYPE),
    nameIdx_(readU2(is)),
    descriptorIdx_(readU2(is))
{

}

std::ostream& ConstantNameAndType::dump(std::ostream& os) 
{
  return os << "nameAndType (name=" << nameIdx_
            << ", descriptor=" << descriptorIdx_ << ')';
}

ConstantUtf8::ConstantUtf8( ClassFile* cf, std::istream& is)
  : Constant(cf, Constant::UTF8)
{
  uint16_t length = readU2(is);
  char *buf = (char *)alloca(length);
  std::streamsize s = is.rdbuf()->sgetn(buf, length);
  if (s != length)
    llvm_unreachable(
      "Could not read string constant from input stream");
  utf8_.assign(buf, length);
}

std::ostream& ConstantUtf8::dump(std::ostream& os) 
{
  return os << utf8_;
}

//===----------------------------------------------------------------------===//
// Member implementation
Member::Member( ClassFile* parent, std::istream& is)
  : parent_(parent),
    accessFlags_(readU2(is)),
    nameIdx_(readU2(is)),
    descriptorIdx_(readU2(is))
{
  readAttributes(attributes_, parent_, is);
}

Member::~Member()
{
  for_each(attributes_.begin(), attributes_.end(), deleter<Attribute>);
}

//===----------------------------------------------------------------------===//
// Field implementation
Field::Field( ClassFile* parent, std::istream& is)
  : Member(parent, is)
{

}

Field::~Field()
{

}

std::ostream& Field::dump(std::ostream& os) 
{
  os << *getName() << ' ' << *getDescriptor() << '\n'
     << "Flags:";
  if (isPublic()) os << " public";
  if (isPrivate()) os << " private";
  if (isProtected()) os << " protected";
  if (isStatic()) os << " static";
  if (isFinal()) os << " final";
  if (isVolatile()) os << " volatile";
  if (isTransient()) os << " transient";

//   dumpCollection(attributes_, "Attribute", os);

  return os;
}

ConstantValueAttribute* Field::getConstantValueAttribute() 
{
  if (!isStatic())
    return NULL;

  return (ConstantValueAttribute*) getAttribute(attributes_,
                                                Attribute::CONSTANT_VALUE);
}

//===----------------------------------------------------------------------===//
// Method implementation
Method::Method( ClassFile* parent, std::istream& is)
  : Member(parent, is)
{

}

Method::~Method()
{

}

std::ostream& Method::dump(std::ostream& os) 
{
  os << *getName() << ' ' << *getDescriptor() << '\n'
     << "Flags:";
  if (isPublic()) os << " public";
  if (isPrivate()) os << " private";
  if (isProtected()) os << " protected";
  if (isStatic()) os << " static";
  if (isFinal()) os << " final";
  if (isSynchronized()) os << " synchronized";
  if (isNative()) os << " native";
  if (isStrict()) os << " strict";

//   dumpCollection(attributes_, "Attribute", os);

  return os;
}

CodeAttribute* Method::getCodeAttribute() 
{
  return (CodeAttribute*) getAttribute(attributes_, Attribute::CODE);
}

ExceptionsAttribute* Method::getExceptionsAttribute() 
{
  return (ExceptionsAttribute*) getAttribute(attributes_,
                                             Attribute::EXCEPTIONS);
}

//===----------------------------------------------------------------------===//
// Attribute implementation
 std::string Attribute::CONSTANT_VALUE = "ConstantValue";
 std::string Attribute::CODE = "Code";
 std::string Attribute::EXCEPTIONS = "Exceptions";
 std::string Attribute::INNER_CLASSES = "InnerClasses";
 std::string Attribute::SYNTHETIC = "Synthetic";
 std::string Attribute::SOURCE_FILE = "SourceFile";
 std::string Attribute::LINE_NUMBER_TABLE = "LineNumberTable";
 std::string Attribute::LOCAL_VARIABLE_TABLE = "LocalVariableTable";
 std::string Attribute::DEPRECATED = "Deprecated";

Attribute* Attribute::readAttribute( ClassFile* cf, std::istream& is)
{
  uint16_t nameIdx = readU2(is);
  ConstantUtf8* name = cf->getConstantUtf8(nameIdx);
  if (!name)
    llvm_unreachable("Representation of attribute name is not of type ConstantUtf8");

  if (CONSTANT_VALUE == name->str())
    return new ConstantValueAttribute(cf, nameIdx, is);
  else if (CODE == name->str())
    return new CodeAttribute(cf, nameIdx, is);
  else {
    uint32_t length = readU4(is);
    is.ignore(length);
    return new Attribute(cf, nameIdx, is);
  }
}

Attribute::Attribute( ClassFile* cf, uint16_t nameIdx, std::istream& is)
  : parent_(cf),
    nameIdx_(nameIdx)
{

}

Attribute::~Attribute()
{

}

std::ostream& Attribute::dump(std::ostream& os) 
{
  return os << *getName();
}

//===----------------------------------------------------------------------===//
// AttributeConstantValue implementation
ConstantValueAttribute::ConstantValueAttribute( ClassFile* cf,
                                               uint16_t nameIdx,
                                               std::istream& is)
  : Attribute(cf, nameIdx, is)
{
  uint32_t length = readU4(is);
  if (length != 2)
    llvm_unreachable("Length of ConstantValueAttribute is not 2");
  valueIdx_ = readU2(is);
}

std::ostream& ConstantValueAttribute::dump(std::ostream& os) 
{
  return Attribute::dump(os) << ": " << *getValue();
}

//===----------------------------------------------------------------------===//
// AttributeCode implementation
CodeAttribute::CodeAttribute( ClassFile* cf,
                             uint16_t nameIdx,
                             std::istream& is)
  : Attribute(cf, nameIdx, is)
{
  uint32_t length = readU4(is);
  maxStack_ = readU2(is);
  maxLocals_ = readU2(is);
  codeSize_ = readU4(is);
  code_ = new uint8_t[codeSize_];
  std::streamsize s = is.rdbuf()->sgetn(reinterpret_cast<char*>(code_), codeSize_);
  if (s != (std::streamsize) codeSize_)
    llvm_unreachable(
      "Could not read code from input stream");
  uint16_t exceptCount = readU2(is);
  exceptions_.reserve(exceptCount);
  while (exceptCount--)
    exceptions_.push_back(new Exception(cf, is));
  readAttributes(attributes_, cf, is);
}

CodeAttribute::~CodeAttribute()
{
  delete[] code_;
  for_each(exceptions_.begin(), exceptions_.end(), deleter<Exception>);
  for_each(attributes_.begin(), attributes_.end(), deleter<Attribute>);
}

std::ostream& CodeAttribute::dump(std::ostream& os) 
{
  Attribute::dump(os)
    << '\n'
    << "Max stack: " << maxStack_ << '\n'
    << "Max locals: " << maxLocals_ << '\n'
    << "Code size: " << codeSize_ << '\n';
//   dumpCollection(exceptions_, "Exception", os);
//   dumpCollection(attributes_, "Attribute", os);

  return os;
}

CodeAttribute::Exception::Exception( ClassFile* cf, std::istream& is)
  : parent_(cf),
    startPc_(readU2(is)),
    endPc_(readU2(is)),
    handlerPc_(readU2(is)),
    catchTypeIdx_(readU2(is))
{
  if (catchTypeIdx_ && !cf->getConstantClass(catchTypeIdx_))
    llvm_unreachable("Representation of catch type is not of type ConstantClass");
}

std::ostream& CodeAttribute::Exception::dump(std::ostream& os) 
{
  if (getCatchType())
    os << *getCatchType() << '\n';
  else
    os << "catch-all\n";
  return os << "Start PC: " << startPc_ << '\n'
            << "End PC: " << endPc_ << '\n'
            << "Handler PC: " << handlerPc_;
}

//===----------------------------------------------------------------------===//
// AttributeExceptions implementation
ExceptionsAttribute::ExceptionsAttribute( ClassFile* cf,
                                         uint16_t nameIdx,
                                         std::istream& is)
  : Attribute(cf, nameIdx, is)
{
  uint32_t length = readU4(is);
  uint16_t count = readU2(is);
  exceptions_.reserve(count);
  while (count--)
    exceptions_.push_back(readU2(is));
}

std::ostream& ExceptionsAttribute::dump(std::ostream& os) 
{
  Attribute::dump(os) << ": ";
  for (unsigned i = 0, e = getNumExceptions(); i != e; ++i)
    os << *getException(i) << ' ';
  return os;
}
