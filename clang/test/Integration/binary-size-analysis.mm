// REQUIRES: x86-registered-target

// RUN: %clang -arch x86_64 -c -g %s -o %t.o

// TODO: Check in these binaries to make this test less brittle.

// RUN: llvm-dwarfdump size-info -baseline %t.o -stats-dir %t.stats
// RUN: FileCheck %s -input-file %t.stats/dwarfdump-size-info.file-view.stats -check-prefix FILE
// RUN: FileCheck %s -input-file %t.stats/dwarfdump-size-info.class-view.stats -check-prefix CLASS
// RUN: FileCheck %s -input-file %t.stats/dwarfdump-size-info.function-view.stats -check-prefix FUNCTION
// RUN: FileCheck %s -input-file %t.stats/dwarfdump-size-info.inlining-view.stats -check-prefix INLINING

// RUN: llvm-dwarfdump size-info -baseline %t.o -target %t.o -stats-dir %t.stats
// RUN: cat %t.stats/dwarfdump-size-info.file-view.diffstats | count 0
// RUN: cat %t.stats/dwarfdump-size-info.class-view.diffstats | count 0
// RUN: cat %t.stats/dwarfdump-size-info.function-view.diffstats | count 0
// RUN: cat %t.stats/dwarfdump-size-info.inlining-view.diffstats | count 0

struct A {
  int x = 1;
  void __attribute__((always_inline)) method() { ++x; }
};

struct B : A {};

class Base {
public:
  virtual void method() {}
};

class Derived : public Base {
public:
  void method() override {}
  void method2() {}
  void method3();
};

void Derived::method3() {
  auto helper_lambda = [this]() -> void {
    method();
  };
  helper_lambda();
}

static int global1;

class ClassWithStaticMember {
public:
  static int static_class_member;
};

namespace NS1 {
  struct Base { virtual void foo() {} };
}

namespace NS2 {
  struct Base : public NS1::Base {};
}

int main() {
  A a1;
  a1.method();

  B b1;
  b1.method();

  Base *b = new Derived;
  b->method();
  static_cast<Derived *>(b)->method2();
  static_cast<Derived *>(b)->method3();

  global1++;

  ClassWithStaticMember::static_class_member++;

  NS1::Base().foo();
  NS2::Base().foo();

  return a1.x;
}

// FILE: [file];_ZN7DerivedC2Ev [function] 55
// FILE: [file];_ZN7Derived7method3Ev [function] 35
// FILE: [file];_ZZN7Derived7method3EvENK3$_0clEv [function] 30
// FILE: [file];_ZN4BaseC2Ev [function] 28
// FILE: [file];_ZN1AC1Ev [function] 27
// FILE: [file];_ZN1BC1Ev [function] 27
// FILE: [file];_ZN1BC2Ev [function] 27
// FILE: [file];_ZN7DerivedC1Ev [function] 27
// FILE: [file];_ZN1AC2Ev [function] 20
// FILE: [file];_ZN4Base6methodEv [function] 10
// FILE: [file];_ZN7Derived6methodEv [function] 10
// FILE: [file];_ZN7Derived7method2Ev [function] 10

// CLASS: Base [class];Derived [class];_ZN7Derived7method3Ev [function] 35
// CLASS: Base [class];Derived [class];_ZN7Derived6methodEv [function] 10
// CLASS: Base [class];Derived [class];_ZN7Derived7method2Ev [function] 10
// CLASS: Base [class];_ZN4Base6methodEv [function] 10
// CLASS: NS1::Base [class];_ZN3NS14Base3fooEv [function] 10

// FUNCTION: _ZN7DerivedC2Ev [function] 55
// FUNCTION: _ZN7Derived7method3Ev [function] 35
// FUNCTION: _ZZN7Derived7method3EvENK3$_0clEv [function] 30
// FUNCTION: _ZN4BaseC2Ev [function] 28
// FUNCTION: _ZN1AC1Ev [function] 27
// FUNCTION: _ZN1BC1Ev [function] 27
// FUNCTION: _ZN1BC2Ev [function] 27
// FUNCTION: _ZN7DerivedC1Ev [function] 27
// FUNCTION: _ZN1AC2Ev [function] 20
// FUNCTION: _ZN4Base6methodEv [function] 10
// FUNCTION: _ZN7Derived6methodEv [function] 10
// FUNCTION: _ZN7Derived7method2Ev [function] 10

// INLINING: _ZN1A6methodEv [inlining-target] 14
