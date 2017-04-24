//===- PrettyClassDefinitionDumper.cpp --------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "PrettyClassDefinitionDumper.h"

#include "LinePrinter.h"
#include "PrettyClassLayoutGraphicalDumper.h"
#include "PrettyClassLayoutTextDumper.h"
#include "llvm-pdbdump.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/DebugInfo/PDB/PDBSymbolTypeBaseClass.h"
#include "llvm/DebugInfo/PDB/PDBSymbolTypeUDT.h"
#include "llvm/DebugInfo/PDB/UDTLayout.h"

#include "llvm/Support/Format.h"

using namespace llvm;
using namespace llvm::pdb;

ClassDefinitionDumper::ClassDefinitionDumper(LinePrinter &P)
    : PDBSymDumper(true), Printer(P) {}

void ClassDefinitionDumper::start(const PDBSymbolTypeUDT &Class) {
  assert(opts::pretty::ClassFormat !=
         opts::pretty::ClassDefinitionFormat::None);

  ClassLayout Layout(Class);
  start(Layout);
}

void ClassDefinitionDumper::start(const ClassLayout &Layout) {
  prettyPrintClassIntro(Layout);

  switch (opts::pretty::ClassFormat) {
  case opts::pretty::ClassDefinitionFormat::Graphical: {
    PrettyClassLayoutGraphicalDumper Dumper(Printer, 0);
    DumpedAnything = Dumper.start(Layout);
    break;
  }
  case opts::pretty::ClassDefinitionFormat::Standard:
  case opts::pretty::ClassDefinitionFormat::Layout: {
    PrettyClassLayoutTextDumper Dumper(Printer);
    DumpedAnything |= Dumper.start(Layout);
    break;
  }
  default:
    llvm_unreachable("Unreachable!");
  }

  prettyPrintClassOutro(Layout);
}

void ClassDefinitionDumper::prettyPrintClassIntro(const ClassLayout &Layout) {
  DumpedAnything = false;
  Printer.NewLine();

  uint32_t Size = Layout.getSize();
  const PDBSymbolTypeUDT &Class = Layout.getClass();

  WithColor(Printer, PDB_ColorItem::Keyword).get() << Class.getUdtKind() << " ";
  WithColor(Printer, PDB_ColorItem::Type).get() << Class.getName();
  WithColor(Printer, PDB_ColorItem::Comment).get() << " [sizeof = " << Size
                                                   << "]";
  uint32_t BaseCount = Layout.bases().size();
  if (BaseCount > 0) {
    Printer.Indent();
    char NextSeparator = ':';
    for (auto BC : Layout.bases()) {
      const auto &Base = BC->getBase();
      if (Base.isIndirectVirtualBaseClass())
        continue;

      Printer.NewLine();
      Printer << NextSeparator << " ";
      WithColor(Printer, PDB_ColorItem::Keyword).get() << Base.getAccess();
      if (BC->isVirtualBase())
        WithColor(Printer, PDB_ColorItem::Keyword).get() << " virtual";

      WithColor(Printer, PDB_ColorItem::Type).get() << " " << Base.getName();
      NextSeparator = ',';
    }

    Printer.Unindent();
  }

  Printer << " {";
  Printer.Indent();
}

void ClassDefinitionDumper::prettyPrintClassOutro(const ClassLayout &Layout) {
  Printer.Unindent();
  if (DumpedAnything)
    Printer.NewLine();
  Printer << "}";
  Printer.NewLine();
  if (Layout.deepPaddingSize() > 0) {
    APFloat Pct(100.0 * (double)Layout.deepPaddingSize() /
                (double)Layout.getSize());
    SmallString<8> PctStr;
    Pct.toString(PctStr, 4);
    WithColor(Printer, PDB_ColorItem::Padding).get()
        << "Total padding " << Layout.deepPaddingSize() << " bytes (" << PctStr
        << "% of class size)";
    Printer.NewLine();
  }
}
