//=-- llvm-tree.h - Utility functions for working with GCC trees --*- C++ -*-=//
//
// Copyright (C) 2010  Duncan Sands.
//
// This file is part of DragonEgg.
//
// DragonEgg is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later version.
//
// DragonEgg is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// DragonEgg; see the file COPYING.  If not, write to the Free Software
// Foundation, 51 Franklin Street, Suite 500, Boston, MA 02110-1335, USA.
//
//===----------------------------------------------------------------------===//
// This file declares utility functions for working with GCC trees.
//===----------------------------------------------------------------------===//

#ifndef LLVM_TREE_H
#define LLVM_TREE_H

// System headers
#include <string>

union tree_node;

namespace llvm {

/// getDescriptiveName - Return a helpful name for the given tree, or an empty
/// string if no sensible name was found.  These names are used to make the IR
/// more readable, and have no official status.
std::string getDescriptiveName(union tree_node *t);

}

#endif /* LLVM_TREE_H */
