// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// https://adventofcode.com/2024/day/8

library "day8_common";

import Core library "io";
import Core library "range";
import library "io_utils";

class Grid {
  fn Read() -> Grid {
    returned var me: Grid;
    for (y: i32 in Core.Range(50)) {
      for (x: i32 in Core.Range(50)) {
        me.data[x][y] = ReadChar();
      }
      SkipNewline();
    }
    return var;
  }

  var data: array(array(i32, 50), 50);
}
