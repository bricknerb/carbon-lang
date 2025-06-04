// main.carbon

library "Main";

import Cpp library "hello_world.h";

fn Run() -> i32 {
  var s : Cpp.S;
  Cpp.hello_world(s);
  return 0;
}
