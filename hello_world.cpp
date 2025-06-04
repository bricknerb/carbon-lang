// hello_world.cpp

#include "hello_world.h"

#include <cstdio>

void hello_world2(S s) { printf("hello_world2: %d\n", s.x); }

void hello_world(S s) {
  printf("hello_world: %d\n", s.x);
  hello_world2(s);
}
