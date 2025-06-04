// hello_world.h

struct S {
  S(const S&) { x = 1; }
  int x;
};

void hello_world(S s);
