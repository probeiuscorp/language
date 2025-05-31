#include <stdio.h>

struct AnyValue {
  long long kind;
  long long data;
};
extern const struct AnyValue u_main;
extern struct AnyValue demand(struct AnyValue);

void print_about(struct AnyValue value) {
  printf("%lld\n", value.kind);
  printf("%lld\n", value.data);
}

int main() {
  printf("%p\n", &u_main);
  printf("got from main:\n");
  print_about(u_main);
  printf("demanded:\n");
  print_about(demand(u_main));
  return 0;
}
