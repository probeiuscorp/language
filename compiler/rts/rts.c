#include <stdio.h>
#include <stdlib.h>

struct AnyValue {
  long long kind;
  long long data;
};
typedef struct AnyValue AnyValue;
struct Data0 {
  long long tag;
};
struct Data1 {
  long long tag;
  AnyValue pos1;
};
struct Data2 {
  long long tag;
  AnyValue pos1;
  AnyValue pos2;
};

extern const AnyValue u_main;
extern const AnyValue u_Unit;
extern const AnyValue u_Cons;
extern const AnyValue u_Nil;
extern AnyValue demand(AnyValue);
extern AnyValue evaluateClosure(AnyValue f, AnyValue x);
extern AnyValue valueOfChar(long long codepoint);

AnyValue print_about(AnyValue value) {
  printf("ABOUT kind: %lld   data: %lld\n", value.kind, value.data);
  return value;
}

AnyValue performIO(AnyValue zio) {
  struct Data0* io = (struct Data0*) demand(zio).data;
  switch(io->tag) {
    // IO <$>
    case 0: {
      struct Data2* io2 = (struct Data2*) io;
      AnyValue res = performIO(io2->pos2);
      return evaluateClosure(io2->pos1, res);
    };
    // IO <*>
    case 1: {
      struct Data2* ios2 = (struct Data2*) io;
      AnyValue f = performIO(ios2->pos1);
      AnyValue x = performIO(ios2->pos2);
      return evaluateClosure(f, x);
    };
    // IO join
    case 2: {
      struct Data1* io1 = (struct Data1*) io;
      return performIO(performIO(io1->pos1));
    };
    // getLine
    case 3: {
      char *line = NULL;
      size_t len = 0;
      ssize_t read = getline(&line, &len, stdin);
      AnyValue string = u_Nil;
      if(read != -1) {
        for(int i = read - 2; i >= 0; i--) {
          string = evaluateClosure(evaluateClosure(u_Cons, valueOfChar(line[i])), string);
        }
      }
      free(line);
      return string;
    };
    // putStrLn
    case 4: {
      // TODO: buffer this IO
      struct Data1 *io1 = (struct Data1*) io;
      AnyValue head = io1->pos1;
      for(;;) {
        struct Data2 *data = (struct Data2*) demand(head).data;
        if(data->tag == 9) break;
        putc(demand(data->pos1).data, stdout);
        head = data->pos2;
      }
      putc('\n', stdout);
      return u_Unit;
    }
    default: {
      printf("unknown IO tag: %lld\n", io->tag);
      exit(1);
    };
  }
}

int main() {
  performIO(u_main);
  return 0;
}
