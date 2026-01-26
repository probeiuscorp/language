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

void dump_hex(const void* data, size_t size) {
	char ascii[17];
	size_t i, j;
	ascii[16] = '\0';
	for (i = 0; i < size; ++i) {
		printf("%02X ", ((unsigned char*)data)[i]);
		if (((unsigned char*)data)[i] >= ' ' && ((unsigned char*)data)[i] <= '~') {
			ascii[i % 16] = ((unsigned char*)data)[i];
		} else {
			ascii[i % 16] = '.';
		}
		if ((i+1) % 8 == 0 || i+1 == size) {
			printf(" ");
			if ((i+1) % 16 == 0) {
				printf("|  %s \n", ascii);
			} else if (i+1 == size) {
				ascii[(i+1) % 16] = '\0';
				if ((i+1) % 16 <= 8) {
					printf(" ");
				}
				for (j = (i+1) % 16; j < 16; ++j) {
					printf("   ");
				}
				printf("|  %s \n", ascii);
			}
		}
	}
}
void print_record(long long *record) {
  printf("--- print_record ---\n");
  fflush(stdout);
  long long size = record[0];
  printf("size: %lld\n", size);
  fflush(stdout);
  printf("--- fields ---\n");
  for (int i = 0; i <= 3 * size; i++) {
    printf("%lld  %p\n", record[i], record[i]);
  }
  printf("---\n");

  for (int i = 0; i < size; i++) {
    long long tag = record[i * 3 + 1];
    long long kind = record[i * 3 + 2];
    long long data = record[i * 3 + 3];
    printf("row: %lld | %lld | %lld (%p)\n", tag, kind, data, data);
  }
  dump_hex(record, 1 + size * 3 * sizeof(long long));
}

AnyValue print_about(AnyValue value) {
  printf("ABOUT kind: %lld   data: %lld (%p)\n", value.kind, value.data, value.data);
  return value;
}

void print_int(long long value) {
  printf("from LLVM: %lld (%p)\n", value, value);
  fflush(stdout);
}
void print_debug(AnyValue value) {
  printf("<< from LLVM:\n");
  print_about(value);
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
  // AnyValue x = demand(u_main);
  // printf("%p (%p)\n", &x, &x.data);
  // print_about(x);
  // print_record((long long*) x.data);
  performIO(u_main);
  return 0;
}
