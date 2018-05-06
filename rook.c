#include <stdlib.h>
#include <strings.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

struct value;

struct cons {
	struct value *car;
	struct value *cdr;
};

struct string {
	size_t len;
	char  *data;
};

struct symbol {
	char *name;
};


struct value {
	enum {
		CONS,
		STRING,
		SYMBOL
	} type;
	union {
		struct cons    cons;
		struct string  string;
		struct symbol *symbol;
	};
};

void *
mem(size_t n)
{
	void *x = calloc(n, sizeof(char));
	if (!x) {
		fprintf(stderr, "*** malloc failed.\n");
		exit(2);
	}
	return x;
}
#define mems(n,t) (mem((n)*sizeof(t)))
#define make(obj) (mem(sizeof(obj)))

static struct value *
new_cons(struct value *car, struct value *cdr)
{
	struct value *v;

	v = make(struct value);
	v->type = CONS;
	v->cons.car = car;
	v->cons.cdr = cdr;
	return v;
}

static struct value *
new_string(const char *src, size_t n)
{
	struct value *v;

	v = make(struct value);
	v->type = STRING;
	v->string.data = mems(n, char);
	v->string.len = n;
	memcpy(v->string.data, src, n);
	return v;
}

static struct value *
new_symbol(const char *name)
{
	struct value *v;

	v = make(struct value);
	v->type = SYMBOL;
	v->symbol = make(struct symbol);
	v->symbol->name = strdup(name);
	return v;
}

static char *
slurp(const char *file)
{
	int fd;
	ssize_t n, nread;
	char *src, *p;

	fd = open(file, O_RDONLY);
	if (fd < 0) return NULL;

	n = lseek(fd, 0, SEEK_END);
	if (n < 0 || lseek(fd, 0, SEEK_SET) < 0) {
		close(fd);
		return NULL;
	}

	p = src = mems(n+1, char);
	while ((nread = read(fd, p, n)) > 0) {
		p += nread;
		n -= nread;
	}
	if (nread < 0) {
		free(src);
		close(fd);
		return NULL;
	}

	close(fd);
	return src;
}

int
main(int argc, char **argv)
{
	char *src;

	if (argc != 2) {
		fprintf(stderr, "USAGE: %s source.rk\n", argv[0]);
		exit(1);
	}

	src = slurp(argv[1]);
	if (!src) {
		fprintf(stderr, "%s: %s (error %d)\n", argv[1], strerror(errno), errno);
		exit(1);
	}

	fprintf(stderr, "========\n%s========\n", src);
	free(src);
	return 0;
}
