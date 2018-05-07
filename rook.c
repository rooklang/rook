#include <stdlib.h>
#include <strings.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <ctype.h>

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

static void
free_value(struct value *v)
{
	switch (v->type) {
	case STRING:
		free(v->string.data);
		break;

	case SYMBOL:
		free(v->symbol->name);
		free(v->symbol);
		break;

	case CONS:
		/* noop */
		break;
	}

	free(v);
}

static void
fprintv(FILE *io, int in, struct value *v)
{
	char *pre;

	pre = mems(in + 1, char);
	memset(pre, ' ', in);

	switch (v->type) {
	case CONS:
		fprintf(io, "%s(", pre);
		fprintv(io, 0, v->cons.car);
		fprintf(io, "\n");
		fprintv(io, in+1, v->cons.cdr);
		fprintf(io, ")\n");
		break;

	case STRING:
		fprintf(io, "%s\"%s\"", pre, v->string.data);
		break;

	case SYMBOL:
		fprintf(io, "%s:%s", pre, v->symbol->name);
		break;
	}
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

	p = src = mems((size_t)n+1, char);
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

enum ttype {
	T_OPENP,
	T_CLOSEP,
	T_IDENT,
	T_STRING,
};

struct pos {
	const char *src;

	int offset;
	int line;
	int column;
};

/* copy_pos(&dst, &src)

   set all members of the dst pos structure
   to the values of corresponding members of
   the src pos structure.
 */
#define copy_pos(dst,src) memcpy((dst), (src), sizeof(struct pos))

/* init_pos(&pos, "src")

   initialize a pos struct to point to the
   beginning of the raw source code string
   given as the second argument.
 */
static inline void
init_pos(struct pos *pos, const char *src)
{
	pos->offset = 0;
	pos->line = pos->column = 1;
	pos->src = src;
}

struct token {
	enum ttype  type;

	struct pos  pos;
	int         len;

	char       *data;
};

struct lexer {
	const char *src;

	struct pos prev;
	struct pos head;
};

static struct token *
new_token(struct lexer *l, enum ttype type, char *data)
{
	struct token *token;

	token = make(struct token);
	token->type = type;
	copy_pos(&token->pos, &l->prev);
	token->len = l->head.offset - l->prev.offset;
	token->data = data;
	return token;
}

static void
free_token(struct token *token)
{
	free(token->data);
	free(token);
}

static char *
lexeme(struct lexer *l)
{
	char *s;
	size_t len;

	len = l->head.offset - l->prev.offset;
	s = mem(len + 1);
	memcpy(s, l->src + l->prev.offset, len);
	return s;
}

/* peek(&lexer) -> c

   return the character under the cursor,
   without changing the position.
 */
static inline char
peek(struct lexer *l)
{
	return l->src[l->head.offset];
}

/* next(&lexer)

   advance the cursor one position, updating
   line / column markers as necessary.

   returns the character that was at offset,
   before any adjustments took place.
 */
static inline char
next(struct lexer *l)
{
	char c = peek(l);

	l->head.offset++;
	l->head.column++;
	if (l->src[l->head.offset] == '\n') {
		l->head.column = 1;
		l->head.line++;
	}

	return c;
}

/* resync(&lexer)

   put the head cursor into the prev cursor,
   overwriting what was there previously.
 */
static inline void
resync(struct lexer *l)
{
	copy_pos(&l->prev, &l->head);
}

static struct token *
lex(struct lexer *l)
{
	char c;

	/* eat whitespace */
	for (c = next(l); c && isspace(c); c = next(l));
	resync(l);

	switch (c) {
	case '\0': return NULL;

	case '(': return new_token(l, T_OPENP, NULL);
	case ')': return new_token(l, T_CLOSEP, NULL);
	//case '"':
	//case ':':
	}
	if (c && isalpha(c)) {
		for (c = next(l); c && !isspace(c); c = next(l));
		return new_token(l, T_IDENT, lexeme(l));
	}
	return NULL;
}

static struct value *
parse(const char *file, const char *src)
{
	int i;
	struct lexer l;
	struct token *token;

	init_pos(&l.head, src);
	init_pos(&l.prev, src);

	while ((token = lex(&l)) != NULL) {
		fprintf(stderr, "token %d, %p %s\n", token->type, token->data, token->data);
		free_token(token);
	}

	return NULL;
}

int
main(int argc, char **argv)
{
	char *src;
	struct value *ast;

	if (argc != 2) {
		fprintf(stderr, "USAGE: %s source.rk\n", argv[0]);
		exit(1);
	}

	src = slurp(argv[1]);
	if (!src) {
		fprintf(stderr, "%s: %s (error %d)\n", argv[1], strerror(errno), errno);
		exit(1);
	}

	ast = parse(argv[1], src);
	if (!ast) {
		fprintf(stderr, "%s: parsing failed.\n", argv[1]);
		exit(1);
	}

	fprintv(stdout, 0, ast);
	free_value(ast);
	free(src);
	return 0;
}
