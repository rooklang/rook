#include <stdlib.h>
#include <string.h>
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

/* FIXME: this is woefully inefficient */
struct symtab_ent {
	struct symbol     *symbol;
	struct symtab_ent *next;
};
struct symtab {
	struct symtab_ent *head;
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

static struct symtab SYMBOLS = {0};

static struct symbol *
intern(const char *name)
{
	struct symtab_ent *e;

	for (e = SYMBOLS.head; e; e = e->next) {
		if (strcmp(e->symbol->name, name) == 0) {
			return e->symbol;
		}
	}
	e = make(struct symtab_ent);
	e->next = SYMBOLS.head;
	e->symbol = make(struct symbol);
	e->symbol->name = strdup(name);
	SYMBOLS.head = e;
	return e->symbol;
}

struct binding {
	struct symbol  *symbol;
	struct value   *value;
	struct binding *next;
};
struct env {
	struct binding *head;
};

static struct value *
get(struct env *env, struct symbol *symbol)
{
	struct binding *b;
	for (b = env->head; b; b = b->next) {
		if (b->symbol == symbol) {
			return b->value;
		}
	}
	return NULL;
}

static struct value *
set(struct env *env, struct symbol *symbol, struct value *value)
{
	struct binding *b;
	for (b = env->head; b; b = b->next) {
		if (b->symbol == symbol) {
			b->value = value;
			return b->value;
		}
	}
	b = make(struct binding);
	b->symbol = symbol;
	b->value  = value;
	b->next   = env->head;
	env->head = b;
	return b->value;
}

static struct value *
def(struct env *env, struct symbol *symbol, struct value *value)
{
	struct binding *b;
	b = make(struct binding);
	b->symbol = symbol;
	b->value  = value;
	b->next   = env->head;
	env->head = b;
	return b->value;
}

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
new_string(const char *src)
{
	struct value *v;
	size_t n = strlen(src);

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
	v->symbol = intern(name);
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
		fprintf(io, "%s(cons ", pre);
		fprintv(io, 0, v->cons.car);
		if (v->cons.cdr) {
			fprintf(io, "\n");
			fprintv(io, in+6, v->cons.cdr);
		}
		fprintf(io, ")");
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
	T_OOPS,
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
	//fprintf(stderr, "PEEK: [%d] = ", l->head.offset);
	//fprintf(stderr, "%02x (%c)\n", l->src[l->head.offset], l->src[l->head.offset]);
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
	l->head.offset++;
	l->head.column++;
	if (l->src[l->head.offset] == '\n') {
		l->head.column = 1;
		l->head.line++;
	}

	return peek(l);
}

/* resync(&lexer)

   put the head cursor into the prev cursor,
   overwriting what was there previously.
 */
static inline void
resync(struct lexer *l)
{
	copy_pos(&l->prev, &l->head);
	//fprintf(stderr, "RESYNC AT [%d]\n", l->prev.offset);
}

static struct token *
lex(struct lexer *l)
{
	char c, q, esc;
	struct token *tok;

	/* eat whitespace */
	for (c = peek(l); c && isspace(c); next(l), c = peek(l));
	resync(l);

	switch (c) {
	case '\0': return NULL;

	case '(': next(l); return new_token(l, T_OPENP, NULL);
	case ')': next(l); return new_token(l, T_CLOSEP, NULL);
	case '"':
		esc = 0; q = c; next(l); resync(l);
		for (c = peek(l); c && (esc || c != q); next(l), c = peek(l));
		tok = new_token(l, T_STRING, lexeme(l)); // fixme
		next(l); return tok;
	}
	if (c && isalpha(c)) {
		for (c = peek(l); c && !isspace(c); next(l), c = peek(l));
		return new_token(l, T_IDENT, lexeme(l));
	}

	return new_token(l, T_OOPS, "syntax error");
}

#define PSTACK 4096

static struct value *
parse(const char *file, const char *src)
{
	struct lexer l;
	struct token *token;
	struct value *value, **stack;
	int top;

	l.src = src;
	init_pos(&l.head, src);
	init_pos(&l.prev, src);

	top = -1;
	stack = mems(PSTACK + 1, struct value *);

	while ((token = lex(&l)) != NULL) {
		if (top == PSTACK) {
			fprintf(stderr, "pstack overflow\n");
			exit(1);
		}
		switch (token->type) {
		case T_OPENP:  stack[++top] = NULL;                    break;
		case T_IDENT:  stack[++top] = new_symbol(token->data); break;
		case T_STRING: stack[++top] = new_string(token->data); break;
		case T_CLOSEP:
			value = NULL;
			while (top >= 0) {
				if (!stack[top]) { /* NULL is the '(' marker */
					stack[top] = value ? value : new_cons(NULL, NULL);
					break;
				}
				value = new_cons(stack[top--], value);
			}
			break;

		case T_OOPS:
			fprintf(stderr, "oops: %s\n", token->data);
			exit(1);
		}

		free_token(token);
	}

	return stack[0];
}

static struct value *
eval(struct value *expr, struct env *env)
{
	/* FIXME: no environment yet... */

	struct value *head, *tail, *cond;

	switch (expr->type) {
	case CONS: /* (op ...) form */
		head = expr->cons.car;
		if (!head) return expr;
		tail = expr->cons.cdr;

		if (head->type == SYMBOL) {
			if (head->symbol == intern("if")) {
				cond = eval(tail->cons.car, env);
				tail = tail->cons.cdr;

				if (cond) {
					return eval(tail->cons.car, env);
				} else {
					tail = tail->cons.cdr;
					return eval(tail->cons.car, env);
				}
			}
			if (head->symbol == intern("printf")) {
				fprintv(stdout, 0, eval(tail->cons.car, env));
				return head;
			}
			return head; /* FIXME */
		} else {
			fprintf(stderr, "non-symbol in calpos!\n");
			exit(2);
		}
		break;

	case STRING:
		return expr;

	case SYMBOL:
		return get(env, expr);

	default:
		fprintf(stderr, "semantic error\n");
		exit(2);
	}
}

int
main(int argc, char **argv)
{
	char *src;
	struct value *ast;
	struct env *env;

	if (argc != 2) {
		fprintf(stderr, "USAGE: %s source.rk\n", argv[0]);
		exit(1);
	}

	src = slurp(argv[1]);
	if (!src) {
		fprintf(stderr, "%s: %s (error %d)\n", argv[1], strerror(errno), errno);
		exit(1);
	}

	env = make(struct env);
	ast = eval(parse(argv[1], src), env);
	if (!ast) {
		fprintf(stderr, "%s: parsing failed.\n", argv[1]);
		exit(1);
	}

	fprintv(stdout, 0, ast);
	free_value(ast);
	free(src);
	return 0;
}
