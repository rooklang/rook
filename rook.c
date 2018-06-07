#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <ctype.h>

#include <getopt.h>

static int QUIET = 0;

struct value;

static struct value *ROOK_TRUE;
static struct value *ROOK_FALSE;

static void
fprintv(FILE *io, int in, struct value *v);

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

typedef struct value * (*primop)(struct value *);

struct value {
	enum {
		CONS,
		STRING,
		NUMBER,
		SYMBOL,
		BOOLEAN,
		PRIMOP,
	} type;
	union {
		struct cons    cons;
		struct string  string;
		int            number;
		struct symbol *symbol;
		char           boolean;
		primop         primop;
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
void *
remem(void *x, size_t n)
{
	x = realloc(x, n);
	if (!x) {
		fprintf(stderr, "*** realloc failed.\n");
		exit(2);
	}
	return x;
}
#define make(obj) (mem(sizeof(obj)))
#define mems(n,t) (mem((n)*sizeof(t)))
#define extend(x,n,t) (remem((x),(n)*sizeof(t)))

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

#define list2(a,b) (new_cons((a), new_cons((b), NULL)))

static struct value *
new_primop(primop op)
{
	struct value *v;

	v = make(struct value);
	v->type = PRIMOP;
	v->primop = op;
	return v;
}

static struct value *
new_string(const char *src)
{
	struct value *v;
	char *ins, *head, *end;
	size_t n;

	n = strlen(src);
	v = make(struct value);
	v->type = STRING;
	v->string.data = mems(n+1, char);
	v->string.len = n;
	memcpy(v->string.data, src, n);

	/* unescape */
	ins = head = v->string.data;
	end = head + v->string.len;
	while (head != end) {
		if (*head == '\\') {
			v->string.len--;
			head++;
			if (head == end) {
				fprintf(stderr, "dangling escape character!\n");
				exit(1);
			}
			switch (*head) {
			case 't' : *ins++ = '\t'; break;
			case 'r' : *ins++ = '\r'; break;
			case 'n' : *ins++ = '\n'; break;
			case '\\': *ins++ = '\\'; break;
			case '0' : *ins++ = '\0'; break;
			default:   *ins++ = *head;
			}
			head++;
		} else {
			*ins++ = *head++;
		}
	}
	*ins = '\0';

	return v;
}

static struct value *
new_number(const char *src)
{
	struct value *v;

	v = make(struct value);
	v->type = NUMBER;
	v->number = 0;
	while (src && *src)
		v->number = v->number * 10 + (*src++ - '0');

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
	if (!v) return;

	switch (v->type) {
	case CONS:
		free_value(v->cons.car);
		free_value(v->cons.cdr);
		break;

	case STRING:
		free(v->string.data);
		break;

	default: /* noop */
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

	if (!v) {
		fprintf(io, "%s<NULL>", pre);
		return;
	}

	switch (v->type) {
	case CONS:
		fprintf(io, "%s(", pre);
again:
		fprintv(io, 0, v->cons.car);
		if (v->cons.cdr) {
			if (v->cons.cdr->type == CONS) {
				fprintf(io, " ");
				v = v->cons.cdr;
				goto again;
			} else {
				fprintf(io, " . ");
				fprintv(io, 0, v->cons.cdr);
			}
		}
		fprintf(io, ")");
		break;

	case PRIMOP:
		fprintf(io, "%s<op:%p>", pre, (unsigned char *)&(v->primop));
		break;

	case STRING:
		fprintf(io, "%s\"%s\"", pre, v->string.data);
		break;

	case NUMBER:
		fprintf(io, "%s%d", pre, v->number);
		break;

	case BOOLEAN:
		fprintf(io, "%s#%c", pre, v->boolean ? 't' : 'f');
		break;

	case SYMBOL:
		fprintf(io, "%s%s", pre, v->symbol->name);
		break;
	}
}

enum ttype {
	T_OPENP,
	T_CLOSEP,
	T_QUOTE,
	T_IDENT,
	T_STRING,
	T_NUMBER,
	T_TRUE,
	T_FALSE,
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

struct reader {
	int         fd;
	const char *file;

	char       *src;
	size_t      used;
	size_t      cap;

	struct pos  prev;
	struct pos  head;
};

static struct token *
new_token(struct reader *r, enum ttype type, char *data)
{
	struct token *token;

	token = make(struct token);
	token->type = type;
	copy_pos(&token->pos, &r->prev);
	token->len = r->head.offset - r->prev.offset;
	token->data = data;
	return token;
}

static void
free_token(struct token *token)
{
	free(token->data);
	free(token);
}

#define READER_BLOCK_SIZE 16384;

static void
read_more(struct reader *r)
{
	size_t n;

	if (r->fd < 0)
		return;

	if (r->used == r->cap) {
		r->cap += READER_BLOCK_SIZE;
		r->src = extend(r->src, r->cap, char);
	}

	n = read(r->fd, r->src + r->used, r->cap - r->used);
	if (n < 0) {
		fprintf(stderr, "error reading from %s: %s (error %d)\n",
				r->file, strerror(errno), errno);
		exit(1);

	} else if (n == 0) {
		close(r->fd);
		r->fd = -1;

	} else {
		r->used += n;
		r->src[r->used] = '\0';
	}
}

static char *
lexeme(struct reader *r)
{
	char *s;
	size_t len;

	len = r->head.offset - r->prev.offset;
	s = mem(len + 1);
	memcpy(s, r->src + r->prev.offset, len);
	return s;
}

/* peek(&reader) -> c

   return the character under the cursor,
   without changing the position.
 */
static inline char
peek(struct reader *r)
{
	if (r->head.offset == r->used)
		read_more(r);

	return r->src[r->head.offset];
}

/* next(&reader)

   advance the cursor one position, updating
   line / column markers as necessary.

   returns the character that was at offset,
   before any adjustments took place.
 */
static inline char
next(struct reader *r)
{
	if (r->head.offset == r->used)
		read_more(r);

	r->head.offset++;
	r->head.column++;
	if (r->src[r->head.offset] == '\n') {
		r->head.column = 1;
		r->head.line++;
	}

	return peek(r);
}

/* resync(&reader)

   put the head cursor into the prev cursor,
   overwriting what was there previously.
 */
static inline void
resync(struct reader *r)
{
	copy_pos(&r->prev, &r->head);
}

static struct token *
lex(struct reader *r)
{
	char c, q, esc;
	struct token *tok;

	/* eat whitespace */
again:
	for (c = peek(r); c && isspace(c); next(r), c = peek(r));
	resync(r);

	switch (c) {
	case '\0': return NULL;

	case ';':
		for (; c != '\n'; next(r), c = peek(r));
		goto again;

	case '\'': next(r); return new_token(r, T_QUOTE, NULL);

	case '#':
		next(r);
		switch (peek(r)) {
		case 't': next(r); return new_token(r, T_TRUE,  NULL);
		case 'f': next(r); return new_token(r, T_FALSE, NULL);
		default:
			return new_token(r, T_OOPS, "invalid '#' macro");
		}

	case '(': next(r); return new_token(r, T_OPENP, NULL);
	case ')': next(r); return new_token(r, T_CLOSEP, NULL);
	case '"':
		esc = 0; q = c; next(r); resync(r);
		for (c = peek(r); c && (esc || c != q); next(r), c = peek(r));
		tok = new_token(r, T_STRING, lexeme(r));
		next(r); return tok;
	}
	if (c && isdigit(c)) {
		for (c = peek(r); c && isdigit(c); next(r), c = peek(r));
		return new_token(r, T_NUMBER, lexeme(r));
	}
	if (c) {
		for (c = peek(r); c && !isspace(c) && c != ')'; next(r), c = peek(r));
		return new_token(r, T_IDENT, lexeme(r));
	}

	return new_token(r, T_OOPS, "syntax error");
}

#define truish(ok) ((ok) ? ROOK_TRUE : ROOK_FALSE)
#define CAR(l) ((l)->cons.car)
#define CDR(l) ((l)->cons.cdr)
#define CAAR(l) (CAR(CAR(l)))
#define CADR(l) (CAR(CDR(l)))
#define CDAR(l) (CDR(CAR(l)))
#define CDDR(l) (CDR(CDR(l)))
#define CADAR(l) (CAR(CDR(CAR(l))))
#define CADDR(l) (CAR(CDR(CDR(l))))
#define CADDAR(l) (CAR(CDR(CDR(CAR(l)))))

static struct value *
read1(struct reader *r);

#define readq(r) (new_cons(new_symbol("quote"), new_cons(read1(r), NULL)))

static struct value *
readl(struct reader *r)
{
	struct token *token;
	struct value *lst, *head, *tmp;

	head = lst = NULL;
	while ((token = lex(r)) != NULL) {
		switch (token->type) {
		case T_IDENT:  lst = new_cons(new_symbol(token->data), lst); break;
		case T_STRING: lst = new_cons(new_string(token->data), lst); break;
		case T_NUMBER: lst = new_cons(new_number(token->data), lst); break;
		case T_TRUE:   lst = new_cons(ROOK_TRUE, lst); break;
		case T_FALSE:  lst = new_cons(ROOK_FALSE, lst); break;
		case T_OPENP:  lst = new_cons(readl(r), lst); break;
		case T_QUOTE:  lst = new_cons(readq(r), lst); break;

		case T_OOPS:
			fprintf(stderr, "oops: %s\n", token->data);
			exit(1);

		case T_CLOSEP:
			while (lst) {
				tmp = lst; lst = CDR(lst);
				CDR(tmp) = head; head = tmp;
			}
			return tmp;
			break;
		}
	}

	fprintf(stderr, "unterminated list form!\n");
	exit(1);
}

static struct value *
read1(struct reader *r)
{
	struct token *token;
	struct value *v;

	token = lex(r);
	if (token == NULL)
		return NULL;

	switch (token->type) {
	case T_IDENT:  v = new_symbol(token->data); break;
	case T_STRING: v = new_string(token->data); break;
	case T_NUMBER: v = new_number(token->data); break;
	case T_TRUE:   v = ROOK_TRUE; break;
	case T_FALSE:  v = ROOK_FALSE; break;
	case T_OPENP:  v = readl(r); break;
	case T_QUOTE:  v = readq(r); break;

	case T_OOPS:
		fprintf(stderr, "oops: %s\n", token->data);
		exit(1);

	case T_CLOSEP:
		fprintf(stderr, "too many closing ')' found!\n");
		exit(1);

	default:
		fprintf(stderr, "unrecognized token type %02x\n", token->type);
		exit(1);
	}

	free_token(token);
	return v;
}

static struct reader *
file_reader(const char *file)
{
	int fd;
	struct reader *r;

	fd = open(file, O_RDONLY);
	if (fd < 0) return NULL;

	r = make(struct reader);
	r->fd = fd;
	r->file = file;
	return r;
}

static struct reader *
fd_reader(const char *name, int fd)
{
	struct reader *r;

	r = make(struct reader);
	r->fd = fd;
	r->file = name;
	return r;
}

static void
free_reader(struct reader *r)
{
	free(r->src);
	free(r);
}

static char
truthy(struct value *cond)
{
	switch (cond->type) {
	default:      return 0; /* UNKNOWN */
	case CONS:    return (cond->cons.car || cond->cons.cdr) ? 1 : 0;
	case STRING:  return cond->string.len != 0              ? 1 : 0;
	case NUMBER:  return cond->number != 0                  ? 1 : 0;
	case SYMBOL:  return 1; /* FIXME */
	case BOOLEAN: return cond->boolean;
	}
}

static void
arity(const char *msg, struct value *lst, size_t min, size_t max)
{
	size_t n;

	n = 0;
	while (lst) {
		if (lst->type != CONS) {
			fprintf(stderr, "%s: improper list!\n", msg);
			exit(1);
		}
		n++;
		lst = CDR(lst);
	}

	if (min == max) {
		if (n != min) {
			fprintf(stderr, "%s: wrong arity (expected %li, got %li)\n", msg, min, n);
			exit(1);
		}
		return;
	}

	if (n < min) {
		fprintf(stderr, "%s: wrong arity (expected at least %li, got %li)\n", msg, min, n);
		exit(1);
	}
	if (max > 0 && n > max) {
		fprintf(stderr, "%s: wrong arity (expected no more than %li, got %li)\n", msg, max, n);
		fprintv(stderr, 2, lst);
		fprintf(stderr, "\n");
		exit(1);
	}
}

static struct value *
primop_print(struct value *args)
{
	/* (print e) - print the s-expr e to standard output */
	arity("(print ...)", args, 1, 1);
	if (!QUIET) {
		fprintv(stderr, 0, CAR(args));
		fprintf(stderr, "\n");
	}
	return ROOK_TRUE;
}

static struct value *
primop_env(struct value *args)
{
	/* (env v) - return the value of environment variable v */
	arity("(env ...)", args, 1, 1);

	if (CAR(args)->type == STRING) {
		char *v = getenv(CAR(args)->string.data);
		return new_string(v ? v : "");
	}

	fprintf(stderr, "non-string name to (env name)!\n");
	exit(1);
}

static struct value *
primop_printf(struct value *args)
{
	/* (printf "..." ...) - print a formatted string to standard output */
	/* FIXME:

	   Right now, this 'printf' implementation just prints; it doesn't f.
	   Essentially, the format string is a DSL that we need to implement.
	 */
	arity("(printf ...)", args, 1, 1);
	if (CAR(args)->type != STRING) {
		fprintf(stderr, "non-string argument to printf!\n");
		exit(2);
	}
	if (!QUIET) {
		fprintf(stdout, "%s", CAR(args)->string.data); /* FIXME: doesn't handle \0 embedded */
	}
	return ROOK_TRUE;
}

static struct value *
primop_syscall(struct value *args)
{
	/* (syscall 'name ...) - issue a system call */
	arity("(syscall ...)", args, 1, 0);
	if (CAR(args)->type != SYMBOL) {
		fprintf(stderr, "non-symbol argument to syscall\n");
		exit(2);
	}

	if (CAR(args)->symbol == intern("exit")) {
		arity("(syscall 'exit ...)", args, 2, 2);
		if (CADR(args)->type != NUMBER) {
			fprintf(stderr, "invalid exit code: ");
			fprintv(stderr, 0, CADR(args));
			fprintf(stderr, "\n");
			exit(4);
		}
		exit(CADR(args)->number);
	}

	return ROOK_TRUE;
}

static struct value *
eval(struct value *, struct value *);

static struct value *
evlis(struct value *, struct value *);

static struct value *
evcon(struct value *, struct value *);

static struct value *
append(struct value *, struct value *);

static struct value *
assoc(struct value *, struct value *);

static struct value *
pair(struct value *, struct value *);

static struct value *
eval(struct value *expr, struct value *env)
{
	struct value *op, *lst;

	if (expr == NULL) {
		fprintf(stderr, "NULL expr passed to eval()!\n");
		exit(1);
	}

	if (expr->type == BOOLEAN
	 || expr->type == STRING
	 || expr->type == NUMBER) {
		return expr;
	}

	if (expr->type == SYMBOL) {
		return assoc(expr, env);
	}

	if (expr->type == CONS && CAR(expr)->type == SYMBOL) {
		if (CAR(expr)->symbol == intern("quote")) {
			/* (quote x) - return x, skipping evaluation */
			arity("(quote ...)", CDR(expr), 1, 1);
			return CADR(expr);
		}

		if (CAR(expr)->symbol == intern("atom")) {
			arity("(atom ...)", CDR(expr), 1, 1);
			expr = eval(CADR(expr), env);
			return truish(expr && expr->type != CONS);
		}

		if (CAR(expr)->symbol == intern("eq")) {
			arity("(eq ...)", CDR(expr), 2, 2);
			if (CADR(expr)->type != CADDR(expr)->type)
				return truish(0);

			switch (CADR(expr)->type) {
			default:     return truish(CADR(expr) == CADDR(expr));
			case NUMBER: return truish(CADR(expr)->number == CADDR(expr)->number);
			case STRING: return truish(CADR(expr)->string.len == CADDR(expr)->string.len
			                        && memcmp(CADR(expr)->string.data,
			                                  CADDR(expr)->string.data,
			                                  CADR(expr)->string.len) == 0);
			}
		}

		if (CAR(expr)->symbol == intern("cond")) {
			return evcon(CDR(expr), env);
		}

		if (CAR(expr)->symbol == intern("car")) {
			arity("(car ...)", CDR(expr), 1, 1);
			return CAR(eval(CADR(expr), env)); /* FIXME */
		}

		if (CAR(expr)->symbol == intern("cdr")) {
			arity("(cdr ...)", CDR(expr), 1, 1);
			return CDR(eval(CADR(expr), env)); /* FIXME */
		}

		if (CAR(expr)->symbol == intern("cons")) {
			arity("(cons ...)", CDR(expr), 2, 2);
			return new_cons(eval(CADR(expr), env),
			                eval(CADDR(expr), env));
		}

		if (CAR(expr)->symbol == intern("functions")) {
			/* (functions
			      ((f (args) e...)
			       (g (args) e...))
			     (f ...))
			 */
			arity("(functions ...)", CDR(expr), 2, 2);
			if (CADR(expr)->type != CONS) {
				fprintf(stderr, "(functions ...) requires a list of abstractions as its first argument.\n");
				exit(4);
			}
			lst = CADR(expr);
			while (lst != NULL) {
				env = new_cons(list2(CAAR(lst), new_cons(new_symbol("lambda"), CDAR(lst))), env);
				lst = CDR(lst);
			}

			return eval(CADDR(expr), env);
		}

		op = eval(CAR(expr), env);
		if (op->type == PRIMOP) {
			return op->primop(evlis(CDR(expr), env));
		}

		return eval(new_cons(op, evlis(CDR(expr), env)), env);
	}

	if (expr->type == CONS && CAR(expr)->type == CONS
	 && CAAR(expr)->type == SYMBOL && CAAR(expr)->symbol == intern("label")) {
		return eval(new_cons(CADDAR(expr), CDR(expr)),
		            new_cons(
		              list2(CADAR(expr), CAR(expr)),
		              env));
	}

	if (expr->type == CONS && CAR(expr)->type == CONS
	 && CAAR(expr)->type == SYMBOL && CAAR(expr)->symbol == intern("lambda")) {
		return eval(CADDAR(expr),
		            append(pair(CADAR(expr), evlis(CDR(expr), env)), env));
	}

	fprintf(stderr, "invalid form!\n");
	fprintv(stderr, 4, expr);
	fprintf(stderr, "\n");
	exit(1);
}

static struct value *
evlis(struct value *lst, struct value *env)
{
	if (!lst) return NULL;
	return new_cons(eval(CAR(lst), env),
	                evlis(CDR(lst), env));
}

static struct value *
evcon(struct value *cond, struct value *env)
{
	while (cond) {
		if (truthy(eval(CAAR(cond), env)))
			return eval(CADAR(cond), env);
		cond = CDR(cond);
	}
	return ROOK_FALSE;
}

static struct value *
append(struct value *x, struct value *y)
{
	if (!x) return y;
	return new_cons(CAR(x), append(CDR(x), y));
}

static struct value *
assoc(struct value *var, struct value *alst)
{
	if (var->type != SYMBOL) {
		fprintf(stderr, "invalid assoc(...) call -- var is not a symbol.\n");
		exit(3);
	}

	while (alst && CAR(alst)) {
		if (CAAR(alst)->type == SYMBOL
		 && CAAR(alst)->symbol == var->symbol)
			return CADAR(alst);
		alst = CDR(alst);
	}

	fprintf(stderr, "undefined symbol %s\n", var->symbol->name);
	exit(4);
}

static struct value *
pair(struct value *x, struct value *y)
{
	if (!x && !y) return NULL;
	if (x->type == CONS && y->type == CONS)
		return new_cons(
		         new_cons(CAR(x), new_cons(CAR(y), NULL)),
		         pair(CDR(x), CDR(y)));

	fprintf(stderr, "invalid pair call!\n");
	exit(1);
}

static struct value *
init()
{
	struct value *env = NULL;

	ROOK_TRUE = make(struct value);
	ROOK_TRUE->type = BOOLEAN;
	ROOK_TRUE->boolean = 1;

	ROOK_FALSE = make(struct value);
	ROOK_FALSE->type = BOOLEAN;
	ROOK_FALSE->boolean = 0;

	env = new_cons(list2(new_symbol("print"),   new_primop(primop_print)),   env);
	env = new_cons(list2(new_symbol("env"),     new_primop(primop_env)),     env);
	env = new_cons(list2(new_symbol("printf"),  new_primop(primop_printf)),  env);
	env = new_cons(list2(new_symbol("syscall"), new_primop(primop_syscall)), env);
	return env;
}

static void
repl(struct value *env)
{
	struct reader *r;
	struct value *ast;

	if (isatty(1)) {
		fprintf(stdout, "\033[1;32mrook\033[0m \033[1;31mpre-alpha\033[0m v0\n");
		fprintf(stdout, "copyright (c) 2018 James Hunt & Dennis Bell\n");
		fprintf(stdout, "---\n");
		fprintf(stdout, "\033[1;36m♜\033[0m  ");
	} else {
		fprintf(stdout, "rook pre-alpha v0\n");
		fprintf(stdout, "copyright (c) 2018 James Hunt & Dennis Bell\n");
		fprintf(stdout, "---\n");
		fprintf(stdout, "♜  ");
	}
	fflush(stdout);

	r = fd_reader("<stdin>", 0);
	while ((ast = read1(r)) != 0) {
		fprintv(stdout, 0, eval(ast, env));
		if (isatty(1)) {
			fprintf(stdout, "\n\033[1;36m♜\033[0m  ");
		} else {
			fprintf(stdout, "\n♜  ");
		}
		fflush(stdout);
	}
	fprintf(stdout, "\n");
}

int
main(int argc, char **argv)
{
	struct reader *r;
	struct value *form, *result, *env;

	struct {
		int evaluate;
		int quiet;
	} opts;

	memset(&opts, 0, sizeof(opts));

	for (;;) {
		int c, idx;
		static struct option longs[] = {
			{ "quiet",    no_argument, 0, 'q' },
			{ "evaluate", no_argument, 0, 'E' },
			{ 0,          0,           0,  0  }
		};

		c = getopt_long(argc, argv, "qE", longs, &idx);
		if (c < 0) break;
		switch (c) {
		case 'q':
			opts.quiet = 1;
			break;

		case 'E':
			opts.evaluate = 1;
			break;

		default:
			fprintf(stderr, "USAGE: %s [--evaluate] [--quiet] code.rk\n", argv[0]);
			exit(1);
		}
	}

	QUIET = opts.quiet;
	env = init();

	if (argc - optind != 1) {
		repl(env);
		exit(0);
	}

	if (strcmp(argv[optind], "-") == 0) {
		r = fd_reader("<stdin>", 0);
	} else {
		r = file_reader(argv[optind]);
		if (!r) {
			fprintf(stderr, "%s: %s (error %d)\n", argv[optind], strerror(errno), errno);
			exit(1);
		}
	}

	result = NULL;
	while ((form = read1(r)) != NULL) {
		result = eval(form, env);
		if (!result) {
			fprintf(stderr, "%s: parsing failed.\n", argv[optind]);
			exit(1);
		}
	}

	if (opts.evaluate) {
		fprintv(stdout, 0, result);
		fprintf(stdout, "\n");
	}

	free_value(result);
	free_reader(r);
	return 0;
}
