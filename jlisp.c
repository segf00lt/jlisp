#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include "fmap.c"
#include "pool.c"
#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

#define Array(T) T *
#define Map(T) struct { char *key; T value; } *
#define STATICARRLEN(x) (sizeof(x) / sizeof(*x))
#define STRLEN(s) ((sizeof(s) / sizeof(*s)) - 1)

enum Token_kind {
	T_NONE = 0,
	T_LPAREN,
	T_RPAREN,
	T_QUOTE,
	T_PLUS,
	T_MINUS,
	T_STAR,
	T_SLASH,
	T_PERCENT,
	T_ANGBRACKEQ,
	T_DEFINE,
	T_LAMBDA,
	T_IF,
	T_AND,
	T_OR,
	T_NOT,
	T_LIST,
	T_CAR,
	T_CDR,
	T_NIL,
	T_PRINT,
	T_INTEGER,
	T_REAL,
	T_BOOLEAN,
	T_STRING,
	T_SYMBOL,
	T_EOF,
};

char *Token_kind_debug[] = {
	"T_NONE",
	"T_LPAREN",
	"T_RPAREN",
	"T_QUOTE",
	"T_PLUS",
	"T_MINUS",
	"T_STAR",
	"T_SLASH",
	"T_PERCENT",
	"T_ANGBRACKEQ",
	"T_DEFINE",
	"T_LAMBDA",
	"T_IF",
	"T_AND",
	"T_OR",
	"T_NOT",
	"T_LIST",
	"T_CAR",
	"T_CDR",
	"T_NIL",
	"T_PRINT",
	"T_INTEGER",
	"T_REAL",
	"T_BOOLEAN",
	"T_STRING",
	"T_SYMBOL",
	"T_EOF",
};

char *reserved[] = {
	"___________________________NONE",
	"(",
	")",
	"'",
	"+",
	"-",
	"*",
	"/",
	"%",
	"<=",
	"define",
	"lambda",
	"if",
	"and",
	"or",
	"not",
	"car",
	"cdr",
	"cons",
	"nil",
};

size_t reserved_length[] = {
	0,
	STRLEN("("),
	STRLEN(")"),
	STRLEN("'"),
	STRLEN("+"),
	STRLEN("-"),
	STRLEN("*"),
	STRLEN("/"),
	STRLEN("%"),
	STRLEN("<="),
	STRLEN("define"),
	STRLEN("lambda"),
	STRLEN("if"),
	STRLEN("and"),
	STRLEN("or"),
	STRLEN("not"),
	STRLEN("car"),
	STRLEN("cdr"),
	STRLEN("cons"),
	STRLEN("nil"),
};

enum Expr_kind {
	E_LIST,
	E_SYMBOL,
	E_STRING,
	E_BOOLEAN,
	E_INTEGER,
	E_REAL,
};

char *Expr_kind_debug[] = {
	"E_LIST",
	"E_SYMBOL",
	"E_STRING",
	"E_BOOLEAN",
	"E_INTEGER",
	"E_REAL",
};

typedef enum Token_kind Token_kind;
typedef struct Token Token;
typedef enum Expr_kind Expr_kind;
typedef struct Expr_list Expr_list;
typedef struct Expr Expr;
typedef struct VM VM;
typedef unsigned long Expr_ref;
typedef struct Lexer Lexer;
typedef struct Debug_info Debug_info;

struct Debug_info {
	int line;
	int col;
	char *line_s;
	char *line_e;
};

struct Token {
	Debug_info debug_info;
	Token_kind kind;
	union {
		char *s;
		long i;
		double f;
		bool b;
	};
};

struct Expr_list {
	Expr_ref head;
	Expr_ref tail;
};

struct Expr {
	Expr_kind kind;
	union {
		Expr_list list;
		char *symbol;
		char *string;
		bool boolean;
		long integer;
		double real;
	};
};

struct VM {
	Array(Expr_ref) stack;
};

struct Lexer {
	char *src;
	char *ptr;
	Token unget;
	Token token;
	int eof;
	Debug_info debug_info;
};

Lexer lexer;
Pool strpool;
VM vm;
Array(Expr) expr_arr;
Map(Expr_ref) symbol_table;

void init_lexer(Lexer *l, char *buf) {
	l->unget = l->token = (Token){0};
	l->ptr = l->src = buf;
	l->debug_info = (Debug_info){ .line = 1, .col = 1, .line_s = buf };
	for(l->debug_info.line_e = l->ptr; *(l->debug_info.line_e) != '\n'; ++(l->debug_info.line_e));
	++(l->debug_info.line_e);
}

static inline void unlex(Lexer *lexer) {
	lexer->token = lexer->unget;
}

Token lex(void) {
	Token token;
	char *tp, *s;
	int check;

	tp = s = NULL;
	check = 0;

	while(true) {
		while(isspace(*lexer.ptr)) { /* whitespace */
			if(*lexer.ptr != '\n') {
				++lexer.debug_info.col;
			} else {
				lexer.debug_info.col = 1;
				++lexer.debug_info.line;
				lexer.debug_info.line_e = lexer.debug_info.line_s = lexer.ptr + 1;
			}
			++lexer.ptr;
		}
		/* single-line comments */
		if(*lexer.ptr != ';')
			break;
		while(*lexer.ptr != '\n')
			++lexer.ptr;
		lexer.debug_info.col = 1;
		++lexer.debug_info.line;
	}

	if(lexer.ptr >= lexer.debug_info.line_e) {
		lexer.debug_info.line_s = lexer.debug_info.line_e;
		for(s = lexer.ptr; *s && *s != '\n'; ++s);
		lexer.debug_info.line_e = s + 1;
	}

	tp = lexer.ptr;

	if(*lexer.ptr == 0) {
		lexer.eof = 1;
		token.kind = T_EOF;
		return token;
	}

	if(lexer.token.kind != T_NONE)
		return lexer.token;

	lexer.token = (Token){0};

	for(size_t i = 0; i < STATICARRLEN(reserved); ++i) {
		check = tp[reserved_length[i]];
		if(strstr(tp, reserved[i]) == tp &&
				(check == ' ' ||
				 check == '\t'||
				 check == '\n'||
				 check == ';' ||
				 check == '(' ||
				 check == ')' ||
				 (i >= T_LPAREN && i <= T_ANGBRACKEQ))
		  )
		{
			token.kind = (Token_kind)i;
			token.s = reserved[i];
			token.debug_info = lexer.debug_info;
			lexer.ptr += reserved_length[i];
			lexer.debug_info.col += reserved_length[i];
			lexer.unget = token;
			return token;
		}
	}

	/* numbers, booleans, strings and identifiers */
	s = tp;
	if(*s == '-')
		++s;
	for(check = 0, s = tp; *s && isdigit(*s); ++s)
		++check;
	if(*s == '.' && s[1] != '.') {
		++s;
		for(check = 0; *s && isdigit(*s); ++s)
			++check;
		if(!check) {
			fprintf(stderr, "bad float constant\n");
			exit(1);
		}

		sscanf(tp, "%lf", &token.f);
		token.kind = T_REAL;
		lexer.debug_info.col += s - tp;
		lexer.ptr = s;
		lexer.unget = token;
		return token;
	}
	if(check) {
		sscanf(tp, "%li", &token.i);
		token.kind = T_INTEGER;
		lexer.debug_info.col += s - tp;
		lexer.ptr = s;
		lexer.unget = token;
		return token;
	}

	if(strstr(tp, "true") == tp &&
			(check == ' ' ||
			 check == '\t'||
			 check == '\n'||
			 check == ';' ||
			 check == '(' ||
			 check == ')')
			)
	{
		token.kind = T_BOOLEAN;
		token.b = true;
		lexer.ptr += STRLEN("true");
		lexer.debug_info.col += STRLEN("true");
		lexer.unget = token;
		return token;
	} else 	if(strstr(tp, "false") == tp &&
			(check == ' ' ||
			 check == '\t'||
			 check == '\n'||
			 check == ';' ||
			 check == '(' ||
			 check == ')')
		  )
	{
		token.kind = T_BOOLEAN;
		token.b = false;
		lexer.ptr += STRLEN("false");
		lexer.debug_info.col += STRLEN("false");
		lexer.unget = token;
		return token;
	}

	if(*tp == '"') {
		for(s = tp + 1; (check = *s) && *s != '"'; ++s);
		token.kind = T_STRING;
		token.s = pool_alloc_string(&strpool, s - tp, tp);
		lexer.debug_info.col += s - tp + 1;
		lexer.ptr = s + 1;
		lexer.unget = token;
		return token;
	}

	for(s = tp; *s && !isspace(*s) && *s != ')' && *s != '('; ++s);

	token.kind = T_SYMBOL;
	token.s = pool_alloc_string(&strpool, s - tp, tp);
	lexer.debug_info.col += s - tp;
	lexer.ptr = s;
	lexer.unget = token;

	return token;
}

void parse_error(char *str) {
	fprintf(stderr, "jcc: error: parser: %s", str);
	exit(1);
}

void parse(void) {
	Token t;
	Expr expr;
	size_t len;

	expr = (Expr){0};
	t = lex();
	switch(t.kind) {
		case T_NONE:
			parse_error("invalid token\n");
			break;
		case T_LPAREN:
			len = arrlen(expr_arr);
			expr = (Expr){ .kind = E_LIST, .list = { .head = len + 1 } };
			arrpush(expr_arr, expr);
			while(!lexer.eof && lexer.unget.kind != T_RPAREN)
				parse();
			if(lexer.eof)
				parse_error("unexpected eof\n");
			lexer.unget.kind = T_NONE;
			expr_arr[len].list.tail = arrlen(expr_arr) - 1;
			break;
		case T_RPAREN:
			break;
		case T_QUOTE:
		case T_PLUS: case T_MINUS: case T_STAR: case T_SLASH: case T_PERCENT:
		case T_ANGBRACKEQ:
		case T_DEFINE:
		case T_LAMBDA:
		case T_IF: case T_AND: case T_OR: case T_NOT:
		case T_LIST: case T_CAR: case T_CDR: case T_NIL:
		case T_PRINT:
			expr = (Expr){ .kind = E_SYMBOL, .symbol = reserved[t.kind] };
			arrpush(expr_arr, expr);
			break;
		case T_INTEGER:
			expr = (Expr){ .kind = E_INTEGER, .integer = t.i };
			arrpush(expr_arr, expr);
			break;
		case T_REAL:
			expr = (Expr){ .kind = E_REAL, .real = t.f };
			arrpush(expr_arr, expr);
			break;
		case T_BOOLEAN:
			expr = (Expr){ .kind = E_BOOLEAN, .boolean = t.b };
			arrpush(expr_arr, expr);
			break;
		case T_STRING:
			expr = (Expr){ .kind = E_STRING, .string = t.s };
			arrpush(expr_arr, expr);
			break;
		case T_SYMBOL:
			expr = (Expr){ .kind = E_SYMBOL, .symbol = t.s };
			arrpush(expr_arr, expr);
			break;
		case T_EOF:
			return;
	}
}

void eval() {
	for(size_t i = 0; i < arrlen(expr_arr); ++i) {
		fprintf(stderr, "EXPR %zu\nkind: %s\n", i, Expr_kind_debug[expr_arr[i].kind]);
		switch(expr_arr[i].kind) {
		case E_LIST:
			fprintf(stderr, "head: %lu\ntail: %lu\n\n", expr_arr[i].list.head, expr_arr[i].list.tail);
			break;
		case E_SYMBOL:
			fprintf(stderr, "symbol: %s\n\n", expr_arr[i].symbol);
			break;
		case E_STRING:
			fprintf(stderr, "string: %s\n\n", expr_arr[i].string);
			break;
		case E_BOOLEAN:
			fprintf(stderr, "boolean: %s\n\n", (expr_arr[i].boolean) ? "true" : "false");
			break;
		case E_INTEGER:
			fprintf(stderr, "integer: %li\n\n", expr_arr[i].integer);
			break;
		case E_REAL:
			fprintf(stderr, "real: %lf\n\n", expr_arr[i].real);
			break;
		}
	}
}

int main(int argc, char **argv) {
	if(argc == 1) {
		fprintf(stderr, "no source file\n");
		return 1;
	}

	Fmap file;
	pool_init(&strpool, 1, 4096, 2);
	fmapopen(argv[1], O_RDONLY, &file);
	char *src = file.buf;
	init_lexer(&lexer, src);
	while(!lexer.eof)
		parse();
	eval();

	if(expr_arr)
		arrfree(expr_arr);
	if(symbol_table)
		hmfree(symbol_table);
	pool_free(&strpool);
	fmapclose(&file);
	return 0;
}
