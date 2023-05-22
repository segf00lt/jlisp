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
#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

#define Array(T) T *
#define Map(T) struct { char *key; T value; } *
#define STATICARRLEN(x) (sizeof(x) / sizeof(*x))
#define STRLEN(s) ((sizeof(s) / sizeof(*s)) - 1)

enum Token_kind {
	T_LPAREN = 0,
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
	T_NUMBER,
	T_BOOLEAN,
	T_STRING,
	T_SYMBOL,
	T_EOF,
};

char *Token_kind_debug[] = {
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
	"T_NUMBER",
	"T_BOOLEAN",
	"T_STRING",
	"T_SYMBOL",
	"T_EOF",
};

char *reserved[] = {
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
	E_BOOLEAN,
	E_NUMBER,
};

typedef enum Token_kind Token_kind;
typedef enum Expr_kind Expr_kind;
typedef struct Expr Expr;
typedef unsigned long Expr_ref;
typedef struct Lexer Lexer;
typedef struct Debug_info Debug_info;

struct Debug_info {
	int line;
	int col;
	char *line_s;
	char *line_e;
};

struct Expr {
	Expr_kind kind;
	union {
		Array(Expr_ref) list;
		char *symbol;
		bool boolean;
		struct {
			int i;
			float f;
		} number;
	};
};

struct Lexer {
	char *src;
	char *ptr;
	char *text_s;
	char *text_e;
	char *unget;
	Token_kind token;
	int eof;
	Debug_info debug_info;
};

Lexer lexer;
Array(Expr) expr_arr;
Map(Expr_ref) symbol_table;

void init_lexer(Lexer *l, char *buf) {
	l->token = 0;
	l->text_s = l->text_e = l->unget = NULL;
	l->ptr = l->src = buf;
	l->debug_info = (Debug_info){ .line = 1, .col = 1, .line_s = buf };
	for(l->debug_info.line_e = l->ptr; *(l->debug_info.line_e) != '\n'; ++(l->debug_info.line_e));
	++(l->debug_info.line_e);
}

int lex(void) {
	char *tp, *s;
	int check;

	tp = s = NULL;
	lexer.text_s = lexer.text_e = NULL;
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

	tp = lexer.text_s = lexer.unget = lexer.ptr;

	if(*lexer.ptr == 0) {
		lexer.eof = 1;
		return T_EOF;
	}

	lexer.token = 0;

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
			lexer.text_e = tp + reserved_length[i];
			lexer.ptr += reserved_length[i];
			lexer.debug_info.col += reserved_length[i];
			return (lexer.token = i);
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
	}
	if(check) {
		lexer.debug_info.col += s - tp;
		lexer.text_s = tp;
		lexer.text_e = lexer.ptr = s;
		return (lexer.token = T_NUMBER);
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
		lexer.text_e = tp + STRLEN("true");
		lexer.ptr += STRLEN("true");
		lexer.debug_info.col += STRLEN("true");
		return (lexer.token = T_BOOLEAN);
	} else 	if(strstr(tp, "false") == tp &&
			(check == ' ' ||
			 check == '\t'||
			 check == '\n'||
			 check == ';' ||
			 check == '(' ||
			 check == ')')
		  )
	{
		lexer.text_e = tp + STRLEN("false");
		lexer.ptr += STRLEN("false");
		lexer.debug_info.col += STRLEN("false");
		return (lexer.token = T_BOOLEAN);
	}

	if(*tp == '"') {
		for(s = tp + 1; (check = *s) && *s != '"'; ++s);
		lexer.debug_info.col += s - tp;
		lexer.text_s = tp + 1;
		lexer.text_e = s;
		lexer.ptr = s + 1;
		return (lexer.token = T_STRING);
	}

	for(s = tp; *s && !isspace(*s) && *s != ')' && *s != '('; ++s);

	lexer.debug_info.col += s - tp;
	lexer.text_s = tp;
	lexer.text_e = lexer.ptr = s;

	return (lexer.token = T_SYMBOL);
}

void parse_error(char *expect) {
	fprintf(stderr, "jcc: error: parser expected %s got '", expect);
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fprintf(stderr, "'\n> line: %i, col: %i\n> ", lexer.debug_info.line, lexer.debug_info.col - (int)(lexer.ptr - lexer.unget));
	fwrite(lexer.debug_info.line_s, 1, lexer.debug_info.line_e - lexer.debug_info.line_s, stderr);
	exit(1);
}

void parse(void) {
	Token_kind t;
	do {
		t = lex();
		if(t != T_LPAREN)
			parse_error("(");
		t = lex();
		switch(t) {
		case T_LPAREN:
			break;
		case T_RPAREN:
			break;
		case T_QUOTE:
			break;
		case T_PLUS:
			break;
		case T_MINUS:
			break;
		case T_STAR:
			break;
		case T_SLASH:
			break;
		case T_PERCENT:
			break;
		case T_ANGBRACKEQ:
			break;
		case T_DEFINE:
			break;
		case T_LAMBDA:
			break;
		case T_IF:
			break;
		case T_AND:
			break;
		case T_OR:
			break;
		case T_NOT:
			break;
		case T_LIST:
			break;
		case T_CAR:
			break;
		case T_CDR:
			break;
		case T_NIL:
			break;
		case T_PRINT:
			break;
		case T_NUMBER:
			break;
		case T_BOOLEAN:
			break;
		case T_STRING:
			break;
		case T_SYMBOL:
			break;
		case T_EOF:
			break;
		}
		t = lex();
		if(t != T_RPAREN)
			parse_error(")");
		printf("%s\n", Token_kind_debug[t]);
	} while(!lexer.eof);
}

int main(int argc, char **argv) {
	if(argc == 1) {
		fprintf(stderr, "no source file\n");
		return 1;
	}

	Fmap file;
	fmapopen(argv[1], O_RDONLY, &file);
	char *src = file.buf;
	init_lexer(&lexer, src);
	parse();

	fmapclose(&file);
	return 0;
}
