/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 7 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:339  */

#undef PARSER_DEBUG
#ifdef PARSER_DEBUG
# define YYDEBUG 1
#endif
#define YYERROR_VERBOSE 1
/*
 * Force yacc to use our memory management.  This is a little evil because
 * the macros assume that "parser_state *p" is in scope
 */
#define YYMALLOC(n)    mrb_malloc(p->mrb, (n))
#define YYFREE(o)      mrb_free(p->mrb, (o))
#define YYSTACK_USE_ALLOCA 0

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <mruby.h>
#include <mruby/compile.h>
#include <mruby/proc.h>
#include <mruby/error.h>
#include <mruby/throw.h>
#include "node.h"

#define YYLEX_PARAM p

typedef mrb_ast_node node;
typedef struct mrb_parser_state parser_state;
typedef struct mrb_parser_heredoc_info parser_heredoc_info;

static int yyparse(parser_state *p);
static int yylex(void *lval, parser_state *p);
static void yyerror(parser_state *p, const char *s);
static void yywarn(parser_state *p, const char *s);
static void yywarning(parser_state *p, const char *s);
static void backref_error(parser_state *p, node *n);
static void void_expr_error(parser_state *p, node *n);
static void tokadd(parser_state *p, int32_t c);

#define identchar(c) (ISALNUM(c) || (c) == '_' || !ISASCII(c))

typedef unsigned int stack_type;

#define BITSTACK_PUSH(stack, n) ((stack) = ((stack)<<1)|((n)&1))
#define BITSTACK_POP(stack)     ((stack) = (stack) >> 1)
#define BITSTACK_LEXPOP(stack)  ((stack) = ((stack) >> 1) | ((stack) & 1))
#define BITSTACK_SET_P(stack)   ((stack)&1)

#define COND_PUSH(n)    BITSTACK_PUSH(p->cond_stack, (n))
#define COND_POP()      BITSTACK_POP(p->cond_stack)
#define COND_LEXPOP()   BITSTACK_LEXPOP(p->cond_stack)
#define COND_P()        BITSTACK_SET_P(p->cond_stack)

#define CMDARG_PUSH(n)  BITSTACK_PUSH(p->cmdarg_stack, (n))
#define CMDARG_POP()    BITSTACK_POP(p->cmdarg_stack)
#define CMDARG_LEXPOP() BITSTACK_LEXPOP(p->cmdarg_stack)
#define CMDARG_P()      BITSTACK_SET_P(p->cmdarg_stack)

#define SET_LINENO(c,n) ((c)->lineno = (n))
#define NODE_LINENO(c,n) do {\
  if (n) {\
     (c)->filename_index = (n)->filename_index;\
     (c)->lineno = (n)->lineno;\
  }\
} while (0)

#define sym(x) ((mrb_sym)(intptr_t)(x))
#define nsym(x) ((node*)(intptr_t)(x))
#define nint(x) ((node*)(intptr_t)(x))
#define intn(x) ((int)(intptr_t)(x))

static inline mrb_sym
intern_cstr_gen(parser_state *p, const char *s)
{
  return mrb_intern_cstr(p->mrb, s);
}
#define intern_cstr(s) intern_cstr_gen(p,(s))

static inline mrb_sym
intern_gen(parser_state *p, const char *s, size_t len)
{
  return mrb_intern(p->mrb, s, len);
}
#define intern(s,len) intern_gen(p,(s),(len))

static inline mrb_sym
intern_gen_c(parser_state *p, const char c)
{
  return mrb_intern(p->mrb, &c, 1);
}
#define intern_c(c) intern_gen_c(p,(c))

static void
cons_free_gen(parser_state *p, node *cons)
{
  cons->cdr = p->cells;
  p->cells = cons;
}
#define cons_free(c) cons_free_gen(p, (c))

static void*
parser_palloc(parser_state *p, size_t size)
{
  void *m = mrb_pool_alloc(p->pool, size);

  if (!m) {
    MRB_THROW(p->jmp);
  }
  return m;
}

static node*
cons_gen(parser_state *p, node *car, node *cdr)
{
  node *c;

  if (p->cells) {
    c = p->cells;
    p->cells = p->cells->cdr;
  }
  else {
    c = (node *)parser_palloc(p, sizeof(mrb_ast_node));
  }

  c->car = car;
  c->cdr = cdr;
  c->lineno = p->lineno;
  c->filename_index = p->current_filename_index;
  return c;
}
#define cons(a,b) cons_gen(p,(a),(b))

static node*
list1_gen(parser_state *p, node *a)
{
  return cons(a, 0);
}
#define list1(a) list1_gen(p, (a))

static node*
list2_gen(parser_state *p, node *a, node *b)
{
  return cons(a, cons(b,0));
}
#define list2(a,b) list2_gen(p, (a),(b))

static node*
list3_gen(parser_state *p, node *a, node *b, node *c)
{
  return cons(a, cons(b, cons(c,0)));
}
#define list3(a,b,c) list3_gen(p, (a),(b),(c))

static node*
list4_gen(parser_state *p, node *a, node *b, node *c, node *d)
{
  return cons(a, cons(b, cons(c, cons(d, 0))));
}
#define list4(a,b,c,d) list4_gen(p, (a),(b),(c),(d))

static node*
list5_gen(parser_state *p, node *a, node *b, node *c, node *d, node *e)
{
  return cons(a, cons(b, cons(c, cons(d, cons(e, 0)))));
}
#define list5(a,b,c,d,e) list5_gen(p, (a),(b),(c),(d),(e))

static node*
list6_gen(parser_state *p, node *a, node *b, node *c, node *d, node *e, node *f)
{
  return cons(a, cons(b, cons(c, cons(d, cons(e, cons(f, 0))))));
}
#define list6(a,b,c,d,e,f) list6_gen(p, (a),(b),(c),(d),(e),(f))

static node*
append_gen(parser_state *p, node *a, node *b)
{
  node *c = a;

  if (!a) return b;
  while (c->cdr) {
    c = c->cdr;
  }
  if (b) {
    c->cdr = b;
  }
  return a;
}
#define append(a,b) append_gen(p,(a),(b))
#define push(a,b) append_gen(p,(a),list1(b))

static char*
parser_strndup(parser_state *p, const char *s, size_t len)
{
  char *b = (char *)parser_palloc(p, len+1);

  memcpy(b, s, len);
  b[len] = '\0';
  return b;
}
#undef strndup
#define strndup(s,len) parser_strndup(p, s, len)

static char*
parser_strdup(parser_state *p, const char *s)
{
  return parser_strndup(p, s, strlen(s));
}
#undef strdup
#define strdup(s) parser_strdup(p, s)

/* xxx ----------------------------- */

static node*
local_switch(parser_state *p)
{
  node *prev = p->locals;

  p->locals = cons(0, 0);
  return prev;
}

static void
local_resume(parser_state *p, node *prev)
{
  p->locals = prev;
}

static void
local_nest(parser_state *p)
{
  p->locals = cons(0, p->locals);
}

static void
local_unnest(parser_state *p)
{
  if (p->locals) {
    p->locals = p->locals->cdr;
  }
}

static mrb_bool
local_var_p(parser_state *p, mrb_sym sym)
{
  node *l = p->locals;

  while (l) {
    node *n = l->car;
    while (n) {
      if (sym(n->car) == sym) return TRUE;
      n = n->cdr;
    }
    l = l->cdr;
  }
  return FALSE;
}

static void
local_add_f(parser_state *p, mrb_sym sym)
{
  if (p->locals) {
    p->locals->car = push(p->locals->car, nsym(sym));
  }
}

static void
local_add(parser_state *p, mrb_sym sym)
{
  if (!local_var_p(p, sym)) {
    local_add_f(p, sym);
  }
}

static node*
locals_node(parser_state *p)
{
  return p->locals ? p->locals->car : NULL;
}

/* (:scope (vars..) (prog...)) */
static node*
new_scope(parser_state *p, node *body)
{
  return cons((node*)NODE_SCOPE, cons(locals_node(p), body));
}

/* (:begin prog...) */
static node*
new_begin(parser_state *p, node *body)
{
  if (body) {
    return list2((node*)NODE_BEGIN, body);
  }
  return cons((node*)NODE_BEGIN, 0);
}

#define newline_node(n) (n)

/* (:rescue body rescue else) */
static node*
new_rescue(parser_state *p, node *body, node *resq, node *els)
{
  return list4((node*)NODE_RESCUE, body, resq, els);
}

static node*
new_mod_rescue(parser_state *p, node *body, node *resq)
{
  return new_rescue(p, body, list1(list3(0, 0, resq)), 0);
}

/* (:ensure body ensure) */
static node*
new_ensure(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_ENSURE, cons(a, cons(0, b)));
}

/* (:nil) */
static node*
new_nil(parser_state *p)
{
  return list1((node*)NODE_NIL);
}

/* (:true) */
static node*
new_true(parser_state *p)
{
  return list1((node*)NODE_TRUE);
}

/* (:false) */
static node*
new_false(parser_state *p)
{
  return list1((node*)NODE_FALSE);
}

/* (:alias new old) */
static node*
new_alias(parser_state *p, mrb_sym a, mrb_sym b)
{
  return cons((node*)NODE_ALIAS, cons(nsym(a), nsym(b)));
}

/* (:if cond then else) */
static node*
new_if(parser_state *p, node *a, node *b, node *c)
{
  void_expr_error(p, a);
  return list4((node*)NODE_IF, a, b, c);
}

/* (:unless cond then else) */
static node*
new_unless(parser_state *p, node *a, node *b, node *c)
{
  void_expr_error(p, a);
  return list4((node*)NODE_IF, a, c, b);
}

/* (:while cond body) */
static node*
new_while(parser_state *p, node *a, node *b)
{
  void_expr_error(p, a);
  return cons((node*)NODE_WHILE, cons(a, b));
}

/* (:until cond body) */
static node*
new_until(parser_state *p, node *a, node *b)
{
  void_expr_error(p, a);
  return cons((node*)NODE_UNTIL, cons(a, b));
}

/* (:for var obj body) */
static node*
new_for(parser_state *p, node *v, node *o, node *b)
{
  void_expr_error(p, o);
  return list4((node*)NODE_FOR, v, o, b);
}

/* (:case a ((when ...) body) ((when...) body)) */
static node*
new_case(parser_state *p, node *a, node *b)
{
  node *n = list2((node*)NODE_CASE, a);
  node *n2 = n;

  void_expr_error(p, a);
  while (n2->cdr) {
    n2 = n2->cdr;
  }
  n2->cdr = b;
  return n;
}

/* (:postexe a) */
static node*
new_postexe(parser_state *p, node *a)
{
  return cons((node*)NODE_POSTEXE, a);
}

/* (:self) */
static node*
new_self(parser_state *p)
{
  return list1((node*)NODE_SELF);
}

/* (:call a b c) */
static node*
new_call(parser_state *p, node *a, mrb_sym b, node *c, int pass)
{
  node *n = list4(nint(pass?NODE_CALL:NODE_SCALL), a, nsym(b), c);
  void_expr_error(p, a);
  NODE_LINENO(n, a);
  return n;
}

/* (:fcall self mid args) */
static node*
new_fcall(parser_state *p, mrb_sym b, node *c)
{
  node *n = new_self(p);
  NODE_LINENO(n, c);
  n = list4((node*)NODE_FCALL, n, nsym(b), c);
  NODE_LINENO(n, c);
  return n;
}

/* (:super . c) */
static node*
new_super(parser_state *p, node *c)
{
  return cons((node*)NODE_SUPER, c);
}

/* (:zsuper) */
static node*
new_zsuper(parser_state *p)
{
  return list1((node*)NODE_ZSUPER);
}

/* (:yield . c) */
static node*
new_yield(parser_state *p, node *c)
{
  if (c) {
    if (c->cdr) {
      yyerror(p, "both block arg and actual block given");
    }
    return cons((node*)NODE_YIELD, c->car);
  }
  return cons((node*)NODE_YIELD, 0);
}

/* (:return . c) */
static node*
new_return(parser_state *p, node *c)
{
  return cons((node*)NODE_RETURN, c);
}

/* (:break . c) */
static node*
new_break(parser_state *p, node *c)
{
  return cons((node*)NODE_BREAK, c);
}

/* (:next . c) */
static node*
new_next(parser_state *p, node *c)
{
  return cons((node*)NODE_NEXT, c);
}

/* (:redo) */
static node*
new_redo(parser_state *p)
{
  return list1((node*)NODE_REDO);
}

/* (:retry) */
static node*
new_retry(parser_state *p)
{
  return list1((node*)NODE_RETRY);
}

/* (:dot2 a b) */
static node*
new_dot2(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DOT2, cons(a, b));
}

/* (:dot3 a b) */
static node*
new_dot3(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DOT3, cons(a, b));
}

/* (:colon2 b c) */
static node*
new_colon2(parser_state *p, node *b, mrb_sym c)
{
  void_expr_error(p, b);
  return cons((node*)NODE_COLON2, cons(b, nsym(c)));
}

/* (:colon3 . c) */
static node*
new_colon3(parser_state *p, mrb_sym c)
{
  return cons((node*)NODE_COLON3, nsym(c));
}

/* (:and a b) */
static node*
new_and(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_AND, cons(a, b));
}

/* (:or a b) */
static node*
new_or(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_OR, cons(a, b));
}

/* (:array a...) */
static node*
new_array(parser_state *p, node *a)
{
  return cons((node*)NODE_ARRAY, a);
}

/* (:splat . a) */
static node*
new_splat(parser_state *p, node *a)
{
  return cons((node*)NODE_SPLAT, a);
}

/* (:hash (k . v) (k . v)...) */
static node*
new_hash(parser_state *p, node *a)
{
  return cons((node*)NODE_HASH, a);
}

/* (:sym . a) */
static node*
new_sym(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_SYM, nsym(sym));
}

static mrb_sym
new_strsym(parser_state *p, node* str)
{
  const char *s = (const char*)str->cdr->car;
  size_t len = (size_t)str->cdr->cdr;

  return mrb_intern(p->mrb, s, len);
}

/* (:lvar . a) */
static node*
new_lvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_LVAR, nsym(sym));
}

/* (:gvar . a) */
static node*
new_gvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_GVAR, nsym(sym));
}

/* (:ivar . a) */
static node*
new_ivar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_IVAR, nsym(sym));
}

/* (:cvar . a) */
static node*
new_cvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_CVAR, nsym(sym));
}

/* (:const . a) */
static node*
new_const(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_CONST, nsym(sym));
}

/* (:undef a...) */
static node*
new_undef(parser_state *p, mrb_sym sym)
{
  return list2((node*)NODE_UNDEF, nsym(sym));
}

/* (:class class super body) */
static node*
new_class(parser_state *p, node *c, node *s, node *b)
{
  void_expr_error(p, s);
  return list4((node*)NODE_CLASS, c, s, cons(locals_node(p), b));
}

/* (:sclass obj body) */
static node*
new_sclass(parser_state *p, node *o, node *b)
{
  void_expr_error(p, o);
  return list3((node*)NODE_SCLASS, o, cons(locals_node(p), b));
}

/* (:module module body) */
static node*
new_module(parser_state *p, node *m, node *b)
{
  return list3((node*)NODE_MODULE, m, cons(locals_node(p), b));
}

/* (:def m lv (arg . body)) */
static node*
new_def(parser_state *p, mrb_sym m, node *a, node *b)
{
  return list5((node*)NODE_DEF, nsym(m), locals_node(p), a, b);
}

/* (:sdef obj m lv (arg . body)) */
static node*
new_sdef(parser_state *p, node *o, mrb_sym m, node *a, node *b)
{
  void_expr_error(p, o);
  return list6((node*)NODE_SDEF, o, nsym(m), locals_node(p), a, b);
}

/* (:arg . sym) */
static node*
new_arg(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_ARG, nsym(sym));
}

/* (m o r m2 b) */
/* m: (a b c) */
/* o: ((a . e1) (b . e2)) */
/* r: a */
/* m2: (a b c) */
/* b: a */
static node*
new_args(parser_state *p, node *m, node *opt, mrb_sym rest, node *m2, mrb_sym blk)
{
  node *n;

  n = cons(m2, nsym(blk));
  n = cons(nsym(rest), n);
  n = cons(opt, n);
  return cons(m, n);
}

/* (:block_arg . a) */
static node*
new_block_arg(parser_state *p, node *a)
{
  return cons((node*)NODE_BLOCK_ARG, a);
}

/* (:block arg body) */
static node*
new_block(parser_state *p, node *a, node *b)
{
  return list4((node*)NODE_BLOCK, locals_node(p), a, b);
}

/* (:lambda arg body) */
static node*
new_lambda(parser_state *p, node *a, node *b)
{
  return list4((node*)NODE_LAMBDA, locals_node(p), a, b);
}

/* (:asgn lhs rhs) */
static node*
new_asgn(parser_state *p, node *a, node *b)
{
  void_expr_error(p, b);
  return cons((node*)NODE_ASGN, cons(a, b));
}

/* (:masgn mlhs=(pre rest post)  mrhs) */
static node*
new_masgn(parser_state *p, node *a, node *b)
{
  void_expr_error(p, b);
  return cons((node*)NODE_MASGN, cons(a, b));
}

/* (:asgn lhs rhs) */
static node*
new_op_asgn(parser_state *p, node *a, mrb_sym op, node *b)
{
  void_expr_error(p, b);
  return list4((node*)NODE_OP_ASGN, a, nsym(op), b);
}

/* (:int . i) */
static node*
new_int(parser_state *p, const char *s, int base)
{
  return list3((node*)NODE_INT, (node*)strdup(s), nint(base));
}

/* (:float . i) */
static node*
new_float(parser_state *p, const char *s)
{
  return cons((node*)NODE_FLOAT, (node*)strdup(s));
}

/* (:str . (s . len)) */
static node*
new_str(parser_state *p, const char *s, size_t len)
{
  return cons((node*)NODE_STR, cons((node*)strndup(s, len), nint(len)));
}

/* (:dstr . a) */
static node*
new_dstr(parser_state *p, node *a)
{
  return cons((node*)NODE_DSTR, a);
}

/* (:str . (s . len)) */
static node*
new_xstr(parser_state *p, const char *s, int len)
{
  return cons((node*)NODE_XSTR, cons((node*)strndup(s, len), nint(len)));
}

/* (:xstr . a) */
static node*
new_dxstr(parser_state *p, node *a)
{
  return cons((node*)NODE_DXSTR, a);
}

/* (:dsym . a) */
static node*
new_dsym(parser_state *p, node *a)
{
  return cons((node*)NODE_DSYM, new_dstr(p, a));
}

/* (:regx . (s . (opt . enc))) */
static node*
new_regx(parser_state *p, const char *p1, const char* p2, const char* p3)
{
  return cons((node*)NODE_REGX, cons((node*)p1, cons((node*)p2, (node*)p3)));
}

/* (:dregx . (a . b)) */
static node*
new_dregx(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DREGX, cons(a, b));
}

/* (:backref . n) */
static node*
new_back_ref(parser_state *p, int n)
{
  return cons((node*)NODE_BACK_REF, nint(n));
}

/* (:nthref . n) */
static node*
new_nth_ref(parser_state *p, int n)
{
  return cons((node*)NODE_NTH_REF, nint(n));
}

/* (:heredoc . a) */
static node*
new_heredoc(parser_state *p)
{
  parser_heredoc_info *inf = (parser_heredoc_info *)parser_palloc(p, sizeof(parser_heredoc_info));
  return cons((node*)NODE_HEREDOC, (node*)inf);
}

static void
new_bv(parser_state *p, mrb_sym id)
{
}

static node*
new_literal_delim(parser_state *p)
{
  return cons((node*)NODE_LITERAL_DELIM, 0);
}

/* (:words . a) */
static node*
new_words(parser_state *p, node *a)
{
  return cons((node*)NODE_WORDS, a);
}

/* (:symbols . a) */
static node*
new_symbols(parser_state *p, node *a)
{
  return cons((node*)NODE_SYMBOLS, a);
}

/* xxx ----------------------------- */

/* (:call a op) */
static node*
call_uni_op(parser_state *p, node *recv, const char *m)
{
  void_expr_error(p, recv);
  return new_call(p, recv, intern_cstr(m), 0, 1);
}

/* (:call a op b) */
static node*
call_bin_op(parser_state *p, node *recv, const char *m, node *arg1)
{
  return new_call(p, recv, intern_cstr(m), list1(list1(arg1)), 1);
}

static void
args_with_block(parser_state *p, node *a, node *b)
{
  if (b) {
    if (a->cdr) {
      yyerror(p, "both block arg and actual block given");
    }
    a->cdr = b;
  }
}

static void
call_with_block(parser_state *p, node *a, node *b)
{
  node *n;

  switch ((enum node_type)intn(a->car)) {
  case NODE_SUPER:
  case NODE_ZSUPER:
    if (!a->cdr) a->cdr = cons(0, b);
    else {
      args_with_block(p, a->cdr, b);
    }
    break;
  case NODE_CALL:
  case NODE_FCALL:
  case NODE_SCALL:
    n = a->cdr->cdr->cdr;
    if (!n->car) n->car = cons(0, b);
    else {
      args_with_block(p, n->car, b);
    }
    break;
  default:
    break;
  }
}

static node*
negate_lit(parser_state *p, node *n)
{
  return cons((node*)NODE_NEGATE, n);
}

static node*
cond(node *n)
{
  return n;
}

static node*
ret_args(parser_state *p, node *n)
{
  if (n->cdr) {
    yyerror(p, "block argument should not be given");
    return NULL;
  }
  if (!n->car->cdr) return n->car->car;
  return new_array(p, n->car);
}

static void
assignable(parser_state *p, node *lhs)
{
  if (intn(lhs->car) == NODE_LVAR) {
    local_add(p, sym(lhs->cdr));
  }
}

static node*
var_reference(parser_state *p, node *lhs)
{
  node *n;

  if (intn(lhs->car) == NODE_LVAR) {
    if (!local_var_p(p, sym(lhs->cdr))) {
      n = new_fcall(p, sym(lhs->cdr), 0);
      cons_free(lhs);
      return n;
    }
  }

  return lhs;
}

typedef enum mrb_string_type  string_type;

static node*
new_strterm(parser_state *p, string_type type, int term, int paren)
{
  return cons(nint(type), cons((node*)0, cons(nint(paren), nint(term))));
}

static void
end_strterm(parser_state *p)
{
  cons_free(p->lex_strterm->cdr->cdr);
  cons_free(p->lex_strterm->cdr);
  cons_free(p->lex_strterm);
  p->lex_strterm = NULL;
}

static parser_heredoc_info *
parsing_heredoc_inf(parser_state *p)
{
  node *nd = p->parsing_heredoc;
  if (nd == NULL)
    return NULL;
  /* mrb_assert(nd->car->car == NODE_HEREDOC); */
  return (parser_heredoc_info*)nd->car->cdr;
}

static void
heredoc_treat_nextline(parser_state *p)
{
  if (p->heredocs_from_nextline == NULL)
    return;
  if (p->parsing_heredoc == NULL) {
    node *n;
    p->parsing_heredoc = p->heredocs_from_nextline;
    p->lex_strterm_before_heredoc = p->lex_strterm;
    p->lex_strterm = new_strterm(p, parsing_heredoc_inf(p)->type, 0, 0);
    n = p->all_heredocs;
    if (n) {
      while (n->cdr)
        n = n->cdr;
      n->cdr = p->parsing_heredoc;
    }
    else {
      p->all_heredocs = p->parsing_heredoc;
    }
  }
  else {
    node *n, *m;
    m = p->heredocs_from_nextline;
    while (m->cdr)
      m = m->cdr;
    n = p->all_heredocs;
    mrb_assert(n != NULL);
    if (n == p->parsing_heredoc) {
      m->cdr = n;
      p->all_heredocs = p->heredocs_from_nextline;
      p->parsing_heredoc = p->heredocs_from_nextline;
    }
    else {
      while (n->cdr != p->parsing_heredoc) {
        n = n->cdr;
        mrb_assert(n != NULL);
      }
      m->cdr = n->cdr;
      n->cdr = p->heredocs_from_nextline;
      p->parsing_heredoc = p->heredocs_from_nextline;
    }
  }
  p->heredocs_from_nextline = NULL;
}

static void
heredoc_end(parser_state *p)
{
  p->parsing_heredoc = p->parsing_heredoc->cdr;
  if (p->parsing_heredoc == NULL) {
    p->lstate = EXPR_BEG;
    p->cmd_start = TRUE;
    end_strterm(p);
    p->lex_strterm = p->lex_strterm_before_heredoc;
    p->lex_strterm_before_heredoc = NULL;
    p->heredoc_end_now = TRUE;
  }
  else {
    /* next heredoc */
    p->lex_strterm->car = nint(parsing_heredoc_inf(p)->type);
  }
}
#define is_strterm_type(p,str_func) (intn((p)->lex_strterm->car) & (str_func))

/* xxx ----------------------------- */


#line 1102 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    keyword_class = 258,
    keyword_module = 259,
    keyword_def = 260,
    keyword_begin = 261,
    keyword_if = 262,
    keyword_unless = 263,
    keyword_while = 264,
    keyword_until = 265,
    keyword_for = 266,
    keyword_undef = 267,
    keyword_rescue = 268,
    keyword_ensure = 269,
    keyword_end = 270,
    keyword_then = 271,
    keyword_elsif = 272,
    keyword_else = 273,
    keyword_case = 274,
    keyword_when = 275,
    keyword_break = 276,
    keyword_next = 277,
    keyword_redo = 278,
    keyword_retry = 279,
    keyword_in = 280,
    keyword_do = 281,
    keyword_do_cond = 282,
    keyword_do_block = 283,
    keyword_do_LAMBDA = 284,
    keyword_return = 285,
    keyword_yield = 286,
    keyword_super = 287,
    keyword_self = 288,
    keyword_nil = 289,
    keyword_true = 290,
    keyword_false = 291,
    keyword_and = 292,
    keyword_or = 293,
    keyword_not = 294,
    modifier_if = 295,
    modifier_unless = 296,
    modifier_while = 297,
    modifier_until = 298,
    modifier_rescue = 299,
    keyword_alias = 300,
    keyword_BEGIN = 301,
    keyword_END = 302,
    keyword__LINE__ = 303,
    keyword__FILE__ = 304,
    keyword__ENCODING__ = 305,
    tIDENTIFIER = 306,
    tFID = 307,
    tGVAR = 308,
    tIVAR = 309,
    tCONSTANT = 310,
    tCVAR = 311,
    tLABEL = 312,
    tINTEGER = 313,
    tFLOAT = 314,
    tCHAR = 315,
    tXSTRING = 316,
    tREGEXP = 317,
    tSTRING = 318,
    tSTRING_PART = 319,
    tSTRING_MID = 320,
    tLABEL_END = 321,
    tNTH_REF = 322,
    tBACK_REF = 323,
    tREGEXP_END = 324,
    tUPLUS = 325,
    tUMINUS = 326,
    tPOW = 327,
    tCMP = 328,
    tEQ = 329,
    tEQQ = 330,
    tNEQ = 331,
    tGEQ = 332,
    tLEQ = 333,
    tANDOP = 334,
    tOROP = 335,
    tMATCH = 336,
    tNMATCH = 337,
    tDOT2 = 338,
    tDOT3 = 339,
    tAREF = 340,
    tASET = 341,
    tLSHFT = 342,
    tRSHFT = 343,
    tCOLON2 = 344,
    tCOLON3 = 345,
    tOP_ASGN = 346,
    tASSOC = 347,
    tLPAREN = 348,
    tLPAREN_ARG = 349,
    tRPAREN = 350,
    tLBRACK = 351,
    tLBRACE = 352,
    tLBRACE_ARG = 353,
    tSTAR = 354,
    tAMPER = 355,
    tLAMBDA = 356,
    tANDDOT = 357,
    tSYMBEG = 358,
    tREGEXP_BEG = 359,
    tWORDS_BEG = 360,
    tSYMBOLS_BEG = 361,
    tSTRING_BEG = 362,
    tXSTRING_BEG = 363,
    tSTRING_DVAR = 364,
    tLAMBEG = 365,
    tHEREDOC_BEG = 366,
    tHEREDOC_END = 367,
    tLITERAL_DELIM = 368,
    tHD_LITERAL_DELIM = 369,
    tHD_STRING_PART = 370,
    tHD_STRING_MID = 371,
    tLOWEST = 372,
    tUMINUS_NUM = 373,
    tLAST_TOKEN = 374
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 1047 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:355  */

    node *nd;
    mrb_sym id;
    int num;
    stack_type stack;
    const struct vtable *vars;

#line 1267 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif



int yyparse (parser_state *p);



/* Copy the second part of user declarations.  */

#line 1283 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   11700

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  146
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  163
/* YYNRULES -- Number of rules.  */
#define YYNRULES  560
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  989

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   374

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     145,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   132,     2,     2,     2,   130,   125,     2,
     140,   141,   128,   126,   138,   127,   144,   129,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   120,   143,
     122,   118,   121,   119,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   137,     2,   142,   124,     2,   139,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   135,   123,   136,   133,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   131,   134
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1199,  1199,  1199,  1210,  1216,  1220,  1225,  1229,  1235,
    1237,  1236,  1248,  1275,  1281,  1285,  1290,  1294,  1300,  1300,
    1304,  1308,  1312,  1316,  1320,  1324,  1328,  1333,  1334,  1338,
    1342,  1346,  1350,  1353,  1357,  1361,  1365,  1369,  1373,  1378,
    1382,  1389,  1390,  1394,  1398,  1399,  1403,  1407,  1411,  1415,
    1418,  1427,  1428,  1431,  1432,  1439,  1438,  1451,  1455,  1460,
    1464,  1469,  1473,  1478,  1482,  1486,  1490,  1494,  1500,  1504,
    1510,  1511,  1517,  1521,  1525,  1529,  1533,  1537,  1541,  1545,
    1549,  1553,  1559,  1560,  1566,  1570,  1576,  1580,  1586,  1590,
    1594,  1598,  1602,  1606,  1612,  1618,  1625,  1629,  1633,  1637,
    1641,  1645,  1651,  1657,  1664,  1668,  1671,  1675,  1679,  1686,
    1687,  1688,  1689,  1694,  1701,  1702,  1705,  1709,  1709,  1715,
    1716,  1717,  1718,  1719,  1720,  1721,  1722,  1723,  1724,  1725,
    1726,  1727,  1728,  1729,  1730,  1731,  1732,  1733,  1734,  1735,
    1736,  1737,  1738,  1739,  1740,  1741,  1742,  1743,  1746,  1746,
    1746,  1747,  1747,  1748,  1748,  1748,  1749,  1749,  1749,  1749,
    1750,  1750,  1750,  1751,  1751,  1751,  1752,  1752,  1752,  1752,
    1753,  1753,  1753,  1753,  1754,  1754,  1754,  1754,  1755,  1755,
    1755,  1755,  1756,  1756,  1756,  1756,  1757,  1757,  1760,  1764,
    1768,  1772,  1776,  1780,  1784,  1789,  1794,  1799,  1803,  1807,
    1811,  1815,  1819,  1823,  1827,  1831,  1835,  1839,  1843,  1847,
    1851,  1855,  1859,  1863,  1867,  1871,  1875,  1879,  1883,  1887,
    1891,  1895,  1899,  1903,  1907,  1911,  1915,  1919,  1923,  1927,
    1933,  1934,  1939,  1943,  1950,  1954,  1962,  1968,  1969,  1972,
    1973,  1974,  1979,  1984,  1991,  1997,  2002,  2007,  2012,  2019,
    2019,  2030,  2036,  2040,  2046,  2047,  2050,  2056,  2062,  2067,
    2074,  2079,  2084,  2091,  2092,  2093,  2094,  2095,  2096,  2097,
    2098,  2103,  2102,  2114,  2118,  2113,  2123,  2123,  2127,  2131,
    2135,  2139,  2144,  2149,  2153,  2157,  2161,  2165,  2169,  2170,
    2176,  2182,  2175,  2194,  2202,  2210,  2210,  2210,  2217,  2217,
    2217,  2224,  2230,  2235,  2237,  2234,  2246,  2244,  2260,  2265,
    2258,  2280,  2278,  2293,  2297,  2292,  2312,  2318,  2311,  2333,
    2337,  2341,  2345,  2351,  2358,  2359,  2360,  2363,  2364,  2367,
    2368,  2376,  2377,  2383,  2387,  2390,  2394,  2400,  2404,  2410,
    2414,  2418,  2422,  2426,  2430,  2434,  2438,  2442,  2448,  2452,
    2456,  2460,  2464,  2468,  2472,  2476,  2480,  2484,  2488,  2492,
    2496,  2500,  2504,  2510,  2511,  2518,  2522,  2526,  2533,  2537,
    2543,  2544,  2547,  2552,  2555,  2559,  2565,  2569,  2576,  2575,
    2588,  2598,  2602,  2607,  2614,  2618,  2622,  2626,  2630,  2634,
    2638,  2642,  2646,  2653,  2652,  2665,  2664,  2678,  2686,  2695,
    2698,  2705,  2708,  2712,  2713,  2716,  2720,  2723,  2727,  2730,
    2731,  2732,  2733,  2736,  2737,  2738,  2742,  2748,  2749,  2755,
    2760,  2759,  2770,  2774,  2780,  2784,  2790,  2794,  2800,  2803,
    2804,  2807,  2813,  2819,  2820,  2823,  2830,  2829,  2843,  2847,
    2854,  2858,  2865,  2872,  2873,  2874,  2875,  2876,  2880,  2886,
    2890,  2896,  2897,  2898,  2902,  2908,  2912,  2916,  2920,  2924,
    2930,  2936,  2940,  2944,  2948,  2952,  2956,  2964,  2971,  2982,
    2983,  2987,  2991,  2990,  3006,  3012,  3018,  3022,  3026,  3030,
    3034,  3038,  3042,  3046,  3050,  3054,  3058,  3062,  3066,  3070,
    3075,  3081,  3086,  3091,  3096,  3103,  3107,  3114,  3118,  3124,
    3128,  3134,  3141,  3148,  3155,  3159,  3165,  3169,  3175,  3176,
    3179,  3184,  3191,  3192,  3195,  3202,  3206,  3213,  3218,  3218,
    3243,  3244,  3250,  3255,  3261,  3267,  3272,  3277,  3282,  3289,
    3290,  3291,  3294,  3295,  3296,  3297,  3300,  3301,  3302,  3305,
    3306,  3309,  3313,  3319,  3320,  3326,  3327,  3330,  3331,  3334,
    3337,  3340,  3341,  3342,  3345,  3346,  3347,  3350,  3357,  3358,
    3362
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "keyword_class", "keyword_module",
  "keyword_def", "keyword_begin", "keyword_if", "keyword_unless",
  "keyword_while", "keyword_until", "keyword_for", "keyword_undef",
  "keyword_rescue", "keyword_ensure", "keyword_end", "keyword_then",
  "keyword_elsif", "keyword_else", "keyword_case", "keyword_when",
  "keyword_break", "keyword_next", "keyword_redo", "keyword_retry",
  "keyword_in", "keyword_do", "keyword_do_cond", "keyword_do_block",
  "keyword_do_LAMBDA", "keyword_return", "keyword_yield", "keyword_super",
  "keyword_self", "keyword_nil", "keyword_true", "keyword_false",
  "keyword_and", "keyword_or", "keyword_not", "modifier_if",
  "modifier_unless", "modifier_while", "modifier_until", "modifier_rescue",
  "keyword_alias", "keyword_BEGIN", "keyword_END", "keyword__LINE__",
  "keyword__FILE__", "keyword__ENCODING__", "tIDENTIFIER", "tFID", "tGVAR",
  "tIVAR", "tCONSTANT", "tCVAR", "tLABEL", "tINTEGER", "tFLOAT", "tCHAR",
  "tXSTRING", "tREGEXP", "tSTRING", "tSTRING_PART", "tSTRING_MID",
  "tLABEL_END", "tNTH_REF", "tBACK_REF", "tREGEXP_END", "tUPLUS",
  "tUMINUS", "tPOW", "tCMP", "tEQ", "tEQQ", "tNEQ", "tGEQ", "tLEQ",
  "tANDOP", "tOROP", "tMATCH", "tNMATCH", "tDOT2", "tDOT3", "tAREF",
  "tASET", "tLSHFT", "tRSHFT", "tCOLON2", "tCOLON3", "tOP_ASGN", "tASSOC",
  "tLPAREN", "tLPAREN_ARG", "tRPAREN", "tLBRACK", "tLBRACE", "tLBRACE_ARG",
  "tSTAR", "tAMPER", "tLAMBDA", "tANDDOT", "tSYMBEG", "tREGEXP_BEG",
  "tWORDS_BEG", "tSYMBOLS_BEG", "tSTRING_BEG", "tXSTRING_BEG",
  "tSTRING_DVAR", "tLAMBEG", "tHEREDOC_BEG", "tHEREDOC_END",
  "tLITERAL_DELIM", "tHD_LITERAL_DELIM", "tHD_STRING_PART",
  "tHD_STRING_MID", "tLOWEST", "'='", "'?'", "':'", "'>'", "'<'", "'|'",
  "'^'", "'&'", "'+'", "'-'", "'*'", "'/'", "'%'", "tUMINUS_NUM", "'!'",
  "'~'", "tLAST_TOKEN", "'{'", "'}'", "'['", "','", "'`'", "'('", "')'",
  "']'", "';'", "'.'", "'\\n'", "$accept", "program", "$@1",
  "top_compstmt", "top_stmts", "top_stmt", "@2", "bodystmt", "compstmt",
  "stmts", "stmt", "$@3", "command_asgn", "command_rhs", "expr",
  "expr_value", "command_call", "block_command", "cmd_brace_block", "$@4",
  "command", "mlhs", "mlhs_inner", "mlhs_basic", "mlhs_item", "mlhs_list",
  "mlhs_post", "mlhs_node", "lhs", "cname", "cpath", "fname", "fsym",
  "undef_list", "$@5", "op", "reswords", "arg", "aref_args", "arg_rhs",
  "paren_args", "opt_paren_args", "opt_call_args", "call_args",
  "command_args", "@6", "block_arg", "opt_block_arg", "comma", "args",
  "mrhs", "primary", "@7", "@8", "$@9", "$@10", "@11", "@12", "$@13",
  "$@14", "$@15", "$@16", "$@17", "$@18", "@19", "@20", "@21", "@22",
  "@23", "@24", "@25", "@26", "primary_value", "then", "do", "if_tail",
  "opt_else", "for_var", "f_marg", "f_marg_list", "f_margs", "block_param",
  "opt_block_param", "block_param_def", "opt_bv_decl", "bv_decls", "bvar",
  "f_larglist", "lambda_body", "do_block", "$@27", "block_call",
  "method_call", "brace_block", "@28", "@29", "case_body", "cases",
  "opt_rescue", "exc_list", "exc_var", "opt_ensure", "literal", "string",
  "string_rep", "string_interp", "@30", "xstring", "regexp", "heredoc",
  "heredoc_bodies", "heredoc_body", "heredoc_string_rep",
  "heredoc_string_interp", "@31", "words", "symbol", "basic_symbol", "sym",
  "symbols", "numeric", "variable", "var_lhs", "var_ref", "backref",
  "superclass", "$@32", "f_arglist", "f_args", "f_bad_arg", "f_norm_arg",
  "f_arg_item", "f_arg", "f_opt_asgn", "f_opt", "f_block_opt",
  "f_block_optarg", "f_optarg", "restarg_mark", "f_rest_arg",
  "blkarg_mark", "f_block_arg", "opt_f_block_arg", "singleton", "$@33",
  "assoc_list", "assocs", "assoc", "operation", "operation2", "operation3",
  "dot_or_colon", "call_op", "call_op2", "opt_terms", "opt_nl", "rparen",
  "rbracket", "trailer", "term", "nl", "terms", "none", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,    61,    63,
      58,    62,    60,   124,    94,    38,    43,    45,    42,    47,
      37,   373,    33,   126,   374,   123,   125,    91,    44,    96,
      40,    41,    93,    59,    46,    10
};
# endif

#define YYPACT_NINF -811

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-811)))

#define YYTABLE_NINF -561

#define yytable_value_is_error(Yytable_value) \
  (!!((Yytable_value) == (-561)))

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -811,   107,  2856,  -811,  7378,  9218,  9551,  5481,  -811,  8873,
    8873,  -811,  -811,  9329,  6767,  5222,  7608,  7608,  -811,  -811,
    7608,  3018,  6125,  -811,  -811,  -811,  -811,    45,  6767,  -811,
      22,  -811,  -811,  -811,  5619,  2699,  -811,  -811,  5734,  -811,
    -811,  -811,  -811,  -811,  -811,  -811,  8988,  8988,   121,  4499,
     152,  7953,  8183,  7041,  -811,  6493,   880,  1003,  1047,  1160,
     783,  -811,    67,  9103,  8988,  -811,   640,  -811,  1313,  -811,
     343,  -811,  -811,   151,    75,  -811,    49,  9440,  -811,    81,
   11511,    38,   424,   228,    61,  -811,  -811,  -811,  -811,  -811,
    -811,  -811,  -811,  -811,  -811,    56,   114,  -811,   153,   110,
    -811,  -811,  -811,  -811,  -811,   116,   116,   125,   556,   638,
    8873,   535,  4615,   346,  -811,   179,  -811,   434,  -811,  -811,
     110,  -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,
    -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,
    -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,    33,
      48,    60,   123,  -811,  -811,  -811,  -811,  -811,  -811,   148,
     197,   207,   254,  -811,   286,  -811,  -811,  -811,  -811,  -811,
    -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,
    -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,
    -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,   296,
    3695,   171,   343,    86,   213,   555,   224,   221,   282,    86,
    8873,  8873,   281,  -811,  -811,   605,   330,    79,    83,  -811,
    -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,  6630,  -811,
    -811,   252,  -811,  -811,  -811,  -811,  -811,  -811,   640,  -811,
     572,  -811,   373,  -811,  -811,   640,  8988,  8988,  8988,  8988,
    1055,  8988,  -811, 11490,  -811,  -811,   261,   289,  -811,  -811,
    -811,  7608,  -811,  -811,  -811,  7608,  -811,  -811,  -811,  5338,
    8873,  -811,  -811,   302,  4731,  -811,   616,   367,   160,  7723,
    4499,   316,   640,  1313,   320,   350,  -811,  7723,   320,   337,
     238,   251,  -811, 11490,   355,   251,  -811,   441,  9662,   376,
     617,   621,   632,   809,  -811,  -811,  -811,  -811,  1180,  -811,
    -811,  -811,  -811,  -811,  -811,   586,   904,  -811,  -811,  1194,
    -811,  1212,  -811,  1249,  -811,   791,   444,   468,  -811,  -811,
    -811,  -811,  4990,  8873,  8873,  8873,  8873,  7723,  8873,  8873,
    -811,  -811,  8298,  -811,  4499,  7152,   411,  8298,  8988,  8988,
    8988,  8988,  8988,  8988,  8988,  8988,  8988,  8988,  8988,  8988,
    8988,  8988,  8988,  8988,  8988,  8988,  8988,  8988,  8988,  8988,
    8988,  8988,  8988,  8988,  9931,  -811,  7608,  -811, 10014,  -811,
    -811, 11176,  -811,  -811,  -811,  9103,  9103,  -811,   460,  -811,
     343,  -811,   679,  -811,  -811,  -811,  -811,  -811, 10097,  7608,
   10180,  3695,  8873,  -811,  -811,  -811,  -811,   557,   560,   394,
    -811,  3838,   565,  8988, 10263,  7608, 10346,  8988,  8988,  4124,
      68,    68,    96, 10429,  7608, 10512,  -811,   529,  -811,  4731,
     373,  -811,  -811,  8413,   578,  -811, 11511, 11511, 11511, 11511,
    8988,  1153,  8988,   586,  -811,  7838,  -811,  7493,  -811,   498,
     320,  -811,   467,   471,  -811,  -811,   115,   470,  -811,  -811,
    6767,  4240,   480, 10263, 10346,  8988,  1313,   320,  -811,  -811,
    5106,   489,  1313,  -811,  -811,  8068,  -811,  -811,  -811,  -811,
    -811,  -811,   679,    49,  9662,  -811,  9662, 10595,  7608, 10678,
     525,  -811,  -811,  -811,  -811,  1321,  -811,  -811,  -811,  -811,
     948,  -811,  -811,  -811,  -811,  -811,   508,  8988,  -811,   510,
     604,   528,   625,  -811,  -811,  1252,  4731,   586,  -811,  -811,
    -811,  -811,  -811,  -811,  -811,  8988,  8988,  -811,  -811,  -811,
    -811,  -811,  -811,  -811,  -811,    25,  8988,  -811, 11320,   261,
    -811,   320,  9662,   548,  -811,  -811,  -811,   623,   593,  1958,
    -811,  -811,   705,   412,   367,  1481,  1481,  1481,  1481,  1125,
    1125,  2321,  2036,  1481,  1481, 11570, 11570,   239,   239, 11261,
    1125,  1125,  1076,  1076,  1174,   194,   194,   367,   367,   367,
    3151,  6240,  3417,  6355,  -811,   116,  -811,   320,   241,  -811,
     469,  -811,  -811,  6125,  -811,  -811,  2318,    25,    25,  -811,
   11244,  -811,  -811,  -811,  -811,  -811,   640,  8873,  3695,   677,
      70,  -811,   116,   320,   116,   697,   115,  1001,  6904,  -811,
    8528,   695,  -811,   562,  -811,  5849,  5987,   320,   417,   429,
     695,  -811,  -811,  -811,  -811,    51,    90,   320,   104,   105,
    8873,  6767,   580,   702, 11511,   190,  -811, 11511,  8988, 11511,
     586,  8988, 11490,  -811,   289,  -811,  -811,  1012,  7838,  7263,
    -811,  -811,  -811,   588,  -811,  -811,    23,  1313,   320,   251,
     411,  -811,   410,    70,   320,   283,   393,  -811,  -811,  1321,
     219,  -811,   587,   320,  -811,   320,    71,   948,  -811,  -811,
   11511,   948,  -811,  -811,  1009,  -811,  -811,  -811,   592,  -811,
     367,   367,  -811,   933,  4874,  -811,  -811, 11338,  8643,  -811,
    -811,  9662,  7723,  9103,  8988, 10761,  7608, 10844,   618,  9103,
    9103,  -811,   460,   595,   479,  9103,  9103,  -811,   460,    61,
     151,  4874,  4731,    25,  -811,   640,   730,  -811,  -811,  -811,
     948,  3695,   640,  -811, 11320,  -811,   647,  -811,  4383,   737,
    -811,  8873,   744,  -811,  8988,  8988,   438,  8988,  8988,   747,
    4874,  4874,   120,    68,  -811,  -811,  -811,  8758,  3981, 11511,
   11511,  -811,   633,  -811,  -811,  -811,   401,   320,  1267,   639,
    1331,  -811,   622,   637,  4874,  4731,  -811,  -811,   646,   652,
    -811,   657,  -811,   660,   657,  -811,   320,   680,   667,  9773,
    -811,   668,   670,  -811,   794,  8988, 11405,  -811,  -811, 11511,
    3284,  3550,   320,   466,   492,  8988,  -811,  -811,  -811,  -811,
    -811,  -811,  9103,  -811,  -811,  -811,  -811,  -811,  -811,  -811,
     803,   686,  4731,  3695,  -811,  -811,   320,   812,  -811,  1001,
    9884,    86,  -811,  -811,  4874,  -811,  -811,    86,  -811,  8988,
    -811,   813,   814,  -811, 11511,   244,  7263,  -811,   694,  1267,
     511,  -811,  -811,  1084,   819,   688,   948,  -811,  1009,  -811,
    1009,  -811,  1009,  -811,  -811,   714,  -811,   948,  -811,   799,
     713,   948,  -811,  1009,  -811,  -811, 11423,   523, 11511,  -811,
    -811,  -811,  -811,   710,   843,  -811,  -811,  3695,   811,  -811,
     724,   621,   632,  3695,  -811,  3838,  -811,  -811,  4874,  -811,
    -811,  -811,  1267,   694,  1267,   700,  -811,   329,  -811,  -811,
    -811,  -811,   657,   729,   657,   657,  -811,   733,   734,  -811,
   10927,   657,  -811,   735,   657,  -811,  -811,   861,   679, 11010,
    7608, 11093,   560,   562,   864,   694,  1267,  1084,  -811,  -811,
    1009,  -811,  -811,  -811,   948,  -811,  1009,  -811,   740,   742,
    -811,  1009,  -811,  -811,  -811,   677,    70,   320,   372,   409,
    -811,  -811,  -811,   694,  -811,   657,   657,   750,   657,   657,
     606,  -811,  -811,  1009,  -811,  -811,  -811,   657,  -811
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     0,     1,     0,     0,     0,     0,   271,     0,
       0,   295,   298,     0,     0,   545,   319,   320,   321,   322,
     283,   249,   391,   463,   462,   464,   465,   547,     0,    10,
       0,   467,   466,   468,   455,   531,   457,   456,   459,   458,
     451,   452,   413,   414,   469,   470,     0,     0,     0,     0,
     273,   560,   560,    80,   290,     0,     0,     0,     0,     0,
       0,   428,     0,     0,     0,     3,   545,     6,     9,    27,
      32,    44,    52,    51,     0,    68,     0,    72,    82,     0,
      49,   229,     0,    53,   288,   263,   264,   265,   266,   267,
     411,   410,   440,   412,   409,   461,     0,   268,   269,   249,
       5,     8,   319,   320,   283,   560,   391,     0,   104,   105,
       0,     0,     0,     0,   107,   471,   323,     0,   461,   269,
       0,   311,   158,   168,   159,   155,   184,   185,   186,   187,
     166,   181,   174,   164,   163,   179,   162,   161,   157,   182,
     156,   169,   173,   175,   167,   160,   176,   183,   178,   177,
     170,   180,   165,   154,   172,   171,   153,   151,   152,   148,
     149,   150,   109,   111,   110,   143,   144,   140,   122,   123,
     124,   131,   128,   130,   125,   126,   145,   146,   132,   133,
     137,   127,   129,   119,   120,   121,   134,   135,   136,   138,
     139,   141,   142,   147,   518,   313,   112,   113,   517,     0,
       0,     0,    50,     0,     0,     0,   461,     0,   269,     0,
       0,     0,     0,   334,   333,     0,     0,   461,   269,   177,
     170,   180,   165,   148,   149,   150,   109,   110,     0,   114,
     116,    20,   115,   431,   436,   435,   554,   557,   545,   556,
       0,   433,     0,   558,   555,   546,     0,     0,     0,     0,
       0,     0,   244,   256,    66,   248,   560,   560,   522,    67,
      65,   560,   238,   284,    64,     0,   237,   390,    63,   547,
       0,   548,    18,     0,     0,   207,     0,   208,   280,     0,
       0,     0,   545,    15,   547,    70,    14,     0,   547,     0,
     551,   551,   230,     0,     0,   551,   520,     0,     0,    78,
       0,    88,    95,   490,   445,   444,   446,   447,     0,   443,
     442,   426,   420,   419,   422,     0,     0,   417,   438,     0,
     449,     0,   415,     0,   424,     0,   453,   454,    48,   222,
     223,     4,   546,     0,     0,     0,     0,     0,     0,     0,
     378,   380,     0,    84,     0,    76,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   542,   560,   541,     0,   544,
     543,     0,   395,   393,   289,     0,     0,   384,    57,   287,
     308,   104,   105,   106,   453,   454,   472,   306,     0,   560,
       0,     0,     0,   314,   540,   539,   316,     0,   560,   280,
     325,     0,   324,     0,     0,   560,     0,     0,     0,     0,
       0,     0,   280,     0,   560,     0,   303,     0,   117,     0,
       0,   432,   434,     0,     0,   559,   525,   526,   257,   251,
       0,     0,     0,   254,   245,     0,   253,   254,   246,     0,
     547,   240,   560,   560,   239,   250,   547,     0,   286,    47,
       0,     0,     0,     0,     0,     0,    17,   547,   278,    13,
     546,    69,   274,   277,   281,   553,   231,   552,   553,   233,
     282,   521,    94,    86,     0,    81,     0,     0,   560,     0,
     496,   493,   492,   491,   494,     0,   509,   513,   512,   508,
     490,   291,   375,   495,   497,   499,   560,     0,   506,   560,
     511,   560,     0,   489,   448,     0,     0,   423,   429,   427,
     418,   439,   450,   416,   425,     0,     0,     7,    21,    22,
      23,    24,    25,    45,    46,   560,     0,    28,    30,     0,
      31,   547,     0,    74,    85,    43,    33,    41,     0,   234,
     188,    29,     0,   269,   204,   212,   217,   218,   219,   214,
     216,   226,   227,   220,   221,   197,   198,   224,   225,   547,
     213,   215,   209,   210,   211,   199,   200,   201,   202,   203,
     532,   537,   533,   538,   389,   249,   387,   547,   532,   534,
     533,   535,   388,   560,   532,   533,   249,   560,   560,    34,
     234,   189,    40,   196,    55,    58,     0,     0,     0,   104,
     105,   108,     0,   547,   560,     0,   547,   490,     0,   272,
     560,   560,   401,   560,   326,   536,   279,   547,   532,   533,
     560,   328,   296,   327,   299,   536,   279,   547,   532,   533,
       0,     0,     0,     0,   256,     0,   302,   527,     0,   524,
     255,     0,   258,   252,   560,   523,   236,   254,     0,   243,
     285,   549,    19,     0,    26,   195,    71,    16,   547,   551,
      87,    79,   536,    93,   547,   532,   533,   501,   496,     0,
     346,   337,   339,   547,   335,   547,     0,     0,   482,   516,
     502,     0,   485,   510,     0,   487,   514,   441,     0,   430,
     205,   206,   366,   547,     0,   364,   363,   262,     0,    83,
      77,     0,     0,     0,     0,     0,   560,     0,     0,     0,
       0,   386,    61,     0,   392,     0,     0,   385,    59,   381,
      54,     0,     0,   560,   309,     0,     0,   392,   312,   519,
     490,     0,     0,   317,   402,   403,   560,   404,     0,   560,
     331,     0,     0,   329,     0,     0,   392,     0,     0,     0,
       0,     0,   392,     0,   118,   437,   301,     0,     0,   528,
     259,   247,   560,    11,   275,   232,   392,   547,     0,   344,
       0,   498,     0,   368,     0,     0,   292,   500,   560,   560,
     515,   560,   507,   560,   560,   421,   547,     0,   560,     0,
     504,   560,   560,   362,     0,     0,   260,    75,    42,   235,
     532,   533,   547,   532,   533,     0,    39,   193,    38,   194,
      62,   550,     0,    36,   191,    37,   192,    60,   382,   383,
       0,     0,     0,     0,   473,   307,   547,     0,   475,   490,
       0,     0,   406,   332,     0,    12,   408,     0,   293,     0,
     294,     0,     0,   304,   258,   560,   242,   336,   347,     0,
     342,   338,   374,     0,     0,     0,     0,   478,     0,   480,
       0,   486,     0,   483,   488,     0,   365,   353,   355,     0,
     503,     0,   358,     0,   360,   379,   261,   392,   228,    35,
     190,   396,   394,     0,     0,   474,   315,     0,     0,   405,
       0,    96,   103,     0,   407,     0,   297,   300,     0,   398,
     399,   397,     0,   345,     0,   340,   372,   547,   370,   373,
     377,   376,   560,   560,   560,   560,   367,   560,   560,   280,
       0,   560,   505,   560,   560,    56,   310,     0,   102,     0,
     560,     0,   560,   560,     0,   343,     0,     0,   369,   479,
       0,   476,   481,   484,     0,   350,     0,   352,   536,   279,
     359,     0,   356,   361,   318,    99,   101,   547,   532,   533,
     400,   330,   305,   341,   371,   560,   560,   560,   560,   560,
      97,   477,   351,     0,   348,   354,   357,   560,   349
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -811,  -811,  -811,   430,  -811,    31,  -811,  -370,   382,  -811,
       8,  -811,  -253,  -220,   108,    13,   -48,  -811,  -555,  -811,
     -12,   879,  -166,   -17,   -52,  -259,  -423,     5,  1582,   -59,
     889,    20,     1,  -811,  -811,    29,  -811,   823,  -811,   158,
      -8,  -116,  -325,    93,    89,  -811,  -379,  -231,  -236,    69,
    -300,    11,  -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,
    -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,  -811,
    -811,  -811,   620,  -187,  -377,   -42,  -565,  -811,  -679,  -705,
     227,  -811,  -518,  -811,  -610,  -811,   -44,  -811,  -811,   180,
    -811,  -811,  -811,   -78,  -811,  -811,  -400,  -811,   -31,  -811,
    -811,  -811,  -811,  -811,   111,  -161,  -811,  -811,  -811,  -811,
     599,  -297,  -811,   684,  -811,  -811,  -811,    -7,  -811,  -811,
    -811,  1388,  1784,   910,  1229,  -811,  -811,    80,  -292,  -747,
    -179,  -609,   142,  -641,  -619,  -810,    50,   243,  -811,  -552,
    -811,  -265,   907,  -811,  -811,  -811,    17,  -407,  2225,  -333,
    -811,  -811,   -81,  -811,   -13,   -24,  -146,  -531,  -249,    62,
      -6,    -5,    -2
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    65,    66,    67,   273,   407,   408,   282,
     283,   460,    69,   546,    70,   203,    71,    72,   605,   733,
      73,    74,   284,    75,    76,    77,   485,    78,   204,   114,
     115,   229,   230,   231,   641,   583,   197,    80,   289,   550,
     584,   263,   450,   451,   264,   265,   255,   444,   449,   452,
     540,    81,   200,   287,   668,   288,   303,   686,   210,   760,
     211,   761,   640,   908,   608,   606,   833,   401,   403,   617,
     618,   839,   276,   411,   632,   752,   753,   216,   681,   682,
     683,   796,   704,   705,   782,   917,   918,   501,   786,   341,
     535,    83,    84,   389,   598,   597,   434,   911,   621,   746,
     841,   845,    85,    86,   316,   317,   516,    87,    88,    89,
     650,   239,   240,   241,   429,    90,    91,    92,   310,    93,
      94,   206,   207,    97,   208,   397,   607,   741,   742,   503,
     504,   505,   506,   507,   508,   800,   801,   509,   510,   511,
     512,   790,   688,   199,   402,   294,   453,   258,    99,   612,
     586,   406,   400,   381,   242,   457,   458,   724,   476,   412,
     271,   245,   286
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     100,   378,   380,   270,   252,   252,   384,   232,   252,   244,
      68,   502,    68,   262,   267,   328,   116,   116,   518,   266,
     445,   232,   419,   209,   116,   346,   448,   195,   238,   272,
     643,   615,   285,   257,   257,   101,   196,   257,   513,   484,
     655,   585,   479,   196,   634,   593,   481,   551,   596,   292,
     296,   587,   393,   331,   475,   478,   749,   196,   299,   478,
     244,   332,   799,   671,   116,   759,   653,   614,   291,   295,
     653,   932,   792,   858,   613,   309,   -99,   243,   787,   731,
     732,   585,   737,   593,   196,   256,   256,   382,   116,   256,
     627,   387,   614,   797,   545,   631,   756,   262,   267,   637,
     784,   861,   410,   266,   -96,   702,   762,     3,  -103,   254,
     259,   268,   387,   260,   467,  -101,   919,   202,   202,   710,
     290,  -102,  -463,   202,   416,   326,   327,  -323,   243,   -98,
    -100,   614,   545,   545,   425,   789,   382,  -462,   471,   793,
    -323,   -69,   473,   776,   932,   -97,   518,  -460,   703,  -464,
     518,   802,   338,   339,   913,   520,   614,   274,   520,  -279,
     520,   -83,   520,   674,   520,   599,   602,   820,   319,   321,
     323,   325,  -279,   827,   -96,  -323,   278,  -463,   541,   340,
     233,   785,  -323,   234,   235,   269,   875,   343,   388,   -91,
     237,  -532,  -462,   342,   -88,   464,   383,   244,   233,   347,
     919,   234,   235,   244,  -464,   385,   410,  -279,   685,   945,
    -533,   236,  -465,   237,  -279,   832,   658,   -88,   390,   489,
     699,   -95,   448,   420,   421,   430,   409,   484,   -93,   236,
    -533,   237,   244,   861,   -94,   513,   799,  -467,   736,   244,
     799,   973,   -90,   -92,   386,   383,   483,   792,   309,   252,
     261,   465,   655,   252,   446,   446,   261,   196,   -89,   454,
     237,   787,   748,   285,   433,   269,   348,  -465,   787,   469,
     678,  -103,   491,   492,   493,   494,   244,   470,  -102,   653,
     520,   887,   257,   484,   477,   477,  -466,   466,   807,   477,
     909,   -95,  -467,  -276,   537,   472,  -468,  -276,   -94,   547,
     243,   396,   233,   708,   656,   234,   235,   435,   -90,   116,
     660,   348,   417,   799,   923,  -460,   684,   379,   202,   202,
     745,   666,   371,   372,   373,   928,   244,   285,   443,   933,
     375,   413,   725,   236,   256,   237,   422,   547,   547,   611,
      68,  -466,   -96,  -455,   243,   532,   528,   529,   530,   531,
     543,  -468,   513,   699,   520,   426,   116,   778,   455,   -98,
     518,   441,   518,   527,   252,   369,   370,   371,   372,   373,
     592,   837,   377,   418,   454,  -459,   443,   456,   459,   -90,
     338,   339,   585,   237,   593,   404,   853,   252,   -98,   447,
     428,   812,   592,   433,   435,   709,   237,   454,  -455,   443,
    -103,   -90,   977,   252,   394,   395,   622,   591,   592,   767,
     591,   539,   252,   454,   244,   244,   539,   592,   -92,   515,
     775,   -90,   454,   771,   -90,  -100,   -89,   447,   -90,   591,
    -459,   281,   670,   478,   483,   -91,   980,   461,   803,   348,
     405,   202,   202,   202,   202,   591,   533,   534,   836,   655,
     446,   446,   484,   232,   591,   910,   592,   468,   768,   100,
     545,   662,   654,   894,   244,   237,   545,   947,   -68,    68,
     739,   717,   545,   545,   237,   513,   252,   727,   667,   474,
     729,   592,   633,   633,   -98,   465,   454,   -98,   -98,   196,
     483,   480,   669,   591,   281,   116,   482,   116,   727,   816,
     684,   779,   645,   386,   689,   823,   825,   689,   757,   689,
     616,   -92,  -102,   374,   486,   -98,   525,   -98,   591,   -89,
     758,  -100,   774,   398,  -100,  -100,   375,   937,   -91,   849,
    -103,   -92,   435,   706,   -92,   -98,   375,   781,   -92,   -89,
     526,   771,   -89,   601,   603,   718,   -89,  -100,   -91,   544,
    -532,   -91,  -100,   116,  -100,   -91,   -97,   725,   604,   518,
     726,   376,   678,   723,   491,   492,   493,   494,   377,   545,
     822,   399,   619,   620,   513,   601,   603,   721,   377,   751,
     748,   624,  -529,   726,   -98,   262,   391,  -100,   262,   723,
     392,   266,   514,   646,   266,   706,   706,   -97,   249,   684,
     244,   684,   889,   723,   721,   657,   262,   -92,   614,   659,
    -100,   661,   266,   723,   822,   967,   664,   -89,   747,   750,
     735,   750,    82,   665,    82,   117,   117,   -83,   750,   205,
     205,   857,  -392,   215,   232,   205,   205,   205,   743,   244,
     205,   -97,   764,   677,   414,  -455,   687,   196,   691,   914,
     723,   828,   446,   763,   903,   693,   462,   375,  -455,   483,
     905,   783,   281,   477,  -530,   547,   694,   712,   734,    82,
     196,   547,   818,   300,   722,   772,   696,   547,   547,   783,
     684,   915,   728,   205,   431,   730,   711,   234,   235,   539,
     895,  -529,   415,  -455,   423,  -392,  -529,   300,   233,   377,
    -455,   234,   235,  -536,   252,   463,   487,   375,  -392,   592,
    -461,   713,   738,   748,   454,   202,   765,   766,   375,   375,
     808,  -269,   116,  -461,   773,   780,   281,  -459,   795,   244,
     205,   706,    82,   684,  -269,   684,   244,   821,   815,   840,
    -459,  -392,   424,  -392,   842,   835,   591,   846,   202,   377,
    -392,   844,   233,   415,   488,   234,   235,   244,  -461,   848,
     377,   377,   850,   862,   847,  -461,  -536,   684,  -280,  -269,
     446,   856,   783,  -530,   547,  -459,  -269,   859,  -530,  -536,
     863,  -280,  -459,   236,   866,   237,   689,   689,   723,   689,
     868,   689,   689,   623,   715,   870,   689,   834,   872,   689,
     689,   630,   930,   876,   838,   877,   881,   375,   883,   885,
     116,   642,  -536,   939,  -536,   375,  -280,  -532,   891,   941,
      82,  -536,   892,  -280,   921,   633,   375,   896,   906,   907,
     205,   205,   912,   791,   920,   244,   794,   926,   946,   253,
     253,   244,   716,   253,   324,   798,   935,   312,   313,   377,
     399,   116,   524,   750,   929,   312,   313,   377,   936,   202,
     490,   940,   491,   492,   493,   494,   938,   950,   377,   275,
     277,   954,   956,   961,   253,   293,   964,   817,   819,   972,
    -532,   205,  -533,   824,   826,   205,   329,   330,   983,   205,
     205,   663,   213,   948,    82,   121,   314,   315,   698,    82,
      82,   971,   495,   974,   314,   315,   777,    82,   496,   497,
     829,   970,   817,   819,   517,   824,   826,   198,   300,   897,
     689,   689,   689,   689,   432,   689,   689,   927,   252,   689,
     788,   689,   689,   592,   498,     0,     0,   499,   454,     0,
     622,   750,   311,   723,   312,   313,     0,     0,     0,   500,
       0,     0,    82,   205,   205,   205,   205,    82,   205,   205,
       0,     0,   205,     0,    82,   300,   519,   552,   312,   313,
     591,     0,     0,   689,   689,   689,   689,   689,     0,     0,
     890,     0,     0,     0,   490,   689,   491,   492,   493,   494,
       0,     0,     0,   314,   315,     0,   205,     0,     0,   490,
       0,   491,   492,   493,   494,   552,   552,   890,   922,     0,
     924,     0,     0,     0,   925,     0,     0,   314,   315,   205,
       0,    82,   205,   931,     0,   934,   495,     0,     0,     0,
       0,    82,   496,   497,     0,   205,     0,     0,     0,    82,
       0,   495,     0,     0,   205,     0,     0,   496,   497,    82,
       0,     0,   490,     0,   491,   492,   493,   494,   498,     0,
     678,   499,   491,   492,   493,   494,   318,   312,   313,   436,
     437,   438,   439,   498,   329,     0,   499,     0,   237,     0,
       0,    82,     0,     0,   253,     0,   804,     0,   253,     0,
      82,     0,   975,     0,   495,     0,   976,     0,   978,     0,
     496,   497,   495,   979,   300,     0,   300,     0,   205,   497,
     320,   312,   313,   830,   831,     0,   314,   315,   322,   312,
     313,   440,     0,     0,   233,   987,   498,   234,   235,   499,
     843,     0,     0,     0,   498,   916,    82,   491,   492,   493,
     494,   740,   851,   852,     0,     0,     0,     0,   348,     0,
     855,     0,     0,  -241,  -241,     0,     0,  -241,     0,     0,
     314,   315,   300,   361,   362,   538,   864,   865,   314,   315,
     549,   554,   555,   556,   557,   558,   559,   560,   561,   562,
     563,   564,   565,   566,   567,   568,   569,   570,   571,   572,
     573,   574,   575,   576,   577,   578,   579,   348,     0,   253,
       0,   368,   369,   370,   371,   372,   373,     0,   600,   600,
       0,     0,   361,   362,   893,     0,   523,   312,   313,   648,
       0,     0,   253,   322,   312,   313,   904,   205,    82,     0,
       0,    98,     0,    98,   119,   119,   600,     0,   253,     0,
     600,   600,   218,   514,   312,   313,   348,   253,   366,   367,
     368,   369,   370,   371,   372,   373,   644,   521,   312,   313,
     205,   361,   362,   647,     0,   649,   314,   315,   652,     0,
     293,     0,     0,   314,   315,   522,   312,   313,    98,     0,
       0,     0,   302,     0,     0,   942,     0,   943,   600,     0,
     944,     0,     0,   314,   315,     0,     0,     0,   652,     0,
     369,   370,   371,   372,   373,     0,   302,   314,   315,     0,
       0,   253,   523,   312,   313,   697,   312,   313,   678,     0,
     491,   492,   493,   494,    82,   314,   315,     0,     0,     0,
     690,   300,    82,   552,     0,     0,   205,     0,     0,   552,
     205,    98,     0,     0,     0,   552,   552,     0,   700,   701,
       0,    82,    82,   333,   334,   335,   336,   337,     0,   707,
     679,    82,   314,   315,     0,   314,   315,     0,    82,     0,
       0,   205,   678,     0,   491,   492,   493,   494,     0,     0,
      82,    82,   678,     0,   491,   492,   493,   494,    82,     0,
      95,     0,    95,   118,   118,   118,     0,     0,     0,     0,
       0,   217,     0,     0,    82,    82,     0,     0,     0,     0,
       0,     0,     0,     0,   679,     0,   692,     0,   695,   880,
     680,     0,     0,     0,   679,     0,     0,     0,     0,    98,
     860,     0,     0,     0,     0,     0,     0,    95,     0,     0,
       0,   301,   552,   744,     0,     0,     0,     0,     0,     0,
       0,     0,    82,    82,     0,     0,     0,     0,     0,     0,
     900,     0,     0,     0,    82,   301,     0,     0,     0,     0,
       0,   769,     0,     0,   770,     0,     0,     0,     0,     0,
       0,   652,   293,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      95,     0,     0,    98,     0,     0,     0,     0,    98,    98,
       0,     0,     0,     0,     0,     0,    98,    82,     0,     0,
       0,     0,     0,    82,     0,    82,     0,   302,    82,     0,
       0,   806,     0,     0,     0,     0,   600,   809,     0,   253,
       0,     0,   600,   600,     0,     0,     0,     0,   600,   600,
       0,     0,     0,   348,  -561,  -561,  -561,  -561,   353,   354,
     205,    98,  -561,  -561,     0,     0,    98,     0,   361,   362,
       0,     0,     0,    98,   302,     0,   553,   600,   600,     0,
     600,   600,     0,     0,    79,     0,    79,     0,    95,     0,
     854,     0,     0,     0,     0,   214,     0,     0,     0,     0,
       0,     0,   364,   365,   366,   367,   368,   369,   370,   371,
     372,   373,     0,     0,   553,   553,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   886,     0,
      98,    79,     0,     0,     0,     0,     0,     0,   888,     0,
      98,     0,     0,     0,     0,   600,     0,     0,    98,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    95,     0,     0,     0,     0,    95,    95,     0,
       0,     0,   600,     0,     0,    95,     0,     0,     0,   293,
       0,     0,     0,     0,     0,     0,   301,     0,     0,     0,
      98,     0,     0,     0,    79,   867,   869,     0,   871,    98,
     873,   874,     0,     0,     0,   878,     0,     0,   882,   884,
       0,     0,     0,   302,     0,   302,     0,     0,     0,     0,
      95,     0,     0,     0,     0,    95,     0,     0,     0,     0,
       0,     0,    95,   301,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    98,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   253,     0,     0,     0,     0,     0,     0,
       0,   302,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    79,     0,     0,     0,    96,     0,    96,    95,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    95,
       0,     0,     0,     0,     0,     0,     0,    95,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    95,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   949,
     951,   952,   953,    96,   955,   957,     0,    98,   960,     0,
     962,   963,     0,     0,     0,     0,     0,     0,     0,    95,
       0,     0,     0,     0,     0,     0,    79,     0,    95,     0,
       0,    79,    79,     0,     0,     0,     0,     0,     0,    79,
       0,     0,   301,     0,   301,     0,     0,     0,     0,     0,
       0,     0,   981,   982,   984,   985,   986,     0,     0,     0,
       0,     0,     0,     0,   988,     0,    96,     0,     0,     0,
       0,     0,     0,     0,    95,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    79,     0,     0,     0,     0,    79,
       0,     0,     0,     0,     0,     0,    79,     0,     0,   548,
     301,     0,     0,    98,     0,     0,     0,     0,     0,     0,
     302,    98,   553,     0,     0,     0,     0,     0,   553,     0,
       0,     0,     0,     0,   553,   553,     0,     0,     0,     0,
      98,    98,     0,     0,     0,     0,     0,   548,   548,     0,
      98,     0,     0,     0,     0,     0,     0,    98,     0,     0,
       0,     0,     0,    79,    96,     0,     0,     0,     0,    98,
      98,     0,     0,    79,     0,     0,    95,    98,     0,     0,
       0,    79,   714,     0,     0,     0,     0,     0,     0,     0,
       0,    79,     0,    98,    98,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   119,     0,
     348,   349,   350,   351,   352,   353,   354,   355,   356,   357,
     358,   359,   360,    79,     0,   361,   362,     0,     0,     0,
       0,   553,    79,     0,     0,     0,     0,     0,    96,     0,
       0,    98,    98,    96,    96,     0,     0,     0,     0,   902,
       0,    96,     0,    98,     0,     0,     0,   363,     0,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,     0,
       0,     0,    95,     0,     0,     0,  -256,     0,    79,   301,
      95,     0,     0,     0,     0,     0,     0,     0,   348,   349,
     350,   351,   352,   353,   354,   355,    96,   357,   358,    95,
      95,    96,     0,   361,   362,     0,    98,     0,    96,    95,
       0,    96,    98,     0,    98,     0,    95,    98,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    95,    95,
       0,     0,     0,     0,     0,     0,    95,   364,   365,   366,
     367,   368,   369,   370,   371,   372,   373,     0,     0,    96,
      96,     0,    95,    95,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    96,     0,   118,     0,     0,
      79,     0,     0,     0,     0,    96,     0,     0,     0,     0,
       0,     0,     0,    96,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    96,     0,     0,     0,     0,     0,     0,
      95,    95,     0,     0,     0,     0,     0,     0,   901,     0,
     120,   120,    95,     0,     0,     0,     0,     0,   120,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,   120,     0,     0,     0,   120,   120,   120,     0,
       0,     0,     0,     0,     0,    95,    79,     0,     0,   120,
       0,    95,     0,    95,    79,   548,    95,     0,     0,     0,
      96,   548,   120,     0,     0,     0,     0,   548,   548,     0,
       0,     0,     0,    79,    79,     0,     0,     0,  -560,     0,
       0,     0,     0,    79,     0,     0,     0,     0,     0,     0,
      79,  -560,  -560,  -560,  -560,  -560,  -560,     0,  -560,     0,
       0,     0,    79,    79,  -560,  -560,     0,     0,     0,     0,
      79,     0,     0,     0,     0,  -560,  -560,     0,  -560,  -560,
    -560,  -560,  -560,     0,     0,     0,    79,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    96,   348,   349,   350,   351,   352,   353,   354,
       0,     0,   357,   358,   548,     0,     0,  -560,   361,   362,
       0,     0,     0,     0,    79,    79,     0,     0,     0,     0,
    -560,     0,   899,     0,     0,     0,    79,     0,     0,     0,
    -560,     0,     0,  -560,  -560,     0,     0,     0,     0,     0,
       0,     0,   364,   365,   366,   367,   368,   369,   370,   371,
     372,   373,     0,  -560,  -560,     0,     0,     0,   261,  -560,
       0,  -560,  -560,  -560,     0,     0,     0,     0,     0,     0,
       0,   120,   120,   120,   120,     0,   120,     0,     0,    79,
       0,     0,     0,     0,     0,    79,     0,    79,    96,     0,
      79,     0,     0,     0,     0,     0,    96,    96,     0,     0,
       0,     0,     0,    96,     0,     0,     0,     0,     0,    96,
      96,     0,     0,     0,     0,    96,    96,     0,     0,     0,
       0,     0,     0,   120,     0,    96,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    96,    96,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    96,    96,
     120,     0,     0,   120,   120,   120,   120,   120,   120,   120,
     120,   120,   120,   120,   120,   120,   120,   120,   120,   120,
     120,   120,   120,   120,   120,   120,   120,   120,   120,     0,
       0,     0,     0,     0,     0,     0,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    96,    96,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,     0,   120,   120,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,     0,     0,     0,     0,   120,     0,   120,     0,     0,
     120,     0,   120,     0,     0,     0,     0,     0,     0,     0,
       0,    96,     0,     0,     0,     0,     0,    96,     0,    96,
     120,     0,    96,     0,     0,     0,     0,     0,     0,  -270,
     120,     0,     0,     0,     0,     0,     0,     0,     0,   120,
       0,   120,  -270,  -270,  -270,  -270,  -270,  -270,     0,  -270,
       0,     0,     0,     0,     0,     0,  -270,  -270,  -270,     0,
       0,     0,   120,     0,     0,     0,  -270,  -270,     0,  -270,
    -270,  -270,  -270,  -270,     0,     0,     0,     0,     0,     0,
     120,   120,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,   120,     0,     0,
       0,  -270,  -270,  -270,  -270,  -270,  -270,  -270,  -270,  -270,
    -270,  -270,  -270,  -270,     0,     0,  -270,  -270,  -270,     0,
       0,  -270,     0,     0,     0,     0,     0,  -270,     0,     0,
       0,  -270,     0,     0,     0,     0,     0,     0,     0,  -270,
       0,  -270,     0,     0,  -270,  -270,     0,     0,  -270,  -270,
    -270,  -270,  -270,  -270,  -270,  -270,  -270,  -270,  -270,  -270,
       0,     0,     0,     0,     0,  -270,  -270,  -270,     0,     0,
    -270,  -270,  -270,  -270,  -270,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -560,     4,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
       0,     0,     0,   120,     0,    15,   120,    16,    17,    18,
      19,     0,     0,   120,   120,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,     0,    44,    45,     0,    46,    47,     0,     0,
       0,     0,     0,   120,     0,     0,   120,     0,     0,   120,
       0,     0,     0,     0,     0,     0,    48,     0,     0,    49,
      50,     0,    51,    52,     0,    53,     0,    54,     0,    55,
      56,    57,    58,    59,    60,     0,     0,    61,  -560,     0,
       0,  -560,  -560,     0,     0,     0,     0,     0,     0,   120,
     120,     0,   120,   120,     0,     0,     0,    62,    63,    64,
       0,     0,   120,     0,     0,     0,     0,     0,     0,  -560,
       0,  -560,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -560,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
     120,  -560,  -560,  -560,  -560,  -560,  -560,     0,  -560,     0,
     120,     0,     0,     0,     0,  -560,  -560,     0,     0,     0,
       0,     0,     0,     0,     0,  -560,  -560,     0,  -560,  -560,
    -560,  -560,  -560,     0,     0,   120,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,     0,     0,     0,
    -560,  -560,  -560,  -560,  -560,  -560,  -560,  -560,  -560,  -560,
    -560,  -560,  -560,     0,     0,  -560,  -560,  -560,     0,     0,
    -560,     0,     0,     0,     0,     0,  -560,     0,     0,     0,
    -560,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -560,     0,     0,  -560,  -560,     0,     0,  -560,     0,  -560,
    -560,  -560,  -560,  -560,  -560,  -560,  -560,  -560,  -560,     0,
       0,  -536,     0,     0,  -560,  -560,  -560,     0,   261,  -560,
    -560,  -560,  -560,  -560,  -536,  -536,  -536,     0,  -536,  -536,
       0,  -536,     0,     0,     0,     0,     0,  -536,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -536,  -536,
       0,  -536,  -536,  -536,  -536,  -536,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -536,  -536,  -536,  -536,  -536,  -536,  -536,
    -536,  -536,  -536,  -536,  -536,  -536,     0,     0,  -536,  -536,
    -536,     0,   719,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -536,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -536,     0,     0,  -536,  -536,     0,   -99,
    -536,     0,  -536,  -536,  -536,  -536,  -536,  -536,  -536,  -536,
    -536,  -536,     0,     0,  -536,     0,  -536,  -536,  -536,   -91,
       0,     0,  -536,     0,  -536,  -536,  -536,  -536,  -536,  -536,
       0,  -536,  -536,     0,  -536,     0,     0,     0,     0,     0,
    -536,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -536,  -536,     0,  -536,  -536,  -536,  -536,  -536,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -536,  -536,  -536,  -536,
    -536,  -536,  -536,  -536,  -536,  -536,  -536,  -536,  -536,     0,
       0,  -536,  -536,  -536,     0,   719,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -536,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -536,     0,     0,  -536,
    -536,     0,   -99,  -536,     0,  -536,  -536,  -536,  -536,  -536,
    -536,  -536,  -536,  -536,  -536,     0,     0,  -279,     0,  -536,
    -536,  -536,  -536,     0,     0,  -536,     0,  -536,  -536,  -536,
    -279,  -279,  -279,     0,  -279,  -279,     0,  -279,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -279,  -279,     0,  -279,  -279,  -279,
    -279,  -279,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -279,
    -279,  -279,  -279,  -279,  -279,  -279,  -279,  -279,  -279,  -279,
    -279,  -279,     0,     0,  -279,  -279,  -279,     0,   720,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -279,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -279,
       0,     0,  -279,  -279,     0,  -101,  -279,     0,  -279,  -279,
    -279,  -279,  -279,  -279,  -279,  -279,  -279,  -279,     0,     0,
    -279,     0,     0,  -279,  -279,   -93,     0,     0,  -279,     0,
    -279,  -279,  -279,  -279,  -279,  -279,     0,  -279,  -279,     0,
    -279,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -279,  -279,     0,
    -279,  -279,  -279,  -279,  -279,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -279,  -279,  -279,  -279,  -279,  -279,  -279,  -279,
    -279,  -279,  -279,  -279,  -279,     0,     0,  -279,  -279,  -279,
       0,   720,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -279,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -279,     0,     0,  -279,  -279,     0,  -101,  -279,
       0,  -279,  -279,  -279,  -279,  -279,  -279,  -279,  -279,  -279,
    -279,     0,     0,     0,     0,     0,  -279,  -279,  -279,     0,
       0,  -279,     0,  -279,  -279,  -279,   279,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,  -560,  -560,
    -560,     0,     0,  -560,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,     0,    44,    45,     0,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,     0,     0,    49,    50,
       0,    51,    52,     0,    53,     0,    54,     0,    55,    56,
      57,    58,    59,    60,     0,     0,    61,  -560,     0,     0,
    -560,  -560,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -560,   279,
    -560,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,  -560,     0,  -560,  -560,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,     0,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,     0,
       0,    49,    50,     0,    51,    52,     0,    53,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
    -560,     0,     0,  -560,  -560,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -560,   279,  -560,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,  -560,     0,     0,  -560,
      15,  -560,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,     0,    44,    45,
       0,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,     0,     0,    49,    50,     0,    51,    52,     0,
      53,     0,    54,     0,    55,    56,    57,    58,    59,    60,
       0,     0,    61,  -560,     0,     0,  -560,  -560,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -560,   279,  -560,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,  -560,
       0,     0,  -560,    15,     0,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
       0,    44,    45,     0,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,     0,     0,    49,    50,     0,
      51,    52,     0,    53,     0,    54,     0,    55,    56,    57,
      58,    59,    60,     0,     0,    61,  -560,     0,     0,  -560,
    -560,     4,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,    62,    63,    64,     0,    15,
       0,    16,    17,    18,    19,     0,     0,  -560,     0,  -560,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,     0,    44,    45,     0,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,     0,     0,    49,    50,     0,    51,    52,     0,    53,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,  -560,     0,     0,  -560,  -560,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    62,    63,    64,     0,     0,  -560,     0,     0,     0,
       0,     0,     0,  -560,   279,  -560,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,  -560,  -560,     0,
       0,     0,    15,     0,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,     0,
      44,    45,     0,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,     0,     0,    49,    50,     0,    51,
      52,     0,    53,     0,    54,     0,    55,    56,    57,    58,
      59,    60,     0,     0,    61,  -560,     0,     0,  -560,  -560,
     279,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,     0,    62,    63,    64,     0,    15,     0,
      16,    17,    18,    19,     0,     0,  -560,     0,  -560,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,     0,    44,    45,     0,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
       0,     0,   280,    50,     0,    51,    52,     0,    53,     0,
      54,     0,    55,    56,    57,    58,    59,    60,     0,     0,
      61,  -560,     0,     0,  -560,  -560,   279,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,     0,
      62,    63,    64,     0,    15,     0,    16,    17,    18,    19,
    -560,     0,  -560,     0,  -560,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,     0,    44,    45,     0,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,     0,     0,    49,    50,
       0,    51,    52,     0,    53,     0,    54,     0,    55,    56,
      57,    58,    59,    60,     0,     0,    61,  -560,     0,     0,
    -560,  -560,   279,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,    62,    63,    64,     0,
      15,     0,    16,    17,    18,    19,  -560,     0,  -560,     0,
    -560,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,     0,    44,    45,
       0,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,     0,     0,    49,    50,     0,    51,    52,     0,
      53,     0,    54,     0,    55,    56,    57,    58,    59,    60,
       0,     0,    61,  -560,     0,     0,  -560,  -560,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,    63,    64,     0,     0,  -560,     0,     0,
       0,     0,     0,     0,  -560,   279,  -560,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,  -560,
       0,     0,     0,    15,     0,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
       0,    44,    45,     0,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,     0,     0,    49,    50,     0,
      51,    52,     0,    53,     0,    54,     0,    55,    56,    57,
      58,    59,    60,     0,     0,    61,  -560,     0,     0,  -560,
    -560,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,    62,    63,    64,     0,    15,
       0,    16,    17,    18,    19,     0,     0,  -560,     0,  -560,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,     0,    44,    45,     0,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,     0,     0,    49,    50,     0,    51,    52,     0,    53,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,   233,     0,     0,   234,   235,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
       0,    62,    63,    64,     0,    15,     0,    16,    17,    18,
      19,     0,     0,   236,     0,   237,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,     0,    44,    45,     0,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,     0,     0,    49,
      50,     0,    51,    52,     0,    53,     0,    54,     0,    55,
      56,    57,    58,    59,    60,     0,     0,    61,   233,     0,
       0,   234,   235,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,    62,    63,    64,
       0,    15,     0,    16,    17,    18,    19,     0,     0,   236,
       0,   237,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,     0,    44,
      45,     0,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   201,     0,     0,   112,    50,     0,    51,    52,
       0,     0,     0,    54,     0,    55,    56,    57,    58,    59,
      60,     0,     0,    61,   233,     0,     0,   234,   235,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,    62,    63,    64,     0,    15,     0,    16,
      17,    18,    19,     0,     0,   236,     0,   237,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,     0,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,     0,
       0,   112,    50,     0,    51,    52,     0,     0,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   237,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,     0,     0,
       0,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,     0,     0,     0,     0,     0,   156,   157,   158,   159,
     160,   161,   162,   163,    36,    37,   164,    39,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,   166,   167,   168,   169,   170,   171,   172,   173,
       0,     0,   174,   175,     0,     0,   176,   177,   178,   179,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     180,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   181,   182,   183,   184,   185,   186,   187,   188,
     189,   190,     0,   191,   192,     0,     0,     0,     0,     0,
     193,   194,  -529,  -529,  -529,  -529,  -529,  -529,  -529,  -529,
    -529,     0,     0,     0,     0,     0,     0,     0,  -529,     0,
    -529,  -529,  -529,  -529,     0,  -529,     0,     0,     0,  -529,
    -529,  -529,  -529,  -529,  -529,  -529,     0,     0,  -529,     0,
       0,     0,     0,     0,     0,     0,     0,  -529,  -529,  -529,
    -529,  -529,  -529,  -529,  -529,  -529,  -529,  -529,  -529,  -529,
       0,     0,  -529,     0,     0,  -529,  -529,  -529,     0,  -529,
    -529,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -529,
       0,     0,  -529,  -529,     0,  -529,  -529,     0,  -529,  -529,
    -529,     0,  -529,  -529,  -529,  -529,  -529,  -529,     0,     0,
    -529,     0,     0,     0,     0,     0,     0,  -530,  -530,  -530,
    -530,  -530,  -530,  -530,  -530,  -530,     0,     0,     0,     0,
    -529,  -529,  -529,  -530,  -529,  -530,  -530,  -530,  -530,  -529,
    -530,     0,     0,     0,  -530,  -530,  -530,  -530,  -530,  -530,
    -530,     0,     0,  -530,     0,     0,     0,     0,     0,     0,
       0,     0,  -530,  -530,  -530,  -530,  -530,  -530,  -530,  -530,
    -530,  -530,  -530,  -530,  -530,     0,     0,  -530,     0,     0,
    -530,  -530,  -530,     0,  -530,  -530,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -530,     0,     0,  -530,  -530,     0,
    -530,  -530,     0,  -530,  -530,  -530,     0,  -530,  -530,  -530,
    -530,  -530,  -530,     0,     0,  -530,     0,     0,     0,     0,
       0,     0,  -532,  -532,  -532,  -532,  -532,  -532,  -532,  -532,
    -532,     0,     0,     0,     0,  -530,  -530,  -530,  -532,  -530,
    -532,  -532,  -532,  -532,  -530,     0,     0,     0,     0,  -532,
    -532,  -532,  -532,  -532,  -532,  -532,     0,     0,  -532,     0,
       0,     0,     0,     0,     0,     0,     0,  -532,  -532,  -532,
    -532,  -532,  -532,  -532,  -532,  -532,  -532,  -532,  -532,  -532,
       0,     0,  -532,     0,     0,  -532,  -532,  -532,     0,  -532,
    -532,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -532,
     754,     0,  -532,  -532,     0,  -532,  -532,     0,  -532,  -532,
    -532,     0,  -532,  -532,  -532,  -532,  -532,  -532,     0,     0,
    -532,     0,     0,     0,     0,     0,     0,   -99,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -532,  -532,  -532,     0,     0,     0,     0,     0,     0,  -532,
    -533,  -533,  -533,  -533,  -533,  -533,  -533,  -533,  -533,     0,
       0,     0,     0,     0,     0,     0,  -533,     0,  -533,  -533,
    -533,  -533,     0,     0,     0,     0,     0,  -533,  -533,  -533,
    -533,  -533,  -533,  -533,     0,     0,  -533,     0,     0,     0,
       0,     0,     0,     0,     0,  -533,  -533,  -533,  -533,  -533,
    -533,  -533,  -533,  -533,  -533,  -533,  -533,  -533,     0,     0,
    -533,     0,     0,  -533,  -533,  -533,     0,  -533,  -533,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -533,   755,     0,
    -533,  -533,     0,  -533,  -533,     0,  -533,  -533,  -533,     0,
    -533,  -533,  -533,  -533,  -533,  -533,     0,     0,  -533,     0,
       0,     0,     0,     0,     0,  -101,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -533,  -533,
    -533,     0,     0,     0,     0,     0,     0,  -533,  -249,  -249,
    -249,  -249,  -249,  -249,  -249,  -249,  -249,     0,     0,     0,
       0,     0,     0,     0,  -249,     0,  -249,  -249,  -249,  -249,
       0,     0,     0,     0,     0,  -249,  -249,  -249,  -249,  -249,
    -249,  -249,     0,     0,  -249,     0,     0,     0,     0,     0,
       0,     0,     0,  -249,  -249,  -249,  -249,  -249,  -249,  -249,
    -249,  -249,  -249,  -249,  -249,  -249,     0,     0,  -249,     0,
       0,  -249,  -249,  -249,     0,  -249,  -249,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -249,     0,     0,  -249,  -249,
       0,  -249,  -249,     0,  -249,  -249,  -249,     0,  -249,  -249,
    -249,  -249,  -249,  -249,     0,     0,  -249,     0,     0,     0,
       0,     0,     0,  -534,  -534,  -534,  -534,  -534,  -534,  -534,
    -534,  -534,     0,     0,     0,     0,  -249,  -249,  -249,  -534,
       0,  -534,  -534,  -534,  -534,   261,     0,     0,     0,     0,
    -534,  -534,  -534,  -534,  -534,  -534,  -534,     0,     0,  -534,
       0,     0,     0,     0,     0,     0,     0,     0,  -534,  -534,
    -534,  -534,  -534,  -534,  -534,  -534,  -534,  -534,  -534,  -534,
    -534,     0,     0,  -534,     0,     0,  -534,  -534,  -534,     0,
    -534,  -534,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -534,     0,     0,  -534,  -534,     0,  -534,  -534,     0,  -534,
    -534,  -534,     0,  -534,  -534,  -534,  -534,  -534,  -534,     0,
       0,  -534,     0,     0,     0,     0,     0,     0,  -535,  -535,
    -535,  -535,  -535,  -535,  -535,  -535,  -535,     0,     0,     0,
       0,  -534,  -534,  -534,  -535,     0,  -535,  -535,  -535,  -535,
    -534,     0,     0,     0,     0,  -535,  -535,  -535,  -535,  -535,
    -535,  -535,     0,     0,  -535,     0,     0,     0,     0,     0,
       0,     0,     0,  -535,  -535,  -535,  -535,  -535,  -535,  -535,
    -535,  -535,  -535,  -535,  -535,  -535,     0,     0,  -535,     0,
       0,  -535,  -535,  -535,     0,  -535,  -535,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -535,     0,     0,  -535,  -535,
       0,  -535,  -535,     0,  -535,  -535,  -535,     0,  -535,  -535,
    -535,  -535,  -535,  -535,     0,     0,  -535,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -535,  -535,  -535,     0,
       0,     0,     0,     0,     0,  -535,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,   141,   142,   143,   144,   145,
       0,     0,     0,   146,   147,   148,   219,   220,   221,   222,
     153,   154,   155,     0,     0,     0,     0,     0,   156,   157,
     158,   223,   224,   225,   226,   163,   304,   305,   227,   306,
       0,     0,     0,     0,     0,     0,   307,     0,     0,     0,
       0,     0,     0,   165,   166,   167,   168,   169,   170,   171,
     172,   173,     0,     0,   174,   175,     0,     0,   176,   177,
     178,   179,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   180,     0,     0,     0,     0,     0,     0,     0,
     308,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   181,   182,   183,   184,   185,   186,
     187,   188,   189,   190,     0,   191,   192,     0,     0,     0,
       0,     0,   193,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,   145,     0,     0,     0,
     146,   147,   148,   219,   220,   221,   222,   153,   154,   155,
       0,     0,     0,     0,     0,   156,   157,   158,   223,   224,
     225,   226,   163,   304,   305,   227,   306,     0,     0,     0,
       0,     0,     0,   307,     0,     0,     0,     0,     0,     0,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
       0,   174,   175,     0,     0,   176,   177,   178,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   180,
       0,     0,     0,     0,     0,     0,     0,   427,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   181,   182,   183,   184,   185,   186,   187,   188,   189,
     190,     0,   191,   192,     0,     0,     0,     0,     0,   193,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   143,   144,   145,     0,     0,     0,   146,   147,   148,
     219,   220,   221,   222,   153,   154,   155,     0,     0,     0,
       0,     0,   156,   157,   158,   223,   224,   225,   226,   163,
       0,     0,   227,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,   166,   167,
     168,   169,   170,   171,   172,   173,     0,     0,   174,   175,
       0,     0,   176,   177,   178,   179,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   180,     0,     0,     0,
     228,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,   182,
     183,   184,   185,   186,   187,   188,   189,   190,     0,   191,
     192,     0,     0,     0,     0,     0,   193,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,     0,     0,     0,   146,   147,   148,   219,   220,   221,
     222,   153,   154,   155,     0,     0,     0,     0,     0,   156,
     157,   158,   223,   224,   225,   226,   163,     0,     0,   227,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,   166,   167,   168,   169,   170,
     171,   172,   173,     0,     0,   174,   175,     0,     0,   176,
     177,   178,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,   182,   183,   184,   185,
     186,   187,   188,   189,   190,     0,   191,   192,     0,     0,
       0,     0,     0,   193,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,     0,     0,     0,
      15,     0,   102,   103,    18,    19,     0,     0,     0,     0,
       0,   104,   105,   106,    23,    24,    25,    26,     0,     0,
     107,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,     0,    44,    45,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   297,     0,     0,   112,    50,     0,    51,    52,     0,
       0,     0,    54,     0,    55,    56,    57,    58,    59,    60,
       0,     0,    61,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,     0,     0,
       0,    15,   113,   102,   103,    18,    19,     0,     0,   298,
       0,     0,   104,   105,   106,    23,    24,    25,    26,     0,
       0,   107,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,     0,    44,
      45,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   297,     0,     0,   112,    50,     0,    51,    52,
       0,     0,     0,    54,     0,    55,    56,    57,    58,    59,
      60,     0,     0,    61,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,     0,
       0,     0,    15,   113,   102,   103,    18,    19,     0,     0,
     542,     0,     0,   104,   105,   106,    23,    24,    25,    26,
       0,     0,   107,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
     246,    40,    41,    42,     0,     0,    43,     0,     0,   247,
      44,    45,     0,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,   112,    50,     0,    51,
      52,     0,     0,  -254,    54,     0,    55,    56,    57,    58,
     250,    60,     0,     0,    61,   233,     0,     0,   234,   235,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,     0,    62,   251,    64,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,     0,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,     0,
       0,    49,    50,     0,    51,    52,     0,    53,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    62,
      63,    64,    15,     0,   102,   103,    18,    19,     0,     0,
       0,     0,     0,   104,   105,   106,    23,    24,    25,    26,
       0,     0,   107,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
     246,    40,    41,    42,     0,     0,    43,     0,     0,   247,
      44,    45,     0,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,   112,    50,     0,    51,
      52,     0,     0,     0,    54,     0,    55,    56,    57,    58,
     250,    60,     0,     0,    61,   233,     0,     0,   234,   235,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    62,   251,    64,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,   107,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,   246,    40,    41,    42,     0,
       0,    43,     0,     0,   247,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,     0,
       0,   112,    50,     0,    51,    52,     0,   248,   249,    54,
       0,    55,    56,    57,    58,   250,    60,     0,     0,    61,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,     0,     0,    62,
     251,    64,    15,     0,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,     0,
      44,    45,     0,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,     0,     0,    49,    50,     0,    51,
      52,     0,    53,     0,    54,     0,    55,    56,    57,    58,
      59,    60,     0,     0,    61,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    62,    63,    64,    15,     0,   102,
     103,    18,    19,     0,     0,     0,     0,     0,   104,   105,
     106,    23,    24,    25,    26,     0,     0,   107,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,   246,    40,    41,    42,     0,
       0,    43,     0,     0,   247,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,     0,
       0,   112,    50,     0,    51,    52,     0,   651,   249,    54,
       0,    55,    56,    57,    58,   250,    60,     0,     0,    61,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    62,
     251,    64,    15,     0,   102,   103,    18,    19,     0,     0,
       0,     0,     0,   104,   105,   106,    23,    24,    25,    26,
       0,     0,   107,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
     246,    40,    41,    42,     0,     0,    43,     0,     0,   247,
      44,    45,     0,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,   112,    50,     0,    51,
      52,     0,   248,     0,    54,     0,    55,    56,    57,    58,
     250,    60,     0,     0,    61,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    62,   251,    64,    15,     0,   102,
     103,    18,    19,     0,     0,     0,     0,     0,   104,   105,
     106,    23,    24,    25,    26,     0,     0,   107,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,   246,    40,    41,    42,     0,
       0,    43,     0,     0,   247,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,     0,
       0,   112,    50,     0,    51,    52,     0,   651,     0,    54,
       0,    55,    56,    57,    58,   250,    60,     0,     0,    61,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    62,
     251,    64,    15,     0,   102,   103,    18,    19,     0,     0,
       0,     0,     0,   104,   105,   106,    23,    24,    25,    26,
       0,     0,   107,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
     246,    40,    41,    42,     0,     0,    43,     0,     0,   247,
      44,    45,     0,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,   112,    50,     0,    51,
      52,     0,     0,     0,    54,     0,    55,    56,    57,    58,
     250,    60,     0,     0,    61,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    62,   251,    64,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,   107,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,     0,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,     0,
       0,   112,    50,     0,    51,    52,     0,   536,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    62,
     251,    64,    15,     0,   102,   103,    18,    19,     0,     0,
       0,     0,     0,   104,   105,   106,    23,    24,    25,    26,
       0,     0,   107,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,     0,
      44,    45,     0,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,   112,    50,     0,    51,
      52,     0,   248,     0,    54,     0,    55,    56,    57,    58,
      59,    60,     0,     0,    61,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    62,   251,    64,    15,     0,   102,
     103,    18,    19,     0,     0,     0,     0,     0,   104,   105,
     106,    23,    24,    25,    26,     0,     0,   107,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,     0,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,     0,
       0,   112,    50,     0,    51,    52,     0,   536,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    62,
     251,    64,    15,     0,   102,   103,    18,    19,     0,     0,
       0,     0,     0,   104,   105,   106,    23,    24,    25,    26,
       0,     0,   107,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,     0,
      44,    45,     0,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,   112,    50,     0,    51,
      52,     0,   805,     0,    54,     0,    55,    56,    57,    58,
      59,    60,     0,     0,    61,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    62,   251,    64,    15,     0,   102,
     103,    18,    19,     0,     0,     0,     0,     0,   104,   105,
     106,    23,    24,    25,    26,     0,     0,   107,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,     0,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,     0,
       0,   112,    50,     0,    51,    52,     0,   651,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    62,
     251,    64,    15,     0,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,     0,
      44,    45,     0,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,   112,    50,     0,    51,
      52,     0,     0,     0,    54,     0,    55,    56,    57,    58,
      59,    60,     0,     0,    61,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    62,    63,    64,    15,     0,   102,
     103,    18,    19,     0,     0,     0,     0,     0,   104,   105,
     106,    23,    24,    25,    26,     0,     0,   107,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,     0,    44,    45,     0,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,     0,
       0,   112,    50,     0,    51,    52,     0,     0,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    62,
     251,    64,    15,     0,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,   107,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,     0,
      44,    45,     0,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,   112,    50,     0,    51,
      52,     0,     0,     0,    54,     0,    55,    56,    57,    58,
      59,    60,     0,     0,    61,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    62,   251,    64,    15,     0,   102,
     103,    18,    19,     0,     0,     0,     0,     0,   104,   105,
     106,    23,    24,    25,    26,     0,     0,   107,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,   108,
      35,    36,    37,   109,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,     0,    44,    45,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,   111,     0,
       0,   112,    50,     0,    51,    52,     0,     0,     0,    54,
       0,    55,    56,    57,    58,    59,    60,     0,     0,    61,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,     0,     0,     0,    15,   113,
     102,   103,    18,    19,     0,     0,     0,     0,     0,   104,
     105,   106,    23,    24,    25,    26,     0,     0,   107,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,     0,    44,    45,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   212,
       0,     0,    49,    50,     0,    51,    52,     0,    53,     0,
      54,     0,    55,    56,    57,    58,    59,    60,     0,     0,
      61,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,     0,     0,     0,    15,
     113,   102,   103,    18,    19,     0,     0,     0,     0,     0,
     104,   105,   106,    23,    24,    25,    26,     0,     0,   107,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,     0,    44,    45,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     297,     0,     0,   344,    50,     0,    51,    52,     0,   345,
       0,    54,     0,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,     0,     0,     0,
      15,   113,   102,   103,    18,    19,     0,     0,     0,     0,
       0,   104,   105,   106,    23,    24,    25,    26,     0,     0,
     107,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,   108,    35,    36,    37,   109,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,     0,    44,    45,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,     0,     0,   112,    50,     0,    51,    52,     0,
       0,     0,    54,     0,    55,    56,    57,    58,    59,    60,
       0,     0,    61,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,     0,     0,
       0,    15,   113,   102,   103,    18,    19,     0,     0,     0,
       0,     0,   104,   105,   106,    23,    24,    25,    26,     0,
       0,   107,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,     0,    44,
      45,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   297,     0,     0,   344,    50,     0,    51,    52,
       0,     0,     0,    54,     0,    55,    56,    57,    58,    59,
      60,     0,     0,    61,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,     0,
       0,     0,    15,   113,   102,   103,    18,    19,     0,     0,
       0,     0,     0,   104,   105,   106,    23,    24,    25,    26,
       0,     0,   107,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,     0,
      44,    45,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   879,     0,     0,   112,    50,     0,    51,
      52,     0,     0,     0,    54,     0,    55,    56,    57,    58,
      59,    60,     0,     0,    61,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
       0,     0,     0,    15,   113,   102,   103,    18,    19,     0,
       0,     0,     0,     0,   104,   105,   106,    23,    24,    25,
      26,     0,     0,   107,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
       0,    44,    45,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   898,     0,     0,   112,    50,     0,
      51,    52,   580,   581,     0,    54,   582,    55,    56,    57,
      58,    59,    60,     0,     0,    61,     0,     0,     0,     0,
       0,   165,   166,   167,   168,   169,   170,   171,   172,   173,
       0,     0,   174,   175,     0,   113,   176,   177,   178,   179,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     180,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   181,   182,   183,   184,   185,   186,   187,   188,
     189,   190,     0,   191,   192,   588,   589,     0,     0,   590,
     193,   261,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,   166,   167,   168,   169,   170,
     171,   172,   173,     0,     0,   174,   175,     0,     0,   176,
     177,   178,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,   182,   183,   184,   185,
     186,   187,   188,   189,   190,     0,   191,   192,   609,   581,
       0,     0,   610,   193,   261,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,   166,   167,
     168,   169,   170,   171,   172,   173,     0,     0,   174,   175,
       0,     0,   176,   177,   178,   179,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   180,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,   182,
     183,   184,   185,   186,   187,   188,   189,   190,     0,   191,
     192,   594,   589,     0,     0,   595,   193,   261,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
       0,   174,   175,     0,     0,   176,   177,   178,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   180,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   181,   182,   183,   184,   185,   186,   187,   188,   189,
     190,     0,   191,   192,   625,   581,     0,     0,   626,   193,
     261,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,   166,   167,   168,   169,   170,   171,
     172,   173,     0,     0,   174,   175,     0,     0,   176,   177,
     178,   179,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   180,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   181,   182,   183,   184,   185,   186,
     187,   188,   189,   190,     0,   191,   192,   628,   589,     0,
       0,   629,   193,   261,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,   166,   167,   168,
     169,   170,   171,   172,   173,     0,     0,   174,   175,     0,
       0,   176,   177,   178,   179,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   180,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   181,   182,   183,
     184,   185,   186,   187,   188,   189,   190,     0,   191,   192,
     635,   581,     0,     0,   636,   193,   261,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
     166,   167,   168,   169,   170,   171,   172,   173,     0,     0,
     174,   175,     0,     0,   176,   177,   178,   179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   180,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     181,   182,   183,   184,   185,   186,   187,   188,   189,   190,
       0,   191,   192,   638,   589,     0,     0,   639,   193,   261,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,   166,   167,   168,   169,   170,   171,   172,
     173,     0,     0,   174,   175,     0,     0,   176,   177,   178,
     179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   180,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   181,   182,   183,   184,   185,   186,   187,
     188,   189,   190,     0,   191,   192,   672,   581,     0,     0,
     673,   193,   261,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,   166,   167,   168,   169,
     170,   171,   172,   173,     0,     0,   174,   175,     0,     0,
     176,   177,   178,   179,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   180,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   181,   182,   183,   184,
     185,   186,   187,   188,   189,   190,     0,   191,   192,   675,
     589,     0,     0,   676,   193,   261,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,   166,
     167,   168,   169,   170,   171,   172,   173,     0,     0,   174,
     175,     0,     0,   176,   177,   178,   179,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   180,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
     182,   183,   184,   185,   186,   187,   188,   189,   190,     0,
     191,   192,   810,   581,     0,     0,   811,   193,   261,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,   166,   167,   168,   169,   170,   171,   172,   173,
       0,     0,   174,   175,     0,     0,   176,   177,   178,   179,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     180,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   181,   182,   183,   184,   185,   186,   187,   188,
     189,   190,     0,   191,   192,   813,   589,     0,     0,   814,
     193,   261,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,   166,   167,   168,   169,   170,
     171,   172,   173,     0,     0,   174,   175,     0,     0,   176,
     177,   178,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,   182,   183,   184,   185,
     186,   187,   188,   189,   190,     0,   191,   192,   958,   581,
       0,     0,   959,   193,   261,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,   166,   167,
     168,   169,   170,   171,   172,   173,     0,     0,   174,   175,
       0,     0,   176,   177,   178,   179,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   180,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,   182,
     183,   184,   185,   186,   187,   188,   189,   190,     0,   191,
     192,   965,   581,     0,     0,   966,   193,   261,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
       0,   174,   175,     0,     0,   176,   177,   178,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   180,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   181,   182,   183,   184,   185,   186,   187,   188,   189,
     190,     0,   191,   192,   968,   589,     0,     0,   969,   193,
     261,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,   166,   167,   168,   169,   170,   171,
     172,   173,     0,     0,   174,   175,     0,     0,   176,   177,
     178,   179,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   180,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   181,   182,   183,   184,   185,   186,
     187,   188,   189,   190,     0,   191,   192,   594,   589,     0,
       0,   595,   193,   261,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,   166,   167,   168,
     169,   170,   171,   172,   173,     0,     0,   174,   175,     0,
       0,   176,   177,   178,   179,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   180,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   714,     0,
       0,     0,     0,     0,     0,     0,     0,   181,   182,   183,
     184,   185,   186,   187,   188,   189,   190,     0,   191,   192,
       0,     0,     0,     0,     0,   193,   348,   349,   350,   351,
     352,   353,   354,   355,   356,   357,   358,   359,   360,     0,
       0,   361,   362,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,     0,     0,   361,   362,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   363,     0,   364,   365,   366,   367,   368,
     369,   370,   371,   372,   373,     0,     0,     0,     0,     0,
     363,     0,   364,   365,   366,   367,   368,   369,   370,   371,
     372,   373,   348,   349,   350,   351,   352,   353,   354,   355,
     356,   357,   358,   359,   360,     0,   237,   361,   362,     0,
     348,   349,   350,   351,   352,   353,   354,   355,   356,   357,
     358,   359,   360,     0,     0,   361,   362,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,   364,   365,   366,   367,   368,   369,   370,   371,   372,
     373,     0,     0,     0,     0,     0,     0,   363,  -256,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,     0,
       0,     0,     0,     0,     0,     0,  -257,   348,   349,   350,
     351,   352,   353,   354,   355,   356,   357,   358,   359,   360,
       0,     0,   361,   362,     0,   348,   349,   350,   351,   352,
     353,   354,   355,   356,   357,   358,   359,   360,     0,     0,
     361,   362,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,     0,   364,   365,   366,   367,
     368,   369,   370,   371,   372,   373,     0,     0,     0,     0,
       0,     0,   363,  -258,   364,   365,   366,   367,   368,   369,
     370,   371,   372,   373,     0,     0,     0,     0,     0,     0,
       0,  -259,   348,   349,   350,   351,   352,   353,   354,   355,
     356,   357,   358,   359,   360,     0,     0,   361,   362,     0,
       0,     0,   442,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,     0,     0,   361,   362,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,   364,   365,   366,   367,   368,   369,   370,   371,   372,
     373,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,     0,   364,   365,   366,   367,   368,   369,   370,   371,
     372,   373,   348,   349,   350,   351,   352,   353,   354,   355,
     356,   357,   358,  -561,  -561,     0,     0,   361,   362,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,   365,   366,   367,   368,   369,   370,   371,   372,
     373
};

static const yytype_int16 yycheck[] =
{
       2,    82,    83,    27,    16,    17,    84,    14,    20,    15,
       2,   303,     4,    21,    22,    63,     5,     6,   315,    21,
     256,    28,   209,    10,    13,    77,   257,     7,    15,    28,
     430,   401,    49,    16,    17,     4,     7,    20,   303,   298,
     447,   374,   291,    14,   421,   378,   295,   347,   381,    51,
      52,   376,   111,    66,   290,   291,   621,    28,    53,   295,
      66,    66,   703,   486,    53,   630,   445,   400,    51,    52,
     449,   881,   691,   778,   399,    55,    25,    15,   687,   597,
     598,   414,   613,   416,    55,    16,    17,    26,    77,    20,
     415,    99,   425,   703,   347,    27,   627,   105,   106,   424,
      29,   780,    16,   105,    25,    80,   637,     0,    25,    16,
      17,    22,   120,    20,   280,    25,   863,     9,    10,   542,
      51,    25,    89,    15,   205,    58,    59,    89,    66,    25,
      25,   464,   385,   386,   215,   687,    26,    89,   284,   691,
     102,   118,   288,   674,   954,    25,   443,    91,   123,    89,
     447,   703,    37,    38,   859,   316,   489,   135,   319,    89,
     321,   138,   323,   488,   325,   385,   386,   722,    57,    58,
      59,    60,   102,   728,   118,   137,    55,   144,   344,    28,
     112,   110,   144,   115,   116,   140,   796,   138,    99,   138,
     145,   140,   144,   118,   138,   276,   135,   203,   112,   118,
     947,   115,   116,   209,   144,    91,    16,   137,   500,   914,
     140,   143,    89,   145,   144,   733,   452,   138,   110,   300,
     517,   138,   453,   210,   211,   238,    55,   486,   138,   143,
     140,   145,   238,   912,   138,   500,   877,    89,   608,   245,
     881,   946,   138,   138,    91,   135,   298,   866,   228,   261,
     140,    91,   659,   265,   256,   257,   140,   228,   138,   261,
     145,   870,    18,   280,    20,   140,    72,   144,   877,   282,
      51,   118,    53,    54,    55,    56,   282,   282,   118,   658,
     441,   812,   265,   542,   290,   291,    89,   279,   711,   295,
     855,   138,   144,   141,   342,   287,    89,   145,   138,   347,
     238,   122,   112,   539,   450,   115,   116,   245,    25,   298,
     456,    72,    91,   954,   866,    91,   495,    89,   210,   211,
     620,   467,   128,   129,   130,   877,   332,   344,   138,   881,
     102,   118,    91,   143,   265,   145,    55,   385,   386,   398,
     332,   144,   118,    89,   282,   337,   333,   334,   335,   336,
     345,   144,   617,   650,   515,    25,   345,   138,   265,   118,
     657,   250,   659,   332,   376,   126,   127,   128,   129,   130,
     378,   741,   144,    91,   376,    89,   138,   269,   270,   138,
      37,    38,   715,   145,   717,    89,   763,   399,    16,   138,
     138,   716,   400,    20,   332,   541,   145,   399,   144,   138,
     118,   118,   954,   415,    58,    59,   408,   378,   416,   645,
     381,   342,   424,   415,   420,   421,   347,   425,    25,   308,
     669,   138,   424,   654,   141,    16,    25,   138,   145,   400,
     144,    49,   484,   669,   486,    25,   967,   135,   703,    72,
     144,   333,   334,   335,   336,   416,   338,   339,   740,   856,
     452,   453,   711,   460,   425,   855,   464,   141,   645,   461,
     713,   460,   445,   833,   470,   145,   719,   138,   118,   461,
     616,   552,   725,   726,   145,   740,   488,   593,   470,   142,
     596,   489,   420,   421,   112,    91,   488,   115,   116,   460,
     542,   136,   475,   464,   112,   484,    55,   486,   614,   719,
     679,   680,   433,    91,   506,   725,   726,   509,    91,   511,
     402,   118,   118,    89,   138,   143,    72,   145,   489,   118,
      91,   112,   668,    89,   115,   116,   102,   897,   118,    91,
     118,   138,   470,   535,   141,   118,   102,   683,   145,   138,
      72,   772,   141,   385,   386,   569,   145,   118,   138,   138,
     140,   141,   143,   542,   145,   145,   118,    91,    98,   856,
      91,   137,    51,   587,    53,    54,    55,    56,   144,   822,
      91,   137,    15,    13,   839,   417,   418,   585,   144,    17,
      18,    16,    26,    91,   118,   593,    51,   118,   596,   613,
      55,   593,    63,    15,   596,   597,   598,   118,   100,   778,
     606,   780,   822,   627,   612,   138,   614,   138,   941,   138,
     118,   141,   614,   637,    91,   940,   136,   138,   620,   621,
     607,   623,     2,   465,     4,     5,     6,   138,   630,     9,
      10,   777,    26,    13,   641,    15,    16,    17,   618,   645,
      20,   118,   641,   118,    89,    89,   138,   618,   138,   138,
     674,   729,   654,   640,   841,    51,   274,   102,   102,   711,
     847,   685,   280,   669,    26,   713,   138,    44,   606,    49,
     641,   719,   720,    53,   585,   658,    51,   725,   726,   703,
     859,   860,   593,    63,   112,   596,   138,   115,   116,   620,
     836,   135,   137,   137,    89,    89,   140,    77,   112,   144,
     144,   115,   116,    26,   716,    89,    89,   102,   102,   717,
      89,   118,    15,    18,   716,   607,   136,    15,   102,   102,
     712,    89,   711,   102,   136,   138,   344,    89,   136,   735,
     110,   733,   112,   912,   102,   914,   742,   142,   120,    92,
     102,   135,   137,   137,   746,    15,   717,   749,   640,   144,
     144,    14,   112,   137,   137,   115,   116,   763,   137,    15,
     144,   144,    15,   141,   751,   144,    89,   946,    89,   137,
     772,   138,   796,   135,   822,   137,   144,   138,   140,   102,
     143,   102,   144,   143,   138,   145,   788,   789,   812,   791,
     138,   793,   794,   411,    89,   138,   798,   735,   138,   801,
     802,   419,    89,   123,   742,   138,   138,   102,   138,    15,
     799,   429,   135,    89,   137,   102,   137,   140,    15,   900,
     200,   144,   136,   144,   136,   763,   102,    15,    15,    15,
     210,   211,   138,   691,    15,   841,   694,   123,   138,    16,
      17,   847,   137,    20,    61,   703,   136,    64,    65,   144,
     137,   840,    61,   855,    55,    64,    65,   144,    15,   751,
      51,   137,    53,    54,    55,    56,    55,   138,   144,    46,
      47,   138,   138,   138,    51,    52,    15,   719,   720,    15,
     140,   261,   140,   725,   726,   265,    63,    64,   138,   269,
     270,   461,    13,   917,   274,     6,   113,   114,   516,   279,
     280,   943,    93,   947,   113,   114,   679,   287,    99,   100,
     730,   942,   754,   755,   315,   757,   758,     7,   298,   839,
     922,   923,   924,   925,   240,   927,   928,   877,   940,   931,
     687,   933,   934,   941,   125,    -1,    -1,   128,   940,    -1,
     942,   943,    62,   967,    64,    65,    -1,    -1,    -1,   140,
      -1,    -1,   332,   333,   334,   335,   336,   337,   338,   339,
      -1,    -1,   342,    -1,   344,   345,    62,   347,    64,    65,
     941,    -1,    -1,   975,   976,   977,   978,   979,    -1,    -1,
     822,    -1,    -1,    -1,    51,   987,    53,    54,    55,    56,
      -1,    -1,    -1,   113,   114,    -1,   376,    -1,    -1,    51,
      -1,    53,    54,    55,    56,   385,   386,   849,   866,    -1,
     868,    -1,    -1,    -1,   872,    -1,    -1,   113,   114,   399,
      -1,   401,   402,   881,    -1,   883,    93,    -1,    -1,    -1,
      -1,   411,    99,   100,    -1,   415,    -1,    -1,    -1,   419,
      -1,    93,    -1,    -1,   424,    -1,    -1,    99,   100,   429,
      -1,    -1,    51,    -1,    53,    54,    55,    56,   125,    -1,
      51,   128,    53,    54,    55,    56,    63,    64,    65,   246,
     247,   248,   249,   125,   251,    -1,   128,    -1,   145,    -1,
      -1,   461,    -1,    -1,   261,    -1,   704,    -1,   265,    -1,
     470,    -1,   950,    -1,    93,    -1,   954,    -1,   956,    -1,
      99,   100,    93,   961,   484,    -1,   486,    -1,   488,   100,
      63,    64,    65,   731,   732,    -1,   113,   114,    63,    64,
      65,    66,    -1,    -1,   112,   983,   125,   115,   116,   128,
     748,    -1,    -1,    -1,   125,    51,   516,    53,    54,    55,
      56,   140,   760,   761,    -1,    -1,    -1,    -1,    72,    -1,
     768,    -1,    -1,   141,   142,    -1,    -1,   145,    -1,    -1,
     113,   114,   542,    87,    88,   342,   784,   785,   113,   114,
     347,   348,   349,   350,   351,   352,   353,   354,   355,   356,
     357,   358,   359,   360,   361,   362,   363,   364,   365,   366,
     367,   368,   369,   370,   371,   372,   373,    72,    -1,   376,
      -1,   125,   126,   127,   128,   129,   130,    -1,   385,   386,
      -1,    -1,    87,    88,   832,    -1,    63,    64,    65,    66,
      -1,    -1,   399,    63,    64,    65,   844,   607,   608,    -1,
      -1,     2,    -1,     4,     5,     6,   413,    -1,   415,    -1,
     417,   418,    13,    63,    64,    65,    72,   424,   123,   124,
     125,   126,   127,   128,   129,   130,   433,    63,    64,    65,
     640,    87,    88,   440,    -1,   442,   113,   114,   445,    -1,
     447,    -1,    -1,   113,   114,    63,    64,    65,    49,    -1,
      -1,    -1,    53,    -1,    -1,   903,    -1,   905,   465,    -1,
     908,    -1,    -1,   113,   114,    -1,    -1,    -1,   475,    -1,
     126,   127,   128,   129,   130,    -1,    77,   113,   114,    -1,
      -1,   488,    63,    64,    65,    63,    64,    65,    51,    -1,
      53,    54,    55,    56,   704,   113,   114,    -1,    -1,    -1,
     507,   711,   712,   713,    -1,    -1,   716,    -1,    -1,   719,
     720,   112,    -1,    -1,    -1,   725,   726,    -1,   525,   526,
      -1,   731,   732,    40,    41,    42,    43,    44,    -1,   536,
      93,   741,   113,   114,    -1,   113,   114,    -1,   748,    -1,
      -1,   751,    51,    -1,    53,    54,    55,    56,    -1,    -1,
     760,   761,    51,    -1,    53,    54,    55,    56,   768,    -1,
       2,    -1,     4,     5,     6,     7,    -1,    -1,    -1,    -1,
      -1,    13,    -1,    -1,   784,   785,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    93,    -1,   509,    -1,   511,   799,
      99,    -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,   200,
      99,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      -1,    53,   822,   620,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   832,   833,    -1,    -1,    -1,    -1,    -1,    -1,
     840,    -1,    -1,    -1,   844,    77,    -1,    -1,    -1,    -1,
      -1,   648,    -1,    -1,   651,    -1,    -1,    -1,    -1,    -1,
      -1,   658,   659,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     112,    -1,    -1,   274,    -1,    -1,    -1,    -1,   279,   280,
      -1,    -1,    -1,    -1,    -1,    -1,   287,   897,    -1,    -1,
      -1,    -1,    -1,   903,    -1,   905,    -1,   298,   908,    -1,
      -1,   708,    -1,    -1,    -1,    -1,   713,   714,    -1,   716,
      -1,    -1,   719,   720,    -1,    -1,    -1,    -1,   725,   726,
      -1,    -1,    -1,    72,    73,    74,    75,    76,    77,    78,
     940,   332,    81,    82,    -1,    -1,   337,    -1,    87,    88,
      -1,    -1,    -1,   344,   345,    -1,   347,   754,   755,    -1,
     757,   758,    -1,    -1,     2,    -1,     4,    -1,   200,    -1,
     767,    -1,    -1,    -1,    -1,    13,    -1,    -1,    -1,    -1,
      -1,    -1,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    -1,    -1,   385,   386,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   805,    -1,
     401,    49,    -1,    -1,    -1,    -1,    -1,    -1,   815,    -1,
     411,    -1,    -1,    -1,    -1,   822,    -1,    -1,   419,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   429,    -1,
      -1,    -1,   274,    -1,    -1,    -1,    -1,   279,   280,    -1,
      -1,    -1,   849,    -1,    -1,   287,    -1,    -1,    -1,   856,
      -1,    -1,    -1,    -1,    -1,    -1,   298,    -1,    -1,    -1,
     461,    -1,    -1,    -1,   112,   788,   789,    -1,   791,   470,
     793,   794,    -1,    -1,    -1,   798,    -1,    -1,   801,   802,
      -1,    -1,    -1,   484,    -1,   486,    -1,    -1,    -1,    -1,
     332,    -1,    -1,    -1,    -1,   337,    -1,    -1,    -1,    -1,
      -1,    -1,   344,   345,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   516,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   940,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   542,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   200,    -1,    -1,    -1,     2,    -1,     4,   401,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   411,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   419,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   429,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   922,
     923,   924,   925,    49,   927,   928,    -1,   608,   931,    -1,
     933,   934,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   461,
      -1,    -1,    -1,    -1,    -1,    -1,   274,    -1,   470,    -1,
      -1,   279,   280,    -1,    -1,    -1,    -1,    -1,    -1,   287,
      -1,    -1,   484,    -1,   486,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   975,   976,   977,   978,   979,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   987,    -1,   112,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   516,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   332,    -1,    -1,    -1,    -1,   337,
      -1,    -1,    -1,    -1,    -1,    -1,   344,    -1,    -1,   347,
     542,    -1,    -1,   704,    -1,    -1,    -1,    -1,    -1,    -1,
     711,   712,   713,    -1,    -1,    -1,    -1,    -1,   719,    -1,
      -1,    -1,    -1,    -1,   725,   726,    -1,    -1,    -1,    -1,
     731,   732,    -1,    -1,    -1,    -1,    -1,   385,   386,    -1,
     741,    -1,    -1,    -1,    -1,    -1,    -1,   748,    -1,    -1,
      -1,    -1,    -1,   401,   200,    -1,    -1,    -1,    -1,   760,
     761,    -1,    -1,   411,    -1,    -1,   608,   768,    -1,    -1,
      -1,   419,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   429,    -1,   784,   785,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   799,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,   461,    -1,    87,    88,    -1,    -1,    -1,
      -1,   822,   470,    -1,    -1,    -1,    -1,    -1,   274,    -1,
      -1,   832,   833,   279,   280,    -1,    -1,    -1,    -1,   840,
      -1,   287,    -1,   844,    -1,    -1,    -1,   119,    -1,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,    -1,
      -1,    -1,   704,    -1,    -1,    -1,   138,    -1,   516,   711,
     712,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,   332,    81,    82,   731,
     732,   337,    -1,    87,    88,    -1,   897,    -1,   344,   741,
      -1,   347,   903,    -1,   905,    -1,   748,   908,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   760,   761,
      -1,    -1,    -1,    -1,    -1,    -1,   768,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   130,    -1,    -1,   385,
     386,    -1,   784,   785,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   401,    -1,   799,    -1,    -1,
     608,    -1,    -1,    -1,    -1,   411,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   419,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   429,    -1,    -1,    -1,    -1,    -1,    -1,
     832,   833,    -1,    -1,    -1,    -1,    -1,    -1,   840,    -1,
       5,     6,   844,    -1,    -1,    -1,    -1,    -1,    13,    -1,
      -1,    -1,    -1,    -1,    -1,   461,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   470,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    47,    -1,    -1,    -1,    51,    52,    53,    -1,
      -1,    -1,    -1,    -1,    -1,   897,   704,    -1,    -1,    64,
      -1,   903,    -1,   905,   712,   713,   908,    -1,    -1,    -1,
     516,   719,    77,    -1,    -1,    -1,    -1,   725,   726,    -1,
      -1,    -1,    -1,   731,   732,    -1,    -1,    -1,     0,    -1,
      -1,    -1,    -1,   741,    -1,    -1,    -1,    -1,    -1,    -1,
     748,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      -1,    -1,   760,   761,    26,    27,    -1,    -1,    -1,    -1,
     768,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,   784,   785,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   608,    72,    73,    74,    75,    76,    77,    78,
      -1,    -1,    81,    82,   822,    -1,    -1,    89,    87,    88,
      -1,    -1,    -1,    -1,   832,   833,    -1,    -1,    -1,    -1,
     102,    -1,   840,    -1,    -1,    -1,   844,    -1,    -1,    -1,
     112,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    -1,   135,   136,    -1,    -1,    -1,   140,   141,
      -1,   143,   144,   145,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   246,   247,   248,   249,    -1,   251,    -1,    -1,   897,
      -1,    -1,    -1,    -1,    -1,   903,    -1,   905,   704,    -1,
     908,    -1,    -1,    -1,    -1,    -1,   712,   713,    -1,    -1,
      -1,    -1,    -1,   719,    -1,    -1,    -1,    -1,    -1,   725,
     726,    -1,    -1,    -1,    -1,   731,   732,    -1,    -1,    -1,
      -1,    -1,    -1,   298,    -1,   741,    -1,    -1,    -1,    -1,
      -1,    -1,   748,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   760,   761,    -1,    -1,    -1,    -1,
      -1,    -1,   768,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   784,   785,
     345,    -1,    -1,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   822,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   832,   833,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   844,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   413,    -1,
      -1,    -1,   417,   418,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   433,    -1,
      -1,    -1,    -1,    -1,    -1,   440,    -1,   442,    -1,    -1,
     445,    -1,   447,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   897,    -1,    -1,    -1,    -1,    -1,   903,    -1,   905,
     465,    -1,   908,    -1,    -1,    -1,    -1,    -1,    -1,     0,
     475,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   484,
      -1,   486,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    -1,    -1,    -1,    -1,    -1,    27,    28,    29,    -1,
      -1,    -1,   507,    -1,    -1,    -1,    37,    38,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,
     525,   526,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   536,    -1,    -1,    -1,    -1,    -1,   542,    -1,    -1,
      -1,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    -1,    -1,    87,    88,    89,    -1,
      -1,    92,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
      -1,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
      -1,   112,    -1,    -1,   115,   116,    -1,    -1,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
      -1,    -1,    -1,    -1,    -1,   136,   137,   138,    -1,    -1,
     141,   142,   143,   144,   145,   620,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     0,     1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    -1,    -1,   648,    -1,    19,   651,    21,    22,    23,
      24,    -1,    -1,   658,   659,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    -1,    67,    68,    -1,    70,    71,    -1,    -1,
      -1,    -1,    -1,   708,    -1,    -1,   711,    -1,    -1,   714,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      94,    -1,    96,    97,    -1,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,    -1,    -1,   111,   112,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,   754,
     755,    -1,   757,   758,    -1,    -1,    -1,   131,   132,   133,
      -1,    -1,   767,    -1,    -1,    -1,    -1,    -1,    -1,   143,
      -1,   145,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     0,    -1,
      -1,    -1,    -1,    -1,   799,    -1,    -1,    -1,    -1,    -1,
     805,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
     815,    -1,    -1,    -1,    -1,    27,    28,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,   840,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   849,    -1,    -1,    -1,    -1,    -1,
      -1,   856,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    88,    89,    -1,    -1,
      92,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     112,    -1,    -1,   115,   116,    -1,    -1,   119,    -1,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,    -1,
      -1,     0,    -1,    -1,   136,   137,   138,    -1,   140,   141,
     142,   143,   144,   145,    13,    14,    15,    -1,    17,    18,
      -1,    20,    -1,    -1,    -1,    -1,    -1,    26,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,    87,    88,
      89,    -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   112,    -1,    -1,   115,   116,    -1,   118,
     119,    -1,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    -1,    -1,     0,    -1,   135,   136,   137,   138,
      -1,    -1,   141,    -1,   143,   144,   145,    13,    14,    15,
      -1,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,    -1,
      26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    87,    88,    89,    -1,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,   115,
     116,    -1,   118,   119,    -1,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,    -1,    -1,     0,    -1,   135,
     136,   137,   138,    -1,    -1,   141,    -1,   143,   144,   145,
      13,    14,    15,    -1,    17,    18,    -1,    20,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    -1,    -1,    87,    88,    89,    -1,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      -1,    -1,   115,   116,    -1,   118,   119,    -1,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,    -1,    -1,
       0,    -1,    -1,   136,   137,   138,    -1,    -1,   141,    -1,
     143,   144,   145,    13,    14,    15,    -1,    17,    18,    -1,
      20,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    87,    88,    89,
      -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   112,    -1,    -1,   115,   116,    -1,   118,   119,
      -1,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,    -1,    -1,    -1,    -1,    -1,   136,   137,   138,    -1,
      -1,   141,    -1,   143,   144,   145,     1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    -1,    -1,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    -1,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    -1,    67,    68,    -1,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,
      -1,    96,    97,    -1,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,    -1,    -1,   111,   112,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   131,   132,   133,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,     1,
     145,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    15,    -1,    17,    18,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    -1,    67,    68,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,    -1,    -1,   111,
     112,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
     132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   143,     1,   145,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    15,    -1,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    -1,    67,    68,
      -1,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
      -1,    -1,   111,   112,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   143,     1,   145,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,    15,
      -1,    -1,    18,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      -1,    67,    68,    -1,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,
      96,    97,    -1,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,    -1,    -1,   111,   112,    -1,    -1,   115,
     116,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,   131,   132,   133,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,   143,    -1,   145,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    -1,    67,    68,    -1,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,   112,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,   132,   133,    -1,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,   143,     1,   145,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    14,    15,    -1,
      -1,    -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    -1,
      67,    68,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,    -1,    -1,   111,   112,    -1,    -1,   115,   116,
       1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,   131,   132,   133,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    -1,   143,    -1,   145,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    -1,    67,    68,    -1,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,    -1,    -1,
     111,   112,    -1,    -1,   115,   116,     1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
     131,   132,   133,    -1,    19,    -1,    21,    22,    23,    24,
     141,    -1,   143,    -1,   145,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    -1,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    -1,    67,    68,    -1,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,
      -1,    96,    97,    -1,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,    -1,    -1,   111,   112,    -1,    -1,
     115,   116,     1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,   131,   132,   133,    -1,
      19,    -1,    21,    22,    23,    24,   141,    -1,   143,    -1,
     145,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    -1,    67,    68,
      -1,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
      -1,    -1,   111,   112,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,   132,   133,    -1,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,   143,     1,   145,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,    15,
      -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      -1,    67,    68,    -1,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,
      96,    97,    -1,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,    -1,    -1,   111,   112,    -1,    -1,   115,
     116,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,   131,   132,   133,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,   143,    -1,   145,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    -1,    67,    68,    -1,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,   112,    -1,    -1,   115,   116,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,   131,   132,   133,    -1,    19,    -1,    21,    22,    23,
      24,    -1,    -1,   143,    -1,   145,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    -1,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    -1,    67,    68,    -1,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      94,    -1,    96,    97,    -1,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,    -1,    -1,   111,   112,    -1,
      -1,   115,   116,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,   131,   132,   133,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,   143,
      -1,   145,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    -1,    67,
      68,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,
      -1,    -1,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,    -1,    -1,   111,   112,    -1,    -1,   115,   116,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,   131,   132,   133,    -1,    19,    -1,    21,
      22,    23,    24,    -1,    -1,   143,    -1,   145,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    -1,    67,    68,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    -1,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,    -1,    -1,   111,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
     132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   145,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      -1,    -1,    81,    82,    -1,    -1,    85,    86,    87,    88,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    -1,   132,   133,    -1,    -1,    -1,    -1,    -1,
     139,   140,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    26,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    68,    -1,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,   100,
     101,    -1,   103,   104,   105,   106,   107,   108,    -1,    -1,
     111,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
     131,   132,   133,    19,   135,    21,    22,    23,    24,   140,
      26,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    68,    -1,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,
      96,    97,    -1,    99,   100,   101,    -1,   103,   104,   105,
     106,   107,   108,    -1,    -1,   111,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   131,   132,   133,    19,   135,
      21,    22,    23,    24,   140,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    68,    -1,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      91,    -1,    93,    94,    -1,    96,    97,    -1,    99,   100,
     101,    -1,   103,   104,   105,   106,   107,   108,    -1,    -1,
     111,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,   140,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    68,    -1,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,
      93,    94,    -1,    96,    97,    -1,    99,   100,   101,    -1,
     103,   104,   105,   106,   107,   108,    -1,    -1,   111,    -1,
      -1,    -1,    -1,    -1,    -1,   118,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,   132,
     133,    -1,    -1,    -1,    -1,    -1,    -1,   140,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    68,    -1,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,
      -1,    96,    97,    -1,    99,   100,   101,    -1,   103,   104,
     105,   106,   107,   108,    -1,    -1,   111,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   131,   132,   133,    19,
      -1,    21,    22,    23,    24,   140,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    68,    -1,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,
     100,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   131,   132,   133,    19,    -1,    21,    22,    23,    24,
     140,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    68,    -1,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,
      -1,    96,    97,    -1,    99,   100,   101,    -1,   103,   104,
     105,   106,   107,   108,    -1,    -1,   111,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   131,   132,   133,    -1,
      -1,    -1,    -1,    -1,    -1,   140,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    -1,    -1,    81,    82,    -1,    -1,    85,    86,
      87,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,    -1,   132,   133,    -1,    -1,    -1,
      -1,    -1,   139,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    -1,
      -1,    81,    82,    -1,    -1,    85,    86,    87,    88,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,    -1,   132,   133,    -1,    -1,    -1,    -1,    -1,   139,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    -1,    -1,    81,    82,
      -1,    -1,    85,    86,    87,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
     103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,    -1,   132,
     133,    -1,    -1,    -1,    -1,    -1,   139,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,    85,
      86,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,    -1,   132,   133,    -1,    -1,
      -1,    -1,    -1,   139,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    -1,    67,    68,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,
      -1,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
      -1,    -1,   111,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    19,   131,    21,    22,    23,    24,    -1,    -1,   138,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    -1,    67,
      68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,
      -1,    -1,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,    -1,    -1,   111,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    19,   131,    21,    22,    23,    24,    -1,    -1,
     138,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    68,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    -1,   100,   101,    -1,   103,   104,   105,   106,
     107,   108,    -1,    -1,   111,   112,    -1,    -1,   115,   116,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    -1,   131,   132,   133,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    -1,    67,    68,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,    -1,    -1,   111,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   131,
     132,   133,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    68,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    -1,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,    -1,    -1,   111,   112,    -1,    -1,   115,   116,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   131,   132,   133,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    68,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    99,   100,   101,
      -1,   103,   104,   105,   106,   107,   108,    -1,    -1,   111,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    -1,   131,
     132,   133,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    -1,
      67,    68,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,    -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   131,   132,   133,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    68,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    99,   100,   101,
      -1,   103,   104,   105,   106,   107,   108,    -1,    -1,   111,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   131,
     132,   133,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    68,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,    -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   131,   132,   133,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    68,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,    -1,    -1,   111,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   131,
     132,   133,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    68,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    -1,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,    -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   131,   132,   133,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    -1,    67,    68,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,    -1,    -1,   111,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   131,
     132,   133,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    -1,
      67,    68,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,    -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   131,   132,   133,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    -1,    67,    68,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,    -1,    -1,   111,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   131,
     132,   133,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    -1,
      67,    68,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,    -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   131,   132,   133,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    -1,    67,    68,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,    -1,    -1,   111,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   131,
     132,   133,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    -1,
      67,    68,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    -1,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,    -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   131,   132,   133,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    -1,    67,    68,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    -1,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,    -1,    -1,   111,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   131,
     132,   133,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    -1,
      67,    68,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    -1,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,    -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   131,   132,   133,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    -1,    67,    68,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    -1,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,    -1,    -1,   111,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   131,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    -1,    67,    68,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,    -1,    -1,
     111,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
     131,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    -1,    67,    68,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,    -1,
      -1,   111,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      19,   131,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    -1,    67,    68,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,
      -1,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
      -1,    -1,   111,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    19,   131,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    -1,    67,
      68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,
      -1,    -1,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,    -1,    -1,   111,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    19,   131,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    -1,
      67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    -1,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,    -1,    -1,   111,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,   131,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,
      96,    97,    51,    52,    -1,   101,    55,   103,   104,   105,
     106,   107,   108,    -1,    -1,   111,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      -1,    -1,    81,    82,    -1,   131,    85,    86,    87,    88,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    -1,   132,   133,    51,    52,    -1,    -1,    55,
     139,   140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,    85,
      86,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,    -1,   132,   133,    51,    52,
      -1,    -1,    55,   139,   140,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    -1,    -1,    81,    82,
      -1,    -1,    85,    86,    87,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,    -1,   132,
     133,    51,    52,    -1,    -1,    55,   139,   140,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    -1,
      -1,    81,    82,    -1,    -1,    85,    86,    87,    88,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,    -1,   132,   133,    51,    52,    -1,    -1,    55,   139,
     140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    -1,    -1,    81,    82,    -1,    -1,    85,    86,
      87,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,    -1,   132,   133,    51,    52,    -1,
      -1,    55,   139,   140,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    -1,    -1,    81,    82,    -1,
      -1,    85,    86,    87,    88,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   130,    -1,   132,   133,
      51,    52,    -1,    -1,    55,   139,   140,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    -1,    -1,
      81,    82,    -1,    -1,    85,    86,    87,    88,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
      -1,   132,   133,    51,    52,    -1,    -1,    55,   139,   140,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    -1,    -1,    81,    82,    -1,    -1,    85,    86,    87,
      88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,    -1,   132,   133,    51,    52,    -1,    -1,
      55,   139,   140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,
      85,    86,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,    -1,   132,   133,    51,
      52,    -1,    -1,    55,   139,   140,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    -1,    -1,    81,
      82,    -1,    -1,    85,    86,    87,    88,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,    -1,
     132,   133,    51,    52,    -1,    -1,    55,   139,   140,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      -1,    -1,    81,    82,    -1,    -1,    85,    86,    87,    88,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    -1,   132,   133,    51,    52,    -1,    -1,    55,
     139,   140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,    85,
      86,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,    -1,   132,   133,    51,    52,
      -1,    -1,    55,   139,   140,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    -1,    -1,    81,    82,
      -1,    -1,    85,    86,    87,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,    -1,   132,
     133,    51,    52,    -1,    -1,    55,   139,   140,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    -1,
      -1,    81,    82,    -1,    -1,    85,    86,    87,    88,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,    -1,   132,   133,    51,    52,    -1,    -1,    55,   139,
     140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    -1,    -1,    81,    82,    -1,    -1,    85,    86,
      87,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,    -1,   132,   133,    51,    52,    -1,
      -1,    55,   139,   140,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    -1,    -1,    81,    82,    -1,
      -1,    85,    86,    87,    88,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   130,    -1,   132,   133,
      -1,    -1,    -1,    -1,    -1,   139,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    87,    88,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,    87,    88,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   119,    -1,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,    -1,    -1,    -1,    -1,    -1,
     119,    -1,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,   145,    87,    88,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    88,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,
      -1,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,    -1,    -1,    -1,    -1,    -1,    -1,   119,   138,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   138,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      -1,    -1,    87,    88,    -1,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    -1,    -1,
      87,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   119,    -1,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,    -1,    -1,    -1,    -1,
      -1,    -1,   119,   138,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   138,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    87,    88,    -1,
      -1,    -1,    92,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,    87,    88,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,
      -1,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     119,    -1,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    87,    88,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   147,   148,     0,     1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    19,    21,    22,    23,    24,
      30,    31,    32,    33,    34,    35,    36,    39,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      58,    59,    60,    63,    67,    68,    70,    71,    90,    93,
      94,    96,    97,    99,   101,   103,   104,   105,   106,   107,
     108,   111,   131,   132,   133,   149,   150,   151,   156,   158,
     160,   162,   163,   166,   167,   169,   170,   171,   173,   174,
     183,   197,   218,   237,   238,   248,   249,   253,   254,   255,
     261,   262,   263,   265,   266,   267,   268,   269,   270,   294,
     308,   151,    21,    22,    30,    31,    32,    39,    51,    55,
      87,    90,    93,   131,   175,   176,   197,   218,   267,   270,
     294,   176,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    45,    46,    47,    48,
      49,    50,    51,    52,    55,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    81,    82,    85,    86,    87,    88,
      99,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   132,   133,   139,   140,   177,   181,   182,   269,   289,
     198,    90,   160,   161,   174,   218,   267,   268,   270,   161,
     204,   206,    90,   167,   174,   218,   223,   267,   270,    33,
      34,    35,    36,    48,    49,    50,    51,    55,   103,   177,
     178,   179,   263,   112,   115,   116,   143,   145,   161,   257,
     258,   259,   300,   305,   306,   307,    57,    66,    99,   100,
     107,   132,   166,   183,   189,   192,   195,   292,   293,   189,
     189,   140,   186,   187,   190,   191,   308,   186,   190,   140,
     301,   306,   178,   152,   135,   183,   218,   183,    55,     1,
      93,   154,   155,   156,   168,   169,   308,   199,   201,   184,
     195,   292,   308,   183,   291,   292,   308,    90,   138,   173,
     218,   267,   270,   202,    53,    54,    56,    63,   107,   177,
     264,    62,    64,    65,   113,   114,   250,   251,    63,   250,
      63,   250,    63,   250,    61,   250,    58,    59,   162,   183,
     183,   300,   307,    40,    41,    42,    43,    44,    37,    38,
      28,   235,   118,   138,    93,    99,   170,   118,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    87,    88,   119,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,    89,   102,   137,   144,   298,    89,
     298,   299,    26,   135,   239,    91,    91,   186,   190,   239,
     160,    51,    55,   175,    58,    59,   122,   271,    89,   137,
     298,   213,   290,   214,    89,   144,   297,   153,   154,    55,
      16,   219,   305,   118,    89,   137,   298,    91,    91,   219,
     161,   161,    55,    89,   137,   298,    25,   107,   138,   260,
     300,   112,   259,    20,   242,   305,   183,   183,   183,   183,
      66,   250,    92,   138,   193,   194,   308,   138,   193,   194,
     188,   189,   195,   292,   308,   189,   160,   301,   302,   160,
     157,   135,   154,    89,   298,    91,   156,   168,   141,   300,
     307,   302,   156,   302,   142,   194,   304,   306,   194,   304,
     136,   304,    55,   170,   171,   172,   138,    89,   137,   298,
      51,    53,    54,    55,    56,    93,    99,   100,   125,   128,
     140,   233,   274,   275,   276,   277,   278,   279,   280,   283,
     284,   285,   286,   287,    63,   250,   252,   256,   257,    62,
     251,    63,    63,    63,    61,    72,    72,   151,   161,   161,
     161,   161,   156,   160,   160,   236,    99,   162,   183,   195,
     196,   168,   138,   173,   138,   158,   159,   162,   174,   183,
     185,   196,   218,   270,   183,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
      51,    52,    55,   181,   186,   295,   296,   188,    51,    52,
      55,   181,   186,   295,    51,    55,   295,   241,   240,   159,
     183,   185,   159,   185,    98,   164,   211,   272,   210,    51,
      55,   175,   295,   188,   295,   153,   160,   215,   216,    15,
      13,   244,   308,   154,    16,    51,    55,   188,    51,    55,
     154,    27,   220,   305,   220,    51,    55,   188,    51,    55,
     208,   180,   154,   242,   183,   195,    15,   183,    66,   183,
     256,    99,   183,   192,   292,   293,   302,   138,   194,   138,
     302,   141,   178,   149,   136,   185,   302,   156,   200,   292,
     170,   172,    51,    55,   188,    51,    55,   118,    51,    93,
      99,   224,   225,   226,   276,   274,   203,   138,   288,   308,
     183,   138,   288,    51,   138,   288,    51,    63,   154,   257,
     183,   183,    80,   123,   228,   229,   308,   183,   194,   302,
     172,   138,    44,   118,    44,    89,   137,   298,   301,    91,
      91,   186,   190,   301,   303,    91,    91,   187,   190,   187,
     190,   228,   228,   165,   305,   161,   153,   303,    15,   302,
     140,   273,   274,   177,   183,   196,   245,   308,    18,   222,
     308,    17,   221,   222,    91,    91,   303,    91,    91,   222,
     205,   207,   303,   161,   178,   136,    15,   194,   219,   183,
     183,   193,   292,   136,   302,   304,   303,   226,   138,   276,
     138,   302,   230,   301,    29,   110,   234,   277,   283,   285,
     287,   278,   280,   285,   278,   136,   227,   230,   278,   279,
     281,   282,   285,   287,   154,    99,   183,   172,   156,   183,
      51,    55,   188,    51,    55,   120,   159,   185,   162,   185,
     164,   142,    91,   159,   185,   159,   185,   164,   239,   235,
     154,   154,   228,   212,   305,    15,   274,   153,   305,   217,
      92,   246,   308,   154,    14,   247,   308,   161,    15,    91,
      15,   154,   154,   220,   183,   154,   138,   302,   225,   138,
      99,   224,   141,   143,   154,   154,   138,   288,   138,   288,
     138,   288,   138,   288,   288,   230,   123,   138,   288,    90,
     218,   138,   288,   138,   288,    15,   183,   303,   183,   159,
     185,    15,   136,   154,   153,   302,    15,   273,    90,   174,
     218,   267,   270,   219,   154,   219,    15,    15,   209,   222,
     242,   243,   138,   225,   138,   276,    51,   231,   232,   275,
      15,   136,   278,   285,   278,   278,   123,   282,   285,    55,
      89,   278,   281,   285,   278,   136,    15,   153,    55,    89,
     137,   298,   154,   154,   154,   225,   138,   138,   301,   288,
     138,   288,   288,   288,   138,   288,   138,   288,    51,    55,
     288,   138,   288,   288,    15,    51,    55,   188,    51,    55,
     244,   221,    15,   225,   232,   278,   278,   285,   278,   278,
     303,   288,   288,   138,   288,   288,   288,   278,   288
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   146,   148,   147,   149,   150,   150,   150,   150,   151,
     152,   151,   153,   154,   155,   155,   155,   155,   157,   156,
     156,   156,   156,   156,   156,   156,   156,   156,   156,   156,
     156,   156,   156,   158,   158,   158,   158,   158,   158,   158,
     158,   159,   159,   159,   160,   160,   160,   160,   160,   160,
     161,   162,   162,   163,   163,   165,   164,   166,   166,   166,
     166,   166,   166,   166,   166,   166,   166,   166,   167,   167,
     168,   168,   169,   169,   169,   169,   169,   169,   169,   169,
     169,   169,   170,   170,   171,   171,   172,   172,   173,   173,
     173,   173,   173,   173,   173,   173,   174,   174,   174,   174,
     174,   174,   174,   174,   175,   175,   176,   176,   176,   177,
     177,   177,   177,   177,   178,   178,   179,   180,   179,   181,
     181,   181,   181,   181,   181,   181,   181,   181,   181,   181,
     181,   181,   181,   181,   181,   181,   181,   181,   181,   181,
     181,   181,   181,   181,   181,   181,   181,   181,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
     184,   184,   184,   184,   185,   185,   186,   187,   187,   188,
     188,   188,   188,   188,   189,   189,   189,   189,   189,   191,
     190,   192,   193,   193,   194,   194,   195,   195,   195,   195,
     196,   196,   196,   197,   197,   197,   197,   197,   197,   197,
     197,   198,   197,   199,   200,   197,   201,   197,   197,   197,
     197,   197,   197,   197,   197,   197,   197,   197,   197,   197,
     202,   203,   197,   197,   197,   204,   205,   197,   206,   207,
     197,   197,   197,   208,   209,   197,   210,   197,   211,   212,
     197,   213,   197,   214,   215,   197,   216,   217,   197,   197,
     197,   197,   197,   218,   219,   219,   219,   220,   220,   221,
     221,   222,   222,   223,   223,   224,   224,   225,   225,   226,
     226,   226,   226,   226,   226,   226,   226,   226,   227,   227,
     227,   227,   227,   227,   227,   227,   227,   227,   227,   227,
     227,   227,   227,   228,   228,   229,   229,   229,   230,   230,
     231,   231,   232,   232,   233,   233,   234,   234,   236,   235,
     237,   237,   237,   237,   238,   238,   238,   238,   238,   238,
     238,   238,   238,   240,   239,   241,   239,   242,   243,   243,
     244,   244,   245,   245,   245,   246,   246,   247,   247,   248,
     248,   248,   248,   249,   249,   249,   249,   250,   250,   251,
     252,   251,   251,   251,   253,   253,   254,   254,   255,   256,
     256,   257,   257,   258,   258,   259,   260,   259,   261,   261,
     262,   262,   263,   264,   264,   264,   264,   264,   264,   265,
     265,   266,   266,   266,   266,   267,   267,   267,   267,   267,
     268,   269,   269,   269,   269,   269,   269,   269,   269,   270,
     270,   271,   272,   271,   273,   273,   274,   274,   274,   274,
     274,   274,   274,   274,   274,   274,   274,   274,   274,   274,
     274,   275,   275,   275,   275,   276,   276,   277,   277,   278,
     278,   279,   280,   281,   282,   282,   283,   283,   284,   284,
     285,   285,   286,   286,   287,   288,   288,   289,   290,   289,
     291,   291,   292,   292,   293,   293,   293,   293,   293,   294,
     294,   294,   295,   295,   295,   295,   296,   296,   296,   297,
     297,   298,   298,   299,   299,   300,   300,   301,   301,   302,
     303,   304,   304,   304,   305,   305,   305,   306,   307,   307,
     308
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     3,     2,     1,
       0,     5,     4,     2,     1,     1,     3,     2,     0,     4,
       2,     3,     3,     3,     3,     3,     4,     1,     3,     3,
       3,     3,     1,     3,     3,     6,     5,     5,     5,     5,
       3,     1,     3,     1,     1,     3,     3,     3,     2,     1,
       1,     1,     1,     1,     4,     0,     5,     2,     3,     4,
       5,     4,     5,     2,     2,     2,     2,     2,     1,     3,
       1,     3,     1,     2,     3,     5,     2,     4,     2,     4,
       1,     3,     1,     3,     2,     3,     1,     2,     1,     4,
       3,     3,     3,     3,     2,     1,     1,     4,     3,     3,
       3,     3,     2,     1,     1,     1,     2,     1,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     4,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     3,
       6,     5,     5,     5,     5,     4,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     4,     4,     2,     2,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     2,     3,     3,     3,     3,     6,     1,
       1,     2,     4,     2,     1,     3,     3,     1,     1,     1,
       1,     2,     4,     2,     1,     2,     2,     4,     1,     0,
       2,     2,     2,     1,     1,     2,     1,     2,     3,     4,
       3,     4,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     4,     0,     0,     5,     0,     3,     3,     3,
       2,     3,     3,     1,     2,     4,     3,     2,     1,     2,
       0,     0,     5,     6,     6,     0,     0,     7,     0,     0,
       7,     5,     4,     0,     0,     9,     0,     6,     0,     0,
       8,     0,     5,     0,     0,     7,     0,     0,     9,     1,
       1,     1,     1,     1,     1,     1,     2,     1,     1,     1,
       5,     1,     2,     1,     1,     1,     3,     1,     3,     1,
       4,     6,     3,     5,     2,     4,     1,     3,     6,     8,
       4,     6,     4,     2,     6,     2,     4,     6,     2,     4,
       2,     4,     1,     1,     1,     3,     1,     4,     1,     4,
       1,     3,     1,     1,     4,     1,     3,     3,     0,     5,
       2,     4,     5,     5,     2,     4,     4,     3,     3,     3,
       2,     1,     4,     0,     5,     0,     5,     5,     1,     1,
       6,     1,     1,     1,     1,     2,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     2,     3,     1,     2,     1,
       0,     4,     1,     2,     2,     3,     2,     3,     1,     1,
       2,     1,     2,     1,     2,     1,     0,     4,     2,     3,
       1,     4,     2,     1,     1,     1,     1,     1,     2,     2,
       3,     1,     1,     2,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     0,     4,     3,     2,     6,     8,     4,     6,
       4,     6,     2,     4,     6,     2,     4,     2,     4,     1,
       0,     1,     1,     1,     1,     1,     1,     1,     3,     1,
       3,     2,     2,     2,     1,     3,     1,     3,     1,     1,
       2,     1,     1,     1,     2,     2,     1,     1,     0,     4,
       1,     2,     1,     3,     3,     2,     2,     3,     4,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     1,     0,     1,     2,
       2,     0,     1,     1,     1,     1,     1,     1,     1,     2,
       0
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (p, YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, p); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, parser_state *p)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  YYUSE (p);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, parser_state *p)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep, p);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule, parser_state *p)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              , p);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule, p); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, parser_state *p)
{
  YYUSE (yyvaluep);
  YYUSE (p);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/*----------.
| yyparse.  |
`----------*/

int
yyparse (parser_state *p)
{
/* The lookahead symbol.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

    /* Number of syntax errors so far.  */
    int yynerrs;

    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex (&yylval, p);
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 1199 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->lstate = EXPR_BEG;
                      if (!p->locals) p->locals = cons(0,0);
                    }
#line 5270 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 3:
#line 1204 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->tree = new_scope(p, (yyvsp[0].nd));
                      NODE_LINENO(p->tree, (yyvsp[0].nd));
                    }
#line 5279 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 1211 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5287 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 1217 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5295 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 1221 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 5304 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 1226 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 5312 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 1230 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5320 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 1237 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = local_switch(p);
                    }
#line 5328 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 1241 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "BEGIN not supported");
                      local_resume(p, (yyvsp[-3].nd));
                      (yyval.nd) = 0;
                    }
#line 5338 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 12:
#line 1252 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      if ((yyvsp[-2].nd)) {
                        (yyval.nd) = new_rescue(p, (yyvsp[-3].nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                        NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                      }
                      else if ((yyvsp[-1].nd)) {
                        yywarn(p, "else without rescue is useless");
                        (yyval.nd) = push((yyvsp[-3].nd), (yyvsp[-1].nd));
                      }
                      else {
                        (yyval.nd) = (yyvsp[-3].nd);
                      }
                      if ((yyvsp[0].nd)) {
                        if ((yyval.nd)) {
                          (yyval.nd) = new_ensure(p, (yyval.nd), (yyvsp[0].nd));
                        }
                        else {
                          (yyval.nd) = push((yyvsp[0].nd), new_nil(p));
                        }
                      }
                    }
#line 5364 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 13:
#line 1276 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5372 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 1282 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5380 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 1286 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 5389 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 1291 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 5397 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 1295 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                    }
#line 5405 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 1300 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {p->lstate = EXPR_FNAME;}
#line 5411 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 1301 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_alias(p, (yyvsp[-2].id), (yyvsp[0].id));
                    }
#line 5419 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 1305 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 5427 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 1309 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 5435 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 1313 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 5443 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 1317 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_while(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 5451 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 1321 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_until(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 5459 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 1325 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5467 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 1329 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "END not supported");
                      (yyval.nd) = new_postexe(p, (yyvsp[-1].nd));
                    }
#line 5476 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 1335 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5484 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 1339 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 5492 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 1343 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5500 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 1347 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 5508 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 1354 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5516 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 1358 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 5524 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 1362 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern("[]",2), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 5532 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 1366 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 5540 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 1370 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 5548 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 1374 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = 0;
                    }
#line 5557 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 1379 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 5565 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 1383 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5574 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 1391 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5582 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 1400 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5590 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 1404 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5598 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 1408 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 5606 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 1412 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 5614 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 1419 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      if (!(yyvsp[0].nd)) (yyval.nd) = new_nil(p);
                      else {
                        (yyval.nd) = (yyvsp[0].nd);
                      }
                    }
#line 5625 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 1433 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 5633 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 1439 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      local_nest(p);
                    }
#line 5641 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 1445 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_block(p, (yyvsp[-2].nd), (yyvsp[-1].nd));
                      local_unnest(p);
                    }
#line 5650 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 1452 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 5658 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 1456 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[-2].id), (yyvsp[-1].nd));
                    }
#line 5667 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 1461 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 5675 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 1465 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                   }
#line 5684 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 1470 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 5692 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 1474 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), tCOLON2);
                    }
#line 5701 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 1479 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 5709 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 1483 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 5717 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 1487 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 5725 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 1491 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 5733 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 1495 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 5741 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 1501 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 5749 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 1505 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5757 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 1512 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5765 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 1518 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 5773 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 1522 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1(push((yyvsp[-1].nd),(yyvsp[0].nd)));
                    }
#line 5781 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 1526 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list2((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5789 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 1530 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5797 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 1534 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list2((yyvsp[-1].nd), new_nil(p));
                    }
#line 5805 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 77:
#line 1538 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_nil(p), (yyvsp[0].nd));
                    }
#line 5813 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 1542 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list2(0, (yyvsp[0].nd));
                    }
#line 5821 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 79:
#line 1546 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3(0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5829 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 80:
#line 1550 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list2(0, new_nil(p));
                    }
#line 5837 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 81:
#line 1554 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[0].nd));
                    }
#line 5845 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 1561 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), NULL);
                    }
#line 5853 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 1567 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 5861 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 85:
#line 1571 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 5869 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 86:
#line 1577 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 5877 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 87:
#line 1581 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 5885 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 88:
#line 1587 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 5893 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 89:
#line 1591 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern("[]",2), (yyvsp[-1].nd), '.');
                    }
#line 5901 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 90:
#line 1595 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 5909 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 91:
#line 1599 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 5917 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 92:
#line 1603 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 5925 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 93:
#line 1607 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 5935 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 94:
#line 1613 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 5945 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 95:
#line 1619 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 5954 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 96:
#line 1626 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 5962 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 97:
#line 1630 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern("[]",2), (yyvsp[-1].nd), '.');
                    }
#line 5970 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 98:
#line 1634 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 5978 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 99:
#line 1638 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 5986 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 100:
#line 1642 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 5994 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 101:
#line 1646 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6004 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 102:
#line 1652 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6014 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 103:
#line 1658 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 6023 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 104:
#line 1665 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "class/module name must be CONSTANT");
                    }
#line 6031 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 106:
#line 1672 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons((node*)1, nsym((yyvsp[0].id)));
                    }
#line 6039 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 107:
#line 1676 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons((node*)0, nsym((yyvsp[0].id)));
                    }
#line 6047 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 108:
#line 1680 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), nsym((yyvsp[0].id)));
                    }
#line 6056 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 112:
#line 1690 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6065 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 113:
#line 1695 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6074 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 116:
#line 1706 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_undef(p, (yyvsp[0].id));
                    }
#line 6082 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 117:
#line 1709 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {p->lstate = EXPR_FNAME;}
#line 6088 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 118:
#line 1710 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-3].nd), nsym((yyvsp[0].id)));
                    }
#line 6096 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 119:
#line 1715 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('|');   }
#line 6102 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 120:
#line 1716 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('^');   }
#line 6108 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 121:
#line 1717 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('&');   }
#line 6114 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 122:
#line 1718 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("<=>",3); }
#line 6120 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 123:
#line 1719 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("==",2);  }
#line 6126 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 124:
#line 1720 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("===",3); }
#line 6132 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 125:
#line 1721 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("=~",2);  }
#line 6138 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 126:
#line 1722 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("!~",2);  }
#line 6144 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 127:
#line 1723 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('>');   }
#line 6150 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 128:
#line 1724 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern(">=",2);  }
#line 6156 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 129:
#line 1725 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('<');   }
#line 6162 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 130:
#line 1726 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("<=",2);  }
#line 6168 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 131:
#line 1727 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("!=",2);  }
#line 6174 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 132:
#line 1728 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("<<",2);  }
#line 6180 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 133:
#line 1729 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern(">>",2);  }
#line 6186 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 134:
#line 1730 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('+');   }
#line 6192 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 135:
#line 1731 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('-');   }
#line 6198 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 136:
#line 1732 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('*');   }
#line 6204 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 137:
#line 1733 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('*');   }
#line 6210 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 138:
#line 1734 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('/');   }
#line 6216 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 139:
#line 1735 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('%');   }
#line 6222 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 140:
#line 1736 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("**",2);  }
#line 6228 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 141:
#line 1737 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('!');   }
#line 6234 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 142:
#line 1738 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('~');   }
#line 6240 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 143:
#line 1739 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("+@",2);  }
#line 6246 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 144:
#line 1740 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("-@",2);  }
#line 6252 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 145:
#line 1741 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("[]",2);  }
#line 6258 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 146:
#line 1742 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern("[]=",3); }
#line 6264 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 147:
#line 1743 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    { (yyval.id) = intern_c('`');   }
#line 6270 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 188:
#line 1761 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6278 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 189:
#line 1765 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6286 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 190:
#line 1769 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern("[]",2), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6294 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 191:
#line 1773 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6302 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 192:
#line 1777 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6310 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 193:
#line 1781 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6318 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 194:
#line 1785 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6327 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 195:
#line 1790 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6336 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 196:
#line 1795 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6345 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 197:
#line 1800 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6353 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 198:
#line 1804 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6361 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 199:
#line 1808 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "+", (yyvsp[0].nd));
                    }
#line 6369 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 200:
#line 1812 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "-", (yyvsp[0].nd));
                    }
#line 6377 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 201:
#line 1816 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "*", (yyvsp[0].nd));
                    }
#line 6385 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 202:
#line 1820 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "/", (yyvsp[0].nd));
                    }
#line 6393 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 203:
#line 1824 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "%", (yyvsp[0].nd));
                    }
#line 6401 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 204:
#line 1828 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd));
                    }
#line 6409 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 205:
#line 1832 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)), "-@");
                    }
#line 6417 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 206:
#line 1836 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)), "-@");
                    }
#line 6425 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 207:
#line 1840 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "+@");
                    }
#line 6433 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 208:
#line 1844 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "-@");
                    }
#line 6441 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 209:
#line 1848 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "|", (yyvsp[0].nd));
                    }
#line 6449 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 210:
#line 1852 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "^", (yyvsp[0].nd));
                    }
#line 6457 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 211:
#line 1856 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "&", (yyvsp[0].nd));
                    }
#line 6465 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 212:
#line 1860 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=>", (yyvsp[0].nd));
                    }
#line 6473 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 213:
#line 1864 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">", (yyvsp[0].nd));
                    }
#line 6481 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 214:
#line 1868 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">=", (yyvsp[0].nd));
                    }
#line 6489 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 215:
#line 1872 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<", (yyvsp[0].nd));
                    }
#line 6497 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 216:
#line 1876 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=", (yyvsp[0].nd));
                    }
#line 6505 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 217:
#line 1880 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "==", (yyvsp[0].nd));
                    }
#line 6513 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 218:
#line 1884 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "===", (yyvsp[0].nd));
                    }
#line 6521 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 219:
#line 1888 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!=", (yyvsp[0].nd));
                    }
#line 6529 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 220:
#line 1892 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "=~", (yyvsp[0].nd));
                    }
#line 6537 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 221:
#line 1896 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!~", (yyvsp[0].nd));
                    }
#line 6545 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 222:
#line 1900 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6553 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 223:
#line 1904 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "~");
                    }
#line 6561 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 224:
#line 1908 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<<", (yyvsp[0].nd));
                    }
#line 6569 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 225:
#line 1912 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">>", (yyvsp[0].nd));
                    }
#line 6577 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 226:
#line 1916 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6585 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 227:
#line 1920 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6593 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 228:
#line 1924 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 6601 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 229:
#line 1928 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6609 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 231:
#line 1935 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6618 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 232:
#line 1940 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_hash(p, (yyvsp[-1].nd)));
                    }
#line 6626 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 233:
#line 1944 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(new_hash(p, (yyvsp[-1].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6635 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 234:
#line 1951 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6643 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 235:
#line 1955 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6653 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 236:
#line 1963 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6661 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 241:
#line 1975 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons((yyvsp[-1].nd),0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6670 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 242:
#line 1980 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 6679 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 243:
#line 1985 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(list1(new_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6688 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 244:
#line 1992 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(list1((yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6698 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 245:
#line 1998 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons((yyvsp[-1].nd), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6707 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 246:
#line 2003 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(list1(new_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6716 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 247:
#line 2008 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 6725 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 248:
#line 2013 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(0, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6734 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 249:
#line 2019 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    }
#line 6743 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 250:
#line 2024 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6752 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 251:
#line 2031 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[0].nd));
                    }
#line 6760 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 252:
#line 2037 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6768 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 253:
#line 2041 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = 0;
                    }
#line 6776 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 256:
#line 2051 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[0].nd), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6786 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 257:
#line 2057 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_splat(p, (yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6796 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 258:
#line 2063 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6805 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 259:
#line 2068 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 6814 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 260:
#line 2075 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6823 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 261:
#line 2080 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 6832 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 262:
#line 2085 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 6841 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 270:
#line 2099 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[0].id), 0);
                    }
#line 6849 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 271:
#line 2103 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 6858 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 272:
#line 2109 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->cmdarg_stack = (yyvsp[-2].stack);
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6867 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 273:
#line 2114 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 6876 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 274:
#line 2118 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {p->lstate = EXPR_ENDARG;}
#line 6882 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 275:
#line 2119 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->cmdarg_stack = (yyvsp[-3].stack);
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 6891 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 276:
#line 2123 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {p->lstate = EXPR_ENDARG;}
#line 6897 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 277:
#line 2124 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_nil(p);
                    }
#line 6905 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 278:
#line 2128 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6913 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 279:
#line 2132 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6921 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 280:
#line 2136 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6929 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 281:
#line 2140 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_array(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6938 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 282:
#line 2145 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_hash(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 6947 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 283:
#line 2150 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_return(p, 0);
                    }
#line 6955 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 284:
#line 2154 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 6963 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 285:
#line 2158 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[-1].nd)), "!");
                    }
#line 6971 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 286:
#line 2162 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    }
#line 6979 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 287:
#line 2166 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), cons(0, (yyvsp[0].nd)));
                    }
#line 6987 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 289:
#line 2171 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6996 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 290:
#line 2176 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      local_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    }
#line 7006 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 291:
#line 2182 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7015 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 292:
#line 2187 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->lpar_beg = (yyvsp[-3].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                      local_unnest(p);
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      CMDARG_LEXPOP();
                    }
#line 7027 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 293:
#line 2198 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 7036 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 294:
#line 2206 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 7045 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 295:
#line 2210 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {COND_PUSH(1);}
#line 7051 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 296:
#line 2210 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {COND_POP();}
#line 7057 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 297:
#line 2213 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_while(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 7066 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 298:
#line 2217 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {COND_PUSH(1);}
#line 7072 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 299:
#line 2217 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {COND_POP();}
#line 7078 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 300:
#line 2220 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_until(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 7087 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 301:
#line 2227 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_case(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 7095 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 302:
#line 2231 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[-1].nd));
                    }
#line 7103 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 303:
#line 2235 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {COND_PUSH(1);}
#line 7109 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 304:
#line 2237 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {COND_POP();}
#line 7115 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 305:
#line 2240 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_for(p, (yyvsp[-7].nd), (yyvsp[-4].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                    }
#line 7124 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 306:
#line 2246 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                    }
#line 7134 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 307:
#line 2253 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_class(p, (yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                      local_resume(p, (yyvsp[-2].nd));
                    }
#line 7144 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 308:
#line 2260 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    }
#line 7153 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 309:
#line 2265 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(local_switch(p), nint(p->in_single));
                      p->in_single = 0;
                    }
#line 7162 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 310:
#line 2271 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_sclass(p, (yyvsp[-5].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-7].num));
                      local_resume(p, (yyvsp[-2].nd)->car);
                      p->in_def = (yyvsp[-4].num);
                      p->in_single = intn((yyvsp[-2].nd)->cdr);
                    }
#line 7174 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 311:
#line 2280 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                    }
#line 7184 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 312:
#line 2287 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_module(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-4].num));
                      local_resume(p, (yyvsp[-2].nd));
                    }
#line 7194 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 313:
#line 2293 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7203 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 314:
#line 2297 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->in_def++;
                      (yyval.nd) = local_switch(p);
                    }
#line 7212 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 315:
#line 2304 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_def(p, (yyvsp[-5].id), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                      local_resume(p, (yyvsp[-3].nd));
                      p->in_def--;
                      p->cmdarg_stack = (yyvsp[-4].stack);
                    }
#line 7224 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 316:
#line 2312 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->lstate = EXPR_FNAME;
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7234 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 317:
#line 2318 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->in_single++;
                      p->lstate = EXPR_ENDFN; /* force for args */
                      (yyval.nd) = local_switch(p);
                    }
#line 7244 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 318:
#line 2326 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_sdef(p, (yyvsp[-7].nd), (yyvsp[-4].id), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                      local_resume(p, (yyvsp[-3].nd));
                      p->in_single--;
                      p->cmdarg_stack = (yyvsp[-5].stack);
                    }
#line 7256 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 319:
#line 2334 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_break(p, 0);
                    }
#line 7264 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 320:
#line 2338 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_next(p, 0);
                    }
#line 7272 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 321:
#line 2342 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_redo(p);
                    }
#line 7280 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 322:
#line 2346 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_retry(p);
                    }
#line 7288 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 323:
#line 2352 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 7297 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 330:
#line 2371 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-3].nd)), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 7305 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 332:
#line 2378 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7313 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 333:
#line 2384 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1(list1((yyvsp[0].nd)));
                    }
#line 7321 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 335:
#line 2391 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_arg(p, (yyvsp[0].id));
                    }
#line 7329 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 336:
#line 2395 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), 0);
                    }
#line 7337 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 337:
#line 2401 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 7345 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 338:
#line 2405 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7353 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 339:
#line 2411 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3((yyvsp[0].nd),0,0);
                    }
#line 7361 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 340:
#line 2415 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 7369 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 341:
#line 2419 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3((yyvsp[-5].nd), new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 7377 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 342:
#line 2423 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3((yyvsp[-2].nd), (node*)-1, 0);
                    }
#line 7385 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 343:
#line 2427 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (node*)-1, (yyvsp[0].nd));
                    }
#line 7393 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 344:
#line 2431 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 7401 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 345:
#line 2435 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 7409 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 346:
#line 2439 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3(0, (node*)-1, 0);
                    }
#line 7417 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 347:
#line 2443 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list3(0, (node*)-1, (yyvsp[0].nd));
                    }
#line 7425 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 348:
#line 2449 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 7433 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 349:
#line 2453 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 7441 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 350:
#line 2457 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].id));
                    }
#line 7449 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 351:
#line 2461 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 7457 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 352:
#line 2465 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 7465 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 353:
#line 2469 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, 0);
                    }
#line 7473 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 354:
#line 2473 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 7481 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 355:
#line 2477 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].id));
                    }
#line 7489 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 356:
#line 2481 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 7497 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 357:
#line 2485 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 7505 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 358:
#line 2489 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].id));
                    }
#line 7513 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 359:
#line 2493 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 7521 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 360:
#line 2497 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 7529 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 361:
#line 2501 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 7537 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 362:
#line 2505 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].id));
                    }
#line 7545 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 364:
#line 2512 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7554 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 365:
#line 2519 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = 0;
                    }
#line 7562 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 366:
#line 2523 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = 0;
                    }
#line 7570 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 367:
#line 2527 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 7578 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 368:
#line 2534 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = 0;
                    }
#line 7586 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 369:
#line 2538 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = 0;
                    }
#line 7594 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 372:
#line 2548 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, (yyvsp[0].id));
                      new_bv(p, (yyvsp[0].id));
                    }
#line 7603 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 374:
#line 2556 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 7611 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 375:
#line 2560 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7619 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 376:
#line 2566 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7627 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 377:
#line 2570 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7635 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 378:
#line 2576 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      local_nest(p);
                    }
#line 7643 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 379:
#line 2582 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      local_unnest(p);
                    }
#line 7652 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 380:
#line 2589 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      if ((yyvsp[-1].nd)->car == (node*)NODE_YIELD) {
                        yyerror(p, "block given to yield");
                      }
                      else {
                        call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      }
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7666 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 381:
#line 2599 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 7674 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 382:
#line 2603 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 7683 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 383:
#line 2608 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 7692 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 384:
#line 2615 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7700 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 385:
#line 2619 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 7708 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 386:
#line 2623 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 7716 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 387:
#line 2627 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 7724 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 388:
#line 2631 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), intern("call",4), (yyvsp[0].nd), (yyvsp[-1].num));
                    }
#line 7732 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 389:
#line 2635 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), intern("call",4), (yyvsp[0].nd), tCOLON2);
                    }
#line 7740 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 390:
#line 2639 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 7748 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 391:
#line 2643 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_zsuper(p);
                    }
#line 7756 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 392:
#line 2647 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern("[]",2), (yyvsp[-1].nd), '.');
                    }
#line 7764 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 393:
#line 2653 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      local_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 7773 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 394:
#line 2659 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                    }
#line 7783 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 395:
#line 2665 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      local_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 7792 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 396:
#line 2671 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                    }
#line 7802 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 397:
#line 2681 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = cons(cons((yyvsp[-3].nd), (yyvsp[-1].nd)), (yyvsp[0].nd));
                    }
#line 7810 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 398:
#line 2687 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      if ((yyvsp[0].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[0].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    }
#line 7823 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 400:
#line 2701 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1(list3((yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd)));
                      if ((yyvsp[0].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[0].nd));
                    }
#line 7832 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 402:
#line 2709 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                        (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 7840 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 405:
#line 2717 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7848 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 407:
#line 2724 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7856 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 415:
#line 2739 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7864 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 416:
#line 2743 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_dstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 7872 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 418:
#line 2750 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 7880 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 419:
#line 2756 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 7888 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 420:
#line 2760 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 7897 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 421:
#line 2766 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->lex_strterm = (yyvsp[-2].nd);
                      (yyval.nd) = list2((yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 7906 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 422:
#line 2771 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 7914 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 423:
#line 2775 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 7922 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 424:
#line 2781 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7930 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 425:
#line 2785 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_dxstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 7938 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 426:
#line 2791 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7946 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 427:
#line 2795 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_dregx(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 7954 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 431:
#line 2808 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, new_str(p, "", 0));
                      heredoc_end(p);
                    }
#line 7964 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 432:
#line 2814 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      heredoc_end(p);
                    }
#line 7972 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 435:
#line 2824 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, (yyvsp[0].nd));
                      heredoc_treat_nextline(p);
                    }
#line 7982 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 436:
#line 2830 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 7991 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 437:
#line 2836 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      p->lex_strterm = (yyvsp[-2].nd);
                      inf->doc = push(push(inf->doc, (yyvsp[-3].nd)), (yyvsp[-1].nd));
                    }
#line 8001 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 438:
#line 2844 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_words(p, list1((yyvsp[0].nd)));
                    }
#line 8009 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 439:
#line 2848 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_words(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8017 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 440:
#line 2855 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_sym(p, (yyvsp[0].id));
                    }
#line 8025 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 441:
#line 2859 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->lstate = EXPR_END;
                      (yyval.nd) = new_dsym(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8034 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 442:
#line 2866 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->lstate = EXPR_END;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8043 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 447:
#line 2877 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 8051 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 448:
#line 2881 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 8059 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 449:
#line 2887 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[0].nd)));
                    }
#line 8067 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 450:
#line 2891 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_symbols(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8075 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 453:
#line 2899 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = negate_lit(p, (yyvsp[0].nd));
                    }
#line 8083 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 454:
#line 2903 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = negate_lit(p, (yyvsp[0].nd));
                    }
#line 8091 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 455:
#line 2909 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 8099 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 456:
#line 2913 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_ivar(p, (yyvsp[0].id));
                    }
#line 8107 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 457:
#line 2917 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_gvar(p, (yyvsp[0].id));
                    }
#line 8115 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 458:
#line 2921 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_cvar(p, (yyvsp[0].id));
                    }
#line 8123 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 459:
#line 2925 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_const(p, (yyvsp[0].id));
                    }
#line 8131 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 460:
#line 2931 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 8139 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 461:
#line 2937 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = var_reference(p, (yyvsp[0].nd));
                    }
#line 8147 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 462:
#line 2941 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_nil(p);
                    }
#line 8155 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 463:
#line 2945 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_self(p);
                    }
#line 8163 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 464:
#line 2949 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_true(p);
                    }
#line 8171 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 465:
#line 2953 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_false(p);
                    }
#line 8179 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 466:
#line 2957 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      const char *fn = p->filename;
                      if (!fn) {
                        fn = "(null)";
                      }
                      (yyval.nd) = new_str(p, fn, strlen(fn));
                    }
#line 8191 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 467:
#line 2965 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      char buf[16];

                      snprintf(buf, sizeof(buf), "%d", p->lineno);
                      (yyval.nd) = new_int(p, buf, 10);
                    }
#line 8202 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 468:
#line 2972 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
#ifdef MRB_UTF8_STRING
                      const char *enc = "UTF-8";
#else
                      const char *enc = "ASCII-8BIT";
#endif
                      (yyval.nd) = new_str(p, enc, strlen(enc));
                    }
#line 8215 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 471:
#line 2987 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = 0;
                    }
#line 8223 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 472:
#line 2991 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 8232 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 473:
#line 2996 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8240 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 474:
#line 3007 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 8250 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 475:
#line 3013 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8258 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 476:
#line 3019 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 8266 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 477:
#line 3023 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8274 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 478:
#line 3027 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].id));
                    }
#line 8282 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 479:
#line 3031 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8290 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 480:
#line 3035 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 8298 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 481:
#line 3039 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8306 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 482:
#line 3043 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].id));
                    }
#line 8314 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 483:
#line 3047 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 8322 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 484:
#line 3051 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8330 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 485:
#line 3055 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].id));
                    }
#line 8338 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 486:
#line 3059 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8346 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 487:
#line 3063 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].id));
                    }
#line 8354 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 488:
#line 3067 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8362 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 489:
#line 3071 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].id));
                    }
#line 8370 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 490:
#line 3075 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, 0);
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    }
#line 8379 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 491:
#line 3082 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    }
#line 8388 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 492:
#line 3087 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    }
#line 8397 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 493:
#line 3092 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    }
#line 8406 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 494:
#line 3097 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      yyerror(p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    }
#line 8415 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 495:
#line 3104 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.id) = 0;
                    }
#line 8423 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 496:
#line 3108 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8432 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 497:
#line 3115 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_arg(p, (yyvsp[0].id));
                    }
#line 8440 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 498:
#line 3119 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), 0);
                    }
#line 8448 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 499:
#line 3125 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8456 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 500:
#line 3129 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8464 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 501:
#line 3135 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, (yyvsp[-1].id));
                      (yyval.id) = (yyvsp[-1].id);
                    }
#line 8473 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 502:
#line 3142 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), (yyvsp[0].nd));
                    }
#line 8482 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 503:
#line 3149 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), (yyvsp[0].nd));
                    }
#line 8491 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 504:
#line 3156 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8499 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 505:
#line 3160 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8507 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 506:
#line 3166 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8515 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 507:
#line 3170 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8523 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 510:
#line 3180 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8532 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 511:
#line 3185 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, 0);
                      (yyval.id) = -1;
                    }
#line 8541 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 514:
#line 3196 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8550 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 515:
#line 3203 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8558 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 516:
#line 3207 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      local_add_f(p, 0);
                      (yyval.id) = 0;
                    }
#line 8567 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 517:
#line 3214 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 8576 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 518:
#line 3218 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {p->lstate = EXPR_BEG;}
#line 8582 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 519:
#line 3219 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      if ((yyvsp[-1].nd) == 0) {
                        yyerror(p, "can't define singleton method for ().");
                      }
                      else {
                        switch ((enum node_type)intn((yyvsp[-1].nd)->car)) {
                        case NODE_STR:
                        case NODE_DSTR:
                        case NODE_XSTR:
                        case NODE_DXSTR:
                        case NODE_DREGX:
                        case NODE_MATCH:
                        case NODE_FLOAT:
                        case NODE_ARRAY:
                        case NODE_HEREDOC:
                          yyerror(p, "can't define singleton method for literals");
                        default:
                          break;
                        }
                      }
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8609 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 521:
#line 3245 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8617 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 522:
#line 3251 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 8626 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 523:
#line 3256 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8634 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 524:
#line 3262 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8644 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 525:
#line 3268 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-1].id)), (yyvsp[0].nd));
                    }
#line 8653 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 526:
#line 3273 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, new_strsym(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                    }
#line 8662 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 527:
#line 3278 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, new_strsym(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                    }
#line 8671 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 528:
#line 3283 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_dsym(p, push((yyvsp[-2].nd), (yyvsp[-1].nd))), (yyvsp[0].nd));
                    }
#line 8680 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 541:
#line 3310 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.num) = '.';
                    }
#line 8688 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 542:
#line 3314 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.num) = 0;
                    }
#line 8696 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 544:
#line 3321 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.num) = tCOLON2;
                    }
#line 8704 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 554:
#line 3345 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {yyerrok;}
#line 8710 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 557:
#line 3351 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      p->lineno++;
                      p->column = 0;
                    }
#line 8719 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;

  case 560:
#line 3362 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1646  */
    {
                      (yyval.nd) = 0;
                    }
#line 8727 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
    break;


#line 8731 "build/host-debug/mrbgems/mruby-compiler/core/y.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (p, YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (p, yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, p);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, p);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (p, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, p);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, p);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 3366 "mrbgems/mruby-compiler/core/parse.y" /* yacc.c:1906  */

#define pylval  (*((YYSTYPE*)(p->ylval)))

static void
yyerror(parser_state *p, const char *s)
{
  char* c;
  size_t n;

  if (! p->capture_errors) {
#ifndef MRB_DISABLE_STDIO
    if (p->filename) {
      fprintf(stderr, "%s:%d:%d: %s\n", p->filename, p->lineno, p->column, s);
    }
    else {
      fprintf(stderr, "line %d:%d: %s\n", p->lineno, p->column, s);
    }
#endif
  }
  else if (p->nerr < sizeof(p->error_buffer) / sizeof(p->error_buffer[0])) {
    n = strlen(s);
    c = (char *)parser_palloc(p, n + 1);
    memcpy(c, s, n + 1);
    p->error_buffer[p->nerr].message = c;
    p->error_buffer[p->nerr].lineno = p->lineno;
    p->error_buffer[p->nerr].column = p->column;
  }
  p->nerr++;
}

static void
yyerror_i(parser_state *p, const char *fmt, int i)
{
  char buf[256];

  snprintf(buf, sizeof(buf), fmt, i);
  yyerror(p, buf);
}

static void
yywarn(parser_state *p, const char *s)
{
  char* c;
  size_t n;

  if (! p->capture_errors) {
#ifndef MRB_DISABLE_STDIO
    if (p->filename) {
      fprintf(stderr, "%s:%d:%d: %s\n", p->filename, p->lineno, p->column, s);
    }
    else {
      fprintf(stderr, "line %d:%d: %s\n", p->lineno, p->column, s);
    }
#endif
  }
  else if (p->nwarn < sizeof(p->warn_buffer) / sizeof(p->warn_buffer[0])) {
    n = strlen(s);
    c = (char *)parser_palloc(p, n + 1);
    memcpy(c, s, n + 1);
    p->warn_buffer[p->nwarn].message = c;
    p->warn_buffer[p->nwarn].lineno = p->lineno;
    p->warn_buffer[p->nwarn].column = p->column;
  }
  p->nwarn++;
}

static void
yywarning(parser_state *p, const char *s)
{
  yywarn(p, s);
}

static void
yywarning_s(parser_state *p, const char *fmt, const char *s)
{
  char buf[256];

  snprintf(buf, sizeof(buf), fmt, s);
  yywarning(p, buf);
}

static void
backref_error(parser_state *p, node *n)
{
  int c;

  c = (int)(intptr_t)n->car;

  if (c == NODE_NTH_REF) {
    yyerror_i(p, "can't set variable $%" MRB_PRId, (mrb_int)(intptr_t)n->cdr);
  }
  else if (c == NODE_BACK_REF) {
    yyerror_i(p, "can't set variable $%c", (int)(intptr_t)n->cdr);
  }
  else {
    mrb_bug(p->mrb, "Internal error in backref_error() : n=>car == %S", mrb_fixnum_value(c));
  }
}

static void
void_expr_error(parser_state *p, node *n)
{
  int c;

  if (n == NULL) return;
  c = (int)(intptr_t)n->car;
  switch (c) {
  case NODE_BREAK:
  case NODE_RETURN:
  case NODE_NEXT:
  case NODE_REDO:
  case NODE_RETRY:
    yyerror(p, "void value expression");
    break;
  case NODE_AND:
  case NODE_OR:
    void_expr_error(p, n->cdr->car);
    void_expr_error(p, n->cdr->cdr);
    break;
  case NODE_BEGIN:
    if (n->cdr) {
      while (n->cdr) {
        n = n->cdr;
      }
      void_expr_error(p, n->car);
    }
    break;
  default:
    break;
  }
}

static void pushback(parser_state *p, int c);
static mrb_bool peeks(parser_state *p, const char *s);
static mrb_bool skips(parser_state *p, const char *s);

static inline int
nextc(parser_state *p)
{
  int c;

  if (p->pb) {
    node *tmp;

    c = (int)(intptr_t)p->pb->car;
    tmp = p->pb;
    p->pb = p->pb->cdr;
    cons_free(tmp);
  }
  else {
#ifndef MRB_DISABLE_STDIO
    if (p->f) {
      if (feof(p->f)) goto eof;
      c = fgetc(p->f);
      if (c == EOF) goto eof;
    }
    else
#endif
      if (!p->s || p->s >= p->send) {
        goto eof;
      }
      else {
        c = (unsigned char)*p->s++;
      }
  }
  if (c >= 0) {
    p->column++;
  }
  if (c == '\r') {
    c = nextc(p);
    if (c != '\n') {
      pushback(p, c);
      return '\r';
    }
    return c;
  }
  return c;

  eof:
  if (!p->cxt) return -1;
  else {
    if (p->cxt->partial_hook(p) < 0)
      return -1;                /* end of program(s) */
    return -2;                  /* end of a file in the program files */
  }
}

static void
pushback(parser_state *p, int c)
{
  if (c >= 0) {
    p->column--;
  }
  p->pb = cons((node*)(intptr_t)c, p->pb);
}

static void
skip(parser_state *p, char term)
{
  int c;

  for (;;) {
    c = nextc(p);
    if (c < 0) break;
    if (c == term) break;
  }
}

static int
peekc_n(parser_state *p, int n)
{
  node *list = 0;
  int c0;

  do {
    c0 = nextc(p);
    if (c0 == -1) return c0;    /* do not skip partial EOF */
    if (c0 >= 0) --p->column;
    list = push(list, (node*)(intptr_t)c0);
  } while(n--);
  if (p->pb) {
    p->pb = append((node*)list, p->pb);
  }
  else {
    p->pb = list;
  }
  return c0;
}

static mrb_bool
peek_n(parser_state *p, int c, int n)
{
  return peekc_n(p, n) == c && c >= 0;
}
#define peek(p,c) peek_n((p), (c), 0)

static mrb_bool
peeks(parser_state *p, const char *s)
{
  size_t len = strlen(s);

#ifndef MRB_DISABLE_STDIO
  if (p->f) {
    int n = 0;
    while (*s) {
      if (!peek_n(p, *s++, n++)) return FALSE;
    }
    return TRUE;
  }
  else
#endif
    if (p->s && p->s + len <= p->send) {
      if (memcmp(p->s, s, len) == 0) return TRUE;
    }
  return FALSE;
}

static mrb_bool
skips(parser_state *p, const char *s)
{
  int c;

  for (;;) {
    /* skip until first char */
    for (;;) {
      c = nextc(p);
      if (c < 0) return FALSE;
      if (c == '\n') {
        p->lineno++;
        p->column = 0;
      }
      if (c == *s) break;
    }
    s++;
    if (peeks(p, s)) {
      size_t len = strlen(s);

      while (len--) {
        if (nextc(p) == '\n') {
          p->lineno++;
          p->column = 0;
        }
      }
      return TRUE;
    }
    else{
      s--;
    }
  }
  return FALSE;
}


static int
newtok(parser_state *p)
{
  if (p->tokbuf != p->buf) {
    mrb_free(p->mrb, p->tokbuf);
    p->tokbuf = p->buf;
    p->tsiz = MRB_PARSER_TOKBUF_SIZE;
  }
  p->tidx = 0;
  return p->column - 1;
}

static void
tokadd(parser_state *p, int32_t c)
{
  char utf8[4];
  int i, len;

  /* mrb_assert(-0x10FFFF <= c && c <= 0xFF); */
  if (c >= 0) {
    /* Single byte from source or non-Unicode escape */
    utf8[0] = (char)c;
    len = 1;
  }
  else {
    /* Unicode character */
    c = -c;
    if (c < 0x80) {
      utf8[0] = (char)c;
      len = 1;
    }
    else if (c < 0x800) {
      utf8[0] = (char)(0xC0 | (c >> 6));
      utf8[1] = (char)(0x80 | (c & 0x3F));
      len = 2;
    }
    else if (c < 0x10000) {
      utf8[0] = (char)(0xE0 |  (c >> 12)        );
      utf8[1] = (char)(0x80 | ((c >>  6) & 0x3F));
      utf8[2] = (char)(0x80 | ( c        & 0x3F));
      len = 3;
    }
    else {
      utf8[0] = (char)(0xF0 |  (c >> 18)        );
      utf8[1] = (char)(0x80 | ((c >> 12) & 0x3F));
      utf8[2] = (char)(0x80 | ((c >>  6) & 0x3F));
      utf8[3] = (char)(0x80 | ( c        & 0x3F));
      len = 4;
    }
  }
  if (p->tidx+len >= p->tsiz) {
    if (p->tsiz >= MRB_PARSER_TOKBUF_MAX) {
      p->tidx += len;
      return;
    }
    p->tsiz *= 2;
    if (p->tokbuf == p->buf) {
      p->tokbuf = (char*)mrb_malloc(p->mrb, p->tsiz);
      memcpy(p->tokbuf, p->buf, MRB_PARSER_TOKBUF_SIZE);
    }
    else {
      p->tokbuf = (char*)mrb_realloc(p->mrb, p->tokbuf, p->tsiz);
    }
  }
  for (i = 0; i < len; i++) {
    p->tokbuf[p->tidx++] = utf8[i];
  }
}

static int
toklast(parser_state *p)
{
  return p->tokbuf[p->tidx-1];
}

static void
tokfix(parser_state *p)
{
  if (p->tidx >= MRB_PARSER_TOKBUF_MAX) {
    p->tidx = MRB_PARSER_TOKBUF_MAX-1;
    yyerror(p, "string too long (truncated)");
  }
  p->tokbuf[p->tidx] = '\0';
}

static const char*
tok(parser_state *p)
{
  return p->tokbuf;
}

static int
toklen(parser_state *p)
{
  return p->tidx;
}

#define IS_ARG() (p->lstate == EXPR_ARG || p->lstate == EXPR_CMDARG)
#define IS_END() (p->lstate == EXPR_END || p->lstate == EXPR_ENDARG || p->lstate == EXPR_ENDFN)
#define IS_BEG() (p->lstate == EXPR_BEG || p->lstate == EXPR_MID || p->lstate == EXPR_VALUE || p->lstate == EXPR_CLASS)
#define IS_SPCARG(c) (IS_ARG() && space_seen && !ISSPACE(c))
#define IS_LABEL_POSSIBLE() ((p->lstate == EXPR_BEG && !cmd_state) || IS_ARG())
#define IS_LABEL_SUFFIX(n) (peek_n(p, ':',(n)) && !peek_n(p, ':', (n)+1))

static int32_t
scan_oct(const int *start, int len, int *retlen)
{
  const int *s = start;
  int32_t retval = 0;

  /* mrb_assert(len <= 3) */
  while (len-- && *s >= '0' && *s <= '7') {
    retval <<= 3;
    retval |= *s++ - '0';
  }
  *retlen = (int)(s - start);

  return retval;
}

static int32_t
scan_hex(parser_state *p, const int *start, int len, int *retlen)
{
  static const char hexdigit[] = "0123456789abcdef0123456789ABCDEF";
  const int *s = start;
  uint32_t retval = 0;
  char *tmp;

  /* mrb_assert(len <= 8) */
  while (len-- && *s && (tmp = (char*)strchr(hexdigit, *s))) {
    retval <<= 4;
    retval |= (tmp - hexdigit) & 15;
    s++;
  }
  *retlen = (int)(s - start);

  return (int32_t)retval;
}

static int32_t
read_escape_unicode(parser_state *p, int limit)
{
  int buf[9];
  int i;
  int32_t hex;

  /* Look for opening brace */
  i = 0;
  buf[0] = nextc(p);
  if (buf[0] < 0) {
  eof:
    yyerror(p, "invalid escape character syntax");
    return -1;
  }
  if (ISXDIGIT(buf[0])) {
    /* \uxxxx form */
    for (i=1; i<limit; i++) {
      buf[i] = nextc(p);
      if (buf[i] < 0) goto eof;
      if (!ISXDIGIT(buf[i])) {
        pushback(p, buf[i]);
        break;
      }
    }
  }
  else {
    pushback(p, buf[0]);
  }
  hex = scan_hex(p, buf, i, &i);
  if (i == 0 || hex > 0x10FFFF || (hex & 0xFFFFF800) == 0xD800) {
    yyerror(p, "invalid Unicode code point");
    return -1;
  }
  return hex;
}

/* Return negative to indicate Unicode code point */
static int32_t
read_escape(parser_state *p)
{
  int32_t c;

  switch (c = nextc(p)) {
  case '\\':/* Backslash */
    return c;

  case 'n':/* newline */
    return '\n';

  case 't':/* horizontal tab */
    return '\t';

  case 'r':/* carriage-return */
    return '\r';

  case 'f':/* form-feed */
    return '\f';

  case 'v':/* vertical tab */
    return '\13';

  case 'a':/* alarm(bell) */
    return '\007';

  case 'e':/* escape */
    return 033;

  case '0': case '1': case '2': case '3': /* octal constant */
  case '4': case '5': case '6': case '7':
  {
    int buf[3];
    int i;

    buf[0] = c;
    for (i=1; i<3; i++) {
      buf[i] = nextc(p);
      if (buf[i] < 0) goto eof;
      if (buf[i] < '0' || '7' < buf[i]) {
        pushback(p, buf[i]);
        break;
      }
    }
    c = scan_oct(buf, i, &i);
  }
  return c;

  case 'x':     /* hex constant */
  {
    int buf[2];
    int i;

    for (i=0; i<2; i++) {
      buf[i] = nextc(p);
      if (buf[i] < 0) goto eof;
      if (!ISXDIGIT(buf[i])) {
        pushback(p, buf[i]);
        break;
      }
    }
    if (i == 0) {
      yyerror(p, "invalid hex escape");
      return -1;
    }
    return scan_hex(p, buf, i, &i);
  }

  case 'u':     /* Unicode */
    if (peek(p, '{')) {
      /* \u{xxxxxxxx} form */
      nextc(p);
      c = read_escape_unicode(p, 8);
      if (c < 0) return 0;
      if (nextc(p) != '}') goto eof;
    }
    else {
      c = read_escape_unicode(p, 4);
      if (c < 0) return 0;
    }
  return -c;

  case 'b':/* backspace */
    return '\010';

  case 's':/* space */
    return ' ';

  case 'M':
    if ((c = nextc(p)) != '-') {
      yyerror(p, "Invalid escape character syntax");
      pushback(p, c);
      return '\0';
    }
    if ((c = nextc(p)) == '\\') {
      return read_escape(p) | 0x80;
    }
    else if (c < 0) goto eof;
    else {
      return ((c & 0xff) | 0x80);
    }

  case 'C':
    if ((c = nextc(p)) != '-') {
      yyerror(p, "Invalid escape character syntax");
      pushback(p, c);
      return '\0';
    }
  case 'c':
    if ((c = nextc(p))== '\\') {
      c = read_escape(p);
    }
    else if (c == '?')
      return 0177;
    else if (c < 0) goto eof;
    return c & 0x9f;

    eof:
  case -1:
  case -2:                      /* end of a file */
    yyerror(p, "Invalid escape character syntax");
    return '\0';

  default:
    return c;
  }
}

static int
parse_string(parser_state *p)
{
  int c;
  string_type type = (string_type)(intptr_t)p->lex_strterm->car;
  int nest_level = intn(p->lex_strterm->cdr->car);
  int beg = intn(p->lex_strterm->cdr->cdr->car);
  int end = intn(p->lex_strterm->cdr->cdr->cdr);
  parser_heredoc_info *hinf = (type & STR_FUNC_HEREDOC) ? parsing_heredoc_inf(p) : NULL;
  int cmd_state = p->cmd_start;

  if (beg == 0) beg = -3;       /* should never happen */
  if (end == 0) end = -3;
  newtok(p);
  while ((c = nextc(p)) != end || nest_level != 0) {
    if (hinf && (c == '\n' || c < 0)) {
      mrb_bool line_head;
      tokadd(p, '\n');
      tokfix(p);
      p->lineno++;
      p->column = 0;
      line_head = hinf->line_head;
      hinf->line_head = TRUE;
      if (line_head) {
        /* check whether end of heredoc */
        const char *s = tok(p);
        int len = toklen(p);
        if (hinf->allow_indent) {
          while (ISSPACE(*s) && len > 0) {
            ++s;
            --len;
          }
        }
        if ((len-1 == hinf->term_len) && (strncmp(s, hinf->term, len-1) == 0)) {
          if (c < 0) {
            p->parsing_heredoc = NULL;
          }
          else {
            return tHEREDOC_END;
          }
        }
      }
      if (c < 0) {
        char buf[256];
        snprintf(buf, sizeof(buf), "can't find heredoc delimiter \"%s\" anywhere before EOF", hinf->term);
        yyerror(p, buf);
        return 0;
      }
      pylval.nd = new_str(p, tok(p), toklen(p));
      return tHD_STRING_MID;
    }
    if (c < 0) {
      yyerror(p, "unterminated string meets end of file");
      return 0;
    }
    else if (c == beg) {
      nest_level++;
      p->lex_strterm->cdr->car = (node*)(intptr_t)nest_level;
    }
    else if (c == end) {
      nest_level--;
      p->lex_strterm->cdr->car = (node*)(intptr_t)nest_level;
    }
    else if (c == '\\') {
      c = nextc(p);
      if (type & STR_FUNC_EXPAND) {
        if (c == end || c == beg) {
          tokadd(p, c);
        }
        else if (c == '\n') {
          p->lineno++;
          p->column = 0;
          if (type & STR_FUNC_ARRAY) {
            tokadd(p, '\n');
          }
        }
        else if (type & STR_FUNC_REGEXP) {
          tokadd(p, '\\');
          tokadd(p, c);
        }
        else if (c == 'u' && peek(p, '{')) {
          /* \u{xxxx xxxx xxxx} form */
          nextc(p);
          while (1) {
            do c = nextc(p); while (ISSPACE(c));
            if (c == '}') break;
            pushback(p, c);
            c = read_escape_unicode(p, 8);
            if (c < 0) break;
            tokadd(p, -c);
          }
          if (hinf)
            hinf->line_head = FALSE;
        }
        else {
          pushback(p, c);
          tokadd(p, read_escape(p));
          if (hinf)
            hinf->line_head = FALSE;
        }
      }
      else {
        if (c != beg && c != end) {
          if (c == '\n') {
            p->lineno++;
            p->column = 0;
          }
          if (!(c == '\\' || ((type & STR_FUNC_ARRAY) && ISSPACE(c)))) {
            tokadd(p, '\\');
          }
        }
        tokadd(p, c);
      }
      continue;
    }
    else if ((c == '#') && (type & STR_FUNC_EXPAND)) {
      c = nextc(p);
      if (c == '{') {
        tokfix(p);
        p->lstate = EXPR_BEG;
        p->cmd_start = TRUE;
        pylval.nd = new_str(p, tok(p), toklen(p));
        if (hinf) {
          hinf->line_head = FALSE;
          return tHD_STRING_PART;
        }
        return tSTRING_PART;
      }
      tokadd(p, '#');
      pushback(p, c);
      continue;
    }
    if ((type & STR_FUNC_ARRAY) && ISSPACE(c)) {
      if (toklen(p) == 0) {
        do {
          if (c == '\n') {
            p->lineno++;
            p->column = 0;
            heredoc_treat_nextline(p);
            if (p->parsing_heredoc != NULL) {
              return tHD_LITERAL_DELIM;
            }
          }
          c = nextc(p);
        } while (ISSPACE(c));
        pushback(p, c);
        return tLITERAL_DELIM;
      }
      else {
        pushback(p, c);
        tokfix(p);
        pylval.nd = new_str(p, tok(p), toklen(p));
        return tSTRING_MID;
      }
    }
    if (c == '\n') {
      p->lineno++;
      p->column = 0;
    }
    tokadd(p, c);
  }

  tokfix(p);
  p->lstate = EXPR_END;
  end_strterm(p);

  if (type & STR_FUNC_XQUOTE) {
    pylval.nd = new_xstr(p, tok(p), toklen(p));
    return tXSTRING;
  }

  if (type & STR_FUNC_REGEXP) {
    int f = 0;
    int re_opt;
    char *s = strndup(tok(p), toklen(p));
    char flags[3];
    char *flag = flags;
    char enc = '\0';
    char *encp;
    char *dup;

    newtok(p);
    while (re_opt = nextc(p), re_opt >= 0 && ISALPHA(re_opt)) {
      switch (re_opt) {
      case 'i': f |= 1; break;
      case 'x': f |= 2; break;
      case 'm': f |= 4; break;
      case 'u': f |= 16; break;
      case 'n': f |= 32; break;
      default: tokadd(p, re_opt); break;
      }
    }
    pushback(p, re_opt);
    if (toklen(p)) {
      char msg[128];
      tokfix(p);
      snprintf(msg, sizeof(msg), "unknown regexp option%s - %s",
          toklen(p) > 1 ? "s" : "", tok(p));
      yyerror(p, msg);
    }
    if (f != 0) {
      if (f & 1) *flag++ = 'i';
      if (f & 2) *flag++ = 'x';
      if (f & 4) *flag++ = 'm';
      if (f & 16) enc = 'u';
      if (f & 32) enc = 'n';
    }
    if (flag > flags) {
      dup = strndup(flags, (size_t)(flag - flags));
    }
    else {
      dup = NULL;
    }
    if (enc) {
      encp = strndup(&enc, 1);
    }
    else {
      encp = NULL;
    }
    pylval.nd = new_regx(p, s, dup, encp);

    return tREGEXP;
  }
  pylval.nd = new_str(p, tok(p), toklen(p));
  if (IS_LABEL_POSSIBLE()) {
    if (IS_LABEL_SUFFIX(0)) {
      p->lstate = EXPR_BEG;
      nextc(p);
      return tLABEL_END;
    }
  }

  return tSTRING;
}


static int
heredoc_identifier(parser_state *p)
{
  int c;
  int type = str_heredoc;
  mrb_bool indent = FALSE;
  mrb_bool quote = FALSE;
  node *newnode;
  parser_heredoc_info *info;

  c = nextc(p);
  if (ISSPACE(c) || c == '=') {
    pushback(p, c);
    return 0;
  }
  if (c == '-') {
    indent = TRUE;
    c = nextc(p);
  }
  if (c == '\'' || c == '"') {
    int term = c;
    if (c == '\'')
      quote = TRUE;
    newtok(p);
    while ((c = nextc(p)) >= 0 && c != term) {
      if (c == '\n') {
        c = -1;
        break;
      }
      tokadd(p, c);
    }
    if (c < 0) {
      yyerror(p, "unterminated here document identifier");
      return 0;
    }
  }
  else {
    if (c < 0) {
      return 0;                 /* missing here document identifier */
    }
    if (! identchar(c)) {
      pushback(p, c);
      if (indent) pushback(p, '-');
      return 0;
    }
    newtok(p);
    do {
      tokadd(p, c);
    } while ((c = nextc(p)) >= 0 && identchar(c));
    pushback(p, c);
  }
  tokfix(p);
  newnode = new_heredoc(p);
  info = (parser_heredoc_info*)newnode->cdr;
  info->term = strndup(tok(p), toklen(p));
  info->term_len = toklen(p);
  if (! quote)
    type |= STR_FUNC_EXPAND;
  info->type = (string_type)type;
  info->allow_indent = indent;
  info->line_head = TRUE;
  info->doc = NULL;
  p->heredocs_from_nextline = push(p->heredocs_from_nextline, newnode);
  p->lstate = EXPR_END;

  pylval.nd = newnode;
  return tHEREDOC_BEG;
}

static int
arg_ambiguous(parser_state *p)
{
  yywarning(p, "ambiguous first argument; put parentheses or even spaces");
  return 1;
}

#include "lex.def"

static int
parser_yylex(parser_state *p)
{
  int32_t c;
  int space_seen = 0;
  int cmd_state;
  enum mrb_lex_state_enum last_state;
  int token_column;

  if (p->lex_strterm) {
    if (is_strterm_type(p, STR_FUNC_HEREDOC)) {
      if (p->parsing_heredoc != NULL)
        return parse_string(p);
    }
    else
      return parse_string(p);
  }
  cmd_state = p->cmd_start;
  p->cmd_start = FALSE;
  retry:
  last_state = p->lstate;
  switch (c = nextc(p)) {
  case '\004':  /* ^D */
  case '\032':  /* ^Z */
  case '\0':    /* NUL */
  case -1:      /* end of script. */
    if (p->heredocs_from_nextline)
      goto maybe_heredoc;
    return 0;

  /* white spaces */
  case ' ': case '\t': case '\f': case '\r':
  case '\13':   /* '\v' */
    space_seen = 1;
    goto retry;

  case '#':     /* it's a comment */
    skip(p, '\n');
    /* fall through */
  case -2:      /* end of a file */
  case '\n':
    maybe_heredoc:
    heredoc_treat_nextline(p);
  switch (p->lstate) {
  case EXPR_BEG:
  case EXPR_FNAME:
  case EXPR_DOT:
  case EXPR_CLASS:
  case EXPR_VALUE:
    p->lineno++;
    p->column = 0;
    if (p->parsing_heredoc != NULL) {
      if (p->lex_strterm) {
        return parse_string(p);
      }
    }
    goto retry;
  default:
    break;
  }
  if (p->parsing_heredoc != NULL) {
    return '\n';
  }
  while ((c = nextc(p))) {
    switch (c) {
    case ' ': case '\t': case '\f': case '\r':
    case '\13': /* '\v' */
      space_seen = 1;
      break;
    case '.':
      if ((c = nextc(p)) != '.') {
        pushback(p, c);
        pushback(p, '.');
        goto retry;
      }
    case -1:                  /* EOF */
    case -2:                  /* end of a file */
      goto normal_newline;
    default:
      pushback(p, c);
      goto normal_newline;
    }
  }
  normal_newline:
  p->cmd_start = TRUE;
  p->lstate = EXPR_BEG;
  return '\n';

  case '*':
    if ((c = nextc(p)) == '*') {
      if ((c = nextc(p)) == '=') {
        pylval.id = intern("**",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      c = tPOW;
    }
    else {
      if (c == '=') {
        pylval.id = intern_c('*');
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      if (IS_SPCARG(c)) {
        yywarning(p, "'*' interpreted as argument prefix");
        c = tSTAR;
      }
      else if (IS_BEG()) {
        c = tSTAR;
      }
      else {
        c = '*';
      }
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return c;

  case '!':
    c = nextc(p);
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if (c == '@') {
        return '!';
      }
    }
    else {
      p->lstate = EXPR_BEG;
    }
    if (c == '=') {
      return tNEQ;
    }
    if (c == '~') {
      return tNMATCH;
    }
    pushback(p, c);
    return '!';

  case '=':
    if (p->column == 1) {
      static const char begin[] = "begin";
      static const char end[] = "\n=end";
      if (peeks(p, begin)) {
        c = peekc_n(p, sizeof(begin)-1);
        if (c < 0 || ISSPACE(c)) {
          do {
            if (!skips(p, end)) {
              yyerror(p, "embedded document meets end of file");
              return 0;
            }
            c = nextc(p);
          } while (!(c < 0 || ISSPACE(c)));
          if (c != '\n') skip(p, '\n');
          p->lineno++;
          p->column = 0;
          goto retry;
        }
      }
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    if ((c = nextc(p)) == '=') {
      if ((c = nextc(p)) == '=') {
        return tEQQ;
      }
      pushback(p, c);
      return tEQ;
    }
    if (c == '~') {
      return tMATCH;
    }
    else if (c == '>') {
      return tASSOC;
    }
    pushback(p, c);
    return '=';

  case '<':
    c = nextc(p);
    if (c == '<' &&
        p->lstate != EXPR_DOT &&
        p->lstate != EXPR_CLASS &&
        !IS_END() &&
        (!IS_ARG() || space_seen)) {
      int token = heredoc_identifier(p);
      if (token)
        return token;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
      if (p->lstate == EXPR_CLASS) {
        p->cmd_start = TRUE;
      }
    }
    if (c == '=') {
      if ((c = nextc(p)) == '>') {
        return tCMP;
      }
      pushback(p, c);
      return tLEQ;
    }
    if (c == '<') {
      if ((c = nextc(p)) == '=') {
        pylval.id = intern("<<",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tLSHFT;
    }
    pushback(p, c);
    return '<';

  case '>':
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    if ((c = nextc(p)) == '=') {
      return tGEQ;
    }
    if (c == '>') {
      if ((c = nextc(p)) == '=') {
        pylval.id = intern(">>",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tRSHFT;
    }
    pushback(p, c);
    return '>';

  case '"':
    p->lex_strterm = new_strterm(p, str_dquote, '"', 0);
    return tSTRING_BEG;

  case '\'':
    p->lex_strterm = new_strterm(p, str_squote, '\'', 0);
    return parse_string(p);

  case '`':
    if (p->lstate == EXPR_FNAME) {
      p->lstate = EXPR_ENDFN;
      return '`';
    }
    if (p->lstate == EXPR_DOT) {
      if (cmd_state)
        p->lstate = EXPR_CMDARG;
      else
        p->lstate = EXPR_ARG;
      return '`';
    }
    p->lex_strterm = new_strterm(p, str_xquote, '`', 0);
    return tXSTRING_BEG;

  case '?':
    if (IS_END()) {
      p->lstate = EXPR_VALUE;
      return '?';
    }
    c = nextc(p);
    if (c < 0) {
      yyerror(p, "incomplete character syntax");
      return 0;
    }
    if (ISSPACE(c)) {
      if (!IS_ARG()) {
        int c2;
        switch (c) {
        case ' ':
          c2 = 's';
          break;
        case '\n':
          c2 = 'n';
          break;
        case '\t':
          c2 = 't';
          break;
        case '\v':
          c2 = 'v';
          break;
        case '\r':
          c2 = 'r';
          break;
        case '\f':
          c2 = 'f';
          break;
        default:
          c2 = 0;
          break;
        }
        if (c2) {
          char buf[256];
          snprintf(buf, sizeof(buf), "invalid character syntax; use ?\\%c", c2);
          yyerror(p, buf);
        }
      }
      ternary:
      pushback(p, c);
      p->lstate = EXPR_VALUE;
      return '?';
    }
    newtok(p);
    /* need support UTF-8 if configured */
    if ((isalnum(c) || c == '_')) {
      int c2 = nextc(p);
      pushback(p, c2);
      if ((isalnum(c2) || c2 == '_')) {
        goto ternary;
      }
    }
    if (c == '\\') {
      c = read_escape(p);
      tokadd(p, c);
    }
    else {
      tokadd(p, c);
    }
    tokfix(p);
    pylval.nd = new_str(p, tok(p), toklen(p));
    p->lstate = EXPR_END;
    return tCHAR;

  case '&':
    if ((c = nextc(p)) == '&') {
      p->lstate = EXPR_BEG;
      if ((c = nextc(p)) == '=') {
        pylval.id = intern("&&",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tANDOP;
    }
    else if (c == '.') {
      p->lstate = EXPR_DOT;
      return tANDDOT;
    }
    else if (c == '=') {
      pylval.id = intern_c('&');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    pushback(p, c);
    if (IS_SPCARG(c)) {
      yywarning(p, "'&' interpreted as argument prefix");
      c = tAMPER;
    }
    else if (IS_BEG()) {
      c = tAMPER;
    }
    else {
      c = '&';
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return c;

  case '|':
    if ((c = nextc(p)) == '|') {
      p->lstate = EXPR_BEG;
      if ((c = nextc(p)) == '=') {
        pylval.id = intern("||",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tOROP;
    }
    if (c == '=') {
      pylval.id = intern_c('|');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    pushback(p, c);
    return '|';

  case '+':
    c = nextc(p);
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if (c == '@') {
        return tUPLUS;
      }
      pushback(p, c);
      return '+';
    }
    if (c == '=') {
      pylval.id = intern_c('+');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous(p))) {
      p->lstate = EXPR_BEG;
      pushback(p, c);
      if (c >= 0 && ISDIGIT(c)) {
        c = '+';
        goto start_num;
      }
      return tUPLUS;
    }
    p->lstate = EXPR_BEG;
    pushback(p, c);
    return '+';

  case '-':
    c = nextc(p);
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if (c == '@') {
        return tUMINUS;
      }
      pushback(p, c);
      return '-';
    }
    if (c == '=') {
      pylval.id = intern_c('-');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (c == '>') {
      p->lstate = EXPR_ENDFN;
      return tLAMBDA;
    }
    if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous(p))) {
      p->lstate = EXPR_BEG;
      pushback(p, c);
      if (c >= 0 && ISDIGIT(c)) {
        return tUMINUS_NUM;
      }
      return tUMINUS;
    }
    p->lstate = EXPR_BEG;
    pushback(p, c);
    return '-';

  case '.':
    p->lstate = EXPR_BEG;
    if ((c = nextc(p)) == '.') {
      if ((c = nextc(p)) == '.') {
        return tDOT3;
      }
      pushback(p, c);
      return tDOT2;
    }
    pushback(p, c);
    if (c >= 0 && ISDIGIT(c)) {
      yyerror(p, "no .<digit> floating literal anymore; put 0 before dot");
    }
    p->lstate = EXPR_DOT;
    return '.';

    start_num:
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
  {
    int is_float, seen_point, seen_e, nondigit;

    is_float = seen_point = seen_e = nondigit = 0;
    p->lstate = EXPR_END;
    newtok(p);
    if (c == '-' || c == '+') {
      tokadd(p, c);
      c = nextc(p);
    }
    if (c == '0') {
#define no_digits() do {yyerror(p,"numeric literal without digits"); return 0;} while (0)
      int start = toklen(p);
      c = nextc(p);
      if (c == 'x' || c == 'X') {
        /* hexadecimal */
        c = nextc(p);
        if (c >= 0 && ISXDIGIT(c)) {
          do {
            if (c == '_') {
              if (nondigit) break;
              nondigit = c;
              continue;
            }
            if (!ISXDIGIT(c)) break;
            nondigit = 0;
            tokadd(p, tolower(c));
          } while ((c = nextc(p)) >= 0);
        }
        pushback(p, c);
        tokfix(p);
        if (toklen(p) == start) {
          no_digits();
        }
        else if (nondigit) goto trailing_uc;
        pylval.nd = new_int(p, tok(p), 16);
        return tINTEGER;
      }
      if (c == 'b' || c == 'B') {
        /* binary */
        c = nextc(p);
        if (c == '0' || c == '1') {
          do {
            if (c == '_') {
              if (nondigit) break;
              nondigit = c;
              continue;
            }
            if (c != '0' && c != '1') break;
            nondigit = 0;
            tokadd(p, c);
          } while ((c = nextc(p)) >= 0);
        }
        pushback(p, c);
        tokfix(p);
        if (toklen(p) == start) {
          no_digits();
        }
        else if (nondigit) goto trailing_uc;
        pylval.nd = new_int(p, tok(p), 2);
        return tINTEGER;
      }
      if (c == 'd' || c == 'D') {
        /* decimal */
        c = nextc(p);
        if (c >= 0 && ISDIGIT(c)) {
          do {
            if (c == '_') {
              if (nondigit) break;
              nondigit = c;
              continue;
            }
            if (!ISDIGIT(c)) break;
            nondigit = 0;
            tokadd(p, c);
          } while ((c = nextc(p)) >= 0);
        }
        pushback(p, c);
        tokfix(p);
        if (toklen(p) == start) {
          no_digits();
        }
        else if (nondigit) goto trailing_uc;
        pylval.nd = new_int(p, tok(p), 10);
        return tINTEGER;
      }
      if (c == '_') {
        /* 0_0 */
        goto octal_number;
      }
      if (c == 'o' || c == 'O') {
        /* prefixed octal */
        c = nextc(p);
        if (c < 0 || c == '_' || !ISDIGIT(c)) {
          no_digits();
        }
      }
      if (c >= '0' && c <= '7') {
        /* octal */
        octal_number:
        do {
          if (c == '_') {
            if (nondigit) break;
            nondigit = c;
            continue;
          }
          if (c < '0' || c > '9') break;
          if (c > '7') goto invalid_octal;
          nondigit = 0;
          tokadd(p, c);
        } while ((c = nextc(p)) >= 0);

        if (toklen(p) > start) {
          pushback(p, c);
          tokfix(p);
          if (nondigit) goto trailing_uc;
          pylval.nd = new_int(p, tok(p), 8);
          return tINTEGER;
        }
        if (nondigit) {
          pushback(p, c);
          goto trailing_uc;
        }
      }
      if (c > '7' && c <= '9') {
        invalid_octal:
        yyerror(p, "Invalid octal digit");
      }
      else if (c == '.' || c == 'e' || c == 'E') {
        tokadd(p, '0');
      }
      else {
        pushback(p, c);
        pylval.nd = new_int(p, "0", 10);
        return tINTEGER;
      }
    }

    for (;;) {
      switch (c) {
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        nondigit = 0;
        tokadd(p, c);
        break;

      case '.':
        if (nondigit) goto trailing_uc;
        if (seen_point || seen_e) {
          goto decode_num;
        }
        else {
          int c0 = nextc(p);
          if (c0 < 0 || !ISDIGIT(c0)) {
            pushback(p, c0);
            goto decode_num;
          }
          c = c0;
        }
        tokadd(p, '.');
        tokadd(p, c);
        is_float++;
        seen_point++;
        nondigit = 0;
        break;

      case 'e':
      case 'E':
        if (nondigit) {
          pushback(p, c);
          c = nondigit;
          goto decode_num;
        }
        if (seen_e) {
          goto decode_num;
        }
        tokadd(p, c);
        seen_e++;
        is_float++;
        nondigit = c;
        c = nextc(p);
        if (c != '-' && c != '+') continue;
        tokadd(p, c);
        nondigit = c;
        break;

      case '_':       /* '_' in number just ignored */
        if (nondigit) goto decode_num;
        nondigit = c;
        break;

      default:
        goto decode_num;
      }
      c = nextc(p);
    }

    decode_num:
    pushback(p, c);
    if (nondigit) {
      trailing_uc:
      yyerror_i(p, "trailing '%c' in number", nondigit);
    }
    tokfix(p);
    if (is_float) {
      double d;
      char *endp;

      errno = 0;
      d = mrb_float_read(tok(p), &endp);
      if (d == 0 && endp == tok(p)) {
        yywarning_s(p, "corrupted float value %s", tok(p));
      }
      else if (errno == ERANGE) {
        yywarning_s(p, "float %s out of range", tok(p));
        errno = 0;
      }
      pylval.nd = new_float(p, tok(p));
      return tFLOAT;
    }
    pylval.nd = new_int(p, tok(p), 10);
    return tINTEGER;
  }

  case ')':
  case ']':
    p->paren_nest--;
    /* fall through */
  case '}':
    COND_LEXPOP();
    CMDARG_LEXPOP();
    if (c == ')')
      p->lstate = EXPR_ENDFN;
    else
      p->lstate = EXPR_ENDARG;
    return c;

  case ':':
    c = nextc(p);
    if (c == ':') {
      if (IS_BEG() || p->lstate == EXPR_CLASS || IS_SPCARG(-1)) {
        p->lstate = EXPR_BEG;
        return tCOLON3;
      }
      p->lstate = EXPR_DOT;
      return tCOLON2;
    }
    if (IS_END() || ISSPACE(c)) {
      pushback(p, c);
      p->lstate = EXPR_BEG;
      return ':';
    }
    pushback(p, c);
    p->lstate = EXPR_FNAME;
    return tSYMBEG;

  case '/':
    if (IS_BEG()) {
      p->lex_strterm = new_strterm(p, str_regexp, '/', 0);
      return tREGEXP_BEG;
    }
    if ((c = nextc(p)) == '=') {
      pylval.id = intern_c('/');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    pushback(p, c);
    if (IS_SPCARG(c)) {
      p->lex_strterm = new_strterm(p, str_regexp, '/', 0);
      return tREGEXP_BEG;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return '/';

  case '^':
    if ((c = nextc(p)) == '=') {
      pylval.id = intern_c('^');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    pushback(p, c);
    return '^';

  case ';':
    p->lstate = EXPR_BEG;
    return ';';

  case ',':
    p->lstate = EXPR_BEG;
    return ',';

  case '~':
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      if ((c = nextc(p)) != '@') {
        pushback(p, c);
      }
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return '~';

  case '(':
    if (IS_BEG()) {
      c = tLPAREN;
    }
    else if (IS_SPCARG(-1)) {
      c = tLPAREN_ARG;
    }
    p->paren_nest++;
    COND_PUSH(0);
    CMDARG_PUSH(0);
    p->lstate = EXPR_BEG;
    return c;

  case '[':
    p->paren_nest++;
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if ((c = nextc(p)) == ']') {
        if ((c = nextc(p)) == '=') {
          return tASET;
        }
        pushback(p, c);
        return tAREF;
      }
      pushback(p, c);
      return '[';
    }
    else if (IS_BEG()) {
      c = tLBRACK;
    }
    else if (IS_ARG() && space_seen) {
      c = tLBRACK;
    }
    p->lstate = EXPR_BEG;
    COND_PUSH(0);
    CMDARG_PUSH(0);
    return c;

  case '{':
    if (p->lpar_beg && p->lpar_beg == p->paren_nest) {
      p->lstate = EXPR_BEG;
      p->lpar_beg = 0;
      p->paren_nest--;
      COND_PUSH(0);
      CMDARG_PUSH(0);
      return tLAMBEG;
    }
    if (IS_ARG() || p->lstate == EXPR_END || p->lstate == EXPR_ENDFN)
      c = '{';          /* block (primary) */
    else if (p->lstate == EXPR_ENDARG)
      c = tLBRACE_ARG;  /* block (expr) */
    else
      c = tLBRACE;      /* hash */
    COND_PUSH(0);
    CMDARG_PUSH(0);
    p->lstate = EXPR_BEG;
    return c;

  case '\\':
    c = nextc(p);
    if (c == '\n') {
      p->lineno++;
      p->column = 0;
      space_seen = 1;
      goto retry; /* skip \\n */
    }
    pushback(p, c);
    return '\\';

  case '%':
    if (IS_BEG()) {
      int term;
      int paren;

      c = nextc(p);
      quotation:
      if (c < 0 || !ISALNUM(c)) {
        term = c;
        c = 'Q';
      }
      else {
        term = nextc(p);
        if (isalnum(term)) {
          yyerror(p, "unknown type of %string");
          return 0;
        }
      }
      if (c < 0 || term < 0) {
        yyerror(p, "unterminated quoted string meets end of file");
        return 0;
      }
      paren = term;
      if (term == '(') term = ')';
      else if (term == '[') term = ']';
      else if (term == '{') term = '}';
      else if (term == '<') term = '>';
      else paren = 0;

      switch (c) {
      case 'Q':
        p->lex_strterm = new_strterm(p, str_dquote, term, paren);
        return tSTRING_BEG;

      case 'q':
        p->lex_strterm = new_strterm(p, str_squote, term, paren);
        return parse_string(p);

      case 'W':
        p->lex_strterm = new_strterm(p, str_dword, term, paren);
        return tWORDS_BEG;

      case 'w':
        p->lex_strterm = new_strterm(p, str_sword, term, paren);
        return tWORDS_BEG;

      case 'x':
        p->lex_strterm = new_strterm(p, str_xquote, term, paren);
        return tXSTRING_BEG;

      case 'r':
        p->lex_strterm = new_strterm(p, str_regexp, term, paren);
        return tREGEXP_BEG;

      case 's':
        p->lex_strterm = new_strterm(p, str_ssym, term, paren);
        return tSYMBEG;

      case 'I':
        p->lex_strterm = new_strterm(p, str_dsymbols, term, paren);
        return tSYMBOLS_BEG;

      case 'i':
        p->lex_strterm = new_strterm(p, str_ssymbols, term, paren);
        return tSYMBOLS_BEG;

      default:
        yyerror(p, "unknown type of %string");
        return 0;
      }
    }
    if ((c = nextc(p)) == '=') {
      pylval.id = intern_c('%');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (IS_SPCARG(c)) {
      goto quotation;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    pushback(p, c);
    return '%';

  case '$':
    p->lstate = EXPR_END;
    token_column = newtok(p);
    c = nextc(p);
    if (c < 0) {
      yyerror(p, "incomplete global variable syntax");
      return 0;
    }
    switch (c) {
    case '_':     /* $_: last read line string */
      c = nextc(p);
      if (c >= 0 && identchar(c)) { /* if there is more after _ it is a variable */
        tokadd(p, '$');
        tokadd(p, c);
        break;
      }
      pushback(p, c);
      c = '_';
      /* fall through */
    case '~':     /* $~: match-data */
    case '*':     /* $*: argv */
    case '$':     /* $$: pid */
    case '?':     /* $?: last status */
    case '!':     /* $!: error string */
    case '@':     /* $@: error position */
    case '/':     /* $/: input record separator */
    case '\\':    /* $\: output record separator */
    case ';':     /* $;: field separator */
    case ',':     /* $,: output field separator */
    case '.':     /* $.: last read line number */
    case '=':     /* $=: ignorecase */
    case ':':     /* $:: load path */
    case '<':     /* $<: reading filename */
    case '>':     /* $>: default output handle */
    case '\"':    /* $": already loaded files */
      tokadd(p, '$');
      tokadd(p, c);
      tokfix(p);
      pylval.id = intern_cstr(tok(p));
      return tGVAR;

    case '-':
      tokadd(p, '$');
      tokadd(p, c);
      c = nextc(p);
      pushback(p, c);
      gvar:
      tokfix(p);
      pylval.id = intern_cstr(tok(p));
      return tGVAR;

    case '&':     /* $&: last match */
    case '`':     /* $`: string before last match */
    case '\'':    /* $': string after last match */
    case '+':     /* $+: string matches last pattern */
      if (last_state == EXPR_FNAME) {
        tokadd(p, '$');
        tokadd(p, c);
        goto gvar;
      }
      pylval.nd = new_back_ref(p, c);
      return tBACK_REF;

    case '1': case '2': case '3':
    case '4': case '5': case '6':
    case '7': case '8': case '9':
      do {
        tokadd(p, c);
        c = nextc(p);
      } while (c >= 0 && isdigit(c));
      pushback(p, c);
      if (last_state == EXPR_FNAME) goto gvar;
      tokfix(p);
      {
        unsigned long n = strtoul(tok(p), NULL, 10);
        if (n > INT_MAX) {
          yyerror_i(p, "capture group index must be <= %d", INT_MAX);
          return 0;
        }
        pylval.nd = new_nth_ref(p, (int)n);
      }
      return tNTH_REF;

    default:
      if (!identchar(c)) {
        pushback(p,  c);
        return '$';
      }
      /* fall through */
    case '0':
      tokadd(p, '$');
    }
    break;

    case '@':
      c = nextc(p);
      token_column = newtok(p);
      tokadd(p, '@');
      if (c == '@') {
        tokadd(p, '@');
        c = nextc(p);
      }
      if (c < 0) {
        if (p->tidx == 1) {
          yyerror(p, "incomplete instance variable syntax");
        }
        else {
          yyerror(p, "incomplete class variable syntax");
        }
        return 0;
      }
      else if (isdigit(c)) {
        if (p->tidx == 1) {
          yyerror_i(p, "'@%c' is not allowed as an instance variable name", c);
        }
        else {
          yyerror_i(p, "'@@%c' is not allowed as a class variable name", c);
        }
        return 0;
      }
      if (!identchar(c)) {
        pushback(p, c);
        return '@';
      }
      break;

    case '_':
      token_column = newtok(p);
      break;

    default:
      if (!identchar(c)) {
        yyerror_i(p,  "Invalid char '\\x%02X' in expression", c);
        goto retry;
      }

      token_column = newtok(p);
      break;
  }

  do {
    tokadd(p, c);
    c = nextc(p);
    if (c < 0) break;
  } while (identchar(c));
  if (token_column == 0 && toklen(p) == 7 && (c < 0 || c == '\n') &&
      strncmp(tok(p), "__END__", toklen(p)) == 0)
    return -1;

  switch (tok(p)[0]) {
  case '@': case '$':
    pushback(p, c);
    break;
  default:
    if ((c == '!' || c == '?') && !peek(p, '=')) {
      tokadd(p, c);
    }
    else {
      pushback(p, c);
    }
  }
  tokfix(p);
  {
    int result = 0;

    switch (tok(p)[0]) {
    case '$':
      p->lstate = EXPR_END;
      result = tGVAR;
      break;
    case '@':
      p->lstate = EXPR_END;
      if (tok(p)[1] == '@')
        result = tCVAR;
      else
        result = tIVAR;
      break;

    default:
      if (toklast(p) == '!' || toklast(p) == '?') {
        result = tFID;
      }
      else {
        if (p->lstate == EXPR_FNAME) {
          if ((c = nextc(p)) == '=' && !peek(p, '~') && !peek(p, '>') &&
              (!peek(p, '=') || (peek_n(p, '>', 1)))) {
            result = tIDENTIFIER;
            tokadd(p, c);
            tokfix(p);
          }
          else {
            pushback(p, c);
          }
        }
        if (result == 0 && ISUPPER(tok(p)[0])) {
          result = tCONSTANT;
        }
        else {
          result = tIDENTIFIER;
        }
      }

      if (IS_LABEL_POSSIBLE()) {
        if (IS_LABEL_SUFFIX(0)) {
          p->lstate = EXPR_BEG;
          nextc(p);
          tokfix(p);
          pylval.id = intern_cstr(tok(p));
          return tLABEL;
        }
      }
      if (p->lstate != EXPR_DOT) {
        const struct kwtable *kw;

        /* See if it is a reserved word.  */
        kw = mrb_reserved_word(tok(p), toklen(p));
        if (kw) {
          enum mrb_lex_state_enum state = p->lstate;
          pylval.num = p->lineno;
          p->lstate = kw->state;
          if (state == EXPR_FNAME) {
            pylval.id = intern_cstr(kw->name);
            return kw->id[0];
          }
          if (p->lstate == EXPR_BEG) {
            p->cmd_start = TRUE;
          }
          if (kw->id[0] == keyword_do) {
            if (p->lpar_beg && p->lpar_beg == p->paren_nest) {
              p->lpar_beg = 0;
              p->paren_nest--;
              return keyword_do_LAMBDA;
            }
            if (COND_P()) return keyword_do_cond;
            if (CMDARG_P() && state != EXPR_CMDARG)
              return keyword_do_block;
            if (state == EXPR_ENDARG || state == EXPR_BEG)
              return keyword_do_block;
            return keyword_do;
          }
          if (state == EXPR_BEG || state == EXPR_VALUE)
            return kw->id[0];
          else {
            if (kw->id[0] != kw->id[1])
              p->lstate = EXPR_BEG;
            return kw->id[1];
          }
        }
      }

      if (IS_BEG() || p->lstate == EXPR_DOT || IS_ARG()) {
        if (cmd_state) {
          p->lstate = EXPR_CMDARG;
        }
        else {
          p->lstate = EXPR_ARG;
        }
      }
      else if (p->lstate == EXPR_FNAME) {
        p->lstate = EXPR_ENDFN;
      }
      else {
        p->lstate = EXPR_END;
      }
    }
    {
      mrb_sym ident = intern_cstr(tok(p));

      pylval.id = ident;
#if 0
      if (last_state != EXPR_DOT && islower(tok(p)[0]) && lvar_defined(ident)) {
        p->lstate = EXPR_END;
      }
#endif
    }
    return result;
  }
}

static int
yylex(void *lval, parser_state *p)
{
  p->ylval = lval;
  return parser_yylex(p);
}

static void
parser_init_cxt(parser_state *p, mrbc_context *cxt)
{
  if (!cxt) return;
  if (cxt->filename) mrb_parser_set_filename(p, cxt->filename);
  if (cxt->lineno) p->lineno = cxt->lineno;
  if (cxt->syms) {
    int i;

    p->locals = cons(0,0);
    for (i=0; i<cxt->slen; i++) {
      local_add_f(p, cxt->syms[i]);
    }
  }
  p->capture_errors = cxt->capture_errors;
  p->no_optimize = cxt->no_optimize;
  if (cxt->partial_hook) {
    p->cxt = cxt;
  }
}

static void
parser_update_cxt(parser_state *p, mrbc_context *cxt)
{
  node *n, *n0;
  int i = 0;

  if (!cxt) return;
  if ((int)(intptr_t)p->tree->car != NODE_SCOPE) return;
  n0 = n = p->tree->cdr->car;
  while (n) {
    i++;
    n = n->cdr;
  }
  cxt->syms = (mrb_sym *)mrb_realloc(p->mrb, cxt->syms, i*sizeof(mrb_sym));
  cxt->slen = i;
  for (i=0, n=n0; n; i++,n=n->cdr) {
    cxt->syms[i] = sym(n->car);
  }
}

void mrb_codedump_all(mrb_state*, struct RProc*);
void mrb_parser_dump(mrb_state *mrb, node *tree, int offset);

MRB_API void
mrb_parser_parse(parser_state *p, mrbc_context *c)
{
  struct mrb_jmpbuf buf1;
  p->jmp = &buf1;

  MRB_TRY(p->jmp) {
    int n = 1;

    p->cmd_start = TRUE;
    p->in_def = p->in_single = 0;
    p->nerr = p->nwarn = 0;
    p->lex_strterm = NULL;

    parser_init_cxt(p, c);

    if (p->mrb->jmp) {
      n = yyparse(p);
    }
    else {
      struct mrb_jmpbuf buf2;

      p->mrb->jmp = &buf2;
      MRB_TRY(p->mrb->jmp) {
        n = yyparse(p);
      }
      MRB_CATCH(p->mrb->jmp) {
        p->nerr++;
      }
      MRB_END_EXC(p->mrb->jmp);
      p->mrb->jmp = 0;
    }
    if (n != 0 || p->nerr > 0) {
      p->tree = 0;
      return;
    }
    if (!p->tree) {
      p->tree = new_nil(p);
    }
    parser_update_cxt(p, c);
    if (c && c->dump_result) {
      mrb_parser_dump(p->mrb, p->tree, 0);
    }
  }
  MRB_CATCH(p->jmp) {
    yyerror(p, "memory allocation error");
    p->nerr++;
    p->tree = 0;
    return;
  }
  MRB_END_EXC(p->jmp);
}

MRB_API parser_state*
mrb_parser_new(mrb_state *mrb)
{
  mrb_pool *pool;
  parser_state *p;
  static const parser_state parser_state_zero = { 0 };

  pool = mrb_pool_open(mrb);
  if (!pool) return NULL;
  p = (parser_state *)mrb_pool_alloc(pool, sizeof(parser_state));
  if (!p) return NULL;

  *p = parser_state_zero;
  p->mrb = mrb;
  p->pool = pool;

  p->s = p->send = NULL;
#ifndef MRB_DISABLE_STDIO
  p->f = NULL;
#endif

  p->cmd_start = TRUE;
  p->in_def = p->in_single = 0;

  p->capture_errors = FALSE;
  p->lineno = 1;
  p->column = 0;
#if defined(PARSER_TEST) || defined(PARSER_DEBUG)
  yydebug = 1;
#endif
  p->tsiz = MRB_PARSER_TOKBUF_SIZE;
  p->tokbuf = p->buf;

  p->lex_strterm = NULL;
  p->all_heredocs = p->parsing_heredoc = NULL;
  p->lex_strterm_before_heredoc = NULL;

  p->current_filename_index = -1;
  p->filename_table = NULL;
  p->filename_table_length = 0;

  return p;
}

MRB_API void
mrb_parser_free(parser_state *p) {
  if (p->tokbuf != p->buf) {
    mrb_free(p->mrb, p->tokbuf);
  }
  mrb_pool_close(p->pool);
}

MRB_API mrbc_context*
mrbc_context_new(mrb_state *mrb)
{
  return (mrbc_context *)mrb_calloc(mrb, 1, sizeof(mrbc_context));
}

MRB_API void
mrbc_context_free(mrb_state *mrb, mrbc_context *cxt)
{
  mrb_free(mrb, cxt->filename);
  mrb_free(mrb, cxt->syms);
  mrb_free(mrb, cxt);
}

MRB_API const char*
mrbc_filename(mrb_state *mrb, mrbc_context *c, const char *s)
{
  if (s) {
    size_t len = strlen(s);
    char *p = (char *)mrb_malloc(mrb, len + 1);

    memcpy(p, s, len + 1);
    if (c->filename) {
      mrb_free(mrb, c->filename);
    }
    c->filename = p;
  }
  return c->filename;
}

MRB_API void
mrbc_partial_hook(mrb_state *mrb, mrbc_context *c, int (*func)(struct mrb_parser_state*), void *data)
{
  c->partial_hook = func;
  c->partial_data = data;
}

MRB_API void
mrb_parser_set_filename(struct mrb_parser_state *p, const char *f)
{
  mrb_sym sym;
  size_t i;
  mrb_sym* new_table;

  sym = mrb_intern_cstr(p->mrb, f);
  p->filename = mrb_sym2name_len(p->mrb, sym, NULL);
  p->lineno = (p->filename_table_length > 0)? 0 : 1;

  for (i = 0; i < p->filename_table_length; ++i) {
    if (p->filename_table[i] == sym) {
      p->current_filename_index = (int)i;
      return;
    }
  }

  p->current_filename_index = (int)p->filename_table_length++;

  new_table = (mrb_sym*)parser_palloc(p, sizeof(mrb_sym) * p->filename_table_length);
  if (p->filename_table) {
    memmove(new_table, p->filename_table, sizeof(mrb_sym) * p->filename_table_length);
  }
  p->filename_table = new_table;
  p->filename_table[p->filename_table_length - 1] = sym;
}

MRB_API char const*
mrb_parser_get_filename(struct mrb_parser_state* p, uint16_t idx) {
  if (idx >= p->filename_table_length) { return NULL; }
  else {
    return mrb_sym2name_len(p->mrb, p->filename_table[idx], NULL);
  }
}

#ifndef MRB_DISABLE_STDIO
MRB_API parser_state*
mrb_parse_file(mrb_state *mrb, FILE *f, mrbc_context *c)
{
  parser_state *p;

  p = mrb_parser_new(mrb);
  if (!p) return NULL;
  p->s = p->send = NULL;
  p->f = f;

  mrb_parser_parse(p, c);
  return p;
}
#endif

MRB_API parser_state*
mrb_parse_nstring(mrb_state *mrb, const char *s, size_t len, mrbc_context *c)
{
  parser_state *p;

  p = mrb_parser_new(mrb);
  if (!p) return NULL;
  p->s = s;
  p->send = s + len;

  mrb_parser_parse(p, c);
  return p;
}

MRB_API parser_state*
mrb_parse_string(mrb_state *mrb, const char *s, mrbc_context *c)
{
  return mrb_parse_nstring(mrb, s, strlen(s), c);
}

MRB_API mrb_value
mrb_load_exec(mrb_state *mrb, struct mrb_parser_state *p, mrbc_context *c)
{
  struct RClass *target = mrb->object_class;
  struct RProc *proc;
  mrb_value v;
  unsigned int keep = 0;

  if (!p) {
    return mrb_undef_value();
  }
  if (!p->tree || p->nerr) {
    if (c) c->parser_nerr = p->nerr;
    if (p->capture_errors) {
      char buf[256];
      int n;

      n = snprintf(buf, sizeof(buf), "line %d: %s\n",
          p->error_buffer[0].lineno, p->error_buffer[0].message);
      mrb->exc = mrb_obj_ptr(mrb_exc_new(mrb, E_SYNTAX_ERROR, buf, n));
      mrb_parser_free(p);
      return mrb_undef_value();
    }
    else {
      if (mrb->exc == NULL) {
        mrb->exc = mrb_obj_ptr(mrb_exc_new_str_lit(mrb, E_SYNTAX_ERROR, "syntax error"));
      }
      mrb_parser_free(p);
      return mrb_undef_value();
    }
  }
  proc = mrb_generate_code(mrb, p);
  mrb_parser_free(p);
  if (proc == NULL) {
    if (mrb->exc == NULL) {
      mrb->exc = mrb_obj_ptr(mrb_exc_new_str_lit(mrb, E_SCRIPT_ERROR, "codegen error"));
    }
    return mrb_undef_value();
  }
  if (c) {
    if (c->dump_result) mrb_codedump_all(mrb, proc);
    if (c->no_exec) return mrb_obj_value(proc);
    if (c->target_class) {
      target = c->target_class;
    }
    if (c->keep_lv) {
      keep = c->slen + 1;
    }
    else {
      c->keep_lv = TRUE;
    }
  }
  proc->target_class = target;
  if (mrb->c->ci) {
    mrb->c->ci->target_class = target;
  }
  v = mrb_top_run(mrb, proc, mrb_top_self(mrb), keep);
  if (mrb->exc) return mrb_nil_value();
  return v;
}

#ifndef MRB_DISABLE_STDIO
MRB_API mrb_value
mrb_load_file_cxt(mrb_state *mrb, FILE *f, mrbc_context *c)
{
  return mrb_load_exec(mrb, mrb_parse_file(mrb, f, c), c);
}

MRB_API mrb_value
mrb_load_file(mrb_state *mrb, FILE *f)
{
  return mrb_load_file_cxt(mrb, f, NULL);
}
#endif

MRB_API mrb_value
mrb_load_nstring_cxt(mrb_state *mrb, const char *s, size_t len, mrbc_context *c)
{
  return mrb_load_exec(mrb, mrb_parse_nstring(mrb, s, len, c), c);
}

MRB_API mrb_value
mrb_load_nstring(mrb_state *mrb, const char *s, size_t len)
{
  return mrb_load_nstring_cxt(mrb, s, len, NULL);
}

MRB_API mrb_value
mrb_load_string_cxt(mrb_state *mrb, const char *s, mrbc_context *c)
{
  return mrb_load_nstring_cxt(mrb, s, strlen(s), c);
}

MRB_API mrb_value
mrb_load_string(mrb_state *mrb, const char *s)
{
  return mrb_load_string_cxt(mrb, s, NULL);
}

#ifndef MRB_DISABLE_STDIO

static void
dump_prefix(node *tree, int offset)
{
  printf("%05d ", tree->lineno);
  while (offset--) {
    putc(' ', stdout);
    putc(' ', stdout);
  }
}

static void
dump_recur(mrb_state *mrb, node *tree, int offset)
{
  while (tree) {
    mrb_parser_dump(mrb, tree->car, offset);
    tree = tree->cdr;
  }
}

#endif

void
mrb_parser_dump(mrb_state *mrb, node *tree, int offset)
{
#ifndef MRB_DISABLE_STDIO
  int nodetype;

  if (!tree) return;
  again:
  dump_prefix(tree, offset);
  nodetype = (int)(intptr_t)tree->car;
  tree = tree->cdr;
  switch (nodetype) {
  case NODE_BEGIN:
    printf("NODE_BEGIN:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_RESCUE:
    printf("NODE_RESCUE:\n");
    if (tree->car) {
      dump_prefix(tree, offset+1);
      printf("body:\n");
      mrb_parser_dump(mrb, tree->car, offset+2);
    }
    tree = tree->cdr;
    if (tree->car) {
      node *n2 = tree->car;

      dump_prefix(n2, offset+1);
      printf("rescue:\n");
      while (n2) {
        node *n3 = n2->car;
        if (n3->car) {
          dump_prefix(n2, offset+2);
          printf("handle classes:\n");
          dump_recur(mrb, n3->car, offset+3);
        }
        if (n3->cdr->car) {
          dump_prefix(n3, offset+2);
          printf("exc_var:\n");
          mrb_parser_dump(mrb, n3->cdr->car, offset+3);
        }
        if (n3->cdr->cdr->car) {
          dump_prefix(n3, offset+2);
          printf("rescue body:\n");
          mrb_parser_dump(mrb, n3->cdr->cdr->car, offset+3);
        }
        n2 = n2->cdr;
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      dump_prefix(tree, offset+1);
      printf("else:\n");
      mrb_parser_dump(mrb, tree->car, offset+2);
    }
    break;

  case NODE_ENSURE:
    printf("NODE_ENSURE:\n");
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("ensure:\n");
    mrb_parser_dump(mrb, tree->cdr->cdr, offset+2);
    break;

  case NODE_LAMBDA:
    printf("NODE_BLOCK:\n");
    goto block;

  case NODE_BLOCK:
    block:
    printf("NODE_BLOCK:\n");
    tree = tree->cdr;
    if (tree->car) {
      node *n = tree->car;

      if (n->car) {
        dump_prefix(n, offset+1);
        printf("mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("optional args:\n");
        {
          node *n2 = n->car;

          while (n2) {
            dump_prefix(n2, offset+2);
            printf("%s=", mrb_sym2name(mrb, sym(n2->car->car)));
            mrb_parser_dump(mrb, n2->car->cdr, 0);
            n2 = n2->cdr;
          }
        }
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("post mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      if (n->cdr) {
        dump_prefix(n, offset+1);
        printf("blk=&%s\n", mrb_sym2name(mrb, sym(n->cdr)));
      }
    }
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    break;

  case NODE_IF:
    printf("NODE_IF:\n");
    dump_prefix(tree, offset+1);
    printf("cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("then:\n");
    mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    if (tree->cdr->cdr->car) {
      dump_prefix(tree, offset+1);
      printf("else:\n");
      mrb_parser_dump(mrb, tree->cdr->cdr->car, offset+2);
    }
    break;

  case NODE_AND:
    printf("NODE_AND:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_OR:
    printf("NODE_OR:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_CASE:
    printf("NODE_CASE:\n");
    if (tree->car) {
      mrb_parser_dump(mrb, tree->car, offset+1);
    }
    tree = tree->cdr;
    while (tree) {
      dump_prefix(tree, offset+1);
      printf("case:\n");
      dump_recur(mrb, tree->car->car, offset+2);
      dump_prefix(tree, offset+1);
      printf("body:\n");
      mrb_parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_WHILE:
    printf("NODE_WHILE:\n");
    dump_prefix(tree, offset+1);
    printf("cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_UNTIL:
    printf("NODE_UNTIL:\n");
    dump_prefix(tree, offset+1);
    printf("cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_FOR:
    printf("NODE_FOR:\n");
    dump_prefix(tree, offset+1);
    printf("var:\n");
    {
      node *n2 = tree->car;

      if (n2->car) {
        dump_prefix(n2, offset+2);
        printf("pre:\n");
        dump_recur(mrb, n2->car, offset+3);
      }
      n2 = n2->cdr;
      if (n2) {
        if (n2->car) {
          dump_prefix(n2, offset+2);
          printf("rest:\n");
          mrb_parser_dump(mrb, n2->car, offset+3);
        }
        n2 = n2->cdr;
        if (n2) {
          if (n2->car) {
            dump_prefix(n2, offset+2);
            printf("post:\n");
            dump_recur(mrb, n2->car, offset+3);
          }
        }
      }
    }
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf("in:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf("do:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    break;

  case NODE_SCOPE:
    printf("NODE_SCOPE:\n");
    {
      node *n2 = tree->car;
      mrb_bool first_lval = TRUE;

      if (n2 && (n2->car || n2->cdr)) {
        dump_prefix(n2, offset+1);
        printf("local variables:\n");
        dump_prefix(n2, offset+2);
        while (n2) {
          if (n2->car) {
            if (!first_lval) printf(", ");
            printf("%s", mrb_sym2name(mrb, sym(n2->car)));
            first_lval = FALSE;
          }
          n2 = n2->cdr;
        }
        printf("\n");
      }
    }
    tree = tree->cdr;
    offset++;
    goto again;

  case NODE_FCALL:
  case NODE_CALL:
  case NODE_SCALL:
    switch (nodetype) {
    case NODE_FCALL:
      printf("NODE_FCALL:\n"); break;
    case NODE_CALL:
      printf("NODE_CALL(.):\n"); break;
    case NODE_SCALL:
      printf("NODE_SCALL(&.):\n"); break;
    default:
      break;
    }
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("method='%s' (%d)\n",
        mrb_sym2name(mrb, sym(tree->cdr->car)),
        (int)(intptr_t)tree->cdr->car);
    tree = tree->cdr->cdr->car;
    if (tree) {
      dump_prefix(tree, offset+1);
      printf("args:\n");
      dump_recur(mrb, tree->car, offset+2);
      if (tree->cdr) {
        dump_prefix(tree, offset+1);
        printf("block:\n");
        mrb_parser_dump(mrb, tree->cdr, offset+2);
      }
    }
    break;

  case NODE_DOT2:
    printf("NODE_DOT2:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_DOT3:
    printf("NODE_DOT3:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_COLON2:
    printf("NODE_COLON2:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("::%s\n", mrb_sym2name(mrb, sym(tree->cdr)));
    break;

  case NODE_COLON3:
    printf("NODE_COLON3: ::%s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_ARRAY:
    printf("NODE_ARRAY:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_HASH:
    printf("NODE_HASH:\n");
    while (tree) {
      dump_prefix(tree, offset+1);
      printf("key:\n");
      mrb_parser_dump(mrb, tree->car->car, offset+2);
      dump_prefix(tree, offset+1);
      printf("value:\n");
      mrb_parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_SPLAT:
    printf("NODE_SPLAT:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_ASGN:
    printf("NODE_ASGN:\n");
    dump_prefix(tree, offset+1);
    printf("lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_MASGN:
    printf("NODE_MASGN:\n");
    dump_prefix(tree, offset+1);
    printf("mlhs:\n");
    {
      node *n2 = tree->car;

      if (n2->car) {
        dump_prefix(tree, offset+2);
        printf("pre:\n");
        dump_recur(mrb, n2->car, offset+3);
      }
      n2 = n2->cdr;
      if (n2) {
        if (n2->car) {
          dump_prefix(n2, offset+2);
          printf("rest:\n");
          if (n2->car == (node*)-1) {
            dump_prefix(n2, offset+2);
            printf("(empty)\n");
          }
          else {
            mrb_parser_dump(mrb, n2->car, offset+3);
          }
        }
        n2 = n2->cdr;
        if (n2) {
          if (n2->car) {
            dump_prefix(n2, offset+2);
            printf("post:\n");
            dump_recur(mrb, n2->car, offset+3);
          }
        }
      }
    }
    dump_prefix(tree, offset+1);
    printf("rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_OP_ASGN:
    printf("NODE_OP_ASGN:\n");
    dump_prefix(tree, offset+1);
    printf("lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf("op='%s' (%d)\n", mrb_sym2name(mrb, sym(tree->car)), (int)(intptr_t)tree->car);
    tree = tree->cdr;
    mrb_parser_dump(mrb, tree->car, offset+1);
    break;

  case NODE_SUPER:
    printf("NODE_SUPER:\n");
    if (tree) {
      dump_prefix(tree, offset+1);
      printf("args:\n");
      dump_recur(mrb, tree->car, offset+2);
      if (tree->cdr) {
        dump_prefix(tree, offset+1);
        printf("block:\n");
        mrb_parser_dump(mrb, tree->cdr, offset+2);
      }
    }
    break;

  case NODE_ZSUPER:
    printf("NODE_ZSUPER\n");
    break;

  case NODE_RETURN:
    printf("NODE_RETURN:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_YIELD:
    printf("NODE_YIELD:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_BREAK:
    printf("NODE_BREAK:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_NEXT:
    printf("NODE_NEXT:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_REDO:
    printf("NODE_REDO\n");
    break;

  case NODE_RETRY:
    printf("NODE_RETRY\n");
    break;

  case NODE_LVAR:
    printf("NODE_LVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_GVAR:
    printf("NODE_GVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_IVAR:
    printf("NODE_IVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_CVAR:
    printf("NODE_CVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_CONST:
    printf("NODE_CONST %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_MATCH:
    printf("NODE_MATCH:\n");
    dump_prefix(tree, offset + 1);
    printf("lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset + 2);
    dump_prefix(tree, offset + 1);
    printf("rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset + 2);
    break;

  case NODE_BACK_REF:
    printf("NODE_BACK_REF: $%c\n", (int)(intptr_t)tree);
    break;

  case NODE_NTH_REF:
    printf("NODE_NTH_REF: $%" MRB_PRId "\n", (mrb_int)(intptr_t)tree);
    break;

  case NODE_ARG:
    printf("NODE_ARG %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_BLOCK_ARG:
    printf("NODE_BLOCK_ARG:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_INT:
    printf("NODE_INT %s base %d\n", (char*)tree->car, (int)(intptr_t)tree->cdr->car);
    break;

  case NODE_FLOAT:
    printf("NODE_FLOAT %s\n", (char*)tree);
    break;

  case NODE_NEGATE:
    printf("NODE_NEGATE\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_STR:
    printf("NODE_STR \"%s\" len %d\n", (char*)tree->car, (int)(intptr_t)tree->cdr);
    break;

  case NODE_DSTR:
    printf("NODE_DSTR\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_XSTR:
    printf("NODE_XSTR \"%s\" len %d\n", (char*)tree->car, (int)(intptr_t)tree->cdr);
    break;

  case NODE_DXSTR:
    printf("NODE_DXSTR\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_REGX:
    printf("NODE_REGX /%s/%s\n", (char*)tree->car, (char*)tree->cdr);
    break;

  case NODE_DREGX:
    printf("NODE_DREGX\n");
    dump_recur(mrb, tree->car, offset+1);
    dump_prefix(tree, offset);
    printf("tail: %s\n", (char*)tree->cdr->cdr->car);
    if (tree->cdr->cdr->cdr->car) {
      dump_prefix(tree, offset);
      printf("opt: %s\n", (char*)tree->cdr->cdr->cdr->car);
    }
    if (tree->cdr->cdr->cdr->cdr) {
      dump_prefix(tree, offset);
      printf("enc: %s\n", (char*)tree->cdr->cdr->cdr->cdr);
    }
    break;

  case NODE_SYM:
    printf("NODE_SYM :%s (%d)\n", mrb_sym2name(mrb, sym(tree)),
           (int)(intptr_t)tree);
    break;

  case NODE_SELF:
    printf("NODE_SELF\n");
    break;

  case NODE_NIL:
    printf("NODE_NIL\n");
    break;

  case NODE_TRUE:
    printf("NODE_TRUE\n");
    break;

  case NODE_FALSE:
    printf("NODE_FALSE\n");
    break;

  case NODE_ALIAS:
    printf("NODE_ALIAS %s %s:\n",
        mrb_sym2name(mrb, sym(tree->car)),
        mrb_sym2name(mrb, sym(tree->cdr)));
    break;

  case NODE_UNDEF:
    printf("NODE_UNDEF");
    {
      node *t = tree;
      while (t) {
        printf(" %s", mrb_sym2name(mrb, sym(t->car)));
        t = t->cdr;
      }
    }
    printf(":\n");
    break;

  case NODE_CLASS:
    printf("NODE_CLASS:\n");
    if (tree->car->car == (node*)0) {
      dump_prefix(tree, offset+1);
      printf(":%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == (node*)1) {
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else {
      mrb_parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    if (tree->cdr->car) {
      dump_prefix(tree, offset+1);
      printf("super:\n");
      mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    }
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->cdr->car->cdr, offset+2);
    break;

  case NODE_MODULE:
    printf("NODE_MODULE:\n");
    if (tree->car->car == (node*)0) {
      dump_prefix(tree, offset+1);
      printf(":%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == (node*)1) {
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else {
      mrb_parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->car->cdr, offset+2);
    break;

  case NODE_SCLASS:
    printf("NODE_SCLASS:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->car->cdr, offset+2);
    break;

  case NODE_DEF:
    printf("NODE_DEF:\n");
    dump_prefix(tree, offset+1);
    printf("%s\n", mrb_sym2name(mrb, sym(tree->car)));
    tree = tree->cdr;
    {
      node *n2 = tree->car;
      mrb_bool first_lval = TRUE;

      if (n2 && (n2->car || n2->cdr)) {
        dump_prefix(n2, offset+1);
        printf("local variables:\n");
        dump_prefix(n2, offset+2);
        while (n2) {
          if (n2->car) {
            if (!first_lval) printf(", ");
            printf("%s", mrb_sym2name(mrb, sym(n2->car)));
            first_lval = FALSE;
          }
          n2 = n2->cdr;
        }
        printf("\n");
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      node *n = tree->car;

      if (n->car) {
        dump_prefix(n, offset+1);
        printf("mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("optional args:\n");
        {
          node *n2 = n->car;

          while (n2) {
            dump_prefix(n2, offset+2);
            printf("%s=", mrb_sym2name(mrb, sym(n2->car->car)));
            mrb_parser_dump(mrb, n2->car->cdr, 0);
            n2 = n2->cdr;
          }
        }
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("post mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      if (n->cdr) {
        dump_prefix(n, offset+1);
        printf("blk=&%s\n", mrb_sym2name(mrb, sym(n->cdr)));
      }
    }
    mrb_parser_dump(mrb, tree->cdr->car, offset+1);
    break;

  case NODE_SDEF:
    printf("NODE_SDEF:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf(":%s\n", mrb_sym2name(mrb, sym(tree->car)));
    tree = tree->cdr->cdr;
    if (tree->car) {
      node *n = tree->car;

      if (n->car) {
        dump_prefix(n, offset+1);
        printf("mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("optional args:\n");
        {
          node *n2 = n->car;

          while (n2) {
            dump_prefix(n2, offset+2);
            printf("%s=", mrb_sym2name(mrb, sym(n2->car->car)));
            mrb_parser_dump(mrb, n2->car->cdr, 0);
            n2 = n2->cdr;
          }
        }
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("post mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n) {
        dump_prefix(n, offset+1);
        printf("blk=&%s\n", mrb_sym2name(mrb, sym(n)));
      }
    }
    tree = tree->cdr;
    mrb_parser_dump(mrb, tree->car, offset+1);
    break;

  case NODE_POSTEXE:
    printf("NODE_POSTEXE:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_HEREDOC:
    printf("NODE_HEREDOC (<<%s):\n", ((parser_heredoc_info*)tree)->term);
    dump_recur(mrb, ((parser_heredoc_info*)tree)->doc, offset+1);
    break;

  default:
    printf("node type: %d (0x%x)\n", nodetype, (unsigned)nodetype);
    break;
  }
#endif
}
