/*
** $Id: lobject.h $
** Type definitions for Lua objects
** See Copyright Notice in lua.h
*/


#ifndef lobject_h
#define lobject_h


#include <stdarg.h>


#include "llimits.h"
#include "lua.h"


/*
** Extra types for collectable non-values
*/
#define LUA_TUPVAL	LUA_NUMTYPES  /* upvalues */
#define LUA_TPROTO	(LUA_NUMTYPES+1)  /* function prototypes */

// key已经被移除了
#define LUA_TDEADKEY	(LUA_NUMTYPES+2)  /* removed keys in tables */



/*
** number of all possible types (including LUA_TNONE but excluding DEADKEY)
*/
#define LUA_TOTALTYPES		(LUA_TPROTO + 2)


/*
** tags for Tagged Values have the following use of bits:
** bits 0-3: actual tag (a LUA_T* constant)
** bits 4-5: variant bits
** bit 6: whether value is collectable
*/
// 原来这里有说明， 0-3 是基础类型，4-5是扩展类型，6是gc标志

// 合成一个tag 好给tt
/* add variant bits to a type */
#define makevariant(t,v)	((t) | ((v) << 4))



/*
** Union of all Lua values
*/
typedef union Value {
  struct GCObject *gc;    /* collectable objects */        // 这个也是指针，只不过涉及到回收
  void *p;         /* light userdata */                    // 以下几种基本都是 数值型的表示地址或者整型值
  lua_CFunction f; /* light C functions */
  lua_Integer i;   /* integer numbers */
  lua_Number n;    /* float numbers */
} Value;


/*
** Tagged Values. This is the basic representation of values in Lua:
** an actual value plus a tag with its type.
*/

// 按照 value_ 和 tt_ 的命名法，不像是给直接使用的
// Value 是上边的纯Value，然后又附上一个 type tag, 合成了一个带T的Value  TValue
// 注意, tt 里边是按bit复用的，其中包含类型
#define TValuefields	Value value_; lu_byte tt_

typedef struct TValue {
  TValuefields;
} TValue;


// value 的值和指针
#define val_(o)		((o)->value_)
#define valraw(o)	(&val_(o))

// type 的 tag
/* raw type tag of a TValue */
#define rawtt(o)	((o)->tt_)

// 0-3 低4位，掩码0F 用于去掉其他部分，即纯基础数据类型部分。 
/* tag with no variants (bits 0-3) */
#define novariant(t)	((t) & 0x0F)

// 4-5 为类型扩展位, 即 variant 位，这是都打上了
/* type tag of a TValue (bits 0-3 for tags + variant bits 4-5) */
#define withvariant(t)	((t) & 0x3F)

// 都带的
#define ttypetag(o)	withvariant(rawtt(o))

// 不带的
/* type of a TValue */
#define ttype(o)	(novariant(rawtt(o)))


// 在其之上的检查宏
/* Macros to test type */
#define checktag(o,t)		(rawtt(o) == (t))
#define checktype(o,t)		(ttype(o) == (t))


/* Macros for internal tests */

// 检查TValue的全tag (不带gc标志) 与 所指GCObject对象中的是否一致，返回真假
/* collectable object has the same tag as the original value */
#define righttt(obj)		(ttypetag(obj) == gcvalue(obj)->tt)

/*
** Any value being manipulated by the program either is non
** collectable, or the collectable object has the right tag
** and it is not dead. The option 'L == NULL' allows other
** macros using this one to be used where L is not available.
*/
// 判断对象是不是 不存活了 liveness。比如：
// 不需要回收 或者 没死
#define checkliveness(L,obj) \
	((void)L, lua_longassert(!iscollectable(obj) || \
		(righttt(obj) && (L == NULL || !isdead(G(L),gcvalue(obj))))))


/* Macros to set values */

// 给tt赋值
/* set a value's tag */
#define settt_(o,t)	((o)->tt_=(t))


// copy TValue的，copy 了 value值 和 设置tt
// 然后还断言检查了 todo todo
/* main macro to copy values (from 'obj1' to 'obj2') */
#define setobj(L,obj1,obj2) \
	{ TValue *io1=(obj1); const TValue *io2=(obj2); \
          io1->value_ = io2->value_; settt_(io1, io2->tt_); \
	  checkliveness(L,io1); lua_assert(!isnonstrictnil(io1)); }

/*
** Different types of assignments, according to source and destination.
** (They are mostly equal now, but may be different in the future.)
*/

// setobj 的变种
/* from stack to stack */
#define setobjs2s(L,o1,o2)	setobj(L,s2v(o1),s2v(o2))
/* to stack (not from same stack) */
#define setobj2s(L,o1,o2)	setobj(L,s2v(o1),o2)
/* from table to same table */
#define setobjt2t	setobj
/* to new object */
#define setobj2n	setobj
/* to table */
#define setobj2t	setobj


/*
** Entries in a Lua stack. Field 'tbclist' forms a list of all
** to-be-closed variables active in this stack. Dummy entries are
** used when the distance between two tbc variables does not fit
** in an unsigned short. They are represented by delta==0, and
** their real delta is always the maximum value that fits in
** that field.
*/
typedef union StackValue {
  TValue val;
  struct {
    TValuefields;               // 这个 和 TValue 是等的
    unsigned short delta;       // 
  } tbclist;
} StackValue;


/* index to stack elements */
typedef StackValue *StkId;      // 它的指针为Id ?

/* convert a 'StackValue' to a 'TValue' */
#define s2v(o)	(&(o)->val)     // 强制取其 val 部分吧



/*
** {==================================================================
** Nil
** ===================================================================
*/

// nil 值
/* Standard nil */
#define LUA_VNIL	makevariant(LUA_TNIL, 0)

// 空槽位 而非包含了值为nil的槽位？
/* Empty slot (which might be different from a slot containing nil) */
#define LUA_VEMPTY	makevariant(LUA_TNIL, 1)

// 在table中没找到
/* Value returned for a key not found in a table (absent key) */
#define LUA_VABSTKEY	makevariant(LUA_TNIL, 2)

// 以上几个都是nil，但是却有自己的扩展类型，表示不同的意义

// 是否是某种nil
/* macro to test for (any kind of) nil */
#define ttisnil(v)		checktype((v), LUA_TNIL)

// 是否为纯nil
/* macro to test for a standard nil */
#define ttisstrictnil(o)	checktag((o), LUA_VNIL)

// 设置tt为 纯nil
#define setnilvalue(obj) settt_(obj, LUA_VNIL)

// 是否为没table中没找到那种 nil
#define isabstkey(v)		checktag((v), LUA_VABSTKEY)


// 是不纯那种nil不？ 我要不纯的
/*
** macro to detect non-standard nils (used only in assertions)
*/
#define isnonstrictnil(v)	(ttisnil(v) && !ttisstrictnil(v))


// 别名
/*
** By default, entries with any kind of nil are considered empty.
** (In any definition, values associated with absent keys must also
** be accepted as empty.)
*/
#define isempty(v)		ttisnil(v)

// 这是要给TValue赋值么？
/* macro defining a value corresponding to an absent key */
#define ABSTKEYCONSTANT		{NULL}, LUA_VABSTKEY

// 设置tt
/* mark an entry as empty */
#define setempty(v)		settt_(v, LUA_VEMPTY)



/* }================================================================== */


/*
** {==================================================================
** Booleans
** ===================================================================
*/

// 两个bool值, 且也有一定的 类型扩展
// 然后这种，就直接靠类型扩展了，不再需要具体的val了，tt部分就足以
#define LUA_VFALSE	makevariant(LUA_TBOOLEAN, 0)
#define LUA_VTRUE	makevariant(LUA_TBOOLEAN, 1)

// 判断基本类型和扩展类型
// 并且这一类，也不依赖gc标志，所以gc标志位都被忽略了
#define ttisboolean(o)		checktype((o), LUA_TBOOLEAN)
#define ttisfalse(o)		checktag((o), LUA_VFALSE)
#define ttistrue(o)		checktag((o), LUA_VTRUE)

// 条件为假的判断
#define l_isfalse(o)	(ttisfalse(o) || ttisnil(o))

// 设置为 true 或者 false 宏
#define setbfvalue(obj)		settt_(obj, LUA_VFALSE)
#define setbtvalue(obj)		settt_(obj, LUA_VTRUE)

/* }================================================================== */


/*
** {==================================================================
** Threads
** ===================================================================
*/

// thread 类型
#define LUA_VTHREAD		makevariant(LUA_TTHREAD, 0)

// 因涉及gc，先研究gc去
#define ttisthread(o)		checktag((o), ctb(LUA_VTHREAD))

#define thvalue(o)	check_exp(ttisthread(o), gco2th(val_(o).gc))

#define setthvalue(L,obj,x) \
  { TValue *io = (obj); lua_State *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_VTHREAD)); \
    checkliveness(L,io); }

#define setthvalue2s(L,o,t)	setthvalue(L,s2v(o),t)

/* }================================================================== */


/*
** {==================================================================
** Collectable Objects
** ===================================================================
*/

// 三个部分，next 是其一，marked 估计是gc时候的标记，那么 tt 是否是存在冗余呢？
/*
** Common Header for all collectable objects (in macro form, to be
** included in other objects)
*/
#define CommonHeader	struct GCObject *next; lu_byte tt; lu_byte marked


// 这个结构是作为 指针，放到了Value 结果中的，并且如果是TValue的话，是带着tt的。
/* Common type for all collectable objects */
typedef struct GCObject {
  CommonHeader;
} GCObject;

// tt中的第六个bit是否设置，gc标志
/* Bit mark for collectable types */
#define BIT_ISCOLLECTABLE	(1 << 6)

// 如果有设置，则说明是一个 可回收类型的
#define iscollectable(o)	(rawtt(o) & BIT_ISCOLLECTABLE)

// 将现在的tt 直接给附上 可回收标志，collectable tag bit，但没说写回
/* mark a tag as collectable */
#define ctb(t)			((t) | BIT_ISCOLLECTABLE)

// 判定类型为 可回收，并且取val Value中的gc字段
#define gcvalue(o)	check_exp(iscollectable(o), val_(o).gc)

// 直接取Value的gc字段
#define gcvalueraw(v)	((v).gc)

// 给obj设置，x 是个GCObject，被赋值到了Value的gc字段，并且从gc中的tt取值，赋值到obj上 (并顺便打上了 gc标志)
// 其实就是拿GCObject 往TValue上赋值
#define setgcovalue(L,obj,x) \
  { TValue *io = (obj); GCObject *i_g=(x); \
    val_(io).gc = i_g; settt_(io, ctb(i_g->tt)); }

/* }================================================================== */


/*
** {==================================================================
** Numbers
** ===================================================================
*/

// number 也是有两种类型的，integer 和 float
/* Variant tags for numbers */
#define LUA_VNUMINT	makevariant(LUA_TNUMBER, 0)  /* integer numbers */
#define LUA_VNUMFLT	makevariant(LUA_TNUMBER, 1)  /* float numbers */

// 总类型 和 分类型
#define ttisnumber(o)		checktype((o), LUA_TNUMBER)
#define ttisfloat(o)		checktag((o), LUA_VNUMFLT)
#define ttisinteger(o)		checktag((o), LUA_VNUMINT)

// 先检查总类型是否为number，然后根据是否为int型来取 n 或者 i
// int 类型的涉及到类型转换
#define nvalue(o)	check_exp(ttisnumber(o), \
	(ttisinteger(o) ? cast_num(ivalue(o)) : fltvalue(o)))

// float 类型的，
#define fltvalue(o)	check_exp(ttisfloat(o), val_(o).n)     // 取val的n值
#define ivalue(o)	check_exp(ttisinteger(o), val_(o).i)     // 取val的i值

// 没检查类型，直接从Value中取了内部值
#define fltvalueraw(v)	((v).n)
#define ivalueraw(v)	((v).i)

// 给obj设置float，包括n值和 LUA_VNUMFLT类型
#define setfltvalue(obj,x) \
  { TValue *io=(obj); val_(io).n=(x); settt_(io, LUA_VNUMFLT); }

// 检查obj是个float，再设置n值
#define chgfltvalue(obj,x) \
  { TValue *io=(obj); lua_assert(ttisfloat(io)); val_(io).n=(x); }

// 给obj设置int，包括i值和 LUA_VNUMINT类型
#define setivalue(obj,x) \
  { TValue *io=(obj); val_(io).i=(x); settt_(io, LUA_VNUMINT); }

// 检查obj是个int值，再设置i值
#define chgivalue(obj,x) \
  { TValue *io=(obj); lua_assert(ttisinteger(io)); val_(io).i=(x); }

/* }================================================================== */


/*
** {==================================================================
** Strings
** ===================================================================
*/

// 字符串居然也有扩展类型，短字符串 和 长字符串
/* Variant tags for strings */
#define LUA_VSHRSTR	makevariant(LUA_TSTRING, 0)  /* short strings */
#define LUA_VLNGSTR	makevariant(LUA_TSTRING, 1)  /* long strings */

// String 是典型的gc类型了，其间使用了 ctb 就是一律打上了gc标志再比较。反正String类型都是打的
#define ttisstring(o)		checktype((o), LUA_TSTRING)
#define ttisshrstring(o)	checktag((o), ctb(LUA_VSHRSTR))
#define ttislngstring(o)	checktag((o), ctb(LUA_VLNGSTR))

// 将Value的 gc转换为特定的TString中，用的都是gcobject中的tt
#define tsvalueraw(v)	(gco2ts((v).gc))

// 将TValue的 gc转换为特定的TString，且先进行了TValue的类型检查
#define tsvalue(o)	check_exp(ttisstring(o), gco2ts(val_(o).gc))

// 拿TString给TValue赋值，tt 需要另外带上gc标志，将TString转换成普通的GCObject再赋值的
// 最后 通过 checkliveness 检查了一下
#define setsvalue(L,obj,x) \
  { TValue *io = (obj); TString *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(x_->tt)); \
    checkliveness(L,io); }

// 
/* set a string to the stack */
#define setsvalue2s(L,o,s)	setsvalue(L,s2v(o),s)

/* set a string to a new object */
#define setsvalue2n	setsvalue


/*
** Header for a string value.
*/
typedef struct TString {
  CommonHeader;                                    //
  lu_byte extra;  /* reserved words for short strings; "has hash" for longs */
  lu_byte shrlen;  /* length for short strings */  // 数据长度
  unsigned int hash;                               // hash值
  union {
    size_t lnglen;  /* length for long strings */              // 长string的长度
    struct TString *hnext;  /* linked list for hash table */   // hash 链表
  } u;
  char contents[1];                                            // 数据部分
} TString;


// 字符串数据部分
/*
** Get the actual string (array of bytes) from a 'TString'.
*/
#define getstr(ts)  ((ts)->contents)

// TValue的  字符串的 数据部分
/* get the actual string (array of bytes) from a Lua value */
#define svalue(o)       getstr(tsvalue(o))

// 根据长短字符串，返回长度
/* get string length from 'TString *s' */
#define tsslen(s)	((s)->tt == LUA_VSHRSTR ? (s)->shrlen : (s)->u.lnglen)

// 从TValue中取字符串长度
/* get string length from 'TValue *o' */
#define vslen(o)	tsslen(tsvalue(o))

/* }================================================================== */


/*
** {==================================================================
** Userdata
** ===================================================================
*/


/*
** Light userdata should be a variant of userdata, but for compatibility
** reasons they are also different types.
*/
#define LUA_VLIGHTUSERDATA	makevariant(LUA_TLIGHTUSERDATA, 0)

#define LUA_VUSERDATA		makevariant(LUA_TUSERDATA, 0)

#define ttislightuserdata(o)	checktag((o), LUA_VLIGHTUSERDATA)
#define ttisfulluserdata(o)	checktag((o), ctb(LUA_VUSERDATA))

#define pvalue(o)	check_exp(ttislightuserdata(o), val_(o).p)
#define uvalue(o)	check_exp(ttisfulluserdata(o), gco2u(val_(o).gc))

#define pvalueraw(v)	((v).p)

#define setpvalue(obj,x) \
  { TValue *io=(obj); val_(io).p=(x); settt_(io, LUA_VLIGHTUSERDATA); }

#define setuvalue(L,obj,x) \
  { TValue *io = (obj); Udata *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_VUSERDATA)); \
    checkliveness(L,io); }


/* Ensures that addresses after this type are always fully aligned. */
typedef union UValue {
  TValue uv;
  LUAI_MAXALIGN;  /* ensures maximum alignment for udata bytes */
} UValue;


/*
** Header for userdata with user values;
** memory area follows the end of this structure.
*/
typedef struct Udata {
  CommonHeader;
  unsigned short nuvalue;  /* number of user values */
  size_t len;  /* number of bytes */
  struct Table *metatable;
  GCObject *gclist;
  UValue uv[1];  /* user values */
} Udata;


/*
** Header for userdata with no user values. These userdata do not need
** to be gray during GC, and therefore do not need a 'gclist' field.
** To simplify, the code always use 'Udata' for both kinds of userdata,
** making sure it never accesses 'gclist' on userdata with no user values.
** This structure here is used only to compute the correct size for
** this representation. (The 'bindata' field in its end ensures correct
** alignment for binary data following this header.)
*/
typedef struct Udata0 {
  CommonHeader;
  unsigned short nuvalue;  /* number of user values */
  size_t len;  /* number of bytes */
  struct Table *metatable;
  union {LUAI_MAXALIGN;} bindata;
} Udata0;


/* compute the offset of the memory area of a userdata */
#define udatamemoffset(nuv) \
	((nuv) == 0 ? offsetof(Udata0, bindata)  \
                    : offsetof(Udata, uv) + (sizeof(UValue) * (nuv)))

/* get the address of the memory block inside 'Udata' */
#define getudatamem(u)	(cast_charp(u) + udatamemoffset((u)->nuvalue))

/* compute the size of a userdata */
#define sizeudata(nuv,nb)	(udatamemoffset(nuv) + (nb))

/* }================================================================== */


/*
** {==================================================================
** Prototypes
** ===================================================================
*/

#define LUA_VPROTO	makevariant(LUA_TPROTO, 0)


/*
** Description of an upvalue for function prototypes
*/
typedef struct Upvaldesc {
  TString *name;  /* upvalue name (for debug information) */
  lu_byte instack;  /* whether it is in stack (register) */
  lu_byte idx;  /* index of upvalue (in stack or in outer function's list) */
  lu_byte kind;  /* kind of corresponding variable */
} Upvaldesc;


/*
** Description of a local variable for function prototypes
** (used for debug information)
*/
typedef struct LocVar {
  TString *varname;
  int startpc;  /* first point where variable is active */
  int endpc;    /* first point where variable is dead */
} LocVar;


/*
** Associates the absolute line source for a given instruction ('pc').
** The array 'lineinfo' gives, for each instruction, the difference in
** lines from the previous instruction. When that difference does not
** fit into a byte, Lua saves the absolute line for that instruction.
** (Lua also saves the absolute line periodically, to speed up the
** computation of a line number: we can use binary search in the
** absolute-line array, but we must traverse the 'lineinfo' array
** linearly to compute a line.)
*/
typedef struct AbsLineInfo {
  int pc;
  int line;
} AbsLineInfo;

/*
** Function Prototypes
*/
typedef struct Proto {
  CommonHeader;
  lu_byte numparams;  /* number of fixed (named) parameters */
  lu_byte is_vararg;
  lu_byte maxstacksize;  /* number of registers needed by this function */
  int sizeupvalues;  /* size of 'upvalues' */
  int sizek;  /* size of 'k' */
  int sizecode;
  int sizelineinfo;
  int sizep;  /* size of 'p' */
  int sizelocvars;
  int sizeabslineinfo;  /* size of 'abslineinfo' */
  int linedefined;  /* debug information  */
  int lastlinedefined;  /* debug information  */
  TValue *k;  /* constants used by the function */
  Instruction *code;  /* opcodes */
  struct Proto **p;  /* functions defined inside the function */
  Upvaldesc *upvalues;  /* upvalue information */
  ls_byte *lineinfo;  /* information about source lines (debug information) */
  AbsLineInfo *abslineinfo;  /* idem */
  LocVar *locvars;  /* information about local variables (debug information) */
  TString  *source;  /* used for debug information */
  GCObject *gclist;
} Proto;

/* }================================================================== */


/*
** {==================================================================
** Functions
** ===================================================================
*/

#define LUA_VUPVAL	makevariant(LUA_TUPVAL, 0)


/* Variant tags for functions */
#define LUA_VLCL	makevariant(LUA_TFUNCTION, 0)  /* Lua closure */
#define LUA_VLCF	makevariant(LUA_TFUNCTION, 1)  /* light C function */
#define LUA_VCCL	makevariant(LUA_TFUNCTION, 2)  /* C closure */

#define ttisfunction(o)		checktype(o, LUA_TFUNCTION)
#define ttisLclosure(o)		checktag((o), ctb(LUA_VLCL))
#define ttislcf(o)		checktag((o), LUA_VLCF)
#define ttisCclosure(o)		checktag((o), ctb(LUA_VCCL))
#define ttisclosure(o)         (ttisLclosure(o) || ttisCclosure(o))


#define isLfunction(o)	ttisLclosure(o)

#define clvalue(o)	check_exp(ttisclosure(o), gco2cl(val_(o).gc))
#define clLvalue(o)	check_exp(ttisLclosure(o), gco2lcl(val_(o).gc))
#define fvalue(o)	check_exp(ttislcf(o), val_(o).f)
#define clCvalue(o)	check_exp(ttisCclosure(o), gco2ccl(val_(o).gc))

#define fvalueraw(v)	((v).f)

#define setclLvalue(L,obj,x) \
  { TValue *io = (obj); LClosure *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_VLCL)); \
    checkliveness(L,io); }

#define setclLvalue2s(L,o,cl)	setclLvalue(L,s2v(o),cl)

#define setfvalue(obj,x) \
  { TValue *io=(obj); val_(io).f=(x); settt_(io, LUA_VLCF); }

#define setclCvalue(L,obj,x) \
  { TValue *io = (obj); CClosure *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_VCCL)); \
    checkliveness(L,io); }


/*
** Upvalues for Lua closures
*/
typedef struct UpVal {
  CommonHeader;
  lu_byte tbc;  /* true if it represents a to-be-closed variable */
  TValue *v;  /* points to stack or to its own value */
  union {
    struct {  /* (when open) */
      struct UpVal *next;  /* linked list */
      struct UpVal **previous;
    } open;
    TValue value;  /* the value (when closed) */
  } u;
} UpVal;



#define ClosureHeader \
	CommonHeader; lu_byte nupvalues; GCObject *gclist

typedef struct CClosure {
  ClosureHeader;
  lua_CFunction f;
  TValue upvalue[1];  /* list of upvalues */
} CClosure;


typedef struct LClosure {
  ClosureHeader;
  struct Proto *p;
  UpVal *upvals[1];  /* list of upvalues */
} LClosure;


typedef union Closure {
  CClosure c;
  LClosure l;
} Closure;


#define getproto(o)	(clLvalue(o)->p)

/* }================================================================== */


/*
** {==================================================================
** Tables
** ===================================================================
*/
// Table 结构
#define LUA_VTABLE	makevariant(LUA_TTABLE, 0)

// 检查
#define ttistable(o)		checktag((o), ctb(LUA_VTABLE))

// TValue的 gc对象转Table
#define hvalue(o)	check_exp(ttistable(o), gco2t(val_(o).gc))

// 从x往obj上赋值，设置类型，也设置到gc，也 checkliveness
#define sethvalue(L,obj,x) \
  { TValue *io = (obj); Table *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_VTABLE)); \
    checkliveness(L,io); }

#define sethvalue2s(L,o,h)	sethvalue(L,s2v(o),h)


/*
** Nodes for Hash tables: A pack of two TValue's (key-value pairs)
** plus a 'next' field to link colliding entries. The distribution
** of the key's fields ('key_tt' and 'key_val') not forming a proper
** 'TValue' allows for a smaller size for 'Node' both in 4-byte
** and 8-byte alignments.
*/
// table 的Node
typedef union Node {
  struct NodeKey {
    TValuefields;  /* fields for value */   // 和value差不多
    lu_byte key_tt;  /* key type */         // key的tt
    int next;  /* for chaining */           // next 是个位置? 
    Value key_val;  /* key value */         // key的value
  } u;
  TValue i_val;  /* direct access to node's value as a proper 'TValue' */
} Node;


// 给node设置key
/* copy a value into a key */
#define setnodekey(L,node,obj) \
	{ Node *n_=(node); const TValue *io_=(obj); \
	  n_->u.key_val = io_->value_; n_->u.key_tt = io_->tt_; \
	  checkliveness(L,io_); }


// 相反从node的key，往obj里设置
/* copy a value from a key */
#define getnodekey(L,obj,node) \
	{ TValue *io_=(obj); const Node *n_=(node); \
	  io_->value_ = n_->u.key_val; io_->tt_ = n_->u.key_tt; \
	  checkliveness(L,io_); }


/*
** About 'alimit': if 'isrealasize(t)' is true, then 'alimit' is the
** real size of 'array'. Otherwise, the real size of 'array' is the
** smallest power of two not smaller than 'alimit' (or zero iff 'alimit'
** is zero); 'alimit' is then used as a hint for #t.
*/
// flags 的
#define BITRAS		(1 << 7)
#define isrealasize(t)		(!((t)->flags & BITRAS))
#define setrealasize(t)		((t)->flags &= cast_byte(~BITRAS))
#define setnorealasize(t)	((t)->flags |= BITRAS)

// Table 结构
typedef struct Table {
  CommonHeader;
  lu_byte flags;  /* 1<<p means tagmethod(p) is not present */
  lu_byte lsizenode;  /* log2 of size of 'node' array */       // array的长度
  unsigned int alimit;  /* "limit" of 'array' array */         // 
  TValue *array;  /* array part */                             // array的数组
  Node *node;
  Node *lastfree;  /* any free position is before this position */
  struct Table *metatable;          // 元表
  GCObject *gclist;
} Table;


/*
** Macros to manipulate keys inserted in nodes
*/
// node的key信息
#define keytt(node)		((node)->u.key_tt)
#define keyval(node)		((node)->u.key_val)

// key的类型
#define keyisnil(node)		(keytt(node) == LUA_TNIL)
#define keyisinteger(node)	(keytt(node) == LUA_VNUMINT)

// key的int类型
#define keyival(node)		(keyval(node).i)
// key类型
#define keyisshrstr(node)	(keytt(node) == ctb(LUA_VSHRSTR))
// TString
#define keystrval(node)		(gco2ts(keyval(node).gc))

// key的tt设置为nil 能行？
#define setnilkey(node)		(keytt(node) = LUA_TNIL)

// key是否是可回收的
#define keyiscollectable(n)	(keytt(n) & BIT_ISCOLLECTABLE)

// node的key的gc
#define gckey(n)	(keyval(n).gc)
// 只有是可回收的才取gc数据
#define gckeyN(n)	(keyiscollectable(n) ? gckey(n) : NULL)


/*
** Dead keys in tables have the tag DEADKEY but keep their original
** gcvalue. This distinguishes them from regular keys but allows them to
** be found when searched in a special way. ('next' needs that to find
** keys removed from a table during a traversal.)
*/
// deadkey
#define setdeadkey(node)	(keytt(node) = LUA_TDEADKEY)
#define keyisdead(node)		(keytt(node) == LUA_TDEADKEY)

/* }================================================================== */



/*
** 'module' operation for hashing (size is always a power of 2)
*/
#define lmod(s,size) \
	(check_exp((size&(size-1))==0, (cast_int((s) & ((size)-1)))))


#define twoto(x)	(1<<(x))
#define sizenode(t)	(twoto((t)->lsizenode))


/* size of buffer for 'luaO_utf8esc' function */
#define UTF8BUFFSZ	8

LUAI_FUNC int luaO_utf8esc (char *buff, unsigned long x);
LUAI_FUNC int luaO_ceillog2 (unsigned int x);
LUAI_FUNC int luaO_rawarith (lua_State *L, int op, const TValue *p1,
                             const TValue *p2, TValue *res);
LUAI_FUNC void luaO_arith (lua_State *L, int op, const TValue *p1,
                           const TValue *p2, StkId res);
LUAI_FUNC size_t luaO_str2num (const char *s, TValue *o);
LUAI_FUNC int luaO_hexavalue (int c);
LUAI_FUNC void luaO_tostring (lua_State *L, TValue *obj);
LUAI_FUNC const char *luaO_pushvfstring (lua_State *L, const char *fmt,
                                                       va_list argp);
LUAI_FUNC const char *luaO_pushfstring (lua_State *L, const char *fmt, ...);
LUAI_FUNC void luaO_chunkid (char *out, const char *source, size_t srclen);


#endif

