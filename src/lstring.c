/*
** $Id: lstring.c $
** String table (keeps all strings handled by Lua)
** See Copyright Notice in lua.h
*/

#define lstring_c
#define LUA_CORE

#include "lprefix.h"


#include <string.h>

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"


/*
** Maximum size for string table.
*/
// 字符串表最大量
#define MAXSTRTB	cast_int(luaM_limitN(MAX_INT, TString*))


/*
** equality for long strings
*/
// 比较两个长字符串，依次比较 实例，长度，内容
int luaS_eqlngstr (TString *a, TString *b) {
  size_t len = a->u.lnglen;
  lua_assert(a->tt == LUA_VLNGSTR && b->tt == LUA_VLNGSTR);
  return (a == b) ||  /* same instance or... */
    ((len == b->u.lnglen) &&  /* equal length and ... */
     (memcmp(getstr(a), getstr(b), len) == 0));  /* equal contents */
}

// 求hash
unsigned int luaS_hash (const char *str, size_t l, unsigned int seed) {
  unsigned int h = seed ^ cast_uint(l);
  for (; l > 0; l--)
    h ^= ((h<<5) + (h>>2) + cast_byte(str[l - 1]));
  return h;
}

// 求Hash，如果缺少的话
unsigned int luaS_hashlongstr (TString *ts) {
  lua_assert(ts->tt == LUA_VLNGSTR);
  if (ts->extra == 0) {  /* no hash? */
    size_t len = ts->u.lnglen;
    ts->hash = luaS_hash(getstr(ts), len, ts->hash);
    ts->extra = 1;  /* now it has its hash */
  }
  return ts->hash;
}

// table的 重新hash
static void tablerehash (TString **vect, int osize, int nsize) {
  int i;
  // 从 osize 到 nsize 的位置NULL一下，看来是要扩大呀
  for (i = osize; i < nsize; i++)  /* clear new elements */
    vect[i] = NULL;
  // 旧链表都哪来遍历
  for (i = 0; i < osize; i++) {  /* rehash old part of the array */
    TString *p = vect[i];
    vect[i] = NULL;
    while (p) {  /* for each string in the list */
      TString *hnext = p->u.hnext;  /* save next */
      unsigned int h = lmod(p->hash, nsize);  /* new position */   // 位置
      p->u.hnext = vect[h];  /* chain it into array */             // 安装
      vect[h] = p;
      p = hnext;                                                   // 下一个
    }
  }
}
// 感觉有点乱抹一通


/*
** Resize the string table. If allocation fails, keep the current size.
** (This can degrade performance, but any non-zero size should work
** correctly.)
*/
// resize string hash表
void luaS_resize (lua_State *L, int nsize) {
  stringtable *tb = &G(L)->strt;                     // 字符串表
  int osize = tb->size;                              // 旧size
  TString **newvect;                                 // 新hash
  if (nsize < osize)  /* shrinking table? */         // 新hash变小的
    tablerehash(tb->hash, osize, nsize);  /* depopulate shrinking part */
  newvect = luaM_reallocvector(L, tb->hash, osize, nsize, TString*);        // 这个有望缩减内存的
  if (l_unlikely(newvect == NULL)) {  /* reallocation failed? */            // 分配内存失败
    if (nsize < osize)  /* was it shrinking table? */
      tablerehash(tb->hash, nsize, osize);  /* restore to original size */  // 再恢复
    /* leave table as it was */
  }
  else {  /* allocation succeeded */
    tb->hash = newvect;                 // 新hash表
    tb->size = nsize;                   // 新size
    if (nsize > osize)
      tablerehash(newvect, osize, nsize);  /* rehash for new size */
  }
}


/*
** Clear API string cache. (Entries cannot be empty, so fill them with
** a non-collectable string.)
*/
// 这个cache是做什么的？
void luaS_clearcache (global_State *g) {
  int i, j;
  for (i = 0; i < STRCACHE_N; i++)
    for (j = 0; j < STRCACHE_M; j++) {
      if (iswhite(g->strcache[i][j]))  /* will entry be collected? */
        g->strcache[i][j] = g->memerrmsg;  /* replace it with something fixed */
    }
}


/*
** Initialize the string table and the string cache
*/
// 初始化state的 字符串 和 cache
void luaS_init (lua_State *L) {
  global_State *g = G(L);              // state 的 G
  int i, j;
  stringtable *tb = &G(L)->strt;       // 字符串表
  tb->hash = luaM_newvector(L, MINSTRTABSIZE, TString*);          // 分配hash表内存
  tablerehash(tb->hash, 0, MINSTRTABSIZE);  /* clear array */     // rehash
  tb->size = MINSTRTABSIZE;                                       // size
  /* pre-create memory-error message */
  g->memerrmsg = luaS_newliteral(L, MEMERRMSG);                   // 字面量，这个不使用分配内存的，估计也就不用内存回收了
  luaC_fix(L, obj2gco(g->memerrmsg));  /* it should never be collected */  // 特意fix，防止回收

  // cache clear
  for (i = 0; i < STRCACHE_N; i++)  /* fill cache with valid strings */
    for (j = 0; j < STRCACHE_M; j++)
      g->strcache[i][j] = g->memerrmsg;
}


/*
** creates a new string object
*/
// 创建字符串对象 ，指定了长度的，内容后边再填充了
static TString *createstrobj (lua_State *L, size_t l, int tag, unsigned int h) {
  TString *ts;
  GCObject *o;
  size_t totalsize;  /* total size of TString object */
  totalsize = sizelstring(l);                  // 计算总长度
  o = luaC_newobj(L, tag, totalsize);          // 创建GCObject
  ts = gco2ts(o);                              // 装TString
  ts->hash = h;                                // 这hash是没的
  ts->extra = 0;
  getstr(ts)[l] = '\0';  /* ending 0 */        // 末尾的位置 \0 截止
  return ts;
}

// 创建长字符串
TString *luaS_createlngstrobj (lua_State *L, size_t l) {
  TString *ts = createstrobj(L, l, LUA_VLNGSTR, G(L)->seed);   // LUA_VLNGSTR 是类型，seed 居然是G里边的
  ts->u.lnglen = l;
  return ts;
}

// 字符串释放？
void luaS_remove (lua_State *L, TString *ts) {
  stringtable *tb = &G(L)->strt;                             // 字符串表
  TString **p = &tb->hash[lmod(ts->hash, tb->size)];         // hash位置
  while (*p != ts)  /* find previous element */
    p = &(*p)->u.hnext;
  *p = (*p)->u.hnext;  /* remove element from its list */    // 摘除这个节点
  tb->nuse--;                                                // 字符计量
}

// 
static void growstrtab (lua_State *L, stringtable *tb) {
  // 反正上边这段内干正事
  if (l_unlikely(tb->nuse == MAX_INT)) {  /* too many strings? */
    luaC_fullgc(L, 1);  /* try to free some... */
    if (tb->nuse == MAX_INT)  /* still too many? */
      luaM_error(L);  /* cannot even create a message... */
  }
  // 翻倍
  if (tb->size <= MAXSTRTB / 2)  /* can grow string table? */
    luaS_resize(L, tb->size * 2);
}


/*
** Checks whether short string exists and reuses it or creates a new one.
*/
// 短字符串是唯一的
static TString *internshrstr (lua_State *L, const char *str, size_t l) {
  TString *ts;
  global_State *g = G(L);                        // G
  stringtable *tb = &g->strt;                    // 字符串表
  unsigned int h = luaS_hash(str, l, g->seed);   // 算hash
  TString **list = &tb->hash[lmod(h, tb->size)]; // 定位hash位置
  lua_assert(str != NULL);  /* otherwise 'memcmp'/'memcpy' are undefined */
  for (ts = *list; ts != NULL; ts = ts->u.hnext) {
    if (l == ts->shrlen && (memcmp(str, getstr(ts), l * sizeof(char)) == 0)) {
      /* found! */
      if (isdead(g, ts))  /* dead (but not collected yet)? */
        changewhite(ts);  /* resurrect it */     // 复活，和gc有关
      return ts;                                 // 找到
    }
  }

  // 找到最好，没找到是要创建的

  /* else must create a new string */
  if (tb->nuse >= tb->size) {  /* need to grow string table? */
    growstrtab(L, tb);                    // 扩容，并且扩容以后是要重新算hash的
    list = &tb->hash[lmod(h, tb->size)];  /* rehash with new size */
  }

  // 创建对象
  ts = createstrobj(L, l, LUA_VSHRSTR, h);
  memcpy(getstr(ts), str, l * sizeof(char));
  ts->shrlen = cast_byte(l);
  ts->u.hnext = *list;
  *list = ts;
  tb->nuse++;       // 计数增加
  return ts;
}


/*
** new string (with explicit length)
*/
// 创建字符串  带长度来的
TString *luaS_newlstr (lua_State *L, const char *str, size_t l) {
  if (l <= LUAI_MAXSHORTLEN)  /* short string? */
    return internshrstr(L, str, l);    // 短字符串
  else {
    TString *ts;
    if (l_unlikely(l >= (MAX_SIZE - sizeof(TString))/sizeof(char)))       // 那也不能够太长
      luaM_toobig(L);
    ts = luaS_createlngstrobj(L, l);                     // 创建长字符串
    memcpy(getstr(ts), str, l * sizeof(char));           // 内存拷贝上
    return ts;
  }
}


/*
** Create or reuse a zero-terminated string, first checking in the
** cache (using the string address as a key). The cache can contain
** only zero-terminated strings, so it is safe to use 'strcmp' to
** check hits.
*/
// 创建字符串，\0 结束，并且会使用cache
TString *luaS_new (lua_State *L, const char *str) {
  // 使用cache，居然和字符串的地址有关系
  unsigned int i = point2uint(str) % STRCACHE_N;  /* hash */ 
  int j;
  TString **p = G(L)->strcache[i];                    // 短数组 2

  for (j = 0; j < STRCACHE_M; j++) {
    if (strcmp(str, getstr(p[j])) == 0)  /* hit? */
      return p[j];  /* that is it */                  // 命中了，用现成的返回
  }

  /* normal route */
  for (j = STRCACHE_M - 1; j > 0; j--)
    p[j] = p[j - 1];  /* move out last element */     // 都往后移动
  /* new element is first in the list */
  p[0] = luaS_newlstr(L, str, strlen(str));           // 创建一个放最前头 ，还测量过长度了
  return p[0];
}

// 创建UData
Udata *luaS_newudata (lua_State *L, size_t s, int nuvalue) {
  Udata *u;
  int i;
  GCObject *o;
  if (l_unlikely(s > MAX_SIZE - udatamemoffset(nuvalue)))         // 不要太大
    luaM_toobig(L);
  
  o = luaC_newobj(L, LUA_VUSERDATA, sizeudata(nuvalue, s));       // 创建gc对象
  u = gco2u(o);                                                   // 转Udata
  u->len = s;                                                     // 初始化
  u->nuvalue = nuvalue;
  u->metatable = NULL;
  for (i = 0; i < nuvalue; i++)
    setnilvalue(&u->uv[i].uv);
  return u;
}
// 很像是长字符串

