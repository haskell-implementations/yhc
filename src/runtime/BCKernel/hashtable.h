#ifndef __hashtable_h__
#define __hashtable_h__

#include "types.h"

/* simple string hashtable used in module resolving */

/* a link in the hash chain */
typedef struct _HashLink {
  Char*              key;
  void*              value;
  struct _HashLink*  next;
}HashLink;

/* a hash table */
typedef struct _Hashtable {
  Int           size;
  Int           threshhold;
  Int           entries;
  HashLink**    table;
}Hashtable;

/* a free function is used to free the key and value of a hash table entry */
typedef void (*HashFreeFunc)(Char* key, void* value);

/* hash table functions */
Hashtable*  hash_init();
void        hash_add(Hashtable* table, Char* key, void* value);
void        hash_update(Hashtable* table, Char* key, void* value);
void*       hash_lookup(Hashtable* table, Char* key);
void        hash_free(Hashtable* table, HashFreeFunc func);
void        hash_print(Hashtable* table);

/* the default size is the initial size of the hashtable */
#define HASH_DEFAULT_SIZE        16

/* default threshold is what percentage the number of entries is allowed to be compared to  
   the size before the hashtable is expanded and reordered */
#define HASH_DEFAULT_THRESHHOLD  400

#endif
