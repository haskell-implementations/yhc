#include "hashtable.h"

void hash_rebuild(Hashtable* table);
UInt hash_string(Char* str);

Hashtable* hash_init(){
  Hashtable* ret = (Hashtable*)malloc(sizeof(Hashtable));
  ret->size = HASH_DEFAULT_SIZE;
  ret->threshhold = HASH_DEFAULT_THRESHHOLD;
  ret->entries = 0;
  ret->table = (HashLink**)malloc(sizeof(HashLink*) * ret->size);
  memset(ret->table, 0, sizeof(HashLink*) * ret->size);
  return ret;
}

void hash_add(Hashtable* table, Char* key, void* value){
  UInt size, mask, hash;
  HashLink* link;

  if ((table->entries*100)/table->size >= (UInt)table->threshhold){
    hash_rebuild(table);
  }
  size = table->size;
  mask = size - 1;
  hash = hash_string(key) & mask;
  link = (HashLink*)malloc(sizeof(HashLink));

  link->key = key;
  link->value = value;
  link->next = table->table[hash];
  table->table[hash] = link;
  table->entries++;  
}


void* hash_lookup(Hashtable* table, Char* key){
  UInt size = table->size;
  UInt mask = table->size - 1;
  UInt khash = hash_string(key);
  UInt hash = khash & mask;
  HashLink* p;

  assert(hash >= 0 && hash < size);

  for (p = table->table[hash]; p != NULL; p = p->next){
    if (!strcmp(p->key, key)){
      return p->value;
    }
  }  
  return NULL;
}

void hash_free(Hashtable* table, HashFreeFunc func){
  Int i;
  for (i = 0; i < table->size; i++){
    HashLink* p;
    HashLink* next;
    
    for (p = table->table[i]; p != NULL; p = next){
      next = p->next;
      if (func != NULL){
        func(p->key, p->value);
      }
      free(p);
    }
  }
  free(table->table);
  free(table);
}

void hash_rebuild(Hashtable* table){
  Int oldSize = table->size;
  HashLink** oldTable = table->table;
  Int i;

#if 0
  printf("---------------------------------------------------------------------------------\n");
  hash_print(table);
  printf("########## rebuilding hash table, size now = %d!\n", table->size*2);
#endif
  table->size *= 2;
  table->entries = 0;
  table->table = (HashLink**)malloc(sizeof(HashLink*) * table->size);
  memset(table->table, 0, sizeof(HashLink*) * table->size);  
  
  for (i = 0; i < oldSize; i++){
    HashLink* p;
    HashLink* next;

    for (p = oldTable[i]; p != NULL; p = next){
      next = p->next;
      hash_add(table, p->key, p->value);
      free(p);
    }
  }
  free(oldTable);

#if 0
  hash_print(table);
  printf("---------------------------------------------------------------------------------\n");
#endif
}

/* taken from the FNV source 
   http://www.isthe.com/chongo/tech/comp/fnv/ */
UInt hash_string(Char *buf)
{
    UByte *bp = (UByte*)buf;	/* start of buffer */
    UInt hval = 0x811c9dc5; /* MAGIC! */

    while (*bp) {
	/* multiply by the 32 bit FNV magic prime mod 2^32 */
	hval += (hval<<1) + (hval<<4) + (hval<<7) + (hval<<8) + (hval<<24);

	/* xor the bottom with the current octet */
	hval ^= (UInt)*bp++;
    }
    /* return our new hash value */
    return hval;
}



void hash_print(Hashtable* table){
  Int size,i;

  size = table->size;

  printf("Hashtable [size: %ld, ents: %ld] {\n", size, table->entries);

  for (i = 0; i < size; i++){
    HashLink* p;

    p = table->table[i];
    if (!p){
      continue;
    }
    printf("\t%ld -> [ ", i);
    for (;p != NULL; p = p->next){
      printf("%s:%p ", p->key, p->value);
    }
    printf("]\n");    
  }
  printf("}\n");
}
