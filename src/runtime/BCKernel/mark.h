#ifndef __mark_h__
#define __mark_h__

extern FInfo  G_cafTableEnd;
extern FInfo* G_cafTable;
extern Int    G_markCount;

extern FInfo* G_firstCaf;
extern FInfo* G_lastCaf;

#define SELECTOR_SELECT 1
#define SELECTOR_ARG  2

void  mkt_clear();
Bool  mkt_isMarked(Node* p);
Bool  mkt_mark(Node* p);
Node* mkt_firstMarked(Node* p);

void mark();

#endif
