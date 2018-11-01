#ifndef __thread_h__
#define __thread_h__

#include "types.h"

/* Pick the thread implementation */
#if USE_PTHREADS /* POSIX Threads */
#include <pthread.h>
#include <semaphore.h>
#include <fcntl.h>

typedef pthread_t          OSThread;
typedef pthread_mutex_t*   Mutex;
typedef sem_t*             Semaphore;

#elif defined(WIN32)

#define WIN32_MEAN_AND_LEAN
#include <windows.h>

typedef HANDLE OSThread;
typedef HANDLE Mutex;
typedef HANDLE Semaphore;

#else /* No threads */

typedef void* OSThread;
typedef void* Mutex;
typedef void* Semaphore;

#endif /* Thread implementation */


/* general threads stuff */
typedef void (*ThreadProc)(void*);

typedef struct _Thread {
  struct _Thread*     next;
  OSThread            thread;
  Mutex               waitLock;
  ThreadProc          proc;
  void*               arg;
}Thread;

void       yhi_thread_init();
Thread*    yhi_thread_create(ThreadProc proc, void* arg);


/* OS specific bits */
typedef void (*OSThreadProc)(void*);

OSThread   osthread_create(OSThreadProc proc, void* arg);

Mutex      mutex_create();
void       mutex_lock(Mutex mutex);
void       mutex_unlock(Mutex mutex);
void       mutex_destroy(Mutex mutex);

Semaphore  yhi_semaphore_create(Int init);
void       yhi_semaphore_wait(Semaphore sem);
void       yhi_semaphore_signal(Semaphore sem);
void       yhi_semaphore_zero(Semaphore sem);
void       yhi_semaphore_destroy(Semaphore sem);

#endif

