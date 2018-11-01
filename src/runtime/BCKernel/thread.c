#include "thread.h"

/*---------------------------------------------------------------------------------------------------
   General threading code
 ----------------------------------------------------------------------------------------------------*/

/* the mutex for the free thread list */
static Mutex G_freeLock = NULL;

/* linked list of free threads */
static Thread* G_freeThreads = NULL;

/* add a thread onto the waiting list */
static void thread_addToFree(Thread* thread){
  mutex_lock(G_freeLock);
  thread->next = G_freeThreads;
  G_freeThreads = thread;
  mutex_unlock(G_freeLock);
}

/* get a new thread from the free list */
static Thread* thread_getFree(){
  Thread* ret = NULL;

  mutex_lock(G_freeLock);
  if (G_freeThreads){
    ret = G_freeThreads;
    G_freeThreads = ret->next;
  }
  mutex_unlock(G_freeLock);
  return ret;
}

/* the thread proc that all created threads share */
static void thread_proc(void* arg){
  Thread* thread = (Thread*)arg;

  while (true){
    /* this acts as a wait for the main thread to unlock it */
    mutex_lock(thread->waitLock);

    /* call the thread func */
    thread->proc(thread->arg);

    /* put us on the waiting list */
    thread_addToFree(thread);
  }
}

/* called to initialize the threads system */
void yhi_thread_init(){
  G_freeLock = mutex_create();
}

/* allocate a new thread */
Thread* yhi_thread_create(ThreadProc proc, void* arg){
  Thread* ret;

  /* get an existing one if possible */
  ret = thread_getFree();
  if (ret){
    /* use existing */
    ret->proc = proc;
    ret->arg = arg;
    mutex_unlock(ret->waitLock);    /* release the lock to let it continue */
    return ret;
  }else{
    /* allocate new */
    ret = (Thread*)malloc(sizeof(Thread));
    ret->waitLock = mutex_create();
    ret->next = NULL;
    ret->proc = proc;
    ret->arg = arg;
    ret->thread = osthread_create(thread_proc, ret);
    return ret;
  }
}

/*---------------------------------------------------------------------------------------------------
   POSIX threads
 ----------------------------------------------------------------------------------------------------*/

#if USE_PTHREADS

/* create a new OS thread */
OSThread osthread_create(OSThreadProc proc, void* arg){
  OSThread thread;

  int rval = pthread_create(&thread, NULL, (void*)thread_proc, arg);
  assert(rval == 0); /* FIXME: what to do if it fails? */
  return thread;
}

/* create a new mutex */
Mutex mutex_create(){
  Mutex ret = (Mutex)malloc(sizeof(pthread_mutex_t));
  *ret = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
  return ret;
}

/* lock a mutex */
void mutex_lock(Mutex mutex){
  pthread_mutex_lock(mutex);
}

/* unlock a mutex */
void mutex_unlock(Mutex mutex){
  pthread_mutex_unlock(mutex);
}

/* free a mutex */
void mutex_destroy(Mutex mutex){
  free(mutex);
}

/* 1 = yes, -1 = no, 0 = untested */
int sem_init_supported = 0;

/* create a new semaphore */
/* see bug #9 for more details */
Semaphore yhi_semaphore_create(Int init){
  Semaphore ret;
  int rval;
  if (sem_init_supported != -1){
    ret = (Semaphore)malloc(sizeof(sem_t));
    rval = sem_init(ret, 0, init);
  }
  if (sem_init_supported == 0 && rval == -1 && errno == ENOSYS){
    sem_init_supported = -1;
  }
  if (sem_init_supported == -1){
    ret = sem_open("sem", O_CREAT, 0777, 0);
    sem_unlink("sem");
    assert(ret != SEM_FAILED);
  }else{
    assert(rval==0);
  }
  return ret;
}

/* signal a semaphore */
void yhi_semaphore_signal(Semaphore sem){
  int rval = sem_post(sem);
  assert(rval == 0);
}

/* wait on a semaphore */
void yhi_semaphore_wait(Semaphore sem){
  int rval = sem_wait(sem);
  assert(rval == 0);
}

/* zero a semaphore */
void yhi_semaphore_zero(Semaphore sem){
  /* this isn't especially nice ... but beggers can't be choosers */
  while (sem_trywait(sem) == 0){
    /* keep waiting */
  }
}

/* destroy a semaphore */
void yhi_semaphore_destroy(Semaphore sem){
  int rval;
  if (sem_init_supported == 1){
    rval = sem_destroy(sem);
    free(sem);
  }else{
    rval = sem_close(sem);
  }
  assert(rval == 0);
}

#elif defined(WIN32)

struct windows_thread_data{
  OSThreadProc proc;
  void* arg;
};

DWORD WINAPI windows_thread_start(LPVOID lpParameter){
  struct windows_thread_data* wtd = (struct windows_thread_data*) lpParameter;
  OSThreadProc proc = wtd->proc;
  void* arg = wtd->arg;
  free(wtd);
  proc(arg);
  return 0;
}

/* create a new OS thread */
OSThread osthread_create(OSThreadProc proc, void* arg){
  DWORD ThreadId;
  HANDLE hThread;

  struct windows_thread_data* wtd = (struct windows_thread_data*) malloc(sizeof(struct windows_thread_data));
  wtd->proc = proc;
  wtd->arg = arg;

  hThread = CreateThread(NULL, 0, windows_thread_start, wtd, 0, &ThreadId);
  assert(hThread != INVALID_HANDLE_VALUE);
  return hThread;
}

Mutex mutex_create(){
  HANDLE h = CreateSemaphore(NULL, 1, 1, NULL);
  assert(h != INVALID_HANDLE_VALUE);
  return h;
}

void mutex_lock(Mutex mutex){
  WaitForSingleObject(mutex, INFINITE);
}

void mutex_unlock(Mutex mutex){
  ReleaseSemaphore(mutex, 1, NULL);
}

void mutex_destroy(Mutex mutex){
  CloseHandle(mutex);
}

Semaphore yhi_semaphore_create(Int init){
  HANDLE h = CreateSemaphore(NULL, init, 65536, NULL);
  assert(h != INVALID_HANDLE_VALUE);
  return h;
}

void yhi_semaphore_signal(Semaphore sem){
  ReleaseSemaphore(sem, 1, NULL);
}

void yhi_semaphore_wait(Semaphore sem){
  WaitForSingleObject(sem, INFINITE);
}

void yhi_semaphore_zero(Semaphore sem){
  while (WaitForSingleObject(sem, 0) == WAIT_OBJECT_0)
    ; //nothing
}

void yhi_semaphore_destroy(Semaphore sem){
  CloseHandle(sem);
}

#else

OSThread osthread_create(OSThreadProc proc, void* arg){
  printf("Thread creation attempted, but yhi built with single threaded runtime, aborting\n");
  exit(1);
  return NULL;
}

/* dead mutex and semaphore functions */
Mutex mutex_create(){return NULL;}
void mutex_lock(Mutex mutex){}
void mutex_unlock(Mutex mutex){}
void mutex_destroy(Mutex mutex){}
Semaphore yhi_semaphore_create(Int init){return NULL;}
void yhi_semaphore_signal(Semaphore sem){}
void yhi_semaphore_wait(Semaphore sem){}
void yhi_semaphore_zero(Semaphore sem){}
void yhi_semaphore_destroy(Semaphore sem){}

#endif



