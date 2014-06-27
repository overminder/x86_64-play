#include "execute.h"

#if defined(_WIN32)
# define _USES_VIRTUALALLOC
#elif defined(__APPLE__) || defined(__posix)
# define _USES_MMAP
#else
# error Non-supported platform
#endif

#ifdef _USES_MMAP
# include <sys/mman.h>
#elif defined(_USES_VIRTUALALLOC)
# include <windows.h>
#endif

#include <stdio.h>
#include <string.h>

static int
pageAligned(int size) {
  int higher = size & ~4095;
  int lower = size & 4095;
  if (lower) {
    return higher + 4096;
  }
  return size;
}

static void *
allocateExecutableMemory(int size) {
#ifdef _USES_MMAP
  return mmap(NULL, size, PROT_EXEC | PROT_READ | PROT_WRITE,
      MAP_ANON | MAP_PRIVATE, -1, 0);
#else
  return VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT,
    PAGE_EXECUTE_READWRITE);
#endif
}

static void
deallocateMemory(void *addr, int size) {
#ifdef _USES_MMAP
  munmap(addr, size);
#else
  VirtualFree(addr, size, MEM_RELEASE);
#endif
}

typedef void *(*Entry)(void *);

void *
buildAndExecute(void *code, int size, void *arg) {
  int bufSize = pageAligned(size);
  void *buf = allocateExecutableMemory(bufSize);
  memcpy(buf, code, size);

  void *result = ((Entry) buf)(arg);
  deallocateMemory(buf, bufSize);

  return result;
}

