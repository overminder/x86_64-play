#ifndef EXECUTE_H
#define EXECUTE_H

#include <stddef.h>

/* Copies code into a newly-allocated executable memory region,
 * call into the code with *arg, and returns the result. */
void *buildAndExecute(void *code, int size, void *arg);

#endif /* EXECUTE_H */
