#pragma once

#ifdef __cplusplus
extern "C"
{
#endif

void * solite_create();

void solite_destroy(void * solite);

int solite_add_source(void * solite, const char * name, const char * source);

int solite_add_library(void * solite, const char * name, const char * address);

int solite_compile(void * solite, int optimize);

char * solite_abi(void * solite, const char * name);

char * solite_binary(void * solite, const char * name);

char * solite_errors(void * solite);

#ifdef __cplusplus
}
#endif
