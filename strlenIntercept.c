#include <string.h>
#include <stdio.h>

void cf_strlen(char* str, int* out) {
	size_t len = strlen(str);
	printf("strlen intercept. Pointer passed in is %p. String is %s, at address %p, with length %ld.\n", out, str, str, len);
	*out = (int) len;
}
