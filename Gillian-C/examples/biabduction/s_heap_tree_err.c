#include <string.h>

void unsafeFunction() {
    char buffer[5];
    strcpy(buffer, "Hello World"); // Buffer overflow occurs here
}