#include <string.h>

void unsafeFunction() {
    char buffer[5];
    strcpy(buffer, "Helo"); // Buffer overflow occurs here
}