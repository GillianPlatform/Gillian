// Debugging bi-abduction issue

// ----------------------------------------------------------
// WORKS

// Reading the value
void getValue(int *ptr) { int x = *ptr; }

// Writing to the ptr (update ptr value)
void assignValue(int *ptr) { *ptr = 5; }

// ----------------------------------------------------------
// DOESN'T WORK
// In summary - it seems like any operation which uses the fixed resource after
// bi-abduction results in unification plan failure.
// >> So I think the issue is with the fix we applied in bi-abduction.

// Unification plan failure
void readAndRead(int *ptr) {
    int x = *ptr; // (Load operation)
    int y = *ptr; // (Load operation)
}

void writeAndWrite(int *ptr) {
    *ptr = 5; // (Store operation) Writing to the ptr
    *ptr = 6; // (Store operation) Writing to the ptr
}

void writeAndRead(int *ptr) {
    *ptr = 5;     // (Store operation) Writing to the ptr
    int x = *ptr; // (Load operation)
}