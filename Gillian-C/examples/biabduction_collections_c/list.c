#include "list_full.h"

struct list_s {
    size_t size;
    Node *head;
    Node *tail;
};

int list_len(List *list) {
    if (list == NULL) {
        return 0;
    }
    int length = 0;
    for (int i = 0; i < list->size; i++) {
        length++;
    }
    return length;
}