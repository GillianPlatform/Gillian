#include "list_full.h"

struct list_s {
    size_t size;
    Node *head;
    Node *tail;
};

size_t list_contains(List *list, void *element) {
    Node *node = list->head;
    size_t e_count = 0;

    while (node) {
        if (node->data == element)
            e_count++;
        node = node->next;
    }
    return e_count;
}