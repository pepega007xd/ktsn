#include <stdlib.h>

typedef struct SLL {
    struct SLL *next;
    int data;
} SLL;

SLL *construct_list() {
    SLL *start = malloc(1);
    if (start == NULL) {
        return NULL;
    }

    SLL *node = start;

    int __VERIFIER_nondet_int();
    while (__VERIFIER_nondet_int()) {
        node->next = malloc(1);
        if (node->next == NULL) {
            break;
        }

        node = node->next;
    }

    node->next = start;

    return start;
}

void traverse_list(SLL *s) {
    if (s == NULL)
        return;

    SLL *node = s;

    do {
        int d = node->data;
        node = node->next;
    } while (node != s);
}

void free_list(SLL *s) {
    if (s == NULL)
        return;

    SLL *node = s;

    do {
        SLL *next = node->next;
        free(node);
        node = next;
    } while (node != s);
}

int main() {

    SLL *list = construct_list();
    traverse_list(list);
    free_list(list);

    list = construct_list();
    traverse_list(list);
    free_list(list);
}
