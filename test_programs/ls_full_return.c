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

    SLL *s = start;
    int __VERIFIER_nondet_int();
    while (__VERIFIER_nondet_int()) {
        s->next = malloc(1);
        if (s->next == NULL) {
            break;
        }

        s = s->next;
    }

    s->next = NULL;
    return start;
}

void traverse_list(SLL *s) {
    while (s != NULL) {
        int d = s->data;
        s = s->next;
    }
}

void free_list(SLL *s) {
    while (s != NULL) {
        SLL *next = s->next;
        free(s);
        s = next;
    }
}

int main() {
    SLL *list = construct_list();
    traverse_list(list);
    free_list(list);
}
