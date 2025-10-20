#include <stdlib.h>

typedef struct DLL {
    struct DLL *next;
    struct DLL *prev;
    int data;
} DLL;

void construct_list(DLL *s) {
    int __VERIFIER_nondet_int();
    while (__VERIFIER_nondet_int()) {
        DLL *next = malloc(1);
        if (next == NULL) {
            s->next = NULL;
            return;
        }

        next->prev = s;
        s->next = next;
        s = s->next;
    }

    s->next = NULL;
}

void traverse_list(DLL *s) {
    while (s != NULL) {
        int d = s->data;
        s = s->next;
    }
}

void free_list(DLL *s) {
    while (s != NULL) {
        DLL *next = s->next;
        free(s);
        s = next;
    }
}

int main() {
    DLL *start = malloc(1);
    if (start == NULL) {
        return NULL;
    }

    construct_list(start);
    traverse_list(start);
    free_list(start);

    start = malloc(1);
    if (start == NULL) {
        return NULL;
    }

    construct_list(start);
    traverse_list(start);
    free_list(start);
}
