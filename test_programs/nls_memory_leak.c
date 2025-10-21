#include <stdlib.h>

typedef struct SLL {
    struct SLL *next;
    int data;
} SLL;

typedef struct NL {
    struct NL *top;
    struct SLL *next;
    int data;
} NL;

void construct_list(NL *s) {
    int __VERIFIER_nondet_int();
    while (__VERIFIER_nondet_int()) {
        s->top = malloc(1);
        if (s->top == NULL) {
            return;
        }

        s = s->top;
        s->next = NULL;
    }

    s->top = NULL;
    s->next = NULL;
}

void traverse_list(NL *s) {
    while (s != NULL) {
        int d = s->data;
        s = s->top;
    }
}

void free_list(NL *s) {
    while (s != NULL) {
        NL *next = s->top;
        free(s);
        s = next;
    }
}

int main() {
    NL *start = malloc(1);
    if (start == NULL) {
        return 1;
    }
    start->next = NULL;

    construct_list(start);
    traverse_list(start);
    free_list(start);

    start = malloc(1);
    if (start == NULL) {
        return NULL;
    }
    start->next = NULL;

    construct_list(start);
    traverse_list(start);
    // free_list(start);
}
