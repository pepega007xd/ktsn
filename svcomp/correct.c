#include <stdlib.h>

typedef struct SLL {
    struct SLL *next;
    int data;
} SLL;

void construct_list(SLL *s) {
    int nondeterministic;
    while (nondeterministic) {
        s->next = malloc(1);
        if (s->next == NULL) {
            return;
        }

        s = s->next;
    }

    s->next = NULL;
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
    SLL *start = malloc(1);
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
