#include "stdlib.h"

typedef struct List {
    struct List *next;
    int data;
} List;

int main() {
    // List *start = malloc(sizeof(List));
    List *start = malloc(sizeof(List));

    if (start == NULL)
        return 1;

    { // construct a linked list of unknown size
        List *list = start;
        list->next = NULL;

        int __VERIFIER_nondet_int();
        while (__VERIFIER_nondet_int()) {
            List *next = malloc(sizeof(List));
            if (next == NULL)
                return 1;

            list->next = next;
            list = list->next;
        }

        list->next = NULL;
    }

    { // walk to the end of the list
        List *list = start;

        while (list != NULL) {
            List *next = NULL;
            next = list->next;

            list->data = 42;
            list = next;
        }
    }

    { // free the list
        List *list = start;

        while (list != NULL) {
            List *next = NULL;
            next = list->next;

            free(list);
            list = next;
        }
    }
}
