typedef int str;

typedef struct node* korzen;

typedef struct node
{
    str val;
    int count;
    korzen syn1;
    korzen syn2;
} node;


void wstaw(korzen *tree, str el);
int find(korzen tree, str el);
