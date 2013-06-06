#include "BST.h"
#include <stdio.h>


void wstaw(korzen *tree, str el)
{
    //printf("%d ", el);
    if((*tree)==NULL)
    {
      //  putchar('a');
        korzen nowy = malloc(sizeof(node));
        nowy->count=1;
        nowy->val=el;
        nowy->syn1=NULL;
        nowy->syn2=NULL;
        *tree = nowy;
        return;
    }
    if((*tree)->val==el)
    {
        //putchar('b');
        (*tree)->count++;
        return;
    }
    if((*tree)->val>el)
    {
        //putchar('c');
        wstaw(&((*tree)->syn1), el);
        return;
    }
    //putchar('d');
    wstaw(&((*tree)->syn2), el);
    return;
}

int find(korzen tree, str el)
{
    if(tree==NULL)
        return -1;
    if(tree->val==el)
        return tree->count;
    if(tree->val>el)
        return find(tree->syn1, el);
    return find(tree->syn2, el);
}

void print1(korzen tree)
{
    if(tree==NULL)
        return;
    print1(tree->syn1);
    printf("%d ", tree->val);
    print1(tree->syn2);
    return;
}


int main()
{
    printf("ile liczb?: ");
    int n;
    korzen t1, *tree;
    tree=malloc(sizeof(korzen));
    *tree=NULL;
    scanf("%d", &n);
    for(int i=0; i<n;i++)
    {
        int nowy;
        scanf("%d", &nowy);
        wstaw(tree, nowy);
    }
    print1(*tree);
    printf("\n");
    printf("co znalezc?:");
    int szuk;
    scanf("%d", &szuk);
    szuk=find(*tree,szuk);
    if(szuk==-1)
        printf("not found\n");
    else
        printf("wystepuje %d raz(y)", szuk);
    return 0;
}
