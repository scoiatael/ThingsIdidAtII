#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

struct { char table[6]; } moja_str;

int main()
{
  int i;
  for (i = 0; i < 1000; i++) {
    if(moja_str.table[0] != 'c'){
      printf("Bad.\n");
      moja_str.table[0] = 'c';
    }
    sleep(1);
  }
}
