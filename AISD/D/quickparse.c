#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, const char *argv[])
{
  int count = strtol(argv[1],NULL, 10);
  int test_nr;
  char buf[40];
  fgets(buf, 40, stdin);
  sscanf(buf, "%d", &test_nr);
  int i,a,b;
  for (i = 0; i < count-1; i++) {
    int j;
    fgets(buf, 40, stdin);
    sscanf(buf,"%d %d", &a, &b);
    for (j = 0; j < a+b; j++) {
      fgets(buf, 40, stdin);
    }
    fgets(buf, 40, stdin);
  }
  fgets(buf, 40, stdin);
  sscanf(buf, "%d %d", &a, &b);
  printf("%d %d\n", a, b);
  for (i = 0; i < a+b; i++) {
    fgets(buf, 40, stdin);
    if(strlen(buf)>0)
      fputs(buf, stdout); 
  }
  fgets(buf, 40, stdin);
  fputs(buf, stdout); 
  return 0;
}
