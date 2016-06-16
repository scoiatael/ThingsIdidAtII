#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#define isChar(X) (X >= 'A' && X <= 'H')
#define toSetIter(X) (X-'A')
#define forEachChar(X, Y) { for(X='A'; X<='H';X++) { Y; }}
#define True 1
#define False 0

//#define DEBUG

#ifndef DEBUG
#define NDEBUG
#endif

#include <assert.h>

#ifdef DEBUG
#include <unistd.h>
#endif

typedef struct __setofchar__ {
  unsigned char table;
} setOfChars;

typedef struct __chomskygrammar__ {
  setOfChars fstType[26];
  int rule_nr[8];
  struct {
    char fst;
    char snd;
  } table[8][64];
} chGrammar;

void print_setOfChars(setOfChars* arg)
{
  int i;
  forEachChar(i, printf("%d ", ((arg->table) >> toSetIter(i)) & 1););
  printf("|");
}

void print_chGrammar(chGrammar* arg)
{
  assert(arg != NULL);
  printf("fstType:\n");
  int i, j;
  for (j = 0; j < 26; j++) {
    printf("%c: ", j+'a');
    print_setOfChars(&arg->fstType[j]);
    printf("\n");
  }
  printf("sndType:\n");
  forEachChar(i, 
    for (j = 0; j < arg->rule_nr[toSetIter(i)]; j++) {
      printf("%c -> %c %c\n", arg->table[toSetIter(i)][j].snd, i, arg->table[toSetIter(i)][j].fst);
    });
}

char charInSet(setOfChars* set, char arg)
{
  assert(isChar(arg));
  assert(set != NULL);
  return ((set->table) >> (toSetIter(arg))) & 1;
}

void addCharToSet(setOfChars* set, char arg)
{
  assert(isChar(arg));
  assert(set != NULL);
  set->table = (set->table) | 1 << (toSetIter(arg));
}

void clearSet(setOfChars* set)
{
  set->table = 0;
}

char charInGrammar(chGrammar* arg, char a, char b)
{
  assert(isChar(a));
  return charInSet(&arg->fstType[b - 'a'], a);
}

int load_chGrammar(chGrammar* arg, FILE* file)
{
  setOfChars ntermsGenerated;
  clearSet(&ntermsGenerated);
  addCharToSet(&ntermsGenerated, 'A');
  errno = 0;
  assert(arg != NULL);
  int j;
  char i, A, B, C;
  char buffer[512];
  for (j = 0; j < 26; j++) {
    clearSet(&arg->fstType[j]);
  }
  forEachChar(i, arg->rule_nr[toSetIter(i)]=0);
  int fstnr, sndnr;
  fgets(buffer, 512, file);
  sscanf(buffer, "%d %d", &fstnr, &sndnr);
  for (i = 0; i < fstnr; i++) {
    fgets(buffer, 512, file);
    sscanf(buffer, "%c %c %c", &A, &B, &C);
    arg->table[toSetIter(B)][arg->rule_nr[toSetIter(B)]].fst = C;
    arg->table[toSetIter(B)][arg->rule_nr[toSetIter(B)]].snd = A;
    arg->rule_nr[toSetIter(B)]+=1;
#ifdef DEBUG
    printf("%c %c %c\n", A, B, C);
#endif
    addCharToSet(&ntermsGenerated, B);
    addCharToSet(&ntermsGenerated, C);
  }
  for (i = 0; i < sndnr; i++) {
    fgets(buffer, 512, file);
    sscanf(buffer, "%c %c", &A, &B);
#ifdef DEBUG
    printf("%c %c\n", A, B);
#endif
    if(charInSet(&ntermsGenerated, A)==True) {
      addCharToSet(&arg->fstType[B - 'a'], A);
    }
  }
#ifdef DEBUG
  printf("Done loading\n");
#endif
  return errno;
}


//setA `zip` setB = setC
void zipSetsWithGrammar(chGrammar* grammar, setOfChars* setA, setOfChars* setB, setOfChars* setC)
{
  assert(grammar != NULL && setA != NULL && setB != NULL && setC != NULL);
  int i,j;
  forEachChar(i, 
    if(charInSet(setA, i) == True) {
      for (j = 0; j < grammar->rule_nr[toSetIter(i)]; j++) {
        if(charInSet(setB, grammar->table[toSetIter(i)][j].fst) == True) {
          addCharToSet(setC, grammar->table[toSetIter(i)][j].snd);
        }
      }
    });
}

void clearAuxTable(setOfChars arg[512][512], int wrdlen)
{
  int i, j;
  for (i = 0; i < wrdlen; i++) {
    for (j = i; j < wrdlen; j++) {
      clearSet(&arg[i][j]);
    }
  }
}

char initAuxTable(setOfChars arg[512][512], chGrammar* grammar, char wrd[512], int wrdlen)
{
  int i;
  for (i = 0; i < wrdlen; i++) {
    arg[i][i] = grammar->fstType[wrd[i]-'a'];
    if(arg[i][i].table == 0)
      return False;
  }
  return True;
}

setOfChars __AuxTable__[512][512];
char word_chGrammar(chGrammar* grammar, char word[512], int wordlength)
{
#ifdef DEBUG
  printf("Starting search..\n");
  print_chGrammar(grammar);
#endif
  clearAuxTable(__AuxTable__, wordlength);
  if(initAuxTable(__AuxTable__, grammar, word, wordlength)==False) return False;
  int x, y, z;
  for (y = 1; y < wordlength; y++) {
    for(x = 0; x < wordlength-y; x++) {
      for (z = 0; z < y; z++) {
        zipSetsWithGrammar(grammar, &__AuxTable__[x][x + z], &__AuxTable__[x + z + 1][x + y], &__AuxTable__[x][x + y]);
      }
    }
  }
  if(charInSet(&__AuxTable__[0][wordlength - 1], 'A') == True) {
    return True;
  }
  return False;
}

chGrammar mainGrammar;
int main(int argc, const char *argv[])
{
  int testNr, testi;
  char buffer[512];
  scanf("%d", &testNr);
  fgets(buffer, 512, stdin);
  for (testi = 0; testi < testNr; testi++) {
    int errno;
    char word[512];
    if(load_chGrammar(&mainGrammar, stdin) != 0) {
      char* errstr = strerror(errno);
      fprintf(stderr, "Error occured: %s\n", errstr);
      exit(errno);
    }
    fgets(buffer, 512, stdin);
    sscanf(buffer, "%s", word);
#ifdef DEBUG
    printf("Searching for %s..\n", word);
#endif
    if(word_chGrammar(&mainGrammar, word, strlen(word)) == True) {
      printf("TAK\n");
    } else {
      printf("NIE\n");
    }
#ifdef DEBUG
    sleep(1);
#endif
  }
  return 0;
}
