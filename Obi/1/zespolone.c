#include <stdio.h>
#include "zespolone.h"

ZESP dodaj1(ZESP a,ZESP b)
{
    ZESP wynik;
    wynik.re=a.re+b.re;
    wynik.im=a.im+b.im;
    return wynik;
}

ZESP odejmij1(ZESP a,ZESP b)
{
    ZESP wynik;
    wynik.re=a.re-b.re;
    wynik.im=a.im-b.im;
    return wynik;
}

ZESP pomnoz1(ZESP a ,ZESP b)
{
    ZESP wynik;
    wynik.re=a.re*b.re - b.im*a.im;
    wynik.im=a.im*b.re + b.im*a.re;
    return wynik;
}

ZESP podziel1(ZESP a,ZESP b)
{
    ZESP wynik;
    wynik.re=(a.re*b.re + b.im*a.im)/(b.re*b.re + b.im*b.im);
    wynik.im=(a.im*b.re - b.im*a.re)/(b.re*b.re + b.im*b.im);
    return wynik;
}


void dodaj2(ZESP a,ZESP b,ZESP* wynik)
{
    (*wynik)=dodaj1(a,b);
}

void odejmij2(ZESP a,ZESP b,ZESP* wynik)
{
    (*wynik)=odejmij1(a,b);
}
void pomnoz2(ZESP a,ZESP b,ZESP* wynik)
{
    (*wynik)=pomnoz1(a,b);
}
void podziel2(ZESP a,ZESP b,ZESP* wynik)
{
    (*wynik)=podziel1(a,b);
}

int main()
{
    ZESP a,b ,*wynik;
    printf("podaj 2 liczby zespolone (postaci re(a) im(a))\n");
    scanf("%f %f %f %f", &a.re, &a.im, &b.re, &b.im);
    dodaj2(a,b,wynik);
    printf("dodaj: %2f+(%2f)i\n", wynik->re, wynik->im);
    odejmij2(a,b,wynik);
    printf("odejmij: %2f+(%2f)i\n", wynik->re, wynik->im);
    pomnoz2(a,b,wynik);
    printf("pomnoz: %2f+(%2f)i\n", wynik->re, wynik->im);
    if(b.re!=0 || b.im!=0)
    {
        podziel2(a,b,wynik);
        printf("podziel: %2f+(%2f)i\n", wynik->re, wynik->im);
    }
    return 0;
}
