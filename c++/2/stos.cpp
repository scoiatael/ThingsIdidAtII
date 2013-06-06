#include "stos.hpp"
#include <iostream>
using namespace std;
stos::stos()
{
    size=1;
    cur_size=0;
    mem = new double[1];
}

stos::stos(const int &Giv)
{
    size=Giv+1;
    cur_size=0;
    mem = new double[Giv+1];
}

stos::stos( const stos &Giv)
{
    size=Giv.size;
    cur_size=Giv.cur_size;
    mem = new(double[Giv.size]);
    for(int i=0;i<size;i++)
    {
        mem[i]=Giv.mem[i];
    }
}

stos::~stos()
{
    cur_size=0;
}

int stos::get_cur_size()
{
    return cur_size;
}

double stos::pop()
{
    if(cur_size<1)
        throw string("Stack is empty");
    cur_size--;
    return mem[cur_size];
}

double stos::top()
{
    if(cur_size<1)
        throw string("Stack is empty");
    return mem[cur_size-1];
}

void stos::push(const double &Giv)
{
    if(cur_size==size)
        throw string("Stack overflow");
    mem[cur_size]=Giv;
    cur_size++;
}
