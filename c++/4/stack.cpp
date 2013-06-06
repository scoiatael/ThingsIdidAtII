#include "stack.hpp"
#include <iostream>
using namespace std;

stack::stack()
{
    current_size=0;
    current_capacity=2;
    tab=new double[current_capacity];
}

stack::stack( const stack& Kopia)
{
    current_size=Kopia.current_size;
    current_capacity=Kopia.current_capacity;
    tab=new double[current_capacity];
    for(int i=0; i<current_size;i++)
        tab[i]=Kopia.tab[i];
}

stack::~stack()
{
    delete[] tab;
}

void stack::operator = ( const stack& Kopia )
{
    current_size=Kopia.current_size;
    current_capacity=Kopia.current_capacity;
    reset(current_capacity);
    for(int i=0; i<current_size;i++)
        tab[i]=Kopia.tab[i];
}

void stack::push( double d)
{
   // cout << current_capacity << " " << current_size << endl;
    if(current_size>=current_capacity)
        ensure_capacity(current_capacity+1);
    //cout <<"--"<< current_capacity << " " << current_size << endl;
    tab[current_size]=d;
    current_size++;
}

void stack::reset( unsigned int s )
{
    if(s>current_capacity)
        return;
    double *tab=new double[s];
    if (s<current_size)
        current_size=s;
    for(int i=0;i<current_size;i++)
        tab[i]=this->tab[i];
    delete this->tab;
    this->tab=tab;
}

double stack::operator [ ] ( unsigned int i ) const
{
    if(i>=current_size)
        throw string("Wrong stack iterator");
    return tab[current_size-i-1];
}

double& stack::operator [ ] ( unsigned int i )
{
    if(i>=current_size)
        throw string("Wrong stack iterator");
    return tab[current_size-i-1];
}

double stack::top( ) const
{
    if(current_size>0)
        return tab[current_size-1];
}

double& stack::top( )
{
    if(current_size>0)
        return tab[current_size-1];
}

stack& stack::operator += ( const stack& s )
{
    ensure_capacity(current_size+s.current_size);
    for(int i=0; i<s.current_size;i++)
        tab[i+current_size]=s.tab[i];
    current_size+=s.current_size;
    return (*this);
}

stack operator + ( const stack& s1, const stack& s2 )
{
    stack nowy;
    nowy+=s1;
    nowy+=s2;
    return nowy;
}

bool operator == ( const stack& s1, const stack& s2 )
{
    if (s1.current_capacity != s2.current_capacity)
        return false;
    for(int i=0; i<s1.current_size; i++)
        if(s1.tab[i]!=s2.tab[i])
            return false;
    return true;
}
bool operator != ( const stack& s1, const stack& s2 )
{
    return !(s1==s2);
}

ostream& operator << ( ostream& a, const stack& s )
{
    a << "[";
    for(int i=0;i<s.current_size-1; i++)
     a << s.tab[s.current_size-i-1] <<", ";
    a <<s.tab[0]<<"]";
    return a;
}

