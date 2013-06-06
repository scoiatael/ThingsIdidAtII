#include "9.hpp"

template< typename A, typename B >
unionof<A,B>::unionof( const A& a )
   : a(new A(a)), b(NULL)
{ }

template< typename A, typename B >
unionof<A,B>::unionof( const B& b )
{
    this->b = new B(b);
    this->a = NULL;
}

template< typename A, typename B >
unionof<A,B>::unionof( const unionof& u )
{
    if(u.hasfirst())
        this->a = new A(*(u.a));
    else
        this->a = NULL;
    if(u.hassecond())
        this->b = new B(*(u.b));
    else
        this->b = NULL;
}

template< typename A, typename B >
void unionof<A,B>::operator = ( const A& a )
{
    delete(this->a);
    delete(this->b);
    this->a = new A(a);
    this->b = NULL;
}

template< typename A, typename B >
void unionof<A,B>::operator = ( const B& b )
{
    delete(this->b);
    delete(this->a);
    this->b = new B(b);
    this->a = NULL;
}

template< typename A, typename B >
void unionof<A,B>::operator = ( const unionof& u )
{
    delete(this->b);
    delete(this->a);
    if(u.hassecond())
        this->b = new B(*(u.b));
    else
        this->b = NULL;
    if(u.hasfirst())
        this->a = new A(*(u.a));
    else
        this->a = NULL;
}

template< typename A, typename B >
const A& unionof<A,B>::first( ) const
{
    if (a!=NULL)
        return (*a);
    throw "NULL pointer exception (first argument of unionof)";
}

template< typename A, typename B >
A& unionof<A,B>::first( )
{
    if (a!=NULL)
        return (*a);
    throw "NULL pointer exception (first argument of unionof)";
}

template< typename A, typename B >
const B& unionof<A,B>::second( ) const
{
    if (b!=NULL)
        return (*b);
    throw "NULL pointer exception (second argument of unionof)";
}

template< typename A, typename B >
B& unionof<A,B>::second( )
{
    if (b!=NULL)
        return (*b);
    throw "NULL pointer exception (second argument of unionof)";
}

template< typename A, typename B >
bool unionof<A,B>::hasfirst( ) const
{
    return a!=NULL;
}

template< typename A, typename B >
bool unionof<A,B>::hassecond( ) const
{
    return b!=NULL;
}

template< typename A, typename B >
unionof<A,B>::~unionof( )
{
    delete(this->a);
    delete(this->b);
}

template< typename A, typename B >
std::ostream& operator << ( std::ostream& stream, const unionof< A, B > & U)
{
    if(U.hasfirst())
    {
        stream << *(U.a);
        return stream;
    }
    if(U.hassecond())
    {
        stream << *(U.b);
        return stream;
    }
}


int main()
{
    unionof<int, double>C(10);

    for(int i=0; i<10000000; i++)
    {
        //std::cout<< i << std::endl;
        unionof<int, double> D(C);
        unionof<int, double> A(5);
        unionof<int, double> B(6.7);
        A = C;
        B = 5;
        C = A;
    }
    std::cout << C << "\n";
}

