#include "declaration.hpp"
#include <cmath>

const double PI = 4 * std::atan(1);

double dis(const double& Ax, const double& Ay,const double& Bx, const double& By)
{
    return sqrt(pow((Ax-Bx),2)+pow((Ay-By),2));
}

double rectangle::area( ) const
{
    return std::abs((x2-x1)*(y2-y1));
}

double rectangle::circumference( ) const
{
    return std::abs((x2-x1)*2)+std::abs(2*(y2-y1));
}

rectangle* rectangle::clone( ) const
{
    rectangle* x = new rectangle(x1, y1, x2,y2);
    return x;
}

rectangle::rectangle(const double& a1,const double& b1,const double& a2,const double& b2)
: x1(a1), y1(b1), x2(a2), y2(b2)
{}

void rectangle::print( std::ostream& Out) const
{
    Out<<"Rectangle: (" <<x1<<", "<<y1<<"), {"<<x2<<", "<<y2<<")";
}

double triangle::area( ) const
{
    return std::abs((x1-x3)*(y2-y1)-(x1-x2)*(y3-y1))/2;
}

double triangle::circumference( ) const
{
    return dis(x1,y1,x2,y2)+dis(x1,y1,x3,y3)+dis(x2,y2,x3,y3);
}

triangle* triangle::clone( ) const
{
    triangle* x = new triangle(x1,y1,x2,y2,x3,y3);
    return x;
}

void triangle::print( std::ostream& Out) const
{
    Out<<"Triangle: (" <<x1<<", "<<y1<<"), {"<<x2<<", "<<y2<<"), {"<<x3<<", "<<y3<<")";
}

triangle::triangle(const double& a1,const double& b1,const double& a2,const double& b2,const double& a3,const double& b3)
: x1(a1),x2(a2),x3(a3),y1(b1),y2(b2),y3(b3)
{}

double circle::area( ) const
{
    return PI*pow(radius,2);
}

double circle::circumference( ) const
{
    return PI*2*radius;
}

circle* circle::clone( ) const
{
    circle* a = new circle(x,y,radius);
    return a;
}

circle::circle(const double& a,const double& b,const double& r)
: x(a),y(b),radius(r)
{}

void circle::print( std::ostream& Out) const
{
    Out<<"Circle: (" <<x<<", "<<y<<"), "<<radius;
}

surface::surface( const surface& s )
: ref( s. ref -> clone( ))
{ }
surface::surface( const surf& s )
: ref( s. clone( ))
{ }

void surface::operator = ( const surface& s )
{
    if( ref != s. ref )
        {
        delete ref;
        ref = s. ref -> clone( );
        }
}

surface::~surface( )
{
    delete ref;
}

const surf& surface::getsurf( ) const { return *ref; }

std::ostream& operator << ( std::ostream& stream, const surface& s )
{
    (s.getsurf()).print(stream);
    return stream;
}

std::ostream& operator << ( std::ostream& stream,const std::vector< surface > & table )
{
    for( unsigned int i = 0; i < table. size( ); ++ i )
    {
        stream << i << "-th element = " << table [i] << "\n";
    }
    return stream;
}

void print_statistics( const std::vector< surface > & table )
{
    double total_area = 0.0;
    double total_circumference = 0.0;
    for( std::vector< surface > :: const_iterator p = table. begin( ); p != table. end( ); ++ p )
    {
        total_area += p -> getsurf( ). area( );
        total_circumference += p -> getsurf( ). circumference( );
        std::cout << "adding info about " << *p << "\n";
        std::cout << "Area: "<<p -> getsurf( ). area( )<<" Circumference: "<< p -> getsurf( ). circumference( ) <<"\n";
    }
    std::cout << "total area is " << total_area << "\n";
    std::cout << "total circumference is " << total_circumference << "\n";
}
