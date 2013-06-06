#include <iostream>
#include <cmath>
#include <vector>
class surf
{
    public:
        virtual double area( ) const = 0;
        virtual double circumference( ) const = 0;
        virtual surf* clone( ) const = 0;
        virtual void print( std::ostream& ) const = 0;
        ~surf( ){}
};

class rectangle : public surf
{
    double x1, y1;
    double x2, y2;
    public:
        double area( ) const;
        double circumference( ) const;
        rectangle* clone( ) const;
        rectangle(const double&,const double&,const double&,const double&);
        void print( std::ostream& ) const;
        ~rectangle(){}
};

class triangle : public surf
{
    double x1, y1; // Positions of corners.
    double x2, y2;
    double x3, y3;
    public:
        double area( ) const;
        double circumference( ) const;
        triangle* clone( ) const;
        void print( std::ostream& ) const;
        triangle(const double&,const double&,const double&,const double&,const double&,const double&);
        ~triangle(){}
};

class circle : public surf
{
    double x; // Position of center.
    double y;
    double radius;
    public:
        double area( ) const;
        double circumference( ) const;
        circle* clone( ) const;
        circle(const double&,const double&,const double&);
        void print( std::ostream& ) const;
        ~circle(){}
};

struct surface
{
    surf* ref;
    public:
        surface( const surface& s );
        surface( const surf& s );
        void operator = ( const surface& s );
        ~surface( );
        const surf& getsurf( ) const;
};

std::ostream& operator << ( std::ostream& stream, const surface& s );


std::ostream& operator << ( std::ostream& stream,const std::vector< surface > & table );
