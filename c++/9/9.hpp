#include <iostream>

template< typename A, typename B > class unionof;
template< typename A, typename B >
std::ostream& operator << ( std::ostream& stream, const unionof< A, B > & );

template< typename A, typename B >
class unionof
{
    A* a;
    B* b;
    // Invariant: Exactly one of them is non-zero.
    public:
        unionof( const A& a );
        unionof( const B& b );
        unionof( const unionof& u );
        void operator = ( const A& a );
        void operator = ( const B& b );
        void operator = ( const unionof& u );
        const A& first( ) const;
        A& first( );
        const B& second( ) const;
        B& second( );
        bool hasfirst( ) const;
        bool hassecond( ) const;
        ~unionof( );
        friend std::ostream& operator << < > ( std::ostream& stream, const unionof< A, B > & );
};

template< typename A, typename B >
std::ostream& operator << ( std::ostream& stream, const unionof< A, B > & u );
