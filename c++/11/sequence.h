
#ifndef SEQUENCE_INCLUDED
#define SEQUENCE_INCLUDED  1

// A sequence is a sequence of cube rotations:

#include "rotation.h"
#include <vector>
#include <initializer_list>


struct sequence
{

   std::vector< rotation > rot;

   // This will be standard in C++ 11, and it can be used already now
   // with gcc, if you compile with option -std=c++0x

   sequence(const std::initializer_list< rotation > &init )
   {
      for( auto p = init.begin( );
                p != init.end( );
                ++ p )
      {
         rot. push_back( *p );
      }
   }

   sequence( ) { }

   sequence inverse( ) const;

   int length ( ) const;

};

sequence repeat( const sequence& seq, unsigned int i );

sequence operator * ( const sequence& seq1, const sequence& seq2 );

sequence operator * ( const sequence& seq1, const rotation& rotToAdd );

std::ostream& operator << ( std::ostream& stream, const sequence& seq );


#endif

