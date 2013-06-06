
#ifndef SOLVE_INCLUDED
#define SOLVE_INCLUDED 1

#include "cube.h"
#include "sequence.hpp"
#include <map>
#include <queue>


struct cube_cmp
{
   bool operator( ) ( const cube& c1, const cube& c2 ) const;
};


unsigned int cube::distance( ) const
{
   unsigned int dist = 0;

   // We give a high weight to unsolved colors in the bottom:

   for( unsigned int i = 0; i < size; ++ i )
   {
      for( unsigned int j = 0; j < size; ++ j )
      {
         if( (*this)[ down ] [i] [j] != solvedcolor( down ))
         {
            dist += ( size + 1 ) * ( size + 1 ) * ( size + 1 );
         }
      }
   }

   for( unsigned int i = 0; i < size; ++ i )
   {
      // Lower (closer to bottom) gets higher weight:

      for( unsigned int j = 0; j < size; ++ j )
      {
         if( (*this) [ left ] [i] [j] != solvedcolor( left ))
            dist += (i+1)*(i+1)*(i+1);
         if( (*this) [ front ] [i] [j] != solvedcolor( front ))
            dist += (i+1)*(i+1)*(i+1);
         if( (*this) [ right ] [i] [j] != solvedcolor( right ))
            dist += (i+1)*(i+1)*(i+1);
         if( (*this) [ back ] [i] [j] != solvedcolor( back ))
            dist += (i+1)*(i+1)*(i+1);
      }
   }

   // We don't look at the top, because the top is automatically solved
   // when the other surfaces are solved.

   return dist;
}


// pq is an object that we put in a priority queue. It contains
// an std::map< cube, sequence > :: iterator.
// It strongly relies on the fact that the object that the iterator
// points to, is indeed in the map. It also relies on the fact that
// std::map< > never moves objects.

struct pq
{
   std::map< cube, sequence, cube_cmp > :: iterator it;
   unsigned int dist;

   pq( std::map< cube, sequence, cube_cmp > :: iterator & it )
      : it( it ),
        dist( it -> first. distance( ))
   { }

   // Smaller distance is more priority:

   bool operator < ( const pq& other ) const
   {
      return dist > other. dist;
   }

};

std::ostream& operator << ( std::ostream& , const pq& );

cube example3( );
   // Gives an example 3 cube.


sequence solve( const cube& );
   // (Hopefully) solve the cube.

#endif

