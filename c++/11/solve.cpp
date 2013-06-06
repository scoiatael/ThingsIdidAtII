
#include "solve.h"
#include "assert.h"


bool cube_cmp::operator( ) ( const cube& c1, const cube& c2 ) const
{
   ASSERT( c1. size == c2. size );

   auto p1 = c1. sides. begin( );
   auto p2 = c2. sides. begin( );
   while( p1 != c1. sides. end( ))
   {
      if( *p1 > *p2 ) return false;
      if( *p1 < *p2 ) return true;

      ++ p1;
      ++ p2;
   }

   return false;
}


std::ostream& operator << ( std::ostream& stream, const pq& p )
{
   stream << ( p. it -> first ) << "\n";
   stream << "distance = " << p. dist << "\n";

   return stream;
}


// State of my cube on may 24th 2012, 10.02.

cube example3( )
{
   cube res = cube(3);

   res [ cube::down ] [0] [0] = cube::green;
   res [ cube::down ] [0] [1] = cube::red;
   res [ cube::down ] [0] [2] = cube::blue;
   res [ cube::down ] [1] [0] = cube::green;
   res [ cube::down ] [1] [1] = cube::white;
   res [ cube::down ] [1] [2] = cube::white;
   res [ cube::down ] [2] [0] = cube::blue;
   res [ cube::down ] [2] [1] = cube::orange;
   res [ cube::down ] [2] [2] = cube::orange;

   res [ cube::left ] [0] [0] = cube::blue;
   res [ cube::left ] [0] [1] = cube::red;
   res [ cube::left ] [0] [2] = cube::red;
   res [ cube::left ] [1] [0] = cube::green;
   res [ cube::left ] [1] [1] = cube::orange;
   res [ cube::left ] [1] [2] = cube::white;
   res [ cube::left ] [2] [0] = cube::red;
   res [ cube::left ] [2] [1] = cube::orange;
   res [ cube::left ] [2] [2] = cube::orange;

   res [ cube::front ] [0] [0] = cube::yellow;
   res [ cube::front ] [0] [1] = cube::blue;
   res [ cube::front ] [0] [2] = cube::orange;
   res [ cube::front ] [1] [0] = cube::red;
   res [ cube::front ] [1] [1] = cube::yellow;
   res [ cube::front ] [1] [2] = cube::white;
   res [ cube::front ] [2] [0] = cube::white;
   res [ cube::front ] [2] [1] = cube::blue;
   res [ cube::front ] [2] [2] = cube::red;

   res [ cube::right ] [0] [0] = cube::white;
   res [ cube::right ] [0] [1] = cube::white;
   res [ cube::right ] [0] [2] = cube::red;
   res [ cube::right ] [1] [0] = cube::orange;
   res [ cube::right ] [1] [1] = cube::red;
   res [ cube::right ] [1] [2] = cube::orange;
   res [ cube::right ] [2] [0] = cube::yellow;
   res [ cube::right ] [2] [1] = cube::yellow;
   res [ cube::right ] [2] [2] = cube::blue;

   res [ cube::back ] [0] [0] = cube::white;
   res [ cube::back ] [0] [1] = cube::yellow;
   res [ cube::back ] [0] [2] = cube::orange;
   res [ cube::back ] [1] [0] = cube::blue;
   res [ cube::back ] [1] [1] = cube::green;
   res [ cube::back ] [1] [2] = cube::blue;
   res [ cube::back ] [2] [0] = cube::yellow;
   res [ cube::back ] [2] [1] = cube::yellow;
   res [ cube::back ] [2] [2] = cube::green;

   res [ cube::up ] [0] [0] = cube::green;
   res [ cube::up ] [0] [1] = cube::red;
   res [ cube::up ] [0] [2] = cube::green;
   res [ cube::up ] [1] [0] = cube::green;
   res [ cube::up ] [1] [1] = cube::blue;
   res [ cube::up ] [1] [2] = cube::green;
   res [ cube::up ] [2] [0] = cube::white;
   res [ cube::up ] [2] [1] = cube::yellow;
   res [ cube::up ] [2] [2] = cube::yellow;

   return res;
}


sequence solve( const cube& start )
{
   if( start. distance( ) == 0 )
      return sequence( );
         // Already solved.

   std::map< cube, sequence, cube_cmp > reached;
   std::priority_queue< pq > unchecked;

   reached [ start ] = sequence( );

   std::map< cube, sequence, cube_cmp > :: iterator it = reached. begin( );
   std::cout << pq( it ) << "\n";

   unchecked. push( pq( it ));

   // We make a vector with all possible rotations.
   // Note that many of them are redundant.
   // (Rotations over distance 0. Rotations with layers that are
   // more than half down the cube.)


   std::vector< rotation > allrotations;

   for( int dist = -1; dist < 3; ++ dist )
   {
      for( unsigned int layer = 1; layer <= start. getsize( ); ++ layer )
      {
         allrotations. push_back( rotation( cube::left, dist, layer ));
         allrotations. push_back( rotation( cube::right, dist, layer ));
         allrotations. push_back( rotation( cube::up, dist, layer ));
         allrotations. push_back( rotation( cube::down, dist, layer ));
         allrotations. push_back( rotation( cube::front, dist, layer ));
         allrotations. push_back( rotation( cube::back, dist, layer ));
      }
   }
   for( unsigned int i = 0; i < allrotations. size( ); ++ i )
   {
      std::cout << allrotations [i] << "\n";
   }

   bool forever = true;
   while( forever )
   {
      if( unchecked. size( ) == 0 )
         throw std::string( "could not find solution" );

      std::cout << "reached. size( ) = " << reached. size( ) << "\n";
      std::cout << "unchecked. size( ) = " << unchecked. size( ) << "\n";

#if 0
      std::cout << "Reached:\n";
      std::cout << "---------------------------------\n";
      for( auto p = reached. begin( ); p != reached. end( ); ++ p )
      {
         std::cout << ( p -> first ) << "\n";
         std::cout << ( p -> second ) << "\n";
      }
      std::cout << "\n\n";
      std::cout << "Top of Unchecked:\n";
      std::cout << unchecked. top( ) << "\n";
#endif

      std::map< cube, sequence, cube_cmp > :: iterator
      sel = unchecked. top( ). it;
#if 1
      std::cout << "Selected " << ( sel -> first ) << "\n";
      std::cout << "with distance " << sel -> first. distance( ) << "\n";
#endif

      if( unchecked. top( ). dist == ( sel -> first. distance( )))
      {
         for( auto p = allrotations. begin( ); p != allrotations. end( ); ++ p )
         {
            cube next = sel -> first;
            next. rotate( *p );

            auto q = reached. find( next );
            if( q == reached. end( ))
            {
               // This means that next is not in reached.
               // We add it:

               q = reached. insert(
                  std::pair< cube, sequence > :: pair(
                               next,
                               ( sel -> second ) * (*p))). first;

                  // We insert next in reached.
                  // The sequence (second) is obtained by adding (*p) to the
                  // sequence of sel.

                unchecked. push( pq( q ));
            }
            else
            {
               if( sel -> second. length( ) + 1 < q -> second. length( ))
               {
                  // This means that next was already reached, but we
                  // found a shorted sequence of moves leading
                  // towards next.

                  q -> second = ( sel -> second ) * ( *p );
                  unchecked. push( pq( q ));
               }
            }
         }
      }
      else
      {
         ASSERT( unchecked. top( ). dist > ( sel -> first. distance( )));
         std::cout << "skipping!\n";
         QUIT( );
      }
      unchecked. pop( );
   }

   return sequence( );
}


