
#include "square.h"
#include <vector> 


square::square( unsigned int d )
   : dimension( d ),
     required_sum( d * ( d * d + 1 ) / 2 ),
     data( new unsigned int[ d * d ] )
{ }

square:: ~square( ) 
{
   delete [] data;
}

std::ostream& operator << ( std::ostream& stream, const position& p )
{
   stream << "[ " << p.x << ", " << p.y << " ]";
   return stream;
}

std::ostream& operator << ( std::ostream& stream, const square::guess& g )
{
   stream << "guessed that " << g. p << " contains " << g. val;
   return stream;
}

std::ostream& operator << ( std::ostream& stream, const square& sq )
{
   stream << "dimension     " << sq. getdimension( ) << "\n";
   stream << "required_sum  " << sq. getsum( ) << "\n";

   for( unsigned int i = 0; i < sq. getdimension( ); ++ i )
   {
      for( unsigned int j = 0; j < sq. getdimension( ); ++ j ) 
      {
         stream << sq[ position(i,j) ] << "   "; 
      }
      stream << "\n";
   }
   stream << "\n"; 
}


int main( int argc, char* argv [] )
{
   position p( 4, 5 );
   std::cout << p << "\n";


   square sq = square(5);
   std::cout << sq << "\n"; 
   
   {
      square::guess g = square::guess( sq, position(0,0), 4 );
      square::guess g1 = square::guess( sq, position( 1,0 ), 5 ); 

      std::cout << g << "\n";

      std::cout << sq << "\n"; 
   }

   std::cout << sq << "\n"; 

   std::vector< position > order;
   order. push_back( position(0,0));
   order. push_back( position(0,1));

}


