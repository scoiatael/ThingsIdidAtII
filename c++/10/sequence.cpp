
#include "sequence.hpp"


sequence operator * ( const sequence& seq1, const sequence& seq2 )
{
   sequence res;
   for( unsigned int i = 0; i < seq1. rot. size( ); ++ i )
      res. rot. push_back( seq1. rot [i] );
   for( unsigned int i = 0; i < seq2. rot. size( ); ++ i )
      res. rot. push_back( seq2. rot [i] );
   return res;
}

sequence repeat( const sequence& seq, unsigned int i )
{
   sequence res;
   while(i)
   {
      res = res * seq;
      -- i;
   }
   return res;
}

sequence sequence::inverse( ) const
{
   sequence res;

   unsigned int i = rot. size( );
   while(i)
   {
      -- i;
      res. rot. push_back( rot [i]. inverse( ));
   }
   return res;
}

std::ostream& operator << ( std::ostream& stream, const sequence& seq )
{
   for( std::vector< rotation > :: const_iterator
           p = seq. rot. begin( ); p != seq. rot. end( ); ++ p )
   {
      if( p != seq. rot. begin( ))
         stream << "; ";
      stream << *p;
   }
   return stream;
}


