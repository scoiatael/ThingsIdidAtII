
#ifndef ROTATION_INCLUDED
#define ROTATION_INCLUDED    1

#include <iostream>
#include <vector>

#include "cube.h"
#include <initializer_list>


struct rotation 
{
   cube::side s;

   int dist;  // Normally, we use only -1, 1, 2, but others
              // could be used as well, because adding or
              // subtracting 4 to the distance doesn't change
              // the effect. 

   unsigned int layer; 
      // Layer starts counting at 1 and ends at S, where S is the
      // size of the cube.

   // Don't make a default constructor. There is no special rotation!

   // Make a left rotation of the 0-th layer.

   rotation( cube::side s )
      : s(s),
        dist(1),
        layer(1)
   { }

   rotation( cube::side s, int dist )
      : s(s),
        dist( dist ),
        layer(1)
   { }

   rotation( cube::side s, int dist, unsigned int layer )
      : s(s),
        dist( dist ),
        layer( layer )
   { } 

   rotation inverse( ) const
   {
      return rotation( s, -dist, layer );
   }

};


inline std::ostream& operator << ( std::ostream& stream, rotation rot )
{
   stream << rot. s;
   if( rot. dist != 1 )
   { 
      stream << '^'; 
      if( rot. dist < 0 ) 
         stream << '{' << rot. dist << '}';
      else
         stream << rot. dist; 
   }
   if( rot. layer != 1 ) stream << '_' << rot. layer;
   return stream;
}


#endif


