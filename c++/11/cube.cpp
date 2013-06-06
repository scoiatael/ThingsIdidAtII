
#include "cube.h"
#include "rotation.h"
#include "sequence.h"


cube::color cube::solvedcolor( side s )
{
   switch(s)
   {
       case left:  return yellow;
       case front: return white;
       case right: return green;
       case back:  return blue;
       case up:    return red;
       case down:  return orange;
   }

   return yellow;   // Unreachable, but otherwise the compiler whines.
}


std::ostream& operator << ( std::ostream& stream, cube::side s )
{
   switch(s)
   {
   case cube::left:    stream << 'L'; return stream;
   case cube::right:   stream << 'R'; return stream;
   case cube::up:      stream << 'U'; return stream;
   case cube::down:    stream << 'D'; return stream;
   case cube::front:   stream << 'F'; return stream;
   case cube::back:    stream << 'B'; return stream;
   }

   QUIT( );
}


std::ostream& operator << ( std::ostream& stream, cube::color c )
{
   switch(c)
   {
   case cube::white:   stream << "white "; return stream;
   case cube::red:     stream << "red   "; return stream;
   case cube::orange:  stream << "orange" ; return stream;
   case cube::green:   stream << "green "; return stream;
   case cube::blue:    stream << "blue  ";  return stream;
   case cube::yellow:  stream << "yellow"; return stream;
   }

   QUIT( );
}


std::vector< cube::side > cube::allsides( )
{
   std::vector< side > res;
   res. push_back( left );
   res. push_back( right );
   res. push_back( up );
   res. push_back( down );
   res. push_back( front );
   res. push_back( back );
   return res;
}


cube::cube( unsigned int size )
   : size( size ), aux_rotation_table()
{
   ASSERT( size > 1 );

   for( unsigned int i = 0; i < 6; ++ i )
   {
      color col = solvedcolor( getside(i) );

      for( unsigned int j = 0; j < size * size; ++ j )
         sides. push_back( col );
   }
}



std::ostream& operator << ( std::ostream& stream,
                            const cube& c )
{
   for( unsigned int i = 0; i < 6; ++ i )
   {
      stream << cube::getside(i) << ": ";
      for( unsigned int j = 0; j < c. size * c. size; ++ j )
      {
         stream << c. sides [ i * c. size * c. size + j ] << " ";
         if( ( j + 1 ) % c. size == 0 )
            stream << " | ";
      }
      stream << "\n";
   }
   return stream;
}

void cube::rotate( int d, color& c1, color& c2, color& c3, color& c4 )
{
    d = ((d+1)%4) -1;
    switch(d)
    {
        case -1:
        {
            color copy = c1;
            c1 = c2;
            c2 = c3;
            c3 = c4;
            c4 = copy;
            break;
        }
        case 0:
        {
            break;
        }
        case 1:
        {
            color copy = c4;
            c4 = c3;
            c3 = c2;
            c2 = c1;
            c1 = copy;
            break;
        }
        case 2:
        {
            color copy = c1;
            c1 = c3;
            c3 = copy;
            copy = c4;
            c4 = c2;
            c2 = copy;
            break;
        }
    }
}

void cube::rotate_surface_only( side s, int d )
{
    side_index p = (*this) [s];
    for (unsigned int i = 0 ; i< size/2; i++)
        for (unsigned int j=0; j< (size+1)/2; j++)
            rotate(d, p[j][i], p[3-j][i], p[3-j][3-i], p[j][3-i]);
}

void cube::rotate_neighbours(side s, int d, unsigned int layer)
{
    for(unsigned int i=0; i<size; i++)
    {
        const neigh_vec_elem& saux = aux_rotation_table[s];
        rotate(d, (*this)[saux.surf1.surf][saux.surf1.begx + i* saux.surf1.stepx + layer*saux.surf1.nextlx][saux.surf1.begy + i* saux.surf1.stepy+ layer*saux.surf1.nextly],
                    (*this)[saux.surf2.surf][saux.surf2.begx + i* saux.surf2.stepx+ layer*saux.surf2.nextlx][saux.surf2.begy + i* saux.surf2.stepy+ layer*saux.surf2.nextly],
                    (*this)[saux.surf3.surf][saux.surf3.begx + i* saux.surf3.stepx+ layer*saux.surf3.nextlx][saux.surf3.begy + i* saux.surf3.stepy+ layer*saux.surf3.nextly],
                    (*this)[saux.surf4.surf][saux.surf4.begx + i* saux.surf4.stepx+ layer*saux.surf4.nextlx][saux.surf4.begy + i* saux.surf4.stepy+ layer*saux.surf4.nextly]);
    }
}

void cube::rotate( const rotation& rot )
{
    if( rot. layer == 1 )
        rotate_surface_only( rot. s, rot. dist );
    if( rot. layer == size )
    {
        side opposite = up;
        switch( rot. s )
        {
            case left: opposite = right; break;
            case right: opposite = left; break;
            case up: opposite = down; break;
            case down: opposite = up; break;
            case front: opposite = back; break;
            case back: opposite = front; break;
        }
        rotate_surface_only( opposite, - rot. dist );
    }
    rotate_neighbours( rot. s, rot. dist, rot. layer );
}

void cube::rotate( const sequence& seq )
{
    for (auto p = seq.rot.begin(); p!=seq.rot.end(); p++)
    {
        rotate(*p);
    }
}

// If you have openGL, you can use this:

#if 0

#include <SFML/Window.hpp>

void plot( const cube& c )
{
   glClearColor( 0, 0, 0, 1 );
   glClearDepth(1);
   glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );

   glEnable( GL_DEPTH_TEST );
   glDepthFunc( GL_LEQUAL );

   glMatrixMode( GL_PROJECTION );
   glLoadIdentity( );

   glMatrixMode( GL_MODELVIEW );
   glLoadIdentity( );
   glOrtho( 0, 5 * c. size, 0, 4 * c. size, -1, 1 );

   for( unsigned int s = 0; s < 6; ++ s )
   {
      // We determine where to start plotting:

      double x, y;   // Will be the position of upper left corner of
                     // the i-th surface.

      switch(s)
      {
      case 0:
         x = 0; y = 2; break;
      case 1:
         x = 1; y = 2; break;
      case 2:
         x = 2; y = 2; break;
      case 3:
         x = 3; y = 2; break;
      case 4:
         x = 1; y = 3; break;
      case 5:
         x = 1; y = 1; break;
      }

      x = ( x + 0.5 ) * c. size;
      y = ( y + 0.5 ) * c. size;

      for( unsigned int i = 0; i < c. size; ++ i )
         for( unsigned int j = 0; j < c. size; ++ j )
         {
            // We set the color:

            switch( c[ cube::getside(s) ][i][j] )
            {
            case cube::white:
               glColor3f( 1, 1, 1 ); break;
            case cube::red:
               glColor3f( 1, 0, 0 ); break;
            case cube::orange:
               glColor3f( 1, 0.6, 0 ); break;
            case cube::green:
               glColor3f( 0, 1, 0 ); break;
            case cube::blue:
               glColor3f( 0, 0, 1 ); break;
            case cube::yellow:
               glColor3f( 1, 1, 0 ); break;
            }

            // and we plot:

            glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
            glBegin( GL_POLYGON );

            glVertex2f( x + j,       y - i );
            glVertex2f( x + j,       y - i - 0.8 );
            glVertex2f( x + j + 0.8, y - i - 0.8 );
            glVertex2f( x + j + 0.8, y - i );

            glEnd( );
         }
   }

   glFlush( );
}

#endif

