
#ifndef CUBE_INCLUDED
#define CUBE_INCLUDED   1

#include <vector>
#include <iostream>

#include "assert.h"

struct rotation;
struct sequence;

class cube
{
public:
   enum side { left, right, up, down, front, back };
   int sideToInt (side s)
   {
       switch(s)
       {
           case left:
            return 0;
            break;
           case right:
            return 1;
            break;
           case up:
            return 2;
            break;
           case down:
            return 3;
            break;
           case front:
            return 4;
            break;
           case back:
            return 5;
            break;
       }
   }
   side intToSide (int s)
   {
       switch(s)
       {
           case 0:
            return left;
            break;
           case 1:
            return right;
            break;
           case 2:
            return up;
            break;
           case 3:
            return down;
            break;
           case 4:
            return front;
            break;
           case 5:
            return back;
            break;
       }
   }
   enum color { white, red, orange, green, blue, yellow };

private:
   unsigned int size;
   std::vector< color > sides;
      // It has a size of 6 * size * size;

      // We use the following convention:
      // (I defined suitable index operators, so you don't need to
      //  worry about this.)
      //
      // sides [ 0 * d * d ] : left
      // sides [ 1 * d * d ] : front
      // sides [ 2 * d * d ] : right
      // sides [ 3 * d * d ] : back
      // sides [ 4 * d * d ] : up
      // sides [ 5 * d * d ] : down.


      static unsigned int offset( side s )
      {
         switch(s)
         {
         case left: return 0;
         case front: return 1;
         case right: return 2;
         case back: return 3;
         case up: return 4;
         case down: return 5;
         }
         ASSERT( false ); return 0;
      }

      // Inverse of offset( ):

      static side getside( unsigned int i )
      {
         switch(i)
         {
         case 0: return left;
         case 1: return front;
         case 2: return right;
         case 3: return back;
         case 4: return up;
         case 5: return down;
         }
         ASSERT( false ); return left;
      }

      // We have to define a sequence of intermediate index
      // data structures. It is unpleasant.

      struct row_index
      {
         std::vector< color > :: iterator p;
         unsigned int size;

         color& operator [] ( unsigned int j ) const
         {
            ASSERT( j < size );
            return p[j];
         }

         row_index( std::vector< color > :: iterator p,
                    unsigned int size )
            : p(p), size(size)
         { }
      };

      struct row_const_index
      {
         std::vector< color > :: const_iterator p;
         unsigned int size;

         color operator [] ( unsigned int j ) const
         {
            ASSERT( j < size );
            return p[j];
         }

         row_const_index( std::vector< color > :: const_iterator p,
                          unsigned int size )
            : p(p), size(size)
         { }
      };

      struct side_index
      {
         std::vector< color > :: iterator p;
         unsigned int size;

         row_index operator [] ( unsigned int i ) const
         {
            ASSERT( i < size );
            return row_index( p + size * i, size );
         }

         side_index( std::vector< color > :: iterator p,
                     unsigned int size )
            : p(p), size(size)
         { }
      };

      struct side_const_index
      {
         std::vector< color > :: const_iterator p;
         unsigned int size;

         row_const_index operator [] ( unsigned int i ) const
         {
            ASSERT( i < size );
            return row_const_index( p + size * i, size );
         }

         side_const_index( std::vector< color > :: const_iterator p,
                           unsigned int size )
            : p(p), size(size)
         { }
      };

public:
      side_index operator [] ( side s )
      {
         return side_index( sides. begin( ) + offset(s) * size * size,
                            size );
      }

      side_const_index operator [] ( side s ) const
      {
         return side_const_index( sides. begin( ) + offset(s) * size * size,
                                  size );
      }

   static color solvedcolor( side s );
      // Returns the color that side s has, when the cube is solved.
      // The choice is arbitrary, but we have to choose something.
      // I chose the colors of my own cube.
      //
      // left : yellow,
      // front  : white,
      // right :  green,
      // back : blue,
      // up : red,
      // down : orange.

   static std::vector< side > allsides( );
      // This is useful when you want to enumerate all possible
      // moves.

   explicit cube( unsigned int size );
   unsigned int getsize( ) const { return size; }

   void rotate( int d, color& c1, color& c2, color& c3, color& c4 );
      // Rotate colors right over distance i.

   void rotate_surface_only( side s, int d );
      // Rotate a surface. (This function is incomplete,
      // because it doesn't rotate neighbouring faces.)

   void rotate_neighbours( side s, int d, unsigned int j );
      // d is the amount of rotation, j in [1 .. size ] is the distance
      // from surface s.

   void rotate( const rotation& rot );
   void rotate( const sequence& seq );

   friend std::ostream& operator << ( std::ostream& , const cube& );
   friend void plot( const cube& );

private:
    struct neigh_tuple
    {
        side surf;
        int begx;
        int begy;
        int stepx;
        int stepy;
        int nextlx;
        int nextly;
        neigh_tuple(side s, int by, int bx, int sy, int sx, int sly, int slx)
        : surf(s), begx(bx), begy(by), stepx(sx), stepy(sy), nextlx(slx), nextly(sly)
        {}
        neigh_tuple(const neigh_tuple& s)
        : surf(s.surf), begx(s.begx), begy(s.begy), stepx(s.stepx), stepy(s.stepy), nextlx(s.nextlx), nextly(s.nextly)
        {}
    };
    struct neigh_vec_elem
    {
        neigh_tuple surf1;
        neigh_tuple surf2;
        neigh_tuple surf3;
        neigh_tuple surf4;
        neigh_vec_elem(neigh_tuple s1, neigh_tuple s2, neigh_tuple s3, neigh_tuple s4)
        : surf1(s1), surf2(s2), surf3(s3), surf4(s4)
        {}
    };
    struct neigh_table
    {
        std::vector< neigh_vec_elem > table;
        neigh_table()
        {
            //Left side:
            table.push_back(neigh_vec_elem(neigh_tuple(front,0,0,1,0,0,1),neigh_tuple(back, size,size,-1,0,0,-1),neigh_tuple(up,0,0,1,0,0,1),neigh_tuple(down,0,0,1,0,0,1)));
            //Right side:
            table.push_back(neigh_vec_elem(neigh_tuple(front,size,size,-1,0,0,-1),neigh_tuple(back,0,0,1,0,0,1),neigh_tuple(up,size,size,-1,0,0,-1),neigh_tuple(down,size,size,-1,0,0,-1)));
            //Up:
            table.push_back(neigh_vec_elem(neigh_tuple(front,0,size,0,-1,1,0),neigh_tuple(back,0,size,0,-1,1,0),neigh_tuple(left,0,size,0,-1,1,0),neigh_tuple(right,0,size,0,-1,1,0)));
            //Down:
            table.push_back(neigh_vec_elem(neigh_tuple(front,size,0,0,1,-1,0),neigh_tuple(back,size,0,0,1,-1,0),neigh_tuple(left,size,0,0,1,-1,0),neigh_tuple(right,3,0,0,1,-1,0)));
            //Front:
            table.push_back(neigh_vec_elem(neigh_tuple(left,3,3,-1,0,0,-1),neigh_tuple(right,0,0,1,0,0,1),neigh_tuple(up,3,0,0,1,-1,0),neigh_tuple(down,0,3,0,-1,1,0)));
            //Back:
            table.push_back(neigh_vec_elem(neigh_tuple(left,0,0,1,0,0,1),neigh_tuple(right,3,3,-1,0,0,1),neigh_tuple(up,0,3,0,-1,1,0),neigh_tuple(down,0,0,0,1,1,0)));
        }
        const neigh_vec_elem& operator[] (unsigned int i) const
        {
            return table[i];
        }
    };
    neigh_table aux_rotation_table;
};

std::ostream& operator << ( std::ostream& stream, cube::side s );
std::ostream& operator << ( std::ostream& stream, cube::color c );
std::ostream& operator << ( std::ostream& stream, const cube& c );


#endif

