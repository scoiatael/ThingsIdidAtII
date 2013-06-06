
#ifndef SQUARE_INCLUDED
#define SQUARE_INCLUDED  1


#include "assert.h"
#include <iostream>
#include <vector>

// You use class when you want to hide the implementation.
// In such little classes, you have nothing to hide, so you use
// struct.

// I think that in principle, one should define one class in one file.
// Since this is an exercise, I think it is OK to define more than
// one class in a file, but I would never do this in a real program.

struct position
{
   unsigned int x;
   unsigned int y;

   // Don't dream of adding a default constructor.
   // There is no default position!

   position( unsigned int x, unsigned int y )
      : x(x), y(y)
   { }

};


std::ostream& operator << ( std::ostream& stream, const position& );

class square
{
   // Don't even think of adding a default constructor!

   unsigned int dimension;
   unsigned int required_sum;
   unsigned int* data;

public:
   explicit square( unsigned int d );
   square( const square& sq );
   void operator = ( const square& sq );
   ~square( );

   bool rowcorrect( unsigned int ) const;
      // Don't you dare to call this function 'checkrow'.
      // A function must be called after what it returns,
      // not after what it does.
      // Function sin( ) in the standard libarary is not called
      // 'computesin( )'.

   bool columncorrect( unsigned int ) const;
   bool diagtlbrcorrect( ) const;
      // True if the diagonal top left - bottom right is correct.

   bool diagbltrcorrect( ) const;
      // True if the diagonal bottom left - top right is correct.


   unsigned int operator [] ( const position& p ) const
   { return data [ p.x * dimension + p.y ]; }

   unsigned int& operator [] ( const position& p )
   { return data [ p.x * dimension + p.y ]; }

   // These two functions make it possible to write expressions of
   // form square [i][j], either const or non const.

   bool contains( unsigned int k ) const;
      // True if the square contains the number k.

   unsigned int getdimension( ) const { return dimension; }
   unsigned int getsum( ) const { return required_sum; }

   // Guess contains a reference to the square in which the guess
   // was made. The guesses should go out of scope, before the
   // square goes out of scope. Note that this normally will be the case,
   // unless you do something crazy.

   struct guess
   {
      // Meaning that we guessed sq[p] = val;
      // Our destructor removes the guess.

      square* sq;
      position p;
      unsigned int val;

      guess( square& sq, const position& p, unsigned int val )
         : sq( &sq ), p(p), val( val )
      {
         ASSERT( sq [ p ] == 0 );
         sq [p] = val;
      }

      // Copying would be extremely dangerous, so we forbid that.

      ~guess( )
      {
         (*sq) [p] = 0;
      }
   };
};

std::ostream& operator << ( std::ostream& stream, const square& sq );
void printmagicsquares( unsigned int level, square& sq, const std::vector <position> &order );

#endif

