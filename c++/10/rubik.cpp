
#include "sequence.hpp"
#include "cube.h"
#include <stdio.h>
#include <initializer_list>


// If you want to use graphics, switch this on.
// It works in Room 7.


// #include <SFML/Window.hpp>
// Switch this on, if you want to use graphics.

int main( int argc, char* argv [ ] )
{
   // In order to compile this, you need C ++ 11. If it doesn't compile, then
   // you can replace it by a sequence of push_backs.

   sequence seq({   rotation( cube::left ),
                    rotation( cube::left, 1, 3 ),
                    rotation( cube::up ),
                    rotation( cube::left, -1, 3 ),
                    rotation( cube::up, -1, 1 ),
                    rotation( cube::left, -1, 1 ),
                    rotation( cube::up, 1, 1 ),
                    rotation( cube::left, 1, 3 ),
                    rotation( cube::up, -1, 1 ),
                    rotation( cube::left, -1, 3 ) });

   std::cout << seq << "\n";
   std::cout << repeat( seq, 5 ) << "\n";

   sequence corners = repeat( sequence( { { cube::right },
                                          { cube::down, -1 },
                                          { cube::right, -1 },
                                          { cube::down } } ), 2 ) *
                         sequence( { rotation( cube::up ) } ) *
                      repeat( sequence( { { cube::down, -1 },
                                          { cube::right },
                                          { cube::down },
                                          { cube::right, -1 } } ), 2 ) *
                        sequence( { rotation( cube::up, -1 ) } );
   std::cout << corners << "\n";

   cube c(4);

   // Open an SFML window. Switch it on, if you want to use
   // graphics.

   // sf::Window
   //       wind( sf::VideoMode( 1024, 500, 32 ), "Rubik Cube" );

   char q = ' ';
   while( q != 'q' )
   {
      std::cout << c << "\n";
      // plot(c);
      // wind. Display( );
      // Plotting is nicer of course, but you need graphics.

      c. rotate( corners );
      q = getchar( );
   }

   return 0;
}

