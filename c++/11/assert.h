

// Written by Hans de Nivelle,
// December 2004.
// Added Quit on April 13th 2012, which was b.t.w. a Friday.


#ifndef ASSERT_INCLUDED
#define ASSERT_INCLUDED    1


#include <assert.h>

// #define ASSERT( X ) { assert( ( X ) ); }
   // Do a system assert.

// #define ASSERT( X ) { ; }
   // Don't do asserts. 

#define ASSERT(X) { if(!(X)) { std::cout << "ASSERT-ERROR\n\n";assert((X)); }}
   // Usual ASSERT writes into std::error. This version writes an additional
   // message into std::cout, so that it can be redirected and
   // detected. 

#define QUIT( ) { assert( 0 ); }

#endif


