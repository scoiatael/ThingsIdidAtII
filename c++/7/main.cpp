#include <iostream>
#include "square.cpp"

using namespace std;

int main( int argc, char* argv [] )
{

   square sq = square(4);
   std::vector< position > order;
   //As levels are numbered beginning from 1 there needs to be a blank pos at order[0]
   order. push_back( position(-1,-1));
   order. push_back( position(0,0));
   order. push_back( position(0,1));
   order. push_back( position(0,2));
   order. push_back( position(0,3));
   order. push_back( position(1,2));
   order. push_back( position(2,1));
   order. push_back( position(3,0));
   order. push_back( position(2,0));
   order. push_back( position(1,0));
   order. push_back( position(1,1));
   order. push_back( position(1,3));
   order. push_back( position(3,1));
   order. push_back( position(2,2));
   order. push_back( position(2,3));
   order. push_back( position(3,2));
   order. push_back( position(3,3));
   int count=0;
    printmagicsquares(1,sq,order,count);
    std::cout << count <<  " done" << endl;
}


