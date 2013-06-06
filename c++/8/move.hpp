#include <iostream>
#include <queue>
#include <map>
#include <vector>

enum move { move_up, move_left, move_right, move_down };

inline std::ostream& operator << ( std::ostream& stream,
                                   move m )
{
   switch(m)
   {
   case move_up : std::cout << "move-up\n"; return stream;
   case move_left : std::cout << "move-left\n"; return stream;
   case move_right: std::cout << "move-right\n"; return stream;
   case move_down: std::cout << "move-down\n"; return stream;
   }
}

struct illegalmove
{
   move m;

   illegalmove( move m ) : m(m) {}
};

inline std::ostream& operator << ( std::ostream& stream,
                                   illegalmove i )
{
   stream << "doing " << i.m << " is illegal!\n";
   return stream;
}

