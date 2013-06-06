#include <cmath>
#include "fifteen.hpp"

inline int abs(int x)
{
    if(x<0)
        x=-x;
    return x;
}

void fifteen::update_hash()
{
    unsigned long long hashT=0;
    for(int x=0; x<4;x++)
        for(int y=0; y<4; y++)
            hashT+=pow(table[x][y],x+4*y);
    hash = hashT;
}

std::ostream& operator << ( std::ostream& stream, const fifteen& f )
{
    for (int x = 0; x<4; x++)
    {
        for(int y = 0; y<4; y++)
            stream << f.table[x][y] << " ";
        stream << "\n";
    }
    for (unsigned int m=0; m<f.history.size(); m++)
        stream << (f.history).at(m);
    return stream;
}

fifteen::fifteen( )
{
    for (int x = 0; x<4; x++)
        for(int y = 0; y<4; y++)
            table[x][y] = x*4+y;
    update_hash();
}


fifteen::fifteen(const fifteen& obj)
{
    for (int i = 0; i<4; i++)
        for(int j = 0; j<4; j++)
            table[i][j] = obj.table[i][j];
    update_hash();
    x = obj.x;
    y = obj.y;
    history = obj.history;
}

unsigned int fifteen::distance( ) const
{
    int sum = 0;
    for (int x=0; x<4; x++)
        for(int y=0; y<4;y++)
        {
            int val = table[x][y];
            sum+= (abs(val%4-y)+abs(val/4-x));
        }
    return sum;
}

void fifteen::makemove( move m ) throw( illegalmove )
{
    switch(m)
   {
   case move_left :
        y-=1;
        if(y<0)
            throw illegalmove(m);
        table[x][y+1] = table[x][y];
    break;
   case move_up :
        x-=1;
        if(x<0)
            throw illegalmove(m);
        table[x+1][y]=table[x][y];
    break;
   case move_down:
        x+=1;
        if(x>3)
            throw illegalmove(m);
        table[x-1][y]=table[x][y];
    break;
   case move_right:
        y+=1;
        if(y>3)
            throw illegalmove(m);
        table[x][y-1]=table[x][y];
    break;
   }
   table[x][y]=0;
   update_hash();
   history.push_back(m);
}

bool fifteen::issolved( ) const
{
    return distance()==0;
}

bool fifteen_cmp::operator( ) ( const fifteen& f1, const fifteen& f2 )
{
    return f1.hash < f2.hash;
}

void read(fifteen& f )
{
    bool Used[16];
    for (int i=0; i<16;i++)
        Used[i]=false;
    for(int x=0; x<4;x++)
        for(int y=0; y<4;y++)
        {
            int input;
            std::cin >> input;
            if(input==0)
            {
                f.x=x;
                f.y=y;
            }
            f.table[x][y] = input;
            if(Used[input])
                throw "Double number";
            else
                Used[input]=true;
        }
}

fifteen solve( const fifteen& f )
{
   std::map< fifteen, bool, fifteen_cmp > reached;
   reached [f] = true;

   std::priority_queue< fifteen, std::vector< fifteen > , fifteen_better >
      unchecked;
   unchecked. push(f);

   std::vector< move > moves;
   moves. push_back( move_up );
   moves. push_back( move_down );
   moves. push_back( move_left );
   moves. push_back( move_right );



   while( unchecked. size( ))
   {
      fifteen best = unchecked. top( );
      unchecked.pop();
      if(best.issolved())
         {
            std::cout << best;
            return best;
         }
        for(unsigned int m=0; m<moves.size(); m++)
        {
            fifteen copy_moved(best);
            try
            {
                    copy_moved.makemove(moves[m]);
                    if(! reached[copy_moved])
                    {
                        reached.insert(std::pair<fifteen,bool>(copy_moved,true));
                        unchecked.push(copy_moved);
                    }
            }
            catch(illegalmove m )
            {}
        }

   }
   std::cout << "Didn't find one..." << "\n";
   return f;
}

int main( int argc, char* argv [ ] )
{
   fifteen seek;
    read(seek);
    std:: cout << seek << "\n";
   fifteen solution = solve(seek);
   return 0;
}

