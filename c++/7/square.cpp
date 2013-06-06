#include "square.h"
#include <vector>


square::square( unsigned int d )
   : dimension( d ),
     required_sum( d * ( d * d + 1 ) / 2 ),
     data( new unsigned int[ d * d ] )
{
    for(unsigned int i=0; i<dimension*dimension;i++)
        data[i]=0;
}

square:: ~square( )
{
   delete [] data;
}

std::ostream& operator << ( std::ostream& stream, const position& p )
{
   stream << "[ " << p.x << ", " << p.y << " ]";
   return stream;
}

std::ostream& operator << ( std::ostream& stream, const square::guess& g )
{
   stream << "guessed that " << g. p << " contains " << g. val;
   return stream;
}

std::ostream& operator << ( std::ostream& stream, const square& sq )
{
   stream << "dimension     " << sq. getdimension( ) << "\n";
   stream << "required_sum  " << sq. getsum( ) << "\n";

   for( unsigned int i = 0; i < sq. getdimension( ); ++ i )
   {
      for( unsigned int j = 0; j < sq. getdimension( ); ++ j )
      {
         stream << sq[ position(i,j) ] << "   ";
      }
      stream << "\n";
   }
   stream << "\n";
   return stream;
}

bool square::contains( unsigned int k ) const
      // True if the square contains the number k.
      {
          for(unsigned int i=0; i<dimension*dimension;i++)
            if(data[i]==k)
                return true;
            return false;
      }

bool square::rowcorrect( unsigned int row) const
      // Don't you dare to call this function 'checkrow'.
      // A function must be called after what it returns,
      // not after what it does.
      // Function sin( ) in the standard libarary is not called
      // 'computesin( )'.
      {
          unsigned int sum=0;
          for(unsigned int i=0; i<dimension;i++)
          {
            position p(i,row);
            sum+=(*this)[p];
          }
          return sum == required_sum;
      }

bool square::columncorrect( unsigned int col) const
{
          unsigned int sum=0;
          for(unsigned int i=0; i<dimension;i++)
          {
            position p(col,i);
            sum+=(*this)[p];
          }
          return sum==required_sum;
}

bool square::diagtlbrcorrect( ) const
      // True if the diagonal top left - bottom right is correct.
      {
          unsigned int sum=0;
          for(unsigned int i=0; i<dimension;i++)
          {
            position p(i,i);
            sum+=(*this)[p];
          }
          return sum==required_sum;
      }

bool square::diagbltrcorrect( ) const
      // True if the diagonal bottom left - top right is correct.
      {
          unsigned int sum=0;
          for(unsigned int i=0; i<dimension;i++)
          {
            position p(i,dimension-i-1);
            sum+=(*this)[p];
          }
          return sum==required_sum;
      }

void square::operator = ( const square& sq )
{
    if((&sq) == this)
        return;
    dimension = sq.dimension;
    required_sum = sq.required_sum;
    data  = new unsigned int[dimension];
    for(unsigned int i=0; i<dimension*dimension; i++)
        data[i] = sq.data[i];
}

void printmagicsquares( unsigned int level,square& sq,const std::vector< position > & order, int& count )
{
    if(level==17)
    {
        count++;
        //std::cout << sq;
        //std:: << count << std::endl;
        return;
    }

    for(unsigned int i=1;i<sq.getdimension()*sq.getdimension()+1;i++)
    {
        if(! sq.contains(i))
        {
            sq[order[level]] = i;
            bool ok = false;
            switch(level)
            {

                case 4:
                    ok = sq.columncorrect(0);
                    break;
                case 7:
                    ok = sq.diagbltrcorrect();
                    break;
                case 9:
                    ok = sq.rowcorrect(0);
                    break;
                case 11:
                    ok = sq.columncorrect(1);
                    break;
                case 12:
                    ok = sq.rowcorrect(1);
                    break;
                case 14:
                    ok = sq.columncorrect(2);
                    break;
                case 15:
                    ok = sq.rowcorrect(2);
                    break;
                case 16:
                    ok = sq.columncorrect(3) && sq.rowcorrect(3) && sq.diagtlbrcorrect();
                    break;
                default:
                    ok = true;
                    break;
            }
            if (ok)
                printmagicsquares(level+1,sq,order,count);
            sq[order[level]] = 0;
        }
    }
}
