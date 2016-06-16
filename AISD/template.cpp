/********************
 * Lukasz Czaplinski*
 * 247926           *
 * RNO              *
 * ******************/
#include <iostream>
#include <vector>

#ifdef SPRAWDZACZKA
#define NDEBUG
#define assert_msg(X, Y) {}
#endif

#ifndef SPRAWDZACZKA
#define assert_msg(X,Y) { if(!X) { Y; } }
#endif

#include <cassert>
#include <string>
using namespace std;

namespace my
{
  template<class T> void swap(T& a, T&b)
  {
    T temp(a);
    a=b;
    b=temp;
  }

  template<class T> T min(const T& a, const T& b)
  {
    return (a<b) ? a : b; 
  }

  template<class T> T max(const T& a, const T& b)
  {
    return (a>b) ? a : b; 
  }

  template<class T> 
  class safe_con : public vector<T>
  {
    public:
    safe_con()
    : std::vector<T>()
    {}
    safe_con(const safe_con& arg)
    : std::vector<T>(arg)
    {
      assert_msg(false, cout << "safe_con copy constr invoked, copying " << arg.size() << " elements.\n" );
    }
    safe_con(const vector<T>& arg)
    : std::vector<T>(arg)
    {
      assert_msg(false, cout << "safe_con constr from vec invoked, copying " << arg.size() << " elements.\n");
    }
    T& operator[](const unsigned int& n)
    {
      assert(n < this->size());
      return std::vector<T>::operator[](n); 
    }
    T operator[](const unsigned int& n) const
    {
      assert(n < this->size());
      return std::vector<T>::operator[](n); 
    }
    safe_con& operator=(const safe_con& arg)
    {
      assert_msg(false, cout << "safe_con operator= invoked, copying " << arg.size() <<  " elements.\n");
      std::vector<T>::operator=(arg);
      return *this;
    }
  };

  template<class T>
  istream& operator>> (istream& stream, safe_con<T>& arg)
  {
    int size;
    stream >> size;
    arg.resize(size);
    for(unsigned int i=0; i<arg.size(); i++)
      stream >> arg[i];
    return stream;
  } 

  template<class T>
  ostream& operator<< (ostream& stream, const safe_con<T>& arg)
  {
    for(unsigned int i=0; i<arg.size();i++)
      stream << arg[i] << " ";
    return stream;
  }
}
int main()
{
  my::safe_con<int> test, test2;
  cin >> test;
  test2 = test;
  cout << test2 << endl;
}
