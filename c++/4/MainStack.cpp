#include <iostream>
#include "stack.cpp"
using namespace std;

int main()
{
    try
    {
        stack s1;
        for( unsigned int i = 0; i <10; ++ i )
            s1.push(i*2);
        s1.push(45);
        s1.push(45);
        s1.push(46);
        stack s2;
        s2.push(2000);
        s2.push(100);
        stack x = s1 + s2;
        stack y = x + s2;
        for( unsigned int i = 0; i <5; ++ i )
            s1[i]=i*3;
        cout << s1 << endl << y << endl;
        if(x==y)
            cout << "1.==2." << endl;
    }
    catch(string exc)
    {
        cerr << exc;
    }
    return 0;
}
