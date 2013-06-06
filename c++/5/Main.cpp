#include "implementation.cpp"
#include <iostream>
#include <vector>
using namespace std;

int main()
{
    for(int i=0; i<10000000;i++)
    {
        vector< surface > s;
        s.push_back(triangle(0,1,2,3,1,10));
        s.push_back(circle(0,0,4));
        s.push_back(rectangle(0,0,4,4));
        vector<surface> c=s;
        c.push_back(circle(1,1,2));
        s[0]=c[1];
        //cout << s;
        //print_statistics(s);
        //system("pause");
    }
    return 0;
}
