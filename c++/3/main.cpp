#include <iostream>
#include "pierwsze.cpp"
#include <vector>
#include <cstdlib>
using namespace std;

void printV(const vector<long long> &T )
{
    for(int i=0; i<T.size(); i++)
        cout << T[i]<<" ";
    cout << endl;
}

int main()
{
    prime_numbers::Initialize();
    while(true)
    {
        long long number;
        cin>> number;
        //cout << number << endl;
        if(prime_numbers::is_prime(number,true))
            cout << "is prime" << endl;
        else
            printV(prime_numbers::prime_factorization(number));
    }
    //system("pause");
}
