#include "pierwsze.hpp"
#include <cmath>
//fast x^y calculate, turns ll over
long long powmodf(const long long &base, const long long &exp, const long long &mod)
{
    //std::cout << base << " ";
    if(base<0)
        return 1;
    if(base==0||base==1)
        return base;
    if(exp==0)
        return 1;
    if(exp%2==1)
        return (powmodf(base, exp-1, mod)*(base))%mod;
    long long wyn = powmodf(base*base%mod, exp/2, mod)%mod;
    if(wyn<0)
        return 1;
    return wyn;
}

//normal x^y
long long powmod(const long long &base, long long exp, const long long &mod)
{
    long long wyn=1;
    while(exp>0)
    {
        wyn*=base;
        wyn%=mod;
        exp--;
    }
    return wyn;
}
bool prime_numbers::Initialize()
{
    if(!init)
    {
        init=true;
        prime_numbers();
        return true;
    }
    return false;
}

int prime_numbers::Erasto[max];

prime_numbers::prime_numbers()
{
    for(int i=0; i< max; i++)
        Erasto[i]=1;
    for(int i=2; i< max; i++)
        if(Erasto[i]==1)
            for(int k=2*i;k<max; k+=i)
                if(Erasto[k]==1)
                    Erasto[k]=i;

}


std::vector<long long> prime_numbers::prime_factorization(long long number)
{
    //std::cout << 1 << " ";
    std::vector<long long> wyn;
    if(number<0)
    {
        wyn.push_back(-1);
        number*=-1;
    }
    //std::cout<< number-max <<" ";
    if(number>=max)
    {
        //std::cout << "x" << std::endl;
        unsigned long long i=2;
        for(; i<max; i++)
            while(Erasto[i]==1 && number%i==0 && number>=max)
            {
                wyn.push_back(i);
                number/=i;
            }
        if(is_prime(number,true))
        {
            wyn.push_back(number);
            return wyn;
        }
        unsigned long long int end=sqrt(number)+1;
        i-=4;
        //std::cout << i << std::endl;
        while(i%6 != 0)
                i++;
        int del=4;
        std::cout << "x" << std::endl;
        for(;i<end && number>=max; i+=del, del=6-del)
        {
            std::cout << i << std::endl;
            while(number%i==0)
            {
                wyn.push_back(i);
                number/=i;
            }
            if(i%100000==0 && is_prime(number,true))
            {
                wyn.push_back(number);
                return wyn;
            }
        }
    }
    if(number<max)
    {
        while(1)
        {
            int temp=Erasto[number];
            if(temp==1)
            {
                wyn.push_back(number);
                return wyn;
            }
            wyn.push_back(temp);
            number=number/temp;
        }
    }
    if(number!=1)
        wyn.push_back(number);
    return wyn;
}
