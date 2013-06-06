#include <vector>
bool init=false;

long long powmod(const long long&,long long,const long long&);
long long powmodf(const long long &base, const long long &exp, const long long &mod);

class prime_numbers
{
    static const unsigned int max=2097152;
    public:
        static const unsigned long long int LMax=1844674407370955161;
        static inline bool is_prime(long long number, bool if_fast)
            {
                    if(number<0)
                        number*=-1;
                    //std::cout << number << std::endl;
                    if(number<max)
                        return Erasto[number]==1;
                    if(if_fast)
                    {
                        int prime=0;
                        for(int i=2; i<=17; i++)
                        {
                            std::cout << i << " ";
                            if(number%i==0)
                                return false;
                            if(Erasto[i]==1 && number%i!=0 && i%number!=0)
                            {
                                //std:: << i << " ";
                                if(powmodf(i, number-1, number)==1)
                                    prime++;
                                else
                                    prime--;
                            }
                        }
                    //std::cout << std::endl;
                    return (prime>0);
                    }
                    bool prime=true;
                    for(int i=2; i<18; i++)
                    {
                        if(number%i==0)
                            return false;
                        if(Erasto[i]==1 && number%i!=0 && i%number!=0)
                            prime=prime&&(powmod(i, number-1, number)==1);
                    }
                    std::cout << std::endl;
                    return prime;

            }
        static std::vector<long long> prime_factorization(long long);
        static bool Initialize();
    private:
        prime_numbers();
        static int Erasto[max];

};
