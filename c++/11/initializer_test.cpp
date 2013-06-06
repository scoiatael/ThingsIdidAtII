#include <initializer_list>
#include <vector>
#include <iostream>
#include <assert.h>

struct my_int_pair_wrapper
{
    std::pair<int,int> body;
    my_int_pair_wrapper (const std::initializer_list<int> &init)
    : body()
    {
        assert(init.size() == 2);
        auto p = init.begin();
        body.first = *p;
        body.second = *(p+1);
    }
    friend std::ostream& operator<< (std::ostream&, const my_int_pair_wrapper&);
    my_int_pair_wrapper(const int& arg1, const int& arg2)
    : body(arg1, arg2)
    { }
};

std::ostream& operator<< (std::ostream& stream, const my_int_pair_wrapper& arg)
{
    stream << arg.body.first << " " << arg.body.second;
    return stream;
}

struct my_class
{
    std::vector<my_int_pair_wrapper> body;
    my_class(const std::initializer_list<my_int_pair_wrapper> &init)
    : body()
    {
        for (auto p = init.begin(); p<init.end(); p++)
            body.push_back(*p);
    }

    friend std::ostream& operator << (std::ostream&, const my_class&);
};

std::ostream& operator << (std::ostream& stream, const my_class& arg)
{
    for (auto p = arg.body.begin(); p< arg.body.end(); p++)
        stream << (*p) << " ";
    stream << "\n";
    return stream;
}

int main()
{
    my_class test({ my_int_pair_wrapper(12, 1),
                    my_int_pair_wrapper(10, 2),
                    my_int_pair_wrapper(123, 3)});
    std::cout << test;
    return 0;
}
