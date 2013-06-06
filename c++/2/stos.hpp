class stos
{
    int cur_size;
    double *mem;
    public:
        int size;
        void push(const double&);
        double pop();
        double top();
        int get_cur_size();
        stos();
        stos(const int&);
        stos(const stos&);
        ~stos();

};
