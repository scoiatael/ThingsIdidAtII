#include <iostream>
#include "stos.cpp"
using namespace std;

int main()
{
    stos *X=NULL;
    cout << "Instructions:\n new() - create stack with size 1.\n new int - create size with size int.\n top,pop,push - as expected.\n delete - to destroy.\n copy - to invoke copy constructor.\n";
    try
        {
        while(1)
        {
            string S;
            cin >> S;
            if(S=="new()")
            {
                X = new(stos);
            }
            if(S=="copy")
            {
                if(X!=NULL)
                    X=new(stos)(*X);
                else
                    throw string("Stack is not created.\nUse new() or new int to create.\n");
            }
            if(S=="new")
            {
                int size;
                cin >> size;
                X = new(stos)(size);
            }
            if(S== "delete")
            {
                if(X!=NULL)
                    X->~stos();
                else
                    throw string("Stack is not created.\nUse new() or new int to create.\n");
            }
            if(S== "push")
            {
                double Giv;
                cin >> Giv;
                if(X!=NULL)
                    X->push(Giv);
                else
                    throw string("Stack is not created.\nUse new() or new int to create.\n");
            }
            if(S== "pop")
            {
                if(X!=NULL)
                {
                    cout << X->pop() << endl;
                }
                else
                {
                    throw string("Stack is not created.\nUse new() or new int to create.\n");
                }
            }
            if(S== "top")
            {
                if(X!=NULL)
                    cout << X->top() << endl;
                else
                    throw string("Stack is not created.\nUse new() or new int to create.\n");
            }
            if(S== "cur_size")
            {
                if(X!=NULL)
                    cout << X->get_cur_size() << endl;
                else
                    throw string("Stack is not created.\nUse new() or new int to create.\n");
            }
        }
    }
    catch(string err)
    {
        cout << err;
    }
}
