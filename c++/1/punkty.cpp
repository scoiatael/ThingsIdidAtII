#include "punkty.hpp"
#include <iostream>
using namespace std;

float abs(float n)
{
    if(n<0)
        return -n;
    return n;
}
punkt2D::punkt2D(float x, float y)
   x(x),y(y)
{}

odcinek2D::odcinek2D(float x1, float y1, float x2, float y2): B(x1,y1), E(x2,y2){}

void odcinek2D::move_x(float x)
{
    this->B.x+=x;
    this->E.x+=x;
}

void odcinek2D::move_y(float y)
{
    this->B.y+=y;
    this->E.y+=y;
}

bool odcinek2D::cont(punkt2D Giv)
{
    //cout << A.x <<" "<<A.y<<" "<<B.x<<" "<<B.y<<" "<<E.x<<" "<<E.y<<endl;
    vector2D Vec_odc((B.x-E.x),(B.y-E.y)), Vec_pkt((B.x-Giv.x),(B.y-Giv.y));
    if (Vec_odc.x==0)
        return (Vec_odc.x==Vec_pkt.x && abs(Vec_pkt.y)<=abs(Vec_odc.y));
    if (Vec_odc.y==0)
        return (Vec_odc.y==Vec_pkt.y && abs(Vec_pkt.x)<=abs(Vec_odc.x));
    float k1 = Vec_pkt.x/Vec_odc.x;
    float k2 = Vec_pkt.y/Vec_odc.y;
    return (k1==k2 && k1<=1);
}

line::line(odcinek2D* D)
{
    float dx=D->E.x-D->B.x;
    float dy=D->E.y-D->B.y;
    if(dx==0 && dy==0)
    {
        throw string("Line error: given points are the same.");
    }
    if(dx==0)
    {
        A=1;
        B=0;
        C=-D->E.x;
        return;
    }
    if(dy==0)
    {
        A=0;
        B=1;
        C=-D->E.y;
        return;
    }
    B=1;
    A=-dy/dx;
    C=-D->B.y-A*D->B.x;
}

punkt2D* line::intersect(line D)
{
    //cout << A << " " << B<< " " <<C<<endl;
    //cout << D.A<< " " <<D.B<< " " <<D.C<<endl;
    float x,y;
    if(A==0)
    {
        y=-C;
        if(D.B==0)
            x=-D.C;
        else
            x=(C*D.A-D.C)/D.B;
        return new(punkt2D)(x,y);
    }
    if(B==0)
    {
        x=-C;
        if(D.A==0)
            y=-D.C;
        else
            y=(C*D.B-D.C)/D.A;
        return new(punkt2D)(x,y);
    }
    if(D.A==0)
    {
        y=-D.C;
        if(B==0)
            x=-C;
        else
            x=(D.C*A-C)/B;
        return new(punkt2D)(x,y);
    }
    if(D.B==0)
    {
        x=-D.C;
        if(A==0)
            y=-C;
        else
            y=(D.C*B-C)/A;
        return new(punkt2D)(x,y);
    }
    x = (D.C/D.B-C/B)/(A/B-D.A/D.B);
    y = -x*A/B-C/B;
    return new(punkt2D)(x,y);
}

punkt2D* cross(odcinek2D* a, odcinek2D* b)
{
    if(par(a,b))
        return NULL;
    punkt2D* Res = line(a).intersect(line(b));
    //cout << Res->x << " " << Res->y << endl;
    if(a->cont(*Res) && b->cont(*Res))
        return Res;
    return NULL;
}

bool par(odcinek2D* a, odcinek2D* b)
{
    vector2D Vec_a(a->B.x-a->E.x, a->B.y-a->E.y);
    vector2D Vec_b(b->B.x-b->E.x, b->B.y-b->E.y);
    //cout << a->B.y <<" " << a->E.y << " " << a->B.x << " " << a->E.x << endl;
    float k1 = Vec_a.x/Vec_b.x;
    float k2 = Vec_a.y/Vec_b.y;
    return (bool)(k1==k2);
}

bool perp(odcinek2D* a, odcinek2D* b)
{
    vector2D Vec_a(a->B.x-a->E.x, a->B.y-a->E.y);
    vector2D Vec_b(b->B.x-b->E.x, b->B.y-b->E.y);
//    cout << A1 << " "<< B1<< " "<<A2<< " "<<B2<< endl;
    if(Vec_a.y==0||Vec_a.x==0)
    {
        if(Vec_a.x==Vec_a.y)
            return (Vec_b.x==0 && Vec_b.y==0);
        if(Vec_a.y==0)
            return Vec_b.x==0;
        else
            return Vec_b.y==0;
    }
    float k1 = -Vec_b.x/Vec_a.y;
    float k2 = Vec_b.y/Vec_a.x;
    return (bool)(k1==k2);
}

int main()
{
    cout << "(x1,y1,x2,y2):\n";
    odcinek2D* T[2];
    for(int i=0;i<2;i++)
    {
        float x1,y1,x2,y2;
        cin >> x1 >> y1 >> x2 >> y2;
        T[i]=new(odcinek2D)(x1,y1,x2,y2);
        //cout << T[i]->B.x << T[i]->B.y << T[i]->E.x << T[i]->E.y << endl;
    }
    cout << "Par: " << par(T[0], T[1]) << "Perp: " << perp(T[0],T[1]) << endl;
    punkt2D* temp = cross(T[0],T[1]);
    if(temp!=NULL)
        cout << "Intersection: " << temp->x <<" "<< temp->y<<endl;
    return 0;
}
