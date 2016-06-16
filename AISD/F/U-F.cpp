#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#define SPRAWDZACZKA

#ifdef SPRAWDZACZKA
#define NDEBUG
#endif

#include <assert.h>

#ifndef SPRAWDZACZKA
#include <iostream>
#endif

#include <vector>
#include <algorithm>
/*
template<typename p>
struct shared_ptr {
  p* ptr;
  int* hmany;
  shared_ptr()
  :  ptr(NULL), hmany(NULL)
  {}
  shared_ptr(const p* & pt)
  : ptr(new p(*pt)), hmany(new int(1))
  {}
  shared_ptr(const shared_ptr& s)
  : ptr(s.ptr), hmany(s.hmany)
  {
    (*hmany)++;
  }
  shared_ptr operator=(const shared_ptr& r)
  {
    decrease();
    hmany = r.hmany;
    increase();
    ptr = r.ptr;
    return (*this);
  }
  ~shared_ptr()
  {
    decrease();
  }
  private:
  void decrease()
  {
    (*hmany)--;
    if(*hmany == 0) {
      delete(hmany);
      delete(ptr);
    }
  }
  void increase()
  {
    (*hmany)++;
  }
};
*/

template<typename SetName>
struct UFUniv {
  struct node {
    node* parent; 
    SetName name;
    enum { Top, Middle, Null } tag;
    int size;
    node(const SetName& s)
    : parent(), name(s), tag(Top), size(0)
    {}
    node()
      : parent(), name(), tag(Null), size()
    {}
    node operator=(const node& r)
    {
      parent=r.parent;
      name=r.name;
      tag=r.tag;
      size=r.size;
      return (*this);
    }
    private:
    node(const node& p)
    : parent(p.parent), name(p.name), tag(p.tag), size(p.size)
    {}
  };
  node* Elements;
  int size;
  UFUniv(const int& size)
  :  size(size)
  {
    Elements = new node [size];
    for(int i = 0; i < size; i++) {
      Elements[i] = node (SetName(i));
    }
  }
  UFUniv()
  : Elements(NULL), size()
  {}
  UFUniv(const UFUniv& r)
  : size(r.size)
  {
    Elements = new node [size];
    for(int i = 0; i < size; i++) {
      Elements[i] = node (r.Elements(i));
    }
  }
  ~UFUniv()
  {
    delete(Elements);
  }
  private:
  node* top(node* x) const
  {
    if(x->tag == node::Top)
    {
      return x;
    }
    node* r(top(x->parent));
    assert(r->tag == node::Top);
    x->parent = r;
    return r;
  }
  void join(node* r, node* pt)
  {
    assert(pt->tag == node::Top && r->tag == node::Top);
    if(pt->size >= r->size) {
      r->tag = node::Middle;
      pt->size += r->size;
      r->parent = pt;
    } else {
      pt->tag = node::Middle;
      r->size += pt->size;
      pt->parent = r;
    }
  }
  public:
  void join(const int& x, const int& y)
  {
    assert( x >= 0 && x < size);
    node* xtop(&Elements[x]);
    assert( y >= 0 && y < size);
    node* ytop(&Elements[y]);
    join(xtop, ytop);
  }
  SetName find(const int& x) const
  {
    assert(x >= 0 && x < size);
    return top(Elements + x)->name;
  }
};
#ifndef SPRAWDZACZKA
template<typename s>
std::ostream& operator<< (std::ostream& stream, const UFUniv<s>& r)
{
  for(int i=0; i<r.size;i++)
    stream << i << " : " << r.find(i) << "\n";
  return stream;
}

template<typename s>
std::ostream& operator<< (std::ostream& stream, const typename UFUniv<s>::node& r)
{
  stream << &r << ": ";
  if(r.tag == UFUniv<s>::node::Null) {
    stream << "<>";
    return stream;
  }
  if(r.tag == UFUniv<s>::node::Middle)
  {
    stream << r.parent;
    return stream;
  }
  if(r.tag == UFUniv<s>::node::Top)
  {
    stream << r.name;
    return stream;
  }
}

template<typename a, typename b> std::ostream& operator<<(std::ostream& stream, const std::pair<a,b>& p)
{
  stream << "( " << p.first << ", " << p.second << " ) ";
  return stream;
}
#endif

typedef std::pair<int,int> Point;
typedef std::pair<int, Point > wPoint;

struct mycomp
{
  bool operator()(const wPoint& a, const wPoint& b)
  {
    return a.first > b.first;
  }
};


std::vector<wPoint> Graph;
int main()
{
  int n,m;
  scanf("%d %d", &n, &m);
  for(int i=0;i<m;i++)
  {
    int a,b,c;
    scanf("%d %d %d", &a, &b, &c);
    Graph.push_back(wPoint(c, Point(a-1,b-1)));
  }
  std::sort(Graph.begin(), Graph.end(), mycomp());
#ifndef SPRAWDZACZKA
  for(int i=0; i<m;i++)
    std::cout << Graph[i] << "\n";
#endif
  UFUniv<int> MST(n);
  int min=1<<30;
  for(int i=0; i<m; i++)
  {
#ifndef SPRAWDZACZKA
    std::cout << MST << Graph[i] << "\n";
#endif
    int u(Graph[i].second.first), v(Graph[i].second.second), w(Graph[i].first);
    int setU(MST.find(u)), setV(MST.find(v));
    if(setU != setV)
    {
      if(w < min)
        min = w;
      MST.join(setU, setV);
    } 
  }
  printf("%d\n", min);
}
