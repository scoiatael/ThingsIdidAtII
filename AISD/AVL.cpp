#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <iostream>


struct Point {
  int x, y;
  Point(const int& i, const int& j)
  : x(i), y(j)
  {}
  ~Point()
  {}
};

bool operator<(const Point& a, const Point& b)
{
  return (a.x == b.x) ? (a.y < b.y) : (a.x < b.x);
}

std::ostream& operator<<(std::ostream& stream, const Point& p)
{
  stream << "(" << p.x << ", " << p.y << ")";
  return stream;
}

/*
template<class T>
  struct Interval {
    Point<T> start;
    Point<T> end;
    int val;
    Interval()
    : start(0, 0), end(0, 0), val(0)
    {}
    Interval(const Point<T>& s, const Point<T>& e, const int& v)
    : start(s), end(e), val(v)
    {}
    ~Interval()
    {}
    bool contains(const Point<T>& p)
    {
      return (p.x >= start.x) && (p.x <= end.x) && (p.y >= start.y) && (p.y <= end.y);
    }
  };

template<class T>
  bool operator<(const Interval<T>& a, const Interval<T>& b)
{
  return a.start < b.start;
}

template<class T>
  std::ostream& operator<<(std::ostream& stream, const Interval<T>& i)
{
  stream << "|" << i.start << " " << i.end << "|";
  return stream;
}
*/
#ifndef SPRAWDZACZKA
unsigned int voidNodeNr, keyNodeNr;
#endif
template<class Key>
  struct ITNode {
    Key* key;
    ITNode* lesser;
    ITNode* greater;
    unsigned int height;
    ITNode()
    : key(NULL)
    {
      //std::cout << "Creation of void node.\n";
#ifndef SPRAWDZACZKA
      voidNodeNr++;
#endif
      lesser = NULL;
      greater = NULL;
      height = 0;
    }
    ITNode(Key *k)
    : key(k)
    {
      //std::cout << "Creation of node with key " << *k << ".\n";
#ifndef SPRAWDZACZKA
      keyNodeNr++;
#endif
      lesser = new ITNode<Key>;
      greater = new ITNode<Key>;
      height = 1;
    }
    ITNode(const ITNode& n)
    {
      assert(false);
      (*this) = n;
    }
    ITNode& operator=(const ITNode& n)
    {
      assert(false);
      key = n.key;
      if(lesser != NULL) delete(lesser);
      lesser = new(ITNode) (*n.lesser);
      *lesser = *n.lesser;
      if(greater != NULL) delete(greater);
      greater = new(ITNode) (*n.greater);
      *lesser = *n.greater;
      updateHeight();
    }
    ITNode(Key *k, const ITNode* lptr, const ITNode* hptr)
    : key(k), lesser(lptr), greater(hptr)
    {
      assert(false);
      lesser = new(Key) (*lptr);
      *lesser = *lptr;
      greater = new(Key) (*hptr);
      *greater = *hptr;
      updateHeight();
    }
    ~ITNode()
    {
      
#ifndef SPRAWDZACZKA
       if(key == NULL){
         //std::cout << "Void node deleted.\n";
         voidNodeNr--;
       }
       else {
         //std::cout << "Node with key " << *key << " deleted.\n";
         keyNodeNr--;
       }
#endif
      delete(lesser);
      delete(greater);
    }
    ITNode* rotateLeft()
    {
      assert(key != NULL);
      //std::cout << "Rotate left..\n";
      ITNode *p(this->greater);
      assert(p != NULL && p->key != NULL);
      this->greater = p->lesser;
      p->lesser = this;
      updateHeight();
      p->height = std::max(height, p->greater->height) + 1;
      //std::cout << " --l\n";
      return p;
    }

    ITNode* rotateRight()
    {
      assert(key != NULL);
      //std::cout << "Rotate right..\n";
      ITNode *l(this->lesser);
      assert(l != NULL && l->key != NULL);
      this->lesser = l->greater;
      l->greater = this;
      updateHeight();
      l->height = std::max(l->lesser->height, height) + 1;
      //std::cout << " --r\n";
      return l;
    }

    void updateHeight()
    {
      //std::cout << " Height update..\n";
      height = std::max(lesser->height, greater->height) + 1;
      //std::cout << " --h\n";
    }

    bool is_AVL()
    {
      if(height == 0)
        return true;
      if(abs(lesser->height - greater->height) > 1)
        return false;
      return (lesser->is_AVL() && greater->is_AVL());
    }

    int getBalance()
    {
      if(key == NULL)
        return 0;
      return lesser->height - greater->height;
    }

    ITNode<Key>* findMin()
    {
      if(lesser->key == NULL)
        return this;
      return lesser->findMin();
    }
  };

template<class Key>
  std::ostream& operator<<(std::ostream& stream, const ITNode<Key>& arg)
{
  if(arg.lesser != NULL) {
    stream << "{ ";
    stream << *arg.lesser;
    stream << "} ";
  }
  if(arg.key != NULL)
    stream << *arg.key;
  else
    stream << " <> ";
  stream << "(" << arg.height << ") ";
  if(arg.greater != NULL) {
    stream << "[ ";
    stream << *arg.greater;
    stream << "] ";
  }
  return stream;
}

template<class Key>
  bool operator<(const ITNode<Key>& a, const ITNode<Key>& b)
{
  return *a.key < *b.key;
}

template<class Key>
  ITNode<Key>* deleteKey(ITNode<Key>* arg, Key *k)
{
  //std::cout << "Deleting.. " << *k << " from " << *arg << "\n";
  if(arg->key == NULL)
    return arg;
  if(arg->key == k) {
    if(arg->lesser->key == NULL || arg->greater->key == NULL) {
      ITNode<Key>* ret;
      if(arg->lesser->key == NULL) {
        ret = arg->greater;
        arg->greater=NULL;
        delete(arg);
        return ret;
      }
      ret = arg->lesser;
      arg->lesser = NULL;
      delete(arg);
      return ret;
    }
    ITNode<Key>* min = arg->greater->findMin();
    Key* minkey = min->key;
    arg->greater = deleteKey(arg->greater, minkey);
    arg->key = minkey;
  }
  if(*k < *arg->key) {
    arg->lesser = deleteKey(arg->lesser, k);
  } else {
    arg->greater = deleteKey(arg->greater, k);
  }
  arg->updateHeight();
  //std::cout << " = " << *arg << " => " << arg->getBalance() << "\n";
  if(arg->getBalance() == 2) {
    if( arg->lesser->getBalance() >= 0) {
      return arg->rotateRight();
    } else {
      arg->lesser = arg->lesser->rotateLeft();
      return arg->rotateRight();
    }
  }
  if(arg->getBalance() == -2) {
    if(arg->greater->getBalance() <= 0) {
      return arg->rotateLeft();
    } else {
      arg->greater = arg->greater->rotateRight();
      return arg->rotateLeft();
    }
  }
  return arg;
}

template<class Key>
  ITNode<Key>* insertKey(ITNode<Key>* arg, Key * k)
{
  //std::cout << "Inserting.. " << *k << " to " << *arg << "\n";
  if(k == arg->key)
    return arg;
  if(*k < * (arg->key)) {
    if(arg->lesser->key == NULL) {
      delete(arg->lesser);
      arg->lesser = new ITNode<Key> (k);
    } else {
      arg->lesser = insertKey(arg->lesser, k);
    }
    arg->updateHeight();
    if(arg->greater == NULL || /*arg->lesser->height > arg->greater->height + 1*/ arg->getBalance() == 2) {
      if(arg->greater == NULL || /*arg->lesser->lesser->height > arg->lesser->greater->height*/ arg->lesser->getBalance() == 1) {
        return arg->rotateRight();
      } else {
        arg->lesser = arg->lesser->rotateLeft();
        return arg->rotateRight();
      }
    } else {
      return arg;
    }
  } else {
    if(arg->greater->key == NULL) {
      delete(arg->greater);
      arg->greater = new ITNode<Key> (k);
    } else {
      arg->greater = insertKey(arg->greater, k);
    }
    arg->updateHeight();
    if(arg->lesser == NULL || /*arg->greater->height > arg->lesser->height + 1*/ arg->getBalance() == -2) {
      if(arg->lesser == NULL || /*arg->greater->lesser->height < arg->greater->greater->height*/ arg->greater->getBalance() == -1) {
        return arg->rotateLeft();
      } else {
        arg->greater = arg->greater->rotateRight();
        return arg->rotateLeft();
      }
    } else {
      return arg;
    }
  }
}


int main()
{
  const int size(1000);
  for(int _i_ = 0; _i_ < 2000; _i_++) {
    int t[size], z(0);
    srand(time(0));
    ITNode<int>* head = new ITNode<int>(&z);
    for(int i = 1; i < size; i++) {
      //std::cout << *head << "\n---\n";
      t[i] = rand() % (2 * size);
      head = insertKey(head, t + i);
      if(!head->is_AVL())
      {
        std::cout << *head << "\n";
        return 255;
      }
    }
    for(int i = 1; i < size/2; i++) {
      int j = rand() % size;
      head = deleteKey(head, t + j);
      //std::cout << *head << "\n---\n";
      if(!head->is_AVL())
      {
        std::cout << *head << "\n";
        return 255;
      }
    }
    //std::cout << *head << "\n";
    delete(head);
    assert(voidNodeNr == 0);
    assert(keyNodeNr == 0);
//    std::cout << voidNodeNr << " " << keyNodeNr << "\n";
  }
}

/*
Interval<int> t[100010];
int main()
{
  int n, a, b, c, d, e;
  scanf("%d", &n);
  scanf("%d %d %d %d %d", &a, &b, &c, &d, &e);
  t[0] = Interval<int>(Point<int>(a, b), Point<int>(c, d), e);
  ITNode<int> first(t);
  ITNode<int>* head = & first;
  for(int i = 1; i < n; i++) {
    scanf("%d %d %d %d %d", &a, &b, &c, &d, &e);
    t[i] = Interval<int>(Point<int>(a, b), Point<int>(c, d), e);
    head = insertKey(head, &t[i]);
  }
}
*/
