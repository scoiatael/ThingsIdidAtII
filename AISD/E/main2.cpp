#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include <iostream>
#include <algorithm>
#include<vector>

#ifdef SPRAWDZACZKA
#define NDEBUG
#endif

#include <assert.h>

#ifndef SPRAWDZACZKA
//#define debugid
#endif

template<typename Pos1, typename Pos2>
struct Point {
  Pos1 x;
  Pos2 y;
  Point(const Pos1& i, const Pos2& j)
  : x(i), y(j)
  {}
  Point()
  : x(), y()
  {}
  ~Point()
  {}
};

template<typename Pos1, typename Pos2>
bool operator<(const Point<Pos1, Pos2>& a, const Point<Pos1, Pos2>& b)
{
  return (a.x == b.x) ? (a.y < b.y) : (a.x < b.x);
}

template<typename Pos1, typename Pos2>
std::ostream& operator<<(std::ostream& stream, const Point<Pos1, Pos2>& p)
{
  stream << "(" << p.x << ", " << p.y << ")";
  return stream;
}

template<typename Pos, typename Val>
struct bracket {
  typedef Val Value;
  Pos pos;
  Value val;
  bool operator<(const bracket& arg)
  {
    return (pos == arg.pos) ? val > arg.val : pos < arg.pos;
  }
  bracket(Pos p, Value v)
  : pos(p), val(v)
  {}
  bracket(const bracket& b)
  : pos(b.pos), val(b.val)
  {}
  bracket()
  : pos(), val()
  {}
  bool operator==(const bracket<Pos, Val>& other)
  {
    return pos == other.pos && val == other.val;
  }
};

template<typename Pos, typename Val>
std::ostream& operator<<(std::ostream& stream, const bracket<Pos, Val>& b)
{
  stream << b.val << " at " << b.pos << ";";
  return stream;
}

#ifndef SPRAWDZACZKA
unsigned int voidNodeNr, keyNodeNr;
#endif

template<typename Key>
struct ITNode {
  Key* key;
  typename Key::Value accum;
  typename Key::Value treeMax;
  ITNode* lesser;
  ITNode* greater;
  unsigned int height;
  ITNode()
  : key(NULL), accum(0), treeMax(0)
  {
#ifndef SPRAWDZACZKA
    //std::cout << "Creation of void node.\n";
    voidNodeNr++;
#endif
    lesser = NULL;
    greater = NULL;
    height = 0;
  }
  ITNode(Key *k)
  : key(k), accum(k->val), treeMax(k->val)
  {
#ifndef SPRAWDZACZKA
    //std::cout << "Creation of node with key " << *k << ".\n";
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
    accum = n.accum;
    height = n.height;
    treeMax = n.treeMax;
    if(lesser != NULL) delete(lesser);
    lesser = new(ITNode) (*n.lesser);
    *lesser = *n.lesser;
    if(greater != NULL) delete(greater);
    greater = new(ITNode) (*n.greater);
    *lesser = *n.greater;
  }
  ITNode(Key *k, const ITNode* lptr, const ITNode* hptr)
  : key(k), lesser(lptr), greater(hptr), accum(*k->val), treeMax(*k->val)
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
    if(key == NULL) {
      //std::cout << "Void node deleted.\n";
      voidNodeNr--;
    } else {
      //std::cout << "Node with key " << *key << " deleted.\n";
      keyNodeNr--;
    }
#endif
    delete(lesser);
    delete(greater);
  }

  bool compareNode(Key* other)
  {
    return (*key == *other) ? (key < other) : (*other < *key);
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
    p->accum += accum;
    updateTreeMax();
    p->updateTreeMax();
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
    accum -= l->accum;
    updateTreeMax();
    l->updateTreeMax();
    return l;
  }

  void updateHeight()
  {
    //std::cout << " Height update..\n";
    height = std::max(lesser->height, greater->height) + 1;
    //std::cout << " --h\n";
  }

  void updateTreeMax()
  {
    treeMax = std::max(lesser->treeMax, std::max(greater->treeMax + accum, accum));
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

  bool contains(Key* k)
  {
    //std::cout << "Lookin for " << k << " in " << *this << "\n";
    if(key == NULL)
      return false;
    if(key == k)
      return true;
    if(compareNode(k))
      return lesser->contains(k);
    return greater->contains(k);
  }


  typename Key::Value getValueAt(Key* k)
  {
    //std::cout << "Gettin val for " << *k << " in " << *this << "\n";
    assert(contains(k));
    if(key == k)
      return accum;
    if(compareNode(k))
      return lesser->getValueAt(k);
    else
      return accum + greater->getValueAt(k);
  }

  void toVector(std::vector<Key*>& r)
  {
    if(key==NULL)
      return;
    lesser->toVector(r);
    r.push_back(key);
    greater->toVector(r);
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
    stream << *arg.key /*<< "(" << arg.key << ")"*/;
  else
    stream << " <> ";
  stream << "(" << arg.height << ", " << arg.accum << ", " << arg.treeMax  << ") ";
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
  return b.compareNode(a.key);
}

template<class Key>
  ITNode<Key>* deleteKey(ITNode<Key>* arg, Key *k)
{
#ifdef debugid
  std::cout << "Deleting.. " << *k << " from " << *arg << "\n";
#endif
  assert(arg->contains(k));
  if(arg->key == k) {
    if(arg->lesser->key == NULL || arg->greater->key == NULL) {
      ITNode<Key>* ret;
      if(arg->lesser->key == NULL) {
        ret = arg->greater;
        arg->greater = NULL;
        delete(arg);
      }
      else {
      ret = arg->lesser;
      arg->lesser = NULL;
      delete(arg);
      }
      return ret;
    }
    ITNode<Key>* min = arg->greater->findMin();
    Key* minkey = min->key;
    arg->greater = deleteKey(arg->greater, minkey);
    arg->accum += minkey->val - arg->key->val;
    arg->key = minkey;
  } else {
    if(arg->compareNode(k)) {
      arg->lesser = deleteKey(arg->lesser, k);
      arg->accum -= k->val;
    } else {
      arg->greater = deleteKey(arg->greater, k);
    }
  }
  arg->updateHeight();
  arg->updateTreeMax();
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
#ifdef debugid
  std::cout << "Inserting.. " << *k << " to " << *arg << "\n";
#endif
  if(arg->key == NULL)
  {
    delete(arg);
    return new ITNode<Key> (k);
  }
  if(k == arg->key)
    return arg;
  if(arg->compareNode(k)) {
    if(arg->lesser->key == NULL) {
      delete(arg->lesser);
      arg->lesser = new ITNode<Key> (k);
    } else {
      arg->lesser = insertKey(arg->lesser, k);
    }
    arg->updateHeight();
    arg->accum += k->val;
    arg->updateTreeMax();
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
    arg->updateTreeMax();
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

const int maxsize(100001);
std::vector< bracket<int, int> >br(2 * maxsize);

enum brType {Ins, Del};
typedef Point<bracket<int, int>*, bracket<int, int>* > IAux2;
typedef Point<brType, IAux2> IAux1;
typedef Point<int, IAux1 > Interval;
std::vector<Interval> t(2 * maxsize);


int main()
{
  int n, a, b, c, d, e;
  scanf("%d", &n);
  assert(maxsize > n);
  for(int i = 0; i < n; i++) {
    scanf("%d %d %d %d %d", &a, &b, &c, &d, &e);
    br[2*i] = (bracket<int, int>(b, e));
    br[2*i+1] = (bracket<int, int>(d, -e));
    t[2*i] = (Interval(a, IAux1(Ins, IAux2(&br[2*i], &br[2*i+1]))));
    t[2*i+1] = (Interval(c, IAux1(Del, IAux2(&br[2*i], &br[2*i+1]))));
  }
  std::sort(t.begin(), t.begin()+2*n);
  ITNode<bracket<int, int> >* head = new ITNode<bracket<int,int> > ();
  long long max = head->treeMax;
  for(int i = 0; i < 2*n; i++) {
#ifdef debugid
    std::cout << "---\n";
#endif
    //std::cout << *head << "\n";
    if(t[i].y.x == Ins) {
      head = insertKey(head, t[i].y.y.y);
      head = insertKey(head, t[i].y.y.x);
    } else {
      head = deleteKey(head, t[i].y.y.x);
      head = deleteKey(head, t[i].y.y.y);
    }
    if(head->treeMax > max)
      max = head->treeMax;
  }
  printf("%lld\n", max);
}

/*
*/

/*
template<typename Key>
typename Key::Value sumTo(Key** from, Key* to)
{
  typename Key::Value accum(0);
  Key** i;
  for(i = from; *i != to; i++){
    accum += (*i)->val;
    printf("%d(%d) ", (*i)->val, accum);
  }
  accum += (*i)->val;
  printf("%d(%d) ", (*i)->val, accum);
  printf("\n");
  return accum;
}

template<typename Key>
typename Key::Value getMax(Key** from, Key** to)
{
  typename Key::Value accum(0), max(0);
  Key** i;
  for(i = from; i < to; i++) {
    accum += (*i)->val;
    printf("%d(%d) ", (*i)->val, accum);
    if(accum > max)
      max = accum;
  }
  printf("\n");
  return max;
}

int main(int argc, char** argv)
{
  const int size(10);
  srand(time(0)*clock()*clock());
  const int testnr( (argc > 1) ? 1 : 200);
  for(int _i_ = 0; _i_ < testnr; _i_++) {
    std::cout << ".";
    std::cout.flush();
    bracket<int, int> t[size], z(0, 0);
    std::vector<bracket<int,int>* > test;
    ITNode<bracket<int, int> >* head = new ITNode<bracket<int, int> >(&z);
    for(int i = 1; i < size; i++) {
      //std::cout << *head << "\n---\n";
      int pos = rand() % size, val = size - rand() % (2 * size);
      if(argc > 1)
        scanf("%d %d", &pos, &val);
      printf("%d %d\n", pos, val);
      t[i] = bracket<int , int>(pos, val);
      head = insertKey(head, t + i);
      test.clear();
      head->toVector(test);
      if(!head->is_AVL()) {
        std::cout << "Not AVL " << *head << "\n";
        return 255;
      }
      if(!(head->getValueAt(t + i) == sumTo(&test[0], t+i))) {
        std::cout << "Wrong val at " << i << " " << *head << " | " << head->getValueAt(t + i) << " vs " << sumTo(&test[0], t+i) << "\n";
        return 255;
      }
      if(head -> treeMax != getMax(&test[0], &test[test.size()])) {
        std::cout << "Wrong max " << *head << " | " << head->treeMax << " vs " << getMax(&test[0], &test[test.size()]) << "\n";
        return 255;
      }
    }
    
    for(int i = 1; i < size / 2; i++) {
      //std::cout << *head << "\n---\n";
      int j = rand() % size;
      if(argc > 1)
        scanf("%d", &j);
      printf("%d\n", j);
      if(head->contains(t + j)) {
        head = deleteKey(head, t + j);
      }
      if(!head->is_AVL()) {
        std::cout << "Not AVL " << *head << "\n";
        return 255;
      }
      int k;
      do k = rand() % size;
      while(!(head->contains(t + k)));
      if(argc > 1)
        scanf("%d", &k);
      printf("%d\n", k);
      test.clear();
      head->toVector(test);
      std::cout << ">> ";
      for(int j=0; j<test.size(); j++)
        std::cout << test[j]->val << " ";
      std::cout << "\n";
      int val = head->getValueAt(t+k), valp = sumTo(&test[0], t+k);
      if(val != valp) {
        std::cout << "Wrong val at " << t[k] << " " << *head << " | " << val << " vs " << valp << "\n";
        return 255;
      }
      int max = head->treeMax, maxp = getMax(&test[0], &test[test.size()]);
      if(max!=maxp) {
        std::cout << "Wrong max " << *head << " | " << max << " vs " << maxp << "\n";
        return 255;
      }
    }
    //std::cout << *head << "\n";
    delete(head);
    assert(voidNodeNr == 0);
    assert(keyNodeNr == 0);
//    std::cout << voidNodeNr << " " << keyNodeNr << "\n";
  }
  std::cout << "\n";
}
*/
