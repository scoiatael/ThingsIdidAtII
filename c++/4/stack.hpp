class stack
{
unsigned int current_size;
unsigned int current_capacity;
double* tab;
// class invariant is that tab is always
// allocated with a block with current_capacity.
void ensure_capacity( unsigned int c );
// Ensure that stack has capacity of at least c.
public:
stack( ); // Constructs empty stack.
stack( const stack& s ); // These are the 3 essential methods:
~stack( );
void operator = ( const stack& s );
void push( double d );
// Use ensure_capacity, so that
// pushing is always possible, as
// long as memory is not full.
void reset( unsigned int s ); // Resets the stack to length of
// s < size( ).
double operator [ ] ( unsigned int i ) const;
double& operator [ ] ( unsigned int i );
// Be careful, s[0] is equal to top of stack.
// s[ s. size( ) - 1 ] is the deepest element.
double top( ) const;
double& top( );
unsigned int size( ) const { return current_size; }
stack& operator += ( const stack& s );
friend bool operator == ( const stack& s1, const stack& s2 );
friend bool operator != ( const stack& s1, const stack& s2 );
friend std::ostream& operator << ( std::ostream& a, const stack& s );
};

void stack::ensure_capacity( unsigned int c )
{
    if( current_capacity < c )
    {
        // New capacity will be the greater of c and
        // 2 * current_capacity.
        if( c < 2 * current_capacity )
            c = 2 * current_capacity;
        double* newtab = new double[ c ];
        for( unsigned int i = 0; i < c; ++ i )
        newtab[i] = tab[i];
        current_capacity = c;
        delete[] tab;
        tab = newtab;
    }
}
