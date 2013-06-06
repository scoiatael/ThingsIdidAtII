class punkt2D
{
    public:
    float x;
    float y;
    punkt2D(float, float);
};

typedef punkt2D vector2D;

class odcinek2D
{
    public:
    punkt2D B;
    punkt2D E;
    odcinek2D(float,float,float,float);
    bool cont(punkt2D);
    void move_x(float);
    void move_y(float);
};

class line
{
    public:
    float A;
    float B;
    float C;
    line(odcinek2D*);
    punkt2D* intersect(line);
};

punkt2D cross(odcinek2D,odcinek2D );
bool par(odcinek2D*,odcinek2D*);
bool perp(odcinek2D*,odcinek2D*);
