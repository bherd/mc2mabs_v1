#ifndef UTILS_H
#define UTILS_H

class V2D
{
public:

	V2D()
	:	x(0),
		y(0)
	{}

	V2D(double _x, double _y)
	:	x(_x),
		y(_y)
	{}
	
	virtual ~V2D() {}

	double getX() const { return x; }
	double getY() const { return y; }

	void setX(double x) { this->x = x; }
	void setY(double y) { this->y = y; }
	
	V2D operator*=(double f)
	{
		this->x *= f;
		this->y *= f;
		return *this;
	}

	V2D operator*(double f) const
	{
		return V2D(x*f, y*f);
	}

	double len() const
	{
		return sqrt(x*x+y*y);
	}

	double lenSq() const
	{
		return (x*x+y*y);
	}

	void normalise()
	{
		double mod = len();
		x /= mod;
		y /= mod;
	}

	V2D getScaled(double f) const
	{
		V2D v(x,y);
		v.normalise();	
		v *= f;
		return v;
	}

protected:
	double x, y;
};

class P2D
{
public:
	P2D()
	: x(0),
		y(0)
	{}

	P2D(double _x, double _y)
	: x(_x),
		y(_y)
	{}

	virtual ~P2D() {}

	double getX() const { return x; }
	double getY() const { return y; }

	void setX(double x) { this->x = x; }
	void setY(double y) { this->y = y; }

	bool operator==(P2D const& p)
	{
		return ( (x==p.getX()) && (y==p.getY()) );
	}

	P2D operator+(V2D const& v2d) const
	{
		return P2D(x+v2d.getX(), y+v2d.getY());
	}

	P2D operator+=(V2D const& v)
	{
		this->x += v.getX();
		this->y == v.getY();
		return *this;
	}

	V2D operator-(P2D const& p2d) const
	{
		double _x = x-p2d.getX();
		double _y = y-p2d.getY();
		return V2D(_x,_y);
	}
	
protected:
	double x, y;
};

class BB2D
{
public:
	BB2D(P2D _ul, P2D _lr)
	:	ul(_ul),
		lr(_lr)
	{}

	virtual ~BB2D() {}

	bool isWithin(P2D const& p) const
	{	
		bool b1 = ( (p.getX() >= ul.getX()) && (p.getX() <= lr.getX()) );
		bool b2 = ( (p.getY() <= ul.getY()) && (p.getY() >= lr.getY()) );
		return b1 && b2;
	}

protected:
	P2D ul, lr;
};

#endif
