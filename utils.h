#ifndef UTILS_H
#define UTILS_H

/*
 * Copyright (c) 2014 Benjamin C. Herd.
 *
 * This file is part of MC2MABS.
 *
 * MC2MABS is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * MC2MABS is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with MC2MABS. If not, see <http://www.gnu.org/licenses/>.
 */

/*!
 * \brief Simple 2D vector class
 * \author Benjamin C. Herd
 * \date 10/2014
 * \copyright GNU Public License (GPL)
 *
class V2D
{
public:

	/// Default constructor
	V2D()
	:	x(0),
		y(0)
	{}

	/*!
	 * \brief Special constructor
	 * @param[in] _x x coordinate
	 * @param[in] _y y coordinate
	 */
	V2D(double _x, double _y)
	:	x(_x),
		y(_y)
	{}
	
	/// Default destructor
	virtual ~V2D() {}

	/*!
	 * \brief Return the x coordinate
	 * @return x coordinate
	 */
	double getX() const { return x; }

	/*!
	 * \brief Return the y coordinate
	 * @return y coordinate
	 */
	double getY() const { return y; }

	/*!
	 * \brief Set the x coordinate
	 * @param[in] x New x coordinate
	 */
	void setX(double x) { this->x = x; }

	/*!
	 * \brief Set the y coordinate
	 * @param[in] y New y coordinate	 *
	 */
	void setY(double y) { this->y = y; }
	
	/*!
	 * \brief Vector-scalar multiplication and assigment
	 * @param[in] f Scalar to be multiplied with
	 * @return New vector
	 */
	V2D operator*=(double f)
	{
		this->x *= f;
		this->y *= f;
		return *this;
	}

	/*!
	 * \brief Vector-scalar multiplication
	 * @param[in] f Scalar to be multiplied with
	 */
	V2D operator*(double f) const
	{
		return V2D(x*f, y*f);
	}

	/// Return length of the vector
	double len() const
	{
		return sqrt(x*x+y*y);
	}

	/// Return squared length of the vector
	double lenSq() const
	{
		return (x*x+y*y);
	}

	/// Normalise the vector
	void normalise()
	{
		double mod = len();
		x /= mod;
		y /= mod;
	}

	/*!
	 * Return scaled copy of the vector
	 * @param[in] f New length
	 * @return Scaled copy of the vector
	 */
	V2D getScaled(double f) const
	{
		V2D v(x,y);
		v.normalise();	
		v *= f;
		return v;
	}

protected:
	double x; ///< x coordinate
	double y; ///< y coordinate
};

/*!
 * \brief Simple 2D point class
 * \author Benjamin C. Herd
 * \date 10/2014
 * \copyright GNU Public License (GPL)
 */
class P2D
{
public:
	/// Default constructor
	P2D()
	: x(0),
		y(0)
	{}

	/*!
	 * Special constructor
	 * @param[in] _x x coordinate
	 * @param[in] _y y coordinate
	 */
	P2D(double _x, double _y)
	: x(_x),
		y(_y)
	{}

	/// Default destructor
	virtual ~P2D() {}

	/*!
	 * \brief Return the x coordinate
	 * @return x coordinate
	 */
	double getX() const { return x; }

	/*!
	 * \brief Return the y coordinate
	 * @return y coordinate
	 */
	double getY() const { return y; }

	/*!
	 * \brief Set the x coordinate
	 * @param[in] x New x coordinate
	 */
	void setX(double x) { this->x = x; }

	/*!
	 * \brief Set the t coordinate
	 * @param[in] y New y coordinate
	 */
	void setY(double y) { this->y = y; }

	/*!
	 * \brief Comparision operator
	 * @param[in] p P2D object to be compared with
	 * @return True if the two objects describe the same point in space, otherwise false
	 */
	bool operator==(P2D const& p)
	{
		return ( (x==p.getX()) && (y==p.getY()) );
	}

	/*!
	 * \brief Point-vector addition
	 * @param[in] v2d Vector to be added to the current position
	 * @return New position
	 */
	P2D operator+(V2D const& v2d) const
	{
		return P2D(x+v2d.getX(), y+v2d.getY());
	}

	/*!
	 * \brief Point-vector addition and assignment
	 * @param[in] v Vector to be added to the current position
	 * @return New position
	 */
	P2D operator+=(V2D const& v)
	{
		this->x += v.getX();
		this->y == v.getY();
		return *this;
	}

	/*!
	 * \brief Determine distance vector between two points
	 * @param[in] p2d Point to which the distance is to be determined
	 * @return Distance vector
	 */
	V2D operator-(P2D const& p2d) const
	{
		double _x = x-p2d.getX();
		double _y = y-p2d.getY();
		return V2D(_x,_y);
	}
	
protected:
	double x; ///< Length in x direction
	double y; ///< Length in y direction
};

/*!
 * \brief Simple 2D bounding box class
 * \author Benjamin C. Herd
 * \date 10/2014
 * \copyright GNU Public License (GPL)
 */
class BB2D
{
public:
	/*!
	 * \brief Constructor
	 * @param[in] _ul Upper left coordinate
	 * @param[in] _lr Lower right coordinate
	 */
	BB2D(P2D _ul, P2D _lr)
	:	ul(_ul),
		lr(_lr)
	{}

	/// Default destructor
	virtual ~BB2D() {}

	/*!
	 * \brief Determine whether the given point is within the bounding box
	 * @param p Point
	 * @return True if the point is within the given bounding box, otherwise false
	 */
	bool isWithin(P2D const& p) const
	{	
		bool b1 = ( (p.getX() >= ul.getX()) && (p.getX() <= lr.getX()) );
		bool b2 = ( (p.getY() <= ul.getY()) && (p.getY() >= lr.getY()) );
		return b1 && b2;
	}

protected:
	P2D ul; ///< Upper left coordinate
	P2D lr; ///< Lower right coordinate
};

#endif
