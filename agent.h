#ifndef AGENT_H
#define AGENT_H

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

#include <boost/optional/optional.hpp>
#include <boost/smart_ptr/shared_ptr.hpp>
#include <stddef.h>
#include <map>
#include <set>
#include <vector>
#include <iostream>

#include "abs.h"
#include "utils.h"

/// Type definition for a state map (key-value store)
typedef std::map<
	size_t,	// attribute name
	double	// attribute value
> StateMap;

/// Enumeration referring describing the position inside and outside of the two-dimensional state
enum PosState
{
	INSIDE,
	OUTSIDE
};

class Environment;
/*!
 * \brief This class represents an individual agent including its state and behavioural capabilities.
 * \author Benjamin C. Herd
 * \date 10/2014
 * \copyright GNU Public License (GPL)
 */
class Agent
{
public:
	/*!
	 * \brief Main constructor
	 * @param[in] _id agent id
	 * @param[in] _env environment pointer
	 */
	Agent(size_t _id, boost::shared_ptr<Environment> _env);

	///Destructor
	virtual ~Agent();

	/*!
	 * @return Agent id (size_t)
	 */
	size_t self() const { return m_Id; } ///< Return the agent id

	/*!
	 * \brief Return the current position of the agent
	 * @return Current position (P2D)
	 */
	P2D getPos() const { return pos; }

	/*!
	 * \brief Set the current position of the agent
	 * @param[in] p Current position (P2D)
	 */
	void setPos(P2D const& p) { pos = p; }
	
	/*!
	 * \brief Construct and return the current state of the agent
	 * @return Current state (AState)
	 */
	AState getCState() const;

	/*!
	 * \brief Construct and return a set of publicly accessible attributes
	 * @return Public attributes (std::set<size_t>)
	 */
	virtual std::set<size_t> getPublicAtts() const;

	/*!
	 * \brief Perform a single update step.
	 * @param[in] confId Current configuration id
	 * @param[in] tick Current tick
	 * @param[in] env Shared environment pointer
	 */
	virtual void step(
		size_t confId, 
		size_t tick, 
		std::vector<Agent> const& population, 
		boost::shared_ptr<Environment> const& env
	);

	/*!
	 * \brief
	 * Set the current step size of the agent
	 * @param f[in] Step size
	 */
	void setStepSize(float f);

	///Move ahead a single step
	void forward();
	/*!
	 * \brief Move ahead a certain number of steps
	 * @param f Number of steps to move forward
	 */
	void forward(float f);
	
	/*!
	 * \brief Turn towards a certain point of interest
	 * @param[in] p Point of interest
	 */
	void face(P2D const&);	

	/*!
	 * \brief Store a certain position as 'target point'
	 * @param p Point of interest
	 */
	void track(P2D const&);

	///Delete current target point
	void untrack();

	/*!
	 * \brief Turn by a certain angle
	 * @param deg Angle (degrees)
	 */
	void turn(double);

	/*!
	 * \brief Look ahead, i.e. return position that lies a distance ahead
	 * @param f Distance
	 */
	P2D lookAhead(float) const;

	StateMap m_State; ///< Agent state

protected:
	size_t m_Id; ///< Agent id
	P2D pos; ///< Position in a two-dimensional space
	boost::optional<P2D> target; ///< Optional target point
	boost::shared_ptr<Environment> env; ///< Shared environment pointer
	V2D velocity; ///< Current velocity
};

/*!
 * \brief This class represents the environment. At the current stage, only two-dimensional space is supported.
 * \author Benjamin C. Herd
 * \date 10/2014
 * \copyright GNU Public License (GPL)
 */
class Environment
{
public:
	Environment() {} ///< default constructor
	/*!
	 * \brief Special constructor
	 * @param[in] bb Bounding box, i.e. spatial dimension of the environment
	 * @param[in] wr Specifies whether the space is wrapped around
	 */
	Environment(BB2D const& bb, bool wr=true)
	:	bbox(bb),
		wrapped(wr)
	{}

	virtual ~Environment() {} ///< Default destructor

	/*!
	 * \brief Returns the (optional) spatial dimensions of the environment as a two-dimensional bounding box
	 * @return Optional bounding box
	 */
	boost::optional<BB2D> const& getBB() { return bbox; }
	bool isWithin(P2D const& p) const;

	/*!
	 * \brief Perform a single update step
	 * @param[in] confId Current configuration id
	 * @param[in] tick Current tick
	 * @param[in] population Agent population, i.e. vector of agent objects
	 */
	virtual void step(
		size_t confId, 
		size_t tick,
		std::vector<Agent> const& population
	);

protected:
	StateMap m_State; ///< State of the environment
	boost::optional<BB2D> bbox; ///< Optional bounding box
	bool wrapped; ///< Wrapped environment (yes/no)
};

#endif
