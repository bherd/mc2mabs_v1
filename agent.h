#ifndef AGENT_H
#define AGENT_H

#include "abs.h"
#include "utils.h"

#include <map>
#include <vector>
#include <set>

#include <boost/shared_ptr.hpp>
#include <boost/optional.hpp>

typedef std::map<
	size_t,	// attribute name
	double	// attribute value
> StateMap;

enum PosState
{
	INSIDE,
	OUTSIDE
};

class Environment;
class Agent
{
public:
	Agent(size_t, boost::shared_ptr<Environment>);
	virtual ~Agent();

	size_t self() const { return m_Id; }

	P2D getPos() const { return pos; }
	void setPos(P2D const& p) { pos = p; }
	
	AState getCState() const;
	virtual std::set<size_t> getPublicAtts() const;

	virtual void step(
		size_t confId, 
		size_t tick, 
		std::vector<Agent> const& population, 
		boost::shared_ptr<Environment> const& env
	);

	void setStepSize(float);

	void forward();
	void forward(float);
	
	void face(P2D const&);	
	void track(P2D const&);
	void untrack();

	void turn(double);

	P2D lookAhead(float) const;

	StateMap m_State;	

protected:
	size_t m_Id;
	P2D pos;
	boost::optional<P2D> target;
	boost::shared_ptr<Environment> env;
	V2D velocity;
};

class Environment
{
public:
	Environment() {}
	Environment(BB2D const& bb, bool wr=true)
	:	bbox(bb),
		wrapped(wr)
	{}

	virtual ~Environment() {}

	boost::optional<BB2D> const& getBB() { return bbox; }
	bool isWithin(P2D const& p) const;

	virtual void step(
		size_t confId, 
		size_t tick,
		std::vector<Agent> const& population
	);

protected:
	StateMap m_State;	
	boost::optional<BB2D> bbox;
	bool wrapped;
};

#endif
