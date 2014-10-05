#include "agent.h"

#include <stdio.h>

using namespace std;
using namespace boost;

///////////////////////////////////////////////////////////////////////
// AGENT FUNCTIONS
///////////////////////////////////////////////////////////////////////

// agent constructor
Agent::Agent(size_t id, shared_ptr<Environment> _env)
:	env(_env)
{
	m_Id = id;
}

// agent destructor
Agent::~Agent()
{
}

AState Agent::getCState() const
{
	set<size_t> publicAtts = getPublicAtts();

	size_t numAtts = publicAtts.size();

	AState state = new AAtt[numAtts];
	if(state==NULL) {
		printf("Error allocating memory!\n");
		exit(1);
	}

	int i=0;
	StateMap::const_iterator it;
	for(it=m_State.begin(); it!=m_State.end(); ++it) {
		AAtt att = new int[2];
		att[0] = it->first;
		att[1] = (int)it->second;
		if(publicAtts.find(att[0])!=publicAtts.end())
			state[i++] = att;
	}

	return state;
}

void Agent::setStepSize(float f)
{
	velocity.normalise();
	velocity *= f;
	printf("%f/%f", velocity.getX(), velocity.getY());
}

void Agent::forward()
{
	pos += velocity;
	printf("Moving to %.2f/%.2f\n", pos.getX(), pos.getY());	
}

void Agent::forward(float f)
{
	if(velocity.lenSq() != f*f)
	{
		velocity.normalise();
		velocity *= f;
	}
	pos += velocity;
	printf("Moving to %.2f/%.2f\n", pos.getX(), pos.getY());
}

void Agent::face(P2D const& p)
{
	this->velocity = p-pos;
	printf("Velocity: %f/%f\n", velocity.getX(), velocity.getY());
}

void Agent::track(P2D const& p)
{
	this->target = p;
	face(p);	
}

void Agent::untrack()
{
	this->target.reset();
}

void Agent::turn(double deg)
{
	// TODO: this rotation doesn't preserve the length of the velocity vector!!
	// convert degree to radians 
	double rad = deg * (3.14159265 / 180.0);
	printf("Velocity.lenSq() (before): %f\n", velocity.lenSq());

	double rx = (velocity.getX() * cos(rad)) - (velocity.getY() * sin(rad));
  double ry = (velocity.getX() * sin(rad)) + (velocity.getY() * cos(rad));
  velocity.setX(rx);
  velocity.setY(ry);

	//double tY = velocity.getY()-pos.getY();
	//double tX = velocity.getX()-pos.getX();

	//double cosa = cos(rad);
	//double sina = sin(rad);

	//velocity.setX(tX*cosa + tY*sina + pos.getX());
	//velocity.setY(-tX*sina + tY*cosa + pos.getY());
	printf("Velocity.lenSq(): %f\n", velocity.lenSq());
	cout << velocity.lenSq() << endl << flush;
	BOOST_ASSERT( abs(velocity.lenSq()-(double)1.0) <= numeric_limits<double>::epsilon());
}

P2D Agent::lookAhead(float f) const
{
	P2D newPos = pos + velocity.getScaled(f);
	return newPos;
}


///////////////////////////////////////////////////////////////////////
// ENVIRONMENT FUNCTIONS
///////////////////////////////////////////////////////////////////////

bool Environment::isWithin(P2D const& p) const
{	
	return bbox ? bbox->isWithin(p) : false;
}
