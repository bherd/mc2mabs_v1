#include "../abs.h"
#include "../agent.h"

#include <sstream>

using namespace std;
using namespace boost;

///////////////////////////////////////////////////////////////////////
// CONFIG PARAMETERS
///////////////////////////////////////////////////////////////////////

size_t getNumAgents() { return 1000; }
size_t getNumTicks() { return 100; }
size_t getNumAgentAtts() { return 1; }
size_t getNumReps() { return 100; }
bool getCheckingMode() { return false; }
size_t getFragmentSize() { return getNumTicks(); }

set<size_t> Agent::getPublicAtts() const
{
	set<size_t> res;
	StateMap::const_iterator it;
	for(it=m_State.begin(); it!=m_State.end(); ++it)
		res.insert(it->first);
	return res;
}

///////////////////////////////////////////////////////////////////////
// RANDOM NUMBER GENERATION
///////////////////////////////////////////////////////////////////////

boost::mt19937 gen;
boost::uniform_real<float> dist(0, 100);
boost::variate_generator<boost::mt19937&, boost::uniform_real<float> > die(gen, dist);

///////////////////////////////////////////////////////////////////////
// PROPERTY ASSEMBLY
///////////////////////////////////////////////////////////////////////

const char* getProperty()
{
	string conf = getConf(
		1, 
		getCheckingMode(), 
		getFragmentSize(),
		getNumAgents(), 
		getNumAgentAtts(), 
		getNumTicks(), 
		getNumReps()
	);

	return conf.c_str();
}

const char* getFormula(int idx)
{
	//return "GFinally (GFalse)"; 																										// Experiment 1
	return "GGlobally (GFalse)"; 																										// Experiment 2
	//return "GGlobally (GFinally (GLast))"; 																					// Experiment 3 (version 1)
	//return "GFinally (GGlobally (GTrue))";																					// Experiment 3 (version 2)
	//return "GAnd (GGlobally (GFinally (GFalse))) (GFinally (GGlobally (GTrue)))";		// Experiment 4
	//return "GForall ( GALTL (AFinally AFalse) )";																		// Experiment 6
	//return "GForall ( GALTL (AGlobally AFalse) )";																		// Experiment 7
	//return "GForall ( GALTL (AGlobally (AFinally AFalse) ) )";											// Experiment 8
	//return "GForall ( GALTL (AAnd (AGlobally (AFinally AFalse) ) (AFinally (AGlobally ATrue)) ) )";	// Experiment 8
	//return "GFinally ( GAtom 1 )";
}

int _gValue(int, int, vector<Agent>& population) { return 0; }
int _aValue(int, int, int, Agent const&) { return 0; } 

bool gPredicate(int id, int tick) 
{ 
	return (tick == 50);
}

bool aPredicate(int, int, int) { return true; }

enum Attribute 
{
  HEALTH=0,
	TICK=1
};

enum Health
{
	SUSCEPTIBLE=0,
	INFECTED=1,
	RECOVERED=2
};

///////////////////////////////////////////////////////////////////////
// SETUP & TEARDOWN FUNCTIONS
///////////////////////////////////////////////////////////////////////

void _preConf(int confIdx)
{
}

void _preRun(size_t confId, vector<Agent>& population)
{
	// initialise all agents
	vector<Agent>::iterator it;
	for(it=population.begin(); it!=population.end(); ++it)
		it->m_State[HEALTH] = SUSCEPTIBLE;
}

void _preTick(size_t confId, size_t tick, vector<Agent> const& population)
{
}

void _postTick(size_t confId, size_t tick, vector<Agent> const& population)
{
}

void _postRun(size_t confId, vector<Agent>& population)
{
}

void _postConf(int confIdx)
{
}

///////////////////////////////////////////////////////////////////////
// AGENT UPDATE FUNCTION
///////////////////////////////////////////////////////////////////////

void Environment::step(
	size_t confId,
	size_t tick, 
	vector<Agent> const& population)
{
}

float getRand()
{
	return die()/100.0;
}

void Agent::step(
	size_t confId,
	size_t tick, 
	vector<Agent> const& population,
	shared_ptr<Environment> const& env
)
{
	if(m_State[HEALTH] == SUSCEPTIBLE) {
		if(getRand() <= 0.3)
			m_State[HEALTH] = INFECTED;
	}
	else if(m_State[HEALTH] == INFECTED) {
		if(getRand() <= 0.5)
			m_State[HEALTH] = RECOVERED;
		else 
			m_State[HEALTH] = INFECTED;
	}
	else
	{
		if(getRand() <= 0.7)
			m_State[HEALTH] = SUSCEPTIBLE;
		else 
			m_State[HEALTH] = RECOVERED;
	}
}

