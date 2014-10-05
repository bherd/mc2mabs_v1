/*
	TODO:
	- change to SIR model and introduce state transition errors
*/

#include "../abs.h"
#include "../agent.h"

#include <string>
#include <sstream>

using namespace std;
using namespace boost;

///////////////////////////////////////////////////////////////////////
// CONFIG PARAMETERS
///////////////////////////////////////////////////////////////////////

size_t getNumAgents()    { return 1000; }
size_t getNumTicks()     { return 100; }
size_t getNumAgentAtts() { return 1; }
size_t getNumReps()      { return 100; }
bool   getCheckingMode() { return false; }
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

boost::mt19937 gen(time(NULL));
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
	//string f = "GFinally (GExist 1000 (GALTL (AEq (AAttribute 2) (ANumVal 2))))";
	string f = "GGlobally (GForall (GALTL (AImpl (AEq (AAttribute 2) (ANumVal 2)) (ANext (AEq (AAttribute 2) (ANumVal 2))))))"; // naive approach => undecidable

	//string f = "GGlobally (GForall (GALTL (AImpl (AEq (AAttribute 2) (ANumVal 2)) (AOr ALast (ANext (AEq (AAttribute 2) (ANumVal 2)))))))"; // look at a single agent only
	
	//string f = "GForall (GALTL (AGlobally (AImpl (AEq (AAttribute 2) (ANumVal 2)) (ARelease ALast (AEq (AAttribute 2) (ANumVal 2))))))"; // exhaustive variant, still contains ALast (=> slow)
	//string f = "GForall (GALTL (AGlobally (AImpl (AEq (AAttribute 2) (ANumVal 2)) (ARelease AFalse (AEq (AAttribute 2) (ANumVal 2))))))"; // exhaustive variant, without ALast (=> slow)
	//string f = "GForall (GALTL (AGlobally (AImpl (AEq (AAttribute 2) (ANumVal 2)) (AGlobally (AEq (AAttribute 2) (ANumVal 2))))))"; // exhaustive variant, same as the one above (=> slow)
	
	//string f = "GForall (GALTL (AGlobally (ANot (ARelease (AEq (AAttribute 2) (ANumVal 2)) (AEq (AAttribute 2) (ANumVal 0))))))"; // why is the output not correct??
	
	//string f = "GForall (GALTL (ANot (AAnd (AEq (AAttribute 2) (ANumVal 2)) (ANext (AEq (AAttribute 2) (ANumVal 0))))))"; // fragmental variant (=> faster than exhaustive variant)
	//string f = "GForall (GALTL (AImpl (AEq (AAttribute 2) (ANumVal 2)) (ANext (AEq (AAttribute 2) (ANumVal 2)))))"; // fragmental variant (=> faster than exhaustive variant)
  //string f = "GALTL (AGlobally (AImpl (AEq (AAttribute 2) (ANumVal 2)) (AOr ALast (ANext (AEq (AAttribute 2) (ANumVal 2))))))"; // sampling variant, with ALast (=> fast)
  //string f = "GALTL (AGlobally (AImpl (AEq (AAttribute 2) (ANumVal 2)) (AGlobally (AEq (AAttribute 2) (ANumVal 2)))))"; // sampling variant, without ALast (=> fast)
	
	return f.c_str(); 
} 

int _gValue(int, int, std::vector<Agent>&) { return 0; }
int _aValue(int, int, int, Agent const&) { return 0; }
bool gPredicate(int, int) { return true; }
bool aPredicate(int, int, int) { return true; }

enum Attribute 
{
  AGE=0,
  GENDER=1,
  HEALTH=2
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

map<int, vector<int> > nb;

void _preConf(int idx)
{
	// setup neighbourhood relationship
	for(int i=0; i<getNumAgents(); i++)
	{
		// create 5 random neighbours per agent
		// TODO: avoid duplicates and self-loops
		for(int j=0; j<5; j++)
			nb[i].push_back(rand() % 1000);
	}
}

void _preRun(size_t confId, vector<Agent>& population)
{
	// initialise health state of agents
  	int idx=0;
	vector<Agent>::iterator it;
	for(it=population.begin(); it!=population.end(); ++it) {
		if(idx++ == 0)
			it->m_State[HEALTH] = INFECTED;
		else
			it->m_State[HEALTH] = SUSCEPTIBLE;
	}
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

void _postConf(int idx)
{
	// delete neighbourhood relationship
	nb.clear();
}

///////////////////////////////////////////////////////////////////////
// AGENT UPDATE FUNCTION
///////////////////////////////////////////////////////////////////////

float getRand()
{
	return die()/100.0;
}

void Environment::step(
	size_t confId,
	size_t tick, 
	vector<Agent> const& population)
{
}

void Agent::step(
	size_t confId,
	size_t tick, 
	vector<Agent> const& population,
	shared_ptr<Environment> const& env
)
{
	if(m_State[HEALTH] == SUSCEPTIBLE) 
	{	 	
		// determine number of infected neighbours
		float cnt=0;
		vector<int>::iterator it;
		for(it=nb[m_Id].begin(); it!=nb[m_Id].end(); ++it) {
			StateMap::const_iterator itState = population[*it].m_State.find(HEALTH);
			if(itState->second == INFECTED) 
				cnt++;
		}

		// transition to infected based on number of infected neighbours
		if(getRand() <= (cnt/5.0))
			m_State[HEALTH] = INFECTED; 
	}
	else if(m_State[HEALTH] == INFECTED) {
		if(getRand() <= 0.5)
			m_State[HEALTH] = RECOVERED;
	}
	else
	{
			m_State[HEALTH] = RECOVERED;

			//if(getRand() <= 0.8)
				//m_State[HEALTH] = SUSCEPTIBLE;
	}		
}

