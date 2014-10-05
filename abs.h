#ifndef ABS_H
#define ABS_H

#include <stdlib.h>

#include <vector>
#include <string>

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_real.hpp>
#include <boost/random/variate_generator.hpp>

#ifdef __cplusplus
extern "C" {
#endif

class Agent;

typedef int* AAtt;
typedef AAtt* AState;
typedef AState* GState;
typedef GState* GLH;

///////////////////////////////////////////////////////////////////////
// EXPORTED FUNCTIONS (AND THEIR INTERNAL EQUIVALENTS)
///////////////////////////////////////////////////////////////////////

extern GLH run();

extern void preConf(int);     	                		// external initialisation function for a configuration
extern void _preConf(int);  	                  		// internal initialisation function for a configuration

extern void preRun(size_t);	                    		// external initialisation function for a run
extern void _preRun(
	size_t confId, 
	std::vector<Agent>& population
); 																									// internal initialisation function for a run

extern void preTick(																
	size_t confId,
	size_t tick,
	std::vector<Agent> const& population
);																									// external function called before all agents are updated						

extern void _preTick(
	size_t confId, 
	size_t tick,
	std::vector<Agent> const& population
);																									// internal function called before all agents are updated

extern AState stepAgent(size_t, size_t);    				// step function for an individual agent
extern GState step(size_t, size_t);              		// step function for the system (population + environment)

extern void postTick(
	size_t confId, 
	size_t tick,
	std::vector<Agent> const& population
);																									// external function called after all agents have been updated

extern void _postTick(
	size_t confId, 
	size_t tick,
	std::vector<Agent> const& population);						// internal function called after all agents have been updated

extern void postRun(size_t);		                    // external uninitialisation function for a run
extern void _postRun(
	size_t confId, 
	std::vector<Agent>& population
);  																								// internal uninitialisation function for a run

extern void postConf(int);		                  		// external uninitialisation function for a configuration
extern void _postConf(int);   		              		// internal uninitialisation function for a configuration

extern void writeGLHToFile(size_t);             		// writes a group life history to a file

extern const char* getProperty();               		// returns a validation property

extern int gValue(int, int);                    		// group-level numeric function
extern int _gValue(int, int, std::vector<Agent>&);  // group-level numeric function
extern int aValue(int, int, int);               		// agent-level numeric function
extern int _aValue(int, int, int, Agent const&); 		// agent-level numeric function
extern bool gPredicate(int, int);               		// group-level Boolean function
extern bool aPredicate(int, int, int);          		// group-level Boolean function

///////////////////////////////////////////////////////////////////////
// INTERNAL FUNCTIONS
///////////////////////////////////////////////////////////////////////

extern const char* getFormula(int);                  // returns a formula
std::string getConf(
	int, 
	bool, 
	int, 
	int, 
	int, 
	int, 
	int
);                                                   // constructs a string representation of individual configuration values

size_t getNumAgents();                               // returns the number of agents in the simulation
size_t getNumTicks();                                // returns the number of ticks in the simulation
size_t getNumAgentAtts();                            // returns the number of agent attributes
size_t getNumReps();                                 // returns the number of replications
bool   getCheckingMode();                            // returns the checking mode (true=entire path, false=path fragments)
size_t getFragmentSize();                            // returns the fragment size

///////////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
///////////////////////////////////////////////////////////////////////

std::vector<Agent>::iterator getAllWith(int, float);
std::vector<Agent>::iterator robotsHere(Agent const&);

#ifdef __cplusplus
}
#endif

#endif
