#ifndef ABS_H
#define ABS_H

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

typedef int* AAtt; ///< Agent attribute (= sequence of integers)
typedef AAtt* AState; ///< Agent state (= sequence of agent attributes)
typedef AState* GState; ///< Group state (= sequence of agent states)
typedef GState* GT; ///< Group trace (= sequence of group states)

///////////////////////////////////////////////////////////////////////
// EXPORTED FUNCTIONS (AND THEIR INTERNAL EQUIVALENTS)
///////////////////////////////////////////////////////////////////////

/*!
 * \brief Run the simulation and produce a group trace
 * @return Group trace
 */
extern GT run();

/*!
 * \brief External initialisation function for a configuration
 * @param[in] Configuration id
 */
extern void preConf(int);

/*!
 * \brief Internal initialisation function for a configuration
 * @param[in] Configuration id
 */
extern void _preConf(int);

/*!
 * \brief External initialisation function for a run
 * @param[in] confId Configuration id
 */
extern void preRun(size_t);

/*!
 * \brief Internal initialisation function for a run
 * @param[in] confId Configuration id
 * @param[in] population Population (vector of agents)
 */
extern void _preRun(
	size_t confId, 
	std::vector<Agent>& population
);

/*!
 * \brief External function called before all agents are updated
 * @param[in] confId Configuration id
 * @param[in] tick Current tick
 * @param[in] population Population (vector of agents)
 */
extern void preTick(																
	size_t confId,
	size_t tick,
	std::vector<Agent> const& population
);

/*!
 * \brief Internal function called before all agents are updated
 * @param[in] confId Configuration id
 * @param[in] tick Current tick
 * @param[in] population Population (vector of agents)
 */
extern void _preTick(
	size_t confId, 
	size_t tick,
	std::vector<Agent> const& population
);
/*!
 * \brief Step function for an individual agent
 * @param[in] id Agent id
 * @param[in] tick Current tick
 * @return New agent state
 */
extern AState stepAgent(size_t id, size_t tick);

/*!
 * \brief Step function for the system (population + environment)
 * @param[in] confId Configuration id
 * @param[in] tick Current tick
 * @return New group state
 */
extern GState step(size_t, size_t);

/*!
 * \brief External function called after all agents have been updated
 * @param[in] confId Configuration id
 * @param[in] Current tick
 * @param[in] population Population (vector of agents)
 */
extern void postTick(
	size_t confId, 
	size_t tick,
	std::vector<Agent> const& population
);

/*!
 * \brief Internal function called after all agents have been updated
 * @param[in] confId Configuration id
 * @param[in] tick Current tick
 * @param[in] population Population (agent vector)
 */
extern void _postTick(
	size_t confId, 
	size_t tick,
	std::vector<Agent> const& population);

/*!
 * \brief External uninitialisation function for a run
 * @param[in] confId Configuration id
 */
extern void postRun(size_t);

/*!
 * \brief Internal uninitialisation function for a run
 * @param[in] confId Configuration id
 * @param[in] population Population (agent vector)
 */
extern void _postRun(
	size_t confId, 
	std::vector<Agent>& population
);

/*!
 * \brief External uninitialisation function for a configuration
 * @param idx Configuration id
 */
extern void postConf(int idx);

/*!
 * \brief Internal uninitialisation function for a configuration
 * @param idx Configuration id
 */
extern void _postConf(int idx);

/*!
 * \brief Writes a group trace to a file
 * @param[in] run Number of the current run
 */
extern void writeGTToFile(size_t run);

/*!
 * \brief Assembles and returns a validation property
 * @return Validation property as ASCII string
 */
extern const char* getProperty();

/*!
 * \brief Numeric function on the group level
 * @param[in] att Function id
 * @param[in] tick Current tick
 * @return Function result
 */
extern int gValue(int att, int tick);

/*!
 * \brief Numeric function on the group level (internal version)
 * @param[in] att Function id
 * @param[in] tick Current tick
 * @param[in] population Population (agent vector)
 * @return Function result
 */
extern int _gValue(int att, int tick, std::vector<Agent>& population);

/*!
 * \brief Numeric function on the agent level
 * @param[in] att Function id
 * @param[in] id Agent id
 * @param[in] tick Current tick
 * @return Function result
 */
extern int aValue(int att, int id, int tick);

/*!
 * \brief Numeric function on the agent level
 * @param[in] att Function id
 * @param[in] id Agent id
 * @param[in] tick Current tick
 * @param[in] agent Agent object
 * @return Function result
 */
extern int _aValue(int att, int id, int tick, Agent const& agent);

/*!
 * \brief Boolean function on the group level
 * @param[in] att Function id
 * @param[in] tick Current tick
 * @return Function result
 */
extern bool gPredicate(int att, int tick);

/*!
 * \brief Boolean function on the agent level
 * @param[in] att Function id
 * @param[in] id Agent id
 * @param[in] tick Current tick
 * @return Function result
 */
extern bool aPredicate(int att, int id, int tick);

///////////////////////////////////////////////////////////////////////
// INTERNAL FUNCTIONS
///////////////////////////////////////////////////////////////////////

/*!
 * \brief Construct and return a simLTL correctness formula
 * @param[in] id Configuration id
 * @return String representation of the simLTL formula
 */
extern const char* getFormula(int id);

/*!
 * \brief Constructs a string representation of a configuration setting
 * @param[in] conf Configuration id
 * @param[in] conventional Conventional checking? (Obsolete)
 * @param[in] fragmentSize Fragment size to be used for verification
 * @param[in] numAgents Number of agents in the population
 * @param[in] numAtts Number of agent attributes
 * @param[in] numTicks Number of ticks
 * @param[in] numReps Number of replications
 * @return String representation of a configuration setting
 */
std::string getConf(
	int conf,
	bool conventional,
	int fragmentSize,
	int numAgents,
	int numAtts,
	int numTicks,
	int numReps
);

/*!
 * \brief Return the number of agents in the simulation
 * @return Number of agents
 */
size_t getNumAgents();

/*!
 * \brief Return the number of ticks in the simulation
 * @return Number of ticks
 */
size_t getNumTicks();

/*!
 * \brief Return the number of agent attributes
 * @return Number of agent attributes
 */
size_t getNumAgentAtts();

/*!
 * \brief Return the number of replications
 * @return Number of replications
 */
size_t getNumReps();

/*!
 * \brief Return the checking mode
 * @return Checking mode (true=entire path, false=path fragments)
 */
bool   getCheckingMode();

/*!
 * \brief Return the fragment size
 * @return Fragment size
 */
size_t getFragmentSize();

///////////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
///////////////////////////////////////////////////////////////////////

/*!
 * \brief Determine all agents with a certain attribute-value combination
 * @param[in] Attribute
 * @param[in] Value
 * @return Vector of agents with the given attribute-value combination
 */
std::vector<Agent>::iterator getAllWith(int att, float val);

/*!
 * \brief Determine all agents which are close to the given agent.
 * In this context, 'close' means within a radius of 1 unit
 * @param[in] a Agent
 * @return Vector of all nearby agents
 */
std::vector<Agent>::iterator robotsHere(Agent const&);

#ifdef __cplusplus
}
#endif

#endif
