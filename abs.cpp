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

#include "abs.h"
#include "agent.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include <iostream>
#include <vector>
#include <fstream>
#include <sstream>

#include <boost/shared_ptr.hpp>

using namespace std;
using namespace boost;

vector<Agent> population;
shared_ptr<Environment> env(new Environment());

GT glh;

bool glhWritten=false;
int seed=0;

string getConf(
	int conf,
	bool conventional,
	int fragmentSize,
	int numAgents,
	int numAtts,
	int numTicks,
	int numReps
)
{
	stringstream ss;
	
	ss << "Atom Pr {";
	ss << "pConfiguration = " << conf << ",";
	ss << "pConventional = " << (conventional ? "True" : "False") << ", ";
	ss << "pFragmentSize = " << fragmentSize << ", ";
	ss << "pNumAgents = " << numAgents << ",";
	ss << "pNumAtts = " << numAtts << ",";
	ss << "pNumTicks = " << numTicks << ",";
	ss << "pNumReps = " << numReps;
	ss << "}";
	
	return ss.str();
}

///////////////////////////////////////////////////////////////////////
// CORE FUNCTIONS
///////////////////////////////////////////////////////////////////////

int gValue(int att, int tick) {
	_gValue(att, tick, population);
}

int aValue(int att, int id, int tick) {
	_aValue(att, id, tick, population[id]);
}


void preConf(int idx)
{
	_preConf(idx);
}

void preRun(size_t confId)
{
	//printf("Inside preRun\n");
	glh = new GState[getNumTicks()];
	if(glh==NULL) {
		printf("Error allocating memory!\n");
	exit(1);
	}
   
	// initialise all group state placeholders with NULL
	for(size_t i=0; i<getNumTicks(); i++)
		glh[i] = NULL;

	// create population
	for(size_t i=0; i<getNumAgents(); i++) {
		population.push_back(Agent(i, env));
	}

	// initialise RNG
	seed=time(NULL);
	srand(seed);

	// call custom _preRun function
	_preRun(confId, population);
}

void preTick(
	size_t confId, 
	size_t tick,
	vector<Agent> const& population
)
{
	_preTick(confId, tick, population);
}

GState step(size_t confId, size_t tick)
{
	//cout << "Inside step " << tick << endl << flush;
	GState gstate = new AState[getNumAgents()];
	if(gstate==NULL) {
		printf("Error allocating memory!\n");
		exit(1);
	}

	preTick(confId, tick, population);
	
	// update environment
	env->step(confId, tick, population);
	
	// update all agents
	size_t i=0;
	vector<Agent>::iterator it;
	for(it=population.begin(); it!=population.end(); ++it) {
		it->step(confId, tick, population, env);
		AState as = it->getCState();
		gstate[i++] = as;
	}
	postTick(confId, tick, population);

	glh[tick] = gstate;
	//printf("Tick %d done.\n", tick);
	return gstate;
}

void postTick(
	size_t confId, 
	size_t tick,
	vector<Agent> const& population
)
{
	_postTick(confId, tick, population);
}


void postRun(size_t confId)
{
   //printf("Inside postRun\n");

	_postRun(confId, population);

	// iterate over ticks
	for(int i=0; i<getNumTicks(); i++)
	{
		if(glh[i] == NULL)
		continue;

		// iterate over agents
		for(size_t j=0; j<getNumAgents(); j++)
		{
			// iterate over attributes
			for(size_t k=0; k<getNumAgentAtts(); k++)
			{
				//printf("Freeing glh[%d][%d][%d]\n", i, j, k);
				delete[] glh[i][j][k];
			}
			//printf("Freeing glh[%d][%d]\n", i, j);
			delete[] glh[i][j];
		}
		//printf("Freeing glh[%d]\n", i);
		delete[] glh[i];
	}
	//printf("Freeing glh\n");
	delete[] glh;

	//cleanupPopulation();
	population.clear();	
}

void postConf(int idx)
{
	_postConf(idx);
}

GT run()
{
   for(size_t t=0;t<getNumTicks();t++)
   {
      GState gstate = step(0, t); // todo: replace 0 with actual configuration id
      glh[t] = gstate;
   }
   return glh;
}

void writeGTToFile(size_t run)
{
	if(glhWritten)
		return;

	glhWritten=true;
	stringstream ss;
	ss << "counterexample_" << run << "_" << seed << ".csv";

	ofstream ofile(ss.str().c_str());
	if(ofile.is_open())
	{
		// iterate over ticks
		for(int i=0; i<getNumTicks(); i++)
		{
			if(glh[i] == NULL)
				continue;

			// iterate over agents
			for(size_t j=0; j<getNumAgents(); j++)
			{
				// iterate over attributes
				for(size_t k=0; k<getNumAgentAtts(); k++)
				{
					ofile << glh[i][j][k][0] << "=" << glh[i][j][k][1] << ";";
				}
				ofile << ",";
			}
			ofile << "\n";
		}
		ofile.close();
	}		
}

///////////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
///////////////////////////////////////////////////////////////////////

struct Comp
{
 	explicit Comp(int _att, float _val)
	: att(_att),
		val(_val) 
		{}

  	inline bool operator()(Agent& m) const { return m.m_State[att] == val; }
private:
  	int att;
	float val;
};

vector<Agent>::iterator getAllWith(int att, float val) 
{
	return find_if(population.begin(), population.end(), Comp(att,val) );
}

struct RobotsHere
{
  	explicit RobotsHere(P2D _pos, float _d)
	: pos(_pos),
		dist(_d)
		{}

  	inline bool operator()(Agent& m) const 
	{ 
		float distSq = (pos-m.getPos()).lenSq();
		return (distSq <= (dist*dist));
	}
private:
	P2D pos;
	float dist;
};

vector<Agent>::iterator robotsHere(Agent const& a) 
{
	return find_if(population.begin(), population.end(), RobotsHere(a.getPos(), 1.0) );
}

