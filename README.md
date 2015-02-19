Introduction
=========

MC<sup>2</sup>MABS, the Monte Carlo Model Checker for MultiAgent-Based Simulations, is tailored to the analysis of large-scale agent-based simulations. It is designed as a tool framework which incorporates the idea of statistical runtime verification, a combination of runtime verification and statistical model checking. The central characteristic of the tool is the interleaving of simulation and property evaluation. 

The framework comprises as its central components (i) an estimator, (ii) a modelling framework, (iii) a property parser, (iv) a simulator, and (v) a runtime monitor. All components are described in more detail the documentation (links are given below). The typical sequence of actions in a verification experiment using MC<sup>2</sup>MABS is as follows:

1. The user provides (i) the logic of the ABS by utilising the modelling framework, (ii) an associated correctness property, and (iii) the desired precision of the verification results as inputs to the verification framework.
2. The correctness property is translated into a runtime monitor by the property parser.
3. The estimator determines the number of simulation traces necessary to achieve the desired level of precision.
4. The simulator uses the model together with additional configuration information to produce a set of simulation traces.
5. Each simulation trace is observed by a runtime monitor which assesses the correctness of the trace using a given correctness property; due to the online nature of the monitor, a verdict is produced as soon as possible.
6. The individual results are aggregated into an overall verification result and presented to the user.

In terms of implementation, MC<sup>2</sup>MABS consists of two central parts: 

+ a simulator which represents a framework that hosts custom model logic provided in a high-level programming language (C++) and performs the execution of the simulation, and 
+ a verifier which implements the runtime monitoring algorithms and evaluates a given property upon the traces produced by the simulator. 

Communication between the simulator and the veriifier happens via a well-defined functional interface which is realised as a set of mandatory and additional optional callback functions to be implemented by the modeller. Apart from the basic logic required by the monitor, the modeller is free to incorporate arbitrary logic into the simulation. In order to offer a good balance between performance and usability, the simulator is implemented in C++. 

The monitor is written in Haskell. In order to avoid unnecessary computation, it is based on the idea of lazy evaluation, according to which a group state is requested from the simulator if and only if it is strictly required for verification. When requesting a group state, the underlying simulation is triggered to advance for a single step. 

For more information, please refer to the MC<sup>2</sup>MABS website: 

http://sites.google.com/site/herdbenjamin/mc2mabs   
