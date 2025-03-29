
Actor based Process-Interaction Models
--------------------------------------

Top Consepts:

Component.scala	     -- Base Trait for Nodes and Edges
Model.scala          -- Engine to Drive the Simulation Model 
Model_MBM.scala      -- extended for the Method-of-Batched Means (MBM)
Recorder.scala       -- Records the Flow of Actors/Vehicles (Counts and Speed)

Nodes:

Gate.scala           -- Controls the flow of actors (e.g., a traffic light)
Junction.scala       -- Simple connector between pathways
Resource.scala	     -- Component that provides services to actors 
Sink.scala	     -- Terminal for completing actors
Source.scala         -- Generator for making new actors of type SimActor
VSource.scala        -- Generator for making new actors of type Vehicle
WaitQueue.scala      -- FCFS wait queue
WaitQueue_LCFS.scala -- LCFS wait queue (stack)

Edges/Pathways:

Transport.scala      -- Connects From-Node to To-Node - motion governed by Random Variate
VTransport.scala     -- same, except motion governed by equations of motion, e.g., Gipps Dynamics
Path.scala           -- Multi-lane pathway made up of Transports or VTransports
Route.scala	     -- Multi-stage, multi-lane pathway made of Paths and Junctions 

Actors/Tokens:

SimActor.scala       -- General-purpose actor (active entity)
Vehicle.scala        -- Specialized actor with enhanced motion support 
Bus.scala            -- Aggregate actor that moves a collection of actors

Equations of Motion:

Dynamics.scala	     -- Physics of motion, e.g., Car-Following models

Template for Application Models:

Ex_Template.scala

