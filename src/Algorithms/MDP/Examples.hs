{- | This module shows how to solve several example problems using this
library.
-}
module Algorithms.MDP.Examples (
  -- * A discounted problem
  {- | We consider the problem defined in
"Algorithms.MDP.Examples.Ex_3_1"; this example comes from Bersekas
p. 22.

We will solve this problem using regular value iteration. Having
constructed the MDP, we can do this using the 'valueIteration'
function.

@
import Algorithms.MDP.Examples.Ex_3_1
import Algorithms.MDP.ValueIteration

iterations :: [CF State Control Double]
iterations = valueIteration mdp
@

The iterates returned contain estimates of the cost of being at each
state. To see the costs of the state A over the first 10 iterations,
we could do

@
estimates :: [Double]
estimates = map (cost A) (take 10 iterations)
@
-}
  -- * A discounted problem with error bounds
  {- | We consider the same example as above, but this time we use
relative value iteration to compute error bounds on the costs. This
will allow us to use fewer iterations to obtain an accurate cost
estimate.

Since we have already defined the problem, we do this via the
'relativeValueIteration' function.

@
import Algorithms.MDP.Examples.Ex_3_1
import Algorithms.MDP.ValueIteration

iterations :: [CFBounds State Control Double]
iterations = relativeValueIteration mdp
@

The iterates returned contain estimates of the cost of being at each
state, along with associated error bounds. To see the costs of the
state A over the first 10 iterations adjusted for the error bounds, we
could do

@
estimate state (CFBounds cf lb ub) = (z + lb, z + ub)
  where
    z = cost state cf

estimates :: [(Double, Double)]
estimates = map (estimate A) (take 10 iterations)
@

Note that the lower- and upper-bounds returned in the first iteration
are always +/-Infinity, and so it can be useful to consider only the
tail of the iterations.
-}
  -- * An average cost problem
  {- | We consider the problem defined in
"Algorithms.MDP.Examples.Ex_3_2"; this example comes from Bersekas
p. 210.

Here we are interested in computing the long-run average cost of an
undiscounted MDP. For this we use the
'undiscountedRelativeValueIteration' function.

@
import Algorithms.MDP.Examples.Ex_3_2
import Algorithms.MDP.ValueIteration

iterations :: [CFBounds State Control Double]
iterations = undiscountedRelativeValueIteration mdp
@

We can compute cost estimates in the same fashion as above.

@
estimate state (CFBounds cf lb ub) = (lb, ub)

estimates :: [(Double, Double)]
estimates = map (estimate A) (take 10 iterations)
@

It is important to note that in this problem the cost function
returned in each 'CFBounds' object is not to be interpreted as a
vector of costs, but rather as a differential cost vector; however,
the estimates above retrain the same interpretation.

-}
  -- * A continuous-time undiscounted problem
  {- | We now consider a family of problems described by Sennot p. 248.

Here we are interested in first converting a CTMDP to an MDP via
uniformization, and then computing the long-run average cost of the
optimal policy.

To begin, we construct one of the scenarios provided (each scenario is
just an instance of the problem with certain parameters). We then
convert the scenario to an MDP using the 'uniformize' function.

@
import Algorithms.MDP.Examples.MM1
import Algorithms.MDP.CTMDP
import Algorithms.MDP.ValueIteration

scenario :: CTMDP State Action Double
scenario = mkInstance scenario1

mdp :: MDP State Action Double
mdp = uniformize scenario
@

As above, we can use the 'undiscountedRelativeValueIteration'
function to compute cost estimates.

@
iterations :: [CFBounds State Action Double]
iterations = undiscountedRelativeValueIteration mdp

estimate state (CFBounds _ lb ub) = (lb, ub)

estimates :: [(Double, Double)]
estimates = map (estimate A) (take 10 iterations)
@
-}
  ) where

import Algorithms.MDP.ValueIteration
import Algorithms.MDP.CTMDP
