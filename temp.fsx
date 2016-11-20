// RL-Bot
open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
//Bootstrap paket
let dst = ".paket/paket.dll"
if not (File.Exists dst) then
    let urlRef = @"https://fsprojects.github.io/Paket/stable"
    use wc = new Net.WebClient()
    let url = wc.DownloadString(urlRef)
    let tmp = Path.GetTempFileName()
    wc.DownloadFile(url, tmp)
    Directory.CreateDirectory(".paket") |> ignore
    File.Move(tmp, dst)

//Resolve and install the packages

#r ".paket/paket.dll"

Paket.Dependencies.Install """
source https://nuget.org/api/v2
nuget FsLab
"""
#load "packages/FsLab/FsLab.fsx"
open XPlot.Plotly

(***
Reinforcement Learning is a simple yet powerful
technique for teaching agents to learn by interacting
with the environment. It's similar to how you'd train
a pet by giving treats for good behaviour. In that case
we reinforce the good behaviour by a reward. This is also
called positive reinforcement in behavioural psychology.

Let's bring this idea into an agent based system by
simplifying so that we can code it out. An animal has
large number of actions it can take, although we can
largely group these actions to a small number of behaviours.

In the agent based setting, we restrict such that our
agent can only perform a small finite set of actions.
The environment provides (with some randomness) a reward
for the actions taken. Agent's sole purpose is to
maximise the overall reward. But when we introduce the agent
to the environment, it doesn't know what actions reward
the most. It has to learn by experimentation.

Imagine we come across a row of fruit machines (or slot
machines or similar)
https://www.brenlandleisure.com/images/home-080316-fruit-machines.jpg
These machines payout winnings with some probability. For
simplicity we'd assume that these probabilities don't change and
we can represent them with a single number (as opposed
to a probability distribution). We also assume that each
machine has a maximum reward of £5.

We can represent this as follows:
***)
let RewardProbabilities = [|0.4; 0.1; 0.5; 0.35; 0.3; 0.4|]

(***
    Our strategy would be to play one of these machines at
    a time and overtime learn what machines payout better.
***)

let rnd = System.Random()
let maxPayout = 10.0
(***
ComputeReward function returns a reward for a given machine.
Each machine can payout a maximum of £10. We randomly select
what the payout would be.
***)
let ComputeReward (slot:int) =
    match rnd.NextDouble() with
    | pReward when pReward < RewardProbabilities.[slot]
        -> maxPayout * rnd.NextDouble()
    | _ -> 0.0

(***
When you visit a new town, depending on the time you have,
you would explore the it to get to know what it has to offer.
Then once you have gathered enough knowledge, you'd goto the best
places. This is called exploration/exploitation strategy.
You always have to strike a balance between exploration,
and exploitation. If you spend a lot of time exploring,
time for experiencing the best options will be
reduced. If you spend a lot of time exploiting what little
you know, the chance of experiencing the best options are
reduced as you might not have discovered them.
***)
let exploreRewardProbabilities (nTries:int)=
    [|0..nTries|]
    |>Array.map(fun j->
        let slot = rnd.Next(RewardProbabilities.Length-1)
        (slot, ComputeReward slot)
      )
    |>Array.groupBy fst
    |>Array.map(fun (s,arr)->
         let avgReward = arr|>Array.averageBy(fun (s,r)-> r)
         avgReward
       )

(***
In the fruit machines example, we randomly play these
machines a number of times and compute a table with
average reward probabilities. Now we have something to work from.
Now our strategy could be to play the machine that maximally reward
you. Another clever thing we could do is to keep updating
our reward table and keep selecting the best machine so far.
***)

let rewardsTable = exploreRewardProbabilities 500

let learner nTries =
    [|0..nTries|]
    |>Array.map(fun k->
        let bestSlot,Qk =
                rewardsTable
                |>Array.mapi(fun i r -> i,r)
                |>Array.maxBy snd
        let newReward = ComputeReward bestSlot
        //update the rewardsTable
        rewardsTable.[bestSlot] <- Qk + (newReward - Qk)/(float k)
        (k,newReward)
    )

let result = learner 500

let trace1 =
    Scatter(
        x = (result |>Array.map(fst)),
        y = (result |>Array.map(snd))
    )


[trace1;]
|> Chart.Plot
|> Chart.WithWidth 700
|> Chart.WithHeight 500
