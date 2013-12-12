A. Intro

In *A Model of Performance, Interaction, and Improvisation*, Paul Hudak outlines a formal model of musical performance, interaction and improvisation based on the idea that a full musical performance can be understood as a set of complex, interrelated interactions.  

To motivate this model, he observed that in any performance there exists an interaction between a player and himself: throughout the performance, a good musician will continuously make adjustments to his playing based on what he is hearing from his own instrument. Likewise, in an ensemble or orchestral performance, a player is necessarily affected by the performance of each of the other players in the ensemble.  Finally, each player is working off his or her interpretation of the musical score and, in the case where the players are improvising, the produced performance may actually deviate wildly from what is given in the score.

In the paper, these interactions are termed *mutually recursive processes* where
"the recursion captures feedback [and] mutual recursion captures the interaction
between players...".  In concrete terms, we define these relationships of interaction algebraically:

    r = instr(player s r)

where *r* is the realization, the music actually produced by a player, and *s* is the score. 

With this general model for performance and interaction in mind, Hudak asks us to further consider improvisational settings, where he notes, "although the goal of those involved in improvisation in generally one of cooperation, there is also a certain amount of conflict."  Given this natural relationship of conflict and cooperation between players in an improvisational scenario and the mathematical model we can use to frame it, he suggests the application of *game theory* where "engaged processes can be viewed as players in a game, where currency is manifested as aspects of musical aesthetics, and the rules relate to control of such aesthetics."

Grounded in this theoretical framework for how such a music game might work, 
this project set out to explore a new method of algorithmic music composition 
using game theory.   Thus, the goals of the project were:

- implement in Haskell the game theoretical model outlined in the Hudak's paper
- produce a meaningful, new way of algorithmically generating music that 
might resemble human player improvisation.
- allow for the implementation to be extended through user-defined ideas of
  payoffs, strategies and rules.




B2
Game theory is the study of strategic decision making.  It is used to model interactions between agents whose decisions affect each other's welfare.

Before one can formally model a game, a number of things must be defined in the context of that game.  The word "game" refers not to a something a group of kids play at recess, but to any situation with more than two decision-makers.  Those decision-makers are called the players, and there must be a set of rules that discuss the set of legal moves each player can make.  Those each move must be defined in terms of how it effects the state of the game.  In picking what moves to make, each player must adhere their own fixed and well defined strategy that can be algebraically formalized. There must be strict definitions for what information is available in the game to be used by the strategies, both shared amongst the players and not.  Separately from payoff, there must be a measure of success for each player by the end of the game, called that player's payoff.  All of these things together lead to a well defined game that can be modeled and reasoned about.

Much of the use of game theory is centered around the development of sound and optimal strategies.  An optimal strategy is one that will always lead the player to their best possible payoff.  But how can we begin to reason about the process of reaching such a payoff? The answer lies in game trees.  Game trees represent every possible sequence of moves from a starting game state to an ending game state.  The root of the game tree is the starting state, and each branch represents a possible move for a player or chance from an external event to change the game state.  The leaf nodes represent the "game over" states and have a payoff for each player associated with them.  The best strategies are those that themselves have a concept of a game tree, and traverse it in such a way that they know they will reach optimal payoff nodes.  

How does all of this fit into the context of a real game?  Let us consider a formal treatment of the game TicTacToe.  The decision makers in TicTacToe are the characters 'X', and 'O'.  They must alternate moves, and place their mark on a 3 by 3 grid, in a previously unoccupied cell.  When there are three cells in a row occupied by the same player (horizontally, vertically, or diagonally), that player wins and the game is over.  If the board fills up before this can happen, there is a draw.  The players share the knowledge of where they have each moved on the board. but nothing further.  The payoffs are fixed to be a 1 for the winning player, -1 for the losing player, and 0 for each player in the case of a draw.  Now let us see what this might look like in the first few levels of the game tree:

--TicTacToe visual here

As you can see, the first row models player X making a move, and O's moves in the second row branch off from X's.  In extensive games where players each make multiple moves, these game trees can grow very rapidly.  TicTacToe, for example has 25,000+ nodes in it's game tree, even when eliminating nodes where we can through rotational symmetry.

But how do game trees work when more than one player must make a decision at a time?  This is where it is necessary to have strict and defined knowledge boundaries for the players.  Simultaneous games are modeled as a layer of strictness on top of alternating games, like TicTacToe.  Players must still make moves one by one, but the information of that choice does not get shared until all players have made their choices.

--should I go into RPS example here?

--possible transition paragraph here.

---

B3

Traditionally, game theory has been used to mathematically determine the best course of action for any given situation.  However, the best course of action is often the selfish one, and requires much computational effort to calculate on the fly.  A subdivision of game theory, called experimental game theory, examines games in the light of competitive experimentation.  This is particularly relevant to games where sub-optimal strategies might yield a better net result.  In Improvise, we are interested in a yielding music that is good for both players, rather than just one at the cost of the other.  

Hagl is a DSL embedded in Haskell that allows for easy and modular definitions of games.  In Hagl, a game is an instance of the Game type class:

	class GameTree (TreeType g) => Game g where
    		type TreeType g :: * -> * -> *
    		type State g
    		type Move g
    		gameTree :: g -> (TreeType g) (State g) (Move g)

TreeType, State, and Move are associated types that must be defined in terms of the game.  The only function that must be provided is the gameTree function, that takes a game, and builds it's whole game tree.  Hagl provides a number of operations to then interact with the game, which all involve examination of the built game tree, which is why it is a requirement that the TreeType also is an instance of his GameTree class, which contains operations for examining nodes and the moves that branch off from every node.

Although it is possible to write your own instance of GameTree, Hagl provides two generic representations of GameTrees: Continuous, and Discrete.  Intermediate nodes have a state and list of outbound edges associated with them, and terminal nodes are payoff nodes, that contain a list of Floats, which are the payoffs for each player in the order the players were passed into the game.

The game tree edges, as mentioned in the introduction to game theory, must have a concept of payoff associated with them.  In Hagl, this should be represented through 

Players in Hagl are represented as a simple data type: 

	data Player g = forall s. Player Name s (Strategy s g)
              		 | Name ::: Strategy () g

Here the Name is just a String, and they must be associated with a strategy, and possibly maintain personal information within the universally quantified type variable, s.

--struggling to talk intelligently about the strategy type -- how do I explain this:
	data StratM s g a = StratM { unS :: StateT s (ExecM g) a }
	type Strategy s g = StratM s g (Move g)

The execution of a game happens through the evalGame call:
	evalGame :: Game g => g -> [Player g] -> ExecM g a -> IO a

ExecM is the game execution monad, but in this context, it's simply a sequence of operations to evaluate.  This can be running through the entire game with the `finish` call, or stepping a number of times (step >> step >> step, etc.). 

--was weird to try to fit in information about ByPlayer and the other list operations here.  can we just put them in as we need them?


---

C1
Given this general game theoretic framework, how do we then map the fundamental components of a game to aspects of an improvisational performance of two or more musicians? 

The first critical component is the concept of moves or decisions available to each player, and the rules that specify those moves for all possible game states.  For a musical game, moves take the form of *fixed-duration, time-stamped musical events*. Rather than thinking of a player's full performance as a sequence of notes, we imagine it as a sequence of musical events, that is, a stream of his or her decisions of what and how to play at each time-stamp.

Events must necessarily be both time-stamped and of a fixed-duration to ensure that players make their decisions of what to play for each event *at the same time*.  Notes in the musical sense are associated with a given duration, but consider a game in which one player begins playing a whole note and the other a quarter: when should we define the next decision point at which the player's can make their next moves?  When the second player is ready to make another move, the first still has three more beats to play.  Thus, we break notes into musical events of fixed-duration and allow a player to choose to extend the pitch of the previous event if he or she desires to play a note of a longer duration.

In his paper, Hudak notes that the formal model of musical interaction" is limited to controlling an instrument's sound, for the purposes of realizing fundamental parameters such as pitch in addition to more subtle issues of articulation, dynamics and phrasing."  Indeed, in our simple implementation, we focus solely on controlling pitch, ignoring the more subtle parameters and keeping the players’ instruments fixed.

Second, the currency or payoff of the game, used to measure a player's success in the performance, translates to a player's notion of musical aesthetic.  This is a measure of how good the musician considers his own performance sounds, given the actions of all the other players. This preference is unique to each player and may be defined along any axis of the musical design space.  The realm of musical aesthetic is practically infinite, and a full discussion of music theory of this depth is well beyond the scope of this paper.  

Finally, each player in a musical game must implement his or her own strategy, which determines how a he or she will move in any given game state.  In a musical improvisational game, a strategy can be as simple as a player adhering to his or her given score, never improvising at all.  Alternatively, a strategy can be more sophisticated, taking into account past or anticipated payoffs, a player's own past moves or those of another player, or some other musical strategy with the only restriction that any move generated by the strategy fall within those allowed by the rules of the game.

Lastly, for each individual, the goal of the game is to maximize his or her own payoff.  However, we note that unlike zero-sum games, which are purely competitive, a musical game is more cooperative in nature.  Indeed, there exists *some* competition between players, (Hudak imagines two soloists "vying for attention"), however, intuitively, there is a point at which trying to make the other player "sound bad" (i.e. reduce his payoff) has an equally deleterious effect on one's own performance.  Therefore, the best outcome for each player individually is likely to be one in which the sum of their collective payoffs is also maximized.

For example, in the simplest case, we imagine two horn players soloing simultaneously in a jazz quintet.  Using Hudak's algebraic formulation, the relationship between these two players is a pair of mutually recursive functions:

    r1 = instr1(player1 s1 r1 r2)
    r2 = instr2(player2 s2 r1 r2)

In terms of the game, *r* refers to each player's realization, the sequence of time-stamped musical events, *s* refers to each player's score (also a sequence of time-stamped musical events) and *player1* and *player2* stand for strategies employed by the given player.  Legal moves at any given point in the game tree are derived from a player's score and realization and payoffs are calculated from realizations and information about the players' preferences.

---
C2:

Each branch of a game tree represents a move of one player or the chance of something effecting the game state.  We eliminate the possibility of chance in this game, as the only things that effect state in this game are the player's own choices.  So, every branch then represents a choice the performer has made to stick to or deviate from the score.  However, moves in this game are made at the same time, so we must not modify the state associated with the game until both players have made their move.

We can now model how the state changes from node to node, but how are players allowed to pick their moves?  Technically, there are an infinite number of possible deviations from a score.  Since it's not feasible to include an infinite number of states in the game tree, let's first limit it to the 4 octaves on either side of middle C, and further just to notes in the western scale -- a total of 96 notes.  When considering a score with two instrumentalists who get to make two moves each, we're already up to over 85 million nodes in the game tree, as the tree grows exponentially.  Because of this, it is very important to further limit the possible moves as well as construct strategies that take advantage of lazy evaluation and do not explore all nodes of the tree.  Because the effects of changing the legal set of moves in a game are so drastic (on a computational level), this is something dynamic and changeable in an instance of the Improvise game.

---

D1:

In order to implement music as a playable game, we must implement the aforementioned components of games: moves, payoffs, and strategies. The choice made here define the framework for the game itself, and therefore, the range of music that can be produced from it.

The first important piece is the “MusicMv”:

    data MusicMv = Begin  Pitch
                 | Extend Pitch
                 | Rest

This defines a move as either a Begin of a Pitch, Extend of a Pitch, or a Rest. Each represents a musical event of the game’s defined atomic duration. The distinction between begin and extend, however, is somewhat subtle, . A Begin represents the onset or attack of the given pitch, whereas an Extend is a continuation of the previous note without a distinct onset. The difference lies in the following example: Given a sequence of moves [Begin p, Begin p], the sound produced would be a note of length one unit followed by another note of length one. There will be an audible separation of the two notes and a distinct beginning of the second immediately following the termination of the first. In contrast, the move sequence [Begin p, Extend p], would produce a note of twice the duration with no audible onset beyond the first. The notes will sound as one continuous event lasting two units. A Rest represents no audible sound, simply one unit of silence.

The reasoning behind giving Extend a pitch is quite subtle.  It is necessary in our implementation for an Extend to directly follow either a Begin or another Extend. In both cases, the Extension of the preceding note must contain the same pitch. It is unclear then why Extend should be given a pitch at all, as it should be simple enough to look back through the previous moves until the most recent begin, to which the Extend is being applied, is found. We found this to be slightly impractical later on when attempting to write strategies and payoffs. The storage of the Pitch must be weighed against the cost in time of repeatedly looking back. This decision also lifts the burden from the programmer when writing payoffs and strategies, a burden we look to minimize overall.

The next piece of the game implementation is the Performer:

    data Performer = Performer { realization :: [MusicMv]
                                , future      :: [MusicMv] }

A Performer represents an individual player. The realization is a list of musical events that have already occurred, i.e. the player’s interpretation of the score thus far (most recent event first). The future  is a list of the upcoming events, i.e. the remaining portion of her score. This is the aggregate of all the game’s moves.

    type Performance = ByPlayer Performer 

Likewise, a Performance is an aggregate of all the game’s players. A ByPlayer a is a List of a which has length equal to the number of players in the game. ByPlayer is defined in Hagl. This means a Performance contains exactly one Performer value for each player. Each of those Performers contains the set of moves played by that player so far this game as well as the future moves in their score.

Once we have these pieces, the game tree can be built.  Our representation of possible moves from a given state is a finite list.  Because of this, we have a discrete tree rather than a continuous one.  From here, we must design the building of the tree to fit a few requirements.  First, it must keep a state in each node, and continually modify that state as moves get played.  Second, it must appear to players that they are all making moves at once, despite the fact that a node can only represent a move by one player.  Third, it must have leaf nodes populated with final payoff whenever the state makes it clear the game is over.  Finally, it must be efficient and expand as needed rather than on first call of the function.  Discrete trees are represented by the following datatype in Hagl:

data Discrete s mv = Discrete {
  dtreeNode  :: Node s mv,
  dtreeEdges :: [Edge s mv]
} deriving Eq

This is parametrized over the type of the state in the game, s, and the type of moves in the game, mv.  A node in the game tree contains information about the state at that point in the tree, as well as what type of action the node represents.  For a music game, we have no outside influence, do the only actions we must represent are Decisions by a given player or Payoffs at the end state.  Our action datatype is therefore

data Action =
    Decision PlayerID
  | Payoff Payoff
  deriving Eq

The edges in the discrete tree represent the possible moves outward from that node.  They are represented as a move and subtree.  So, each game tree contains many subtrees of the same type.  We can use a recursive function to generate these subtrees and take advantage of lazy evaluation to ensure that the whole game tree does not get expanded at once, and nodes that are never explored in the process of the game will not be expanded at all.  This seems simple, but we still need to accomplish the trickiness of not modifying the state (the entirety of which each player can see at each node), while building out the game tree for a single set of moves (every player making a decision about a single time-stamp).  Because the number of players is fixed, we can simply accumulate all of the moves for a round in a list until everyone has made their decision.  This list is never seen while actually traversing the tree even though it is used in it's generation, so this is safe.  


---
D.abc

So far we've laid out the basic static framework common to every musical improvisation game, however, it is desirable to modularize those aspects of the game that we identify as particular to a unique game execution.  Most obviously, we will want to experiment with different initial scores, that is, a user should be able to play an Improvise game with any song of his or her choosing.

We wrap these dynamic quantities in the fundamental type `Improvise`:

    data Improvise = Imp { state    :: Performance
                         , payoff   :: Performance -> Payoff
                         , playable :: Performance -> PlayerID -> [MusicMv]}

Here a user may define the number of players and their initial scores in  `state`.  In most games, prior to execution, each `Performer` will have a  `future` loaded with an individual score as a list of `MusicMv` and an empty `realization`.  In our implementation, we provide infrastructure for rendering a list of MIDI files of each player's score into an initial state of type `Performance`.

`payoff` is a user-defined function that generates a Hagl Payoff matrix for any given game state.  At various times in the development of Improvise, it was suggested that payoff be generated based on tempo changes, sequences of notes, or even by more sophisticated schemes derived from common jazz improvisational techniques.  Recognizing the fact that there are practically infinitely number of axes upon which to judge musical aesthetic, it is critical that payoff generation have the most general type possible.

In our implementation, we modeled an extremely simple idea of how a player may judge the sound of his or her performance: that of pitch intervals, the relative distance on the scale between two notes.  In Western music theory,some pitch intervals are generally considered to sound consonant, or pleasing -the classic example is that of a major-third, two notes that are four semitones apart on a scale.  Other intervals, like the --, may be termed "dissonant" and are considered to cause tension in a piece.  Most pieces will use a mix of consonant and dissonant sounds to alternately build and resolve tension.

For an Improvise game, this means that each player provides his own interval preferences: an association list of intervals (as a integer number of half-steps, or semitones) and a Float, representing the relative value the player places on playing that interval.  Positive payoffs denote favorable intervals for the player, while negative payoffs signify undesirable ones. 

Intervals not included in the list have a baseline value of zero for the player;an empty interval preference list denotes a player who values all intervals equally.  We also distinguish the "top" player, playing the higher note of the interval from the "bottom" player playing the low note by allowing both positive and negative intervals.  For example a player with the preferences `[(4, 4.0), (-4, 2.0)]` always values major-thirds in a piece, but prefers to be the bottom player.  The idea is that if two players both have positive values for the same interval, they will colluded to play that interval more often.  Alternatively, a mix of positive and negative payoffs for a given interval results in a more competitive relationship between players, who try to force the other to play their interval preferences.

Finally, `playable` is a function for generating the legal moves for a given player for all states of the game.  That is, `playable` is applied at every decision node in the game tree to produce edges.  Since this function has direct bearing on the size of the game tree, care must be taken when defining this function.  

We defined a simple `playable` function derived from the player's score: legal moves are those within a set number of semitones away from the pitch given in the score.  We called this a "range-limited" move generation scheme.  To make the scheme slightly more sophisticated, we also allowed players to "look back" at their most recently played note and play pitches within a range around that pitch as well.  Players always have the option of resting (emitting no sound) but we also stipulate that an Extend move is only legal following a previous Begin or a previous Extend, never after a Rest.

This `playable` function is fairly naive in the realm of music improvisation and many other more realistic schemes have been suggested.  Given the generality of the `playable` function in the implementation of Improvise, it should be possible to generate moves based on some desired key of the piece, or even based on repetitions of notes.  


D2d:
The goal of this game is to get the highest combined payoff for all players.  The optimal strategy would be to look through all the nodes and find the move that would bring you towards the leaf upon which that payoff resides.  However, as previously discussed, such an exploration of the state space would be too computationally intense to be of any use in an interactive tool.  It is also an unrealistic measure for a human to take when improvising, which is what this tool aims to model.  Instead, we came up with a depth limited strategy, which we called maximize.  This strategy depends on the ability to calculate an intermediate payoff, which we're able to accomplish with the interval payoff structure.  Each player calculates the intermediate payoff yielded by all of their possible moves.  They then pick the three that yield the best payoff, explore those nodes by recursively calling the same maximize strategy for the next player making a decision.  At the depth limit, you bubble up the payoff for the person who's decision it is at the root of your exploration, and further prune those three choices down to 1 choice by picking the best of the three as it bubbles up through the recursion.

The reason it is important to explore the tree rather than just picking the move that yields the highest intermediate payoff is that another player's decision in the future may affect your final payoff, and making an optimal move now may cause a player to fall into a bad situation in the future.  The payoff is intermediate because it is not necessarily indicative of the final payoff.

---

E1:

In this section we will walk through examples which illustrate some of the capabilities of the Improvise game. The dynamic components used in the examples are fairly straightforward and should serve to demonstrate what can be accomplished with a basic strategy, payoff, and move domain.

In this first example we use “Mary Had A Little Lamb” as our starting score for both players.

——————————
playImprovise (intervalPayoff [prefs1, prefs2])
          	ByPlayer [mary, mary]
          	(limitByRange 2)
          	[justTheScore, (maximize pay)]
——————————

The arguments are: our interval-based payoff function along with a set of preferred intervals, a ByPlayer of two scores each of which contain “Mary Had A Little Lamb”, the limitByRange possible moves function which limits players to moves at most n half-steps from the score, and finally the strategies to be employed by each player. In this example, the first player is simply going to repeat the score without alterations using the justTheScore strategy. This is a strategy that does not attempt to make any decisions based on musical contexts or payoffs. It simply reiterates the score. The second player is using the maximize strategy discussed earlier using the intervalPayoff function to calculate payoffs with the preferences given. The preferences are as follows:

——————————
prefs1 = [(-3, 2), (-5, 2), (5, 2), (3, 2)]
prefs2 = [(5, 1), (3, 1)]
——————————
The first number in each pair represents an interval in half steps, while the second is the payoff associated with that interval. In this example, player 2 gives a weight of 1 to both a 5 half-step or a 3 half-step interval. Note that intervals are directional, with a span upwards being positive and downwards being negative. You will note that player 1 has no preference as to whether it is on the top or bottom of its preferred intervals, as the numeric opposites each carry the same weight. The default payoff for an interval is 0, but it can be set to either a positive or negative value representing a valuing or devaluing of that interval.

Below is the musical output of this example on the left as a list of moves, annotated with the intervals and payoffs for each player on the right.

["Mr. Score ", "Missus Maxine “]	Interval1 Payoff1 Interval2 Payoff2
[Begin  (A,4),  Begin  (A,4)  ] <—— 	0             	0    	 
[Extend (A,4),  Extend (A,4)  ] <—— 	0             	0    	 
[Begin  (G,4),  Begin  (G,4)  ] <—— 	0             	0    	 
[Extend (G,4),  Extend (G,4)  ] <—— 	0             	0    	 
[Begin  (F,4),  Begin  (F,4)  ] <—— 	0             	0    	 
[Extend (F,4),  Extend (F,4)  ] <—— 	0             	0    	 
[Begin  (G,4),  Begin  (E,4)  ] <——	          -3    	2    	3    	1
[Extend (G,4),  Extend (E,4)  ] <——	-3    	2    	3    	1
[Begin  (A,4),  Begin  (Fs,4) ] <——	-3    	2    	3    	1
[Extend (A,4),  Extend (Fs,4) ] <——	-3    	2    	3    	1
[Begin  (A,4),  Begin  (Fs,4) ] <——	-3    	2    	3    	1
[Extend (A,4),  Extend (Fs,4) ] <——	-3    	2    	3    	1
[Begin  (A,4),  Begin  (Fs,4) ] <——	-3    	2    	3    	1
[Extend (A,4),  Extend (Fs,4) ] <——	-3    	2    	3    	1
[Extend (A,4),  Extend (Fs,4) ] <——	-3    	2    	3    	1
[Extend (A,4),  Extend (Fs,4) ] <——	-3    	2    	3    	1

Payoffs:
[20, 10]

Going through this output, we see that at first both players choose to play in unison, both following the score they were given. This is due to the limited range by which they are allowed to deviate from the score. To this point, they have not had an opportunity to produce any intervals of any worth in terms of payoff. However, when we reach the 7th move, we see player 2 play an E under the score’s (and player 1’s) G. The interval from player 1’s perspective is -3 as the move from a G to an E is down 3 half-steps. Conversely, the interval from player 2’s perspective is an E to a G which is an upward distance of 3 half-steps. Player 1 values the interval -3 at a payoff of 2, while player 2 values their interval, 3, at 1.

We can see that for all of the following moves, the intervals produced are exactly the same. This is quite logical given the input to the game. Looking back at prefs1 and prefs2, we can see that player 1 and player 2 have both achieved the maximum payoff possible, and because the score does not move faster than the range of deviation allotted to player 2, they are able to move in parallel, thus preserving the distance between them. In this way the same interval will be created at each step. However, both players give equal weight to a 5 (or -5) interval as +/-3, so why would player2 not choose to produce any? This turns out to be due to the tie-breaking in the maximize strategy. We chose to have ties in payoff broken by playing the note closest to the original score. This prevents players from deviating further and further on each move, creating notes in entirely different registers that do not remotely resemble the score. Consequently, because player 1 plays only the score, intervals of 3 half-steps are by definition closer to the score than those of 5 half-steps.

Though the choices made in terms of payoffs for this example are rather simple and not necessarily the most meaningful for producing complex or interesting music, they serve to illustrate the functionality of the preference-based payoff system and the effectiveness of the maximize strategy.

The payoffs used could be expanded to include more intervals, or even the whole range of -12 to +12 and different intervals could be given different weights to give them preference over others with lesser positive weights. For example, if we doubled the weight of 5 and -5 in the above payoffs, it is likely we would have seen more of them in the output. Though ties are broken by playing closest to the score, these payoffs would not create a tie, there would be a clearly superior move available according to the payoffs.

While Mary Had a Little Lamb produces interesting and pleasing results, it is not often that two players start out playing and improvising from the same score.  What happens in a more realistic scenario, where they're already playing a pleasing harmony? We tested this with the first ? measures of Don't Stop Believing.

——————————
playImprovise pay 
		ByPlayer [DontStopMiddle, DontStopBass]
		 (limitByRange 2) 
		[justTheScore, maximize pay]


Once again we have one player sticking to the score while the other improvises.  This uses the same payoff function and set of preferences as before.  However, because these players are already harmonizing, the score without any improvisation already has a positive payoff for both players of [16,4].  Here, the first player values the combination of payoffs much more than the second.  However, post improvisation, they have a payoff of [8,15], and the realization sounds similar, but with a slightly different bass line.  The second player has kept in mind a goal of making the whole score better -- the cumulative payoff is 23 rather than 20 -- while still working to focus on improving the score in terms of his preferences.


