# Learning

This document contains a record of my learning process in making this project.

## Starting out

- The first thing I did was get a rough syntactic overview of the language by watching Derek Banas' video on Haskell (https://www.youtube.com/watch?v=02_H3LjqMr8&t=3262s).
- After that I read all of *Learn You a Haskell For Great Good* (http://learnyouahaskell.com). An important part of this process was working through the examples in `ghci`--especially during the chapters on monads.
- Throughout reading *LYAH*, I solved the first eight H-99 problems (inspired by Lisp's L-99 problem set). Here are my solutions: https://github.com/HaydenLeBaron/learn-prog-langs/tree/main/haskell/h99/1-10-lists

## Tic-Tac-Toe MVP

- After that research, I jumped right into working on the tic-tac-toe game.
- I first sat down with a pen and paper and jotted down notes about a potential solution.
  - I realized I could represent the board as a matrix and use some basic linear algebra to detect when the game had ended:

```python
# Multiplying a row unit vector and a matrix sums the columns
(1, 1, 1) * (a b c) = (a+d+g, b+e+h, c+f+i)
            (d e f)
	    (g h i)

# Doing the same thing on transpose(M) sums the rows.

'''
We can use these facts to detect DIM in a row! 
Take the following representation:

- Let DIM be the dimensions of a square matrix M.
- Let u be a DIM-dimensional unit vector.
- Let P be a sequence of positive integers (representing player IDs), 
  where  P[1] = 1
         P[i] = P[i-1] + dim.
	 E.g: DIM=3 => P = 1,4,7,...

Then for all DIM, M, len(P):
  if P[i]*DIM is in the vector (u * M), or transpose(u * M): Player P[i] has DIM in a row on the rows or columns.

Something similar can be done using the trace and antitrace of the matrix (see implementation).
'''
```

- So I had a general solution that scaled with the number of players and the number of dimensions. I began coding.
- I ended up using Data.Matrix to represent the board (https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html)
  - I chose to implement it this way because I wanted to try out this package in particular, but if I were to do it again, solving it with linear algebra was more trouble than it was worth.
- Writing functions to solve the problem was easy since I have had prior functional programming experience.
- IO was hard though since I've never had to use Monads for IO. The hardest part of this was converting my initial naive solution to be more robust using the Maybe monad.
  - I spent a lot more time in `ghci` making sure I understood monads. This was my most valuable resource. I also referred back to *LYAH*.

- Along the way I learned how to use cabal to build my app. I think I just googled "example .cabal file" or something and then looked at an example. I also looked at the https://cabal.readthedocs.io/en/3.6/

## Game history server

- After implementing the MVP I decided I would make a game history HTTP server that would record the outcomes of various games.
- I decided on using the Scotty framework since it looked lightweight.

- Helpful scotty resources I found: 
  - https://github.com/scotty-web/scotty/wiki/Scotty-Tutorials-&-Examples
  - https://gilmi.me/lblog/post/2020/12/05/scotty-bulletin-board <- This was the most helpful source and I based a lot of my code on this blog post. This post was also helpful for finding other packages that would be helpful.
  - https://hackage.haskell.org/package/scotty-0.12/docs/Web-Scotty.html#v:body <- The scotty documentation was helpful too

