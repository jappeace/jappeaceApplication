Title: Adding upper bounds to cabal check
Date: 2022-09-26 19:00 
Category: technique
OPTIONS: toc:nil
Status: draft
Tags: programming, agda, denotational design, zurich hack

Four months ago a colleague of mine got a stern letter
from a hackage maintainer,
telling her to add upper bounds to her newly uploaded package.
Since I've been trying to get colleagues to upload 
our packages to hackage for months,
this experience was rather unsettling.
So I did what any wise person would do,
and complained about this on internet fora.

My friend hecate jumped in,
explaining upperbounds are best practice,
and indeed the maintainer was right.

I countered by saying hackage complains about the most trivial
issues,
like the synopsis field being shorter then the description field.
Couldn't we let hackage automatically tell the user they
should add upper bounds?
because this will indeed help cabal building better.

Here I was pointed to cabal check, and to fix it myself
if I were so passionate about it.
So I opened my angry issue.



I added upper bounds to cabal check.
I'd say most developers would consider a rather trivial
change, considering most of the logic was already in place.
