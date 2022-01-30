TITLE: Mutation driven testing haskell
DATE: 2020-12-24
CATEGORY: reflection
Tags: australia, work
OPTIONS: toc:nil
Status: draft


I was listening to the episode curry-howard the duck of the magic read along.
And they've brought up the subject of mutation driven testing.
As with 90% those guys are talking about,
I've never heard of mutation driven testing. 
What is it then?

It flips testing around, rather then testing your program by modifying tests.
You test your tests by modifying your program.

But hear comes the *really* interesting part, our friends hadn't realized.
What if you use a simple effects system to do this?

Like our beloved MTL.


https://www.uh.edu/serg/publications/fp-mutation.pdf
https://agroce.github.io/isstat14.pdf
