TITLE: Using git for templates
DATE:2020-10-24 15:00
Category: tools
Tags: work, git
OPTIONS: toc:nil

Over the past few years I've started using git as a template
management tool[^9].
For example, I clone my [haskell template project](https://github.com/jappeace/haskell-template-project),
edit the names, edit the `readme` end re-setup the remotes:

```
git remote add template git@github.com:jappeace/haskell-template-project.git
git remote set-url origin git@github.com:YOUR-ORG-OR-USER-NAME/new-project.git
```

This allows me to keep my downstream project up to date with the template.
When I discover another tool, this can be merged into the downstream project for example.
It allows evolving the template over time as I learn more about
whatever programming language I'm using.

It also gives me default branding.
Which makes it look more 'professional'.
Although I don't like branding, I know this is necessary.
Having it there by default makes it so that I don't forget to do this.

When starting a project you also gain some free authoritativeness,
because you have a longer commit history the project looks
like it is more mature.
Although this practice is a little deceptive, 
there is a core of truth, because the template
has gotten a lot of attention.
If you feel offended by this practice, ask yourself first,
why are you trusting git histories? These can be [rewritten anyway](https://git-scm.com/book/en/v2/Git-Tools-Rewriting-History).
This authoritativeness also gives me a small moral boost.
It just *feels* better to have a project with well thought
out tools being started and some history
rather then the 'normal' stuff everyone starts with.

[^9]: This started out as having no better alternative, due to my nixos choice. But now I realise, this is the best alternative.

## Alternatives

The alternatives are using tools that spit out templates.
In the haskell world these are [stack new](https://github.com/commercialhaskell/stack-templates),
[cabal init](https://cabal.readthedocs.io/en/latest/developing-packages.html#using-cabal-init)
or [summoner](https://github.com/kowainik/summoner).
These tools are simply a glorified `sed` on projects folders:
They find and replace variables.
There is no versioning, there is no personalization except what limited
variables give.

Because these tools don't take into account history, the first commit will be a
large commit with a message like 'Initial'.
I find this problematic because it squashes all the
work that went into creating the template.
Information is lost, for no reason.

<!-- TODO verify this-->
These tools don't use git to track their templates.
They could, but they don't.
Maybe they wanted to be VCS[^1] agnostic?
I'm speculating,
but even if you wanted to be that,
you could still use any version system to track the template
and recreate the history in the VCS desired by the user.
This at least gives the user their history of the template back.
While also giving them the choice of VCS[^8]

[^1]: Version Control System
[^8]: I wouldn't bother with this choice unless someone is asking for it.
     I think git has won the VCS battle by a wide [margin](https://softwareengineering.stackexchange.com/questions/136079/are-there-any-statistics-that-show-the-popularity-of-git-versus-svn).
     Of course that's easy for me the say as neither maintainer nor user.
     25% SVN market share is a lot more then I expected (the absolute SVN repo count is still going up!).

## A template hierarchy
Let's go into the deep end and push what it means to be a distributed version control system.
A template of a template can be used to create another new template.
For example, say you're a fan of [nix](https://nixos.org/) or [make](https://www.gnu.org/software/make/manual/make.html),
so why not put a makefile and a [nix pin](https://nix.dev/tutorials/towards-reproducibility-pinning-nixpkgs.html#pinning-nixpkgs)
in some template of templates project.
These tools don't care about which language you use,
so their configuration can be shared among Python, Java and Haskell projects for example.
Maybe for github it could even have a partial travis or github actions CI setup.

Now, say you're starting a new Python project, but have
no template yet for that programming language.
You simply clone this template of templates and customize it to
your own [Pythonic](https://stackoverflow.com/questions/25011078/what-does-pythonic-mean#:~:text=Pythonic%20means%20code%20that%20doesn,is%20intended%20to%20be%20used.) needs.
You can still pull in the upstream changes from your template of templates,
for example to upgrade the nix pin.

The big advantage of course is that a change in the template of templates
can easily merged into all downstream projects with git.

# Conclusion
I convinced the reader that using git is the superior way for template management.
It's simpler, and more flexible.
I also convinced the authors of stack, cabal and summoner to drop their current way of managing 
templates and use git instead.

Or, more likely no-one read this far.
