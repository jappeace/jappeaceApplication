TITLE: Using git for templates
DATE:2020-10-23  
Category: tools
Tags: work, git
OPTIONS: toc:nil
status: draft

Over the past few years I've started using git as a template
management tool.
For example, I clone my [haskell template project](https://github.com/jappeace/haskell-template-project).
Edit the names, edit the `readme` end re-setup the remotes:

```
git remote add template git@github.com:jappeace/haskell-template-project.git
git remote set-url origin git@github.com:YOUR-ORG-OR-USER-NAME/new-project.git
```

This allows me to keep my downstream project up to date with the template.
I can simply merge in changes from the template.
This can happen if I discover another tool, useful for that particular language.
So it allows evolving the template over time as I learn more on
whatever programming language I'm using.

Finally it gives me a bunch of default branding.
Which makes it look more 'professional', 
and can potentially help myself in becoming less obscure.
I find doing this distasteful, but I know it's necessary. 
Having it there by default makes it so that I don't forget.

I use git as a bag of complexity.
Capturing my learning which I'll surely forget in the future.
