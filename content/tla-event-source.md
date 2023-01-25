Title: Authentication in Reflex & Servant
Date: 2023-01-25 15:00
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, reflex, frp, servant-auth, servant
Status: draft
subreddit: haskell programming reflexfrp

My concurrency issue is with postgres transactions happening at the same time.
There are giagantic transactions that need to coincide with smaller ones.
Then we need to record the events happening in these transactions,
and make sure that they appear in the same ordering as the transactions were played.


Message queue: https://www.hillelwayne.com/post/tla-messages/
Formalizing fairness: https://www.hillelwayne.com/post/fairness/
