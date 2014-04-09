title: gitp
date: 2014-04-08

Today I deployed my first [Sublime Text][ST] plugin—and the first thing I've written for actual real-world use. 

## Git

Let's start with [git][]. This very popular version control system, beloved by many, backbone of [Github][], and de facto standard at [Hacker School][HS], can do a lot of marvelous things—and one of my favorites is [*patch mode*](http://nuclearsquid.com/writings/git-add/). Patch mode lets you stage individual portions of your working text for commit, rather than an entire file. I'll explain what I mean:

[git]: http://git-scm.com/
[Github]: http://github.com
[HS]: http://hackerschool.com

Ordinarily, your git workflow might look like this:

```bash
# *edit a text file for a while*
git add .
git commit -m"Here are the three changes I've made to this file since my last commit"
```

In this way you keep a record of all the revisions you've made to your file. If you want to understand how your file changed over time, you can read the commit messages in chronological order. If you ever break something and need to get back, you can revert to a previous version.

### Patch Mode

This is good; however, git can do even better without a lot of difficulty. Using git's *interactive staging* you can commit only *some* of the changes you've made to your documents at any given point. So let's say in the last hour you made three major changes (added 1 new feature, fixed 1 major bug, added some documentation) to your work, consisting of three files, changed in three places each. Now you've got things the way you like them and you want to commit your work and save a revision. With `git add -i` or `git add -p` (or `--patch`), you can separate each of those three changes into their own commit. Now your workflow looks like this:

```bash
# *edit a text file for a while*
git add -p
# *stage the changes having to do with your new feature*
git commit -m"Added this feature"
git add -p
# *stage the changes having to do with your bugfix*
git commit -m"Fixed this bug"
```

And so on. Now when you look back on your changelog for this project, each revision is laid out along *functional* or *logical* lines, as opposed to chronological.

### Patch Mode For All

As a some-time, unproductive writer of prose and poetry as well as code, I have always been interested in the potential for version control systems as applied to natural language text—not just code. Poetry has always struck me as particularly well-suited for version control, since it's organized by line, and not by paragraph. I'm not the only one interested in this sort of thing: [Penflip](http://penflip.com) is one project that can be summed up as "Github for text". 

So imagine: you write a sonnet. In a fit of inspiration, you dash out a few lines:

> They that have power to hurt and will do none, 
That do not do the thing they most do show, 
Who, moving others, are themselves as stone, 
Unmoved, cold, and to temptation slow, 
They rightly do inherit heaven’s graces 
And husband nature’s riches from expense; 
They are the lords and owners of their faces, 
Others but stewards of their excellence. 
The summer’s flower is to the summer sweett, 
Though to itself it only live and die, 
But if that flower with bass infection meet, 
The basest weed outbraves his dignity: 
   For sweatest things turn sourest by their deeds; 
   Lilies that fester smell far worse than weeeds. 

Not bad for a first draft. As with any other git project, you save it in the directory with your git repo (let's call it S94.md), add it to the repo, and commit.  

The next morning you look over your handiwork. In your haste it appears you made a couple spelling errors. You ought to clean that up. And what's with that first line? You've got to *grab* the reader! This is better:

> Check it! If you have power to hurt and don't, 
That do not do the thing they most do show, 
Who, moving others, are themselves as stone, 
Unmoved, cold, and to temptation slow, 
They rightly do inherit heaven’s graces 
And husband nature’s riches from expense; 
They are the lords and owners of their faces, 
Others but stewards of their excellence. 
The summer’s flower is to the summer sweet, 
Though to itself it only live and die, 
But if that flower with base infection meet, 
The basest weed outbraves his dignity: 
   For sweetest things turn sourest by their deeds; 
   Lilies that fester smell far worse than weeds.  

Pleased with your second draft, you stage your file and commit with the message "Cleaned spelling, punched up intro".

This is not great. "Check it"? What is this, *Yo! MTV Raps?* You think better of that first line; better revert. Unfortunately the only commit you have to revert to is riddled with spelling errors. 

This is why `add -p` is so powerful. You can separate your commits out into different actions. Here's what you could have done with git patch mode to create commits that reflect the discrete actions you took, instead of just tracking the document over time.

```diff
➜  sonnets git:(master) ✗ git add -p
diff --git a/S94.md b/S94.md
index e3fa036..2a7e029 100644
--- a/S94.md
+++ b/S94.md
@@ -1,4 +1,4 @@
-They that have power to hurt and will do none,
+Check it! If you have power to hurt and don't,
 That do not do the thing they most do show,
 Who, moving others, are themselves as stone,
 Unmoved, cold, and to temptation slow,
Stage this hunk [y,n,q,a,d,/,j,J,g,e,?]? y

@@ -6,9 +6,9 @@ They rightly do inherit heaven’s graces
 And husband nature’s riches from expense;
 They are the lords and owners of their faces,
 Others but stewards of their excellence.
-The summer’s flower is to the summer sweett,
+The summer’s flower is to the summer sweet,
 Though to itself it only live and die,
-But if that flower with bass infection meet,
+But if that flower with base infection meet,
 The basest weed outbraves his dignity:
-   For sweatest things turn sourest by their deeds;
-   Lilies that fester smell far worse than weeeds.
\ No newline at end of file
+   For sweetest things turn sourest by their deeds;
+   Lilies that fester smell far worse than weeds.
\ No newline at end of file
Stage this hunk [y,n,q,a,d,/,s,e,?]? n

➜  sonnets git:(master) ✗ git commit -m"Punched up intro"

➜  sonnets git:(master) ✗ git add -p
diff --git a/S94.md b/S94.md
index 92f054a..2a7e029 100644
--- a/S94.md
+++ b/S94.md
@@ -6,9 +6,9 @@ They rightly do inherit heaven’s graces
 And husband nature’s riches from expense;
 They are the lords and owners of their faces,
 Others but stewards of their excellence.
-The summer’s flower is to the summer sweett,
+The summer’s flower is to the summer sweet,
 Though to itself it only live and die,
-But if that flower with bass infection meet,
+But if that flower with base infection meet,
 The basest weed outbraves his dignity:
-   For sweatest things turn sourest by their deeds;
-   Lilies that fester smell far worse than weeeds.
\ No newline at end of file
+   For sweetest things turn sourest by their deeds;
+   Lilies that fester smell far worse than weeds.
\ No newline at end of file
Stage this hunk [y,n,q,a,d,/,s,e,?]? y

➜  sonnets git:(master) ✗ git commit -m"Fixed awful spelling"
```

What we see here is a series of two patch mode sessions. In the first session I staged only one of my two changes: the intro punch-up. I then committed just that change. Then I ran patch mode again and staged and committed the second 'hunk'—the various spelling fixes.

Now my commit log looks like this:

```bash
9519acd - (HEAD, master) Fixed awful spelling (6 minutes ago)
c440dd9 - Punched up intro (6 minutes ago)
9f595c8 - Initial commit (6 minutes ago)
```

And once I decide that it would be better if I didn't meddle so thoroughly with the words of the bard, I can rebase that middle commit out, leaving the work that I wanted to keep. Literature is saved.

```bash
2c5f971 - (HEAD, master) Fixed awful spelling (5 seconds ago)
9f595c8 - Initial commit (8 minutes ago)
```

[ST]:http://www.sublimetext.com/3
