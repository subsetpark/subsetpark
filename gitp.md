title: Git Patch Mode and Working With Git in Python
date: 2014-04-08
<!-- status: draft -->

Today I deployed [gitp](https://sublime.wbond.net/packages/gitp), my first [Sublime Text][ST] plugin. 

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

And once I decide that it would be better if I didn't meddle so thoroughly with the words of the bard, I can rebase that middle commit out, leaving the work that I wanted to keep.

```bash
2c5f971 - (HEAD, master) Fixed awful spelling (5 seconds ago)
9f595c8 - Initial commit (8 minutes ago)
```

The order I did it in is not as important as the fact that they're discrete changes within the text. Literature is saved.

## gitp

I'm a big fan of git and git patch mode. So I wanted to see if I could create a frontend to patch mode (synonymous with `add -p`) that would make it slightly more accessible. I use the [SublimeGit](https://sublimegit.net/) plugin for Sublime Text, and I love being able to access git from the text editor, but I wanted to be able to stage individual hunks as well.

So I made my own plugin, called [gitp](https://sublime.wbond.net/packages/gitp). It's available for install right now through the nearly-ubiquitous [Package Control][PC]. 

### What it Does

With `gitp` you can stage individual changes for commit, from your editor.

#### Displaying Hunks

First, all changed areas of your file are indicated with numbered icons in the gutter.

![Active Hunks Screenshot]({filename}/images/hunks.png)

If you'd like to stage a hunk, you can run either of two different commands:

##### Choose Hunks

Sublime Text will ask you to enter the hunks you'd like to stage.

![Choose Hunks]({filename}/images/choose.png)

##### Stage These Hunks

Any hunks that are currently under the cursor will be staged. This supports multiple cursors!

Once a hunk is staged it gets a dot next to it.

![Staged Hunk]({filename}/images/staged.png)

#### Committing Hunks

You can then run the Commit Staged Changes command. Sublime Text will ask you for your commit message and commit your changes.

#### Hunk Management

There are a few other commands that are useful in this line of work:

##### View Hunks

You can view the individual diff results for any change(s) in your project.

![View Hunks]({filename}/images/view.png)

##### Unstage Hunks

You can unstage all your changes and restage as you like.

[ST]: http://www.sublimetext.com/3
[PC]: https://sublime.wbond.net/

### How I Did It

For those interested in following along at home, the source to `gitp` is available on [my Github page][mygh]. 

Sublime Text 3 provides a pretty sophisticated plugin [API][], written in Python 3. In this framework, each command is a class that inherits from a certain command type. So in gitp each of the major commands is a class that inherits from `sublime_plugin.TextCommand`: `ViewHunksCommand`, `CommitStagedChangesCommand`, and so on. In addition there's one other major class that's not exposed as a user command: `DisplayHunksCommand`, which is responsible for updating the gutter icons, and is run as the final step of certain functions, as well as run by the `HunkListener` class, which defines methods in order to listen to certain events: `on_load`, `on_post_save`, `on_activated`.

#### Working With Git

The bulk of `gitp` is, of course, the interaction with the git repository of the currently working file. So my first question was about how best to interact with git objects.

##### The State of Git in Python

There are several available hooks into git for Python developers, which I will briefly touch on.

[Dulwich](http://www.samba.org/~jelmer/dulwich/) and [GitPython](https://gitorious.org/git-python) are two pure-Python libraries for interacting with git. The latter, in particular, has very rich documenation and fully-formed tutorial. The former isn't too shabby either. Unfortunately, both of them are Python 2-only[^1]. And not only is 3 my preferred version of the language, but Sublime Text 3 is necessarily Python 3. So those were both off the table for this project. Which left [pygit2](http://www.pygit2.org/), the official Python bindings to the libgit2 shared library. Pygit2 is actively developed and evidently quite powerful—and unfortunately the documentation is severely lacking. I struggled badly with simply understanding the interface that pygit2 provides to git repositories. Diffs, for instance, where I'd be spending most of my time (as add -p is all about hunks, and hunks are a subset of diffs), have a light [scaffolding](http://www.pygit2.org/diff.html) of documentation which grows progressively thinner as you scroll down, its promising spring green tutorial boxes giving way to terse single-clause descriptors of attributes to completely undescribed classes. Leading the reader more or less at a loss as to what the actual relationship could be betwwen a Diff, a Patch, and a Hunk[^2]. There are upsides; Stack Overflow has been very responsive in trying to puzzle things out with pygit2. But unfortunately there isn't enough there to recommend (for my purposes) pygit2 over simply shelling out.

Because the git command line client, after all, is astoundingly powerful and deeply composable. By design, nearly any discrete 'action' within a git repository can be expressed as a single command with the appropriate flags, and those more complex actions can be expressed as a series of commands without anything being lost in the middle.

So I settled on calling to the command line `git` with Python's `subprocess` module. Ultimately there are only a few commands that I needed to use.

- check if a folder is a git repository: `git rev-parse`
- get a diff: `git diff`, `--cached` to diff with the index, `--unified=1` for prose[^3]
- stage a file: `git add`
- stage changes: `git apply --cached --recount --allow-overlap`. This is the same command that git's own patch mode script runs, which I am grateful to [Greg Price][greg] to pointing out to me. 
- unstage changes: `git reset HEAD`
- commit changes and wait for message: `git commit --file=-`

###### Crunching the Diff

The first major misconception that I had to be disabused of was how git actually stages its hunks in interactive staging mode. The trick is, it doesn't. It doesn't stage *hunks*, "stage this hunk?" notwithstanding. This is the case in pygit2 as well: the act of staging, as seen in the last section, is an `apply` action. Git applies a diff to the most recently committed copy of a file and saves the resulting new file in the index. Which means that if you don't want to stage an *entire* file (which is the point of our project), you are first responsible for coming up with a version of the entire file that you do want to stage.

So this is the meat of the plugin, which takes place in the `stage_hunks` function. I generate a diff that contains all of the changes between the working copy of the file and the most recent commit. I then create a *new* diff out of just the hunks that the user has selected, while carrying over file metadata and newlines and the like. Then I apply that diff. At this point there are three instances of the file in question: the one in the HEAD tree, which has none of the most recent set of changes, the one in the working directory, which has all the changes made since commit, and the one in the index that only has the changes the user has selected. 

#### And Finally...

Really, the rest of the effort expended in this plugin consisted of understanding how to use the Sublime Text API to do what I wanted it to do. I hope that I have not overlooked any major design patterns commonly adhered to by other Sublime Text plugins. 

I have been actively using this plugin myself since it was even barely usable and found it rewarding—rewarding because the plugin has been useful to me, rewarding to be using something that I myself have made, and rewarding that both of those are true of the same project. I hope that it will prove to be useful to others. I have every intention of smoothing off all the rough edges that I can figure out how to smooth, and working to make it a fully-fledged and productive member of the Sublime Text ecosystem. If you would like to use this plugin or provide any feedback, [please feel free!](https://sublime.wbond.net/packages/gitp)

[^1]: At least they were when I started this project, a few weeks ago. I'll be honest—I had thought dulwich was not really being developed any more; but in fact I see that the most recent commit to [dulwich's github page](https://github.com/jelmer/dulwich) is "Merge branch 'python3' of git://github.com/garyvdm/dulwich" which gives some hope for the future.

[^2]: I puzzled it out and I will tell you. This is how diffs, patches, and hunks work in pygit2:

    repo objects have a diff method that produce diff objects. The Diff class basically represents the entire relationship of changes between one tree and another (a tree is essentially a certain snapshot of a filesystem). So you can get a diff between the working tree and the index, the working tree and the most recent commit, or two different commits.

    `[p for p in diff]` produces a list of *Patches*. This is where the documentation falls silent. It's especially confusing because Diff objects also have a `patch` attribute. Here's what these mean: the `patch` attribute of a Diff is a (diff-formatted—this is also why it's confusing) string representing all of the differences between the two trees. A Patch object, on the other hand, represents the relationship of changes between two blobs (that's file-like objects to you and me). Finally, `[h for h in patch]` produces a list of *Hunks*. Hunk objects represent individual changes *within* a file. They are Python objects to represent the sections in a diff file that are denoted by '@@'. 

    Confusingly, while diff objects have patch attributes that return strings of tree-to-tree deltas, patch objects do not have attributes that return strings of blob-to-blob deltas (nor would you want to call these attributes `hunk`), and hunk objects do not have attributes that return strings of the hunks they represent. They return lists of their lines along with metadata indicating what kind of lines they are, but not the hunk strings themselves.

[^3]: My sonnet example above is a bit of a cheat. You'll notice that there is a considerable gap between the hunk that I wanted to revert and the spelling errors. That's because git classifies a hunk as changed text with unchanged context on either side, and by default the context line number is 3. So to be seen as separate hunks changes need to be at least six lines apart. This is fine in code but harder to accomplish in prose; so the default diffing algorithm will likely group changes together that you'd rather ungrouped. In gitp I've accounted for this by checking to see if the active buffer is prose or code, and adjusting the diff output accordingly.

[API]: http://www.sublimetext.com/docs/3/api_reference.html
[mygh]: https://github.com/subsetpark
[greg]: http://web.mit.edu/price/