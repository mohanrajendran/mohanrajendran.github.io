mohan@mohan-Inspiron-1545:~$ cd Desktop/mohanrajendran.github.io/
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git status
# On branch master
# Your branch is ahead of 'origin/master' by 1 commit.
#
# Changes to be committed:
#   (use "git reset HEAD <file>..." to unstage)
#
#	deleted:    _posts/#2014-07-27-SICPSection1_1.md#
#	deleted:    _posts/2014-07-29-SICPSection1_2.md
#
# Changes not staged for commit:
#   (use "git add <file>..." to update what will be committed)
#   (use "git checkout -- <file>..." to discard changes in working directory)
#
#	modified:   SICP.md
#	modified:   _posts/2014-07-27-SICPSection1_1.md
#
# Untracked files:
#   (use "git add <file>..." to include in what will be committed)
#
#	#index.html#
#	.editorconfig
#	.gitignore
#	_posts/2014-07-28-SICPSolution1_1.md
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git add SICP.md 
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git add _posts/8
fatal: pathspec '_posts/8' did not match any files
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git add _posts/*
The following paths are ignored by one of your .gitignore files:
_posts/2014-07-28-SICPSolution1_1.md~
Use -f if you really want to add them.
fatal: no files added
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git status
# On branch master
# Your branch is ahead of 'origin/master' by 1 commit.
#
# Changes to be committed:
#   (use "git reset HEAD <file>..." to unstage)
#
#	modified:   SICP.md
#	deleted:    _posts/#2014-07-27-SICPSection1_1.md#
#	deleted:    _posts/2014-07-29-SICPSection1_2.md
#
# Changes not staged for commit:
#   (use "git add <file>..." to update what will be committed)
#   (use "git checkout -- <file>..." to discard changes in working directory)
#
#	modified:   _posts/2014-07-27-SICPSection1_1.md
#
# Untracked files:
#   (use "git add <file>..." to include in what will be committed)
#
#	#index.html#
#	.editorconfig
#	.gitignore
#	_posts/2014-07-28-SICPSolution1_1.md
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git add _posts/2014-07-28-SICPSolution1_1.md
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git status
# On branch master
# Your branch is ahead of 'origin/master' by 1 commit.
#
# Changes to be committed:
#   (use "git reset HEAD <file>..." to unstage)
#
#	modified:   SICP.md
#	renamed:    _posts/#2014-07-27-SICPSection1_1.md# -> _posts/2014-07-28-SICPSolution1_1.md
#	deleted:    _posts/2014-07-29-SICPSection1_2.md
#
# Changes not staged for commit:
#   (use "git add <file>..." to update what will be committed)
#   (use "git checkout -- <file>..." to discard changes in working directory)
#
#	modified:   _posts/2014-07-27-SICPSection1_1.md
#
# Untracked files:
#   (use "git add <file>..." to include in what will be committed)
#
#	#index.html#
#	.editorconfig
#	.gitignore
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git add _posts/2014-07-27-SICPSection1_1.md 
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git status
# On branch master
# Your branch is ahead of 'origin/master' by 1 commit.
#
# Changes to be committed:
#   (use "git reset HEAD <file>..." to unstage)
#
#	modified:   SICP.md
#	modified:   _posts/2014-07-27-SICPSection1_1.md
#	renamed:    _posts/#2014-07-27-SICPSection1_1.md# -> _posts/2014-07-28-SICPSolution1_1.md
#	deleted:    _posts/2014-07-29-SICPSection1_2.md
#
# Untracked files:
#   (use "git add <file>..." to include in what will be committed)
#
#	#index.html#
#	.editorconfig
#	.gitignore
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git commit -m "Split Section 1.1 summary and solutions"
[master 1ea1105] Split Section 1.1 summary and solutions
 Committer: mohan <mohan@mohan-Inspiron-1545.(none)>
Your name and email address were configured automatically based
on your username and hostname. Please check that they are accurate.
You can suppress this message by setting them explicitly:

    git config --global user.name "Your Name"
    git config --global user.email you@example.com

After doing this, you may fix the identity used for this commit with:

    git commit --amend --reset-author

 4 files changed, 77 insertions(+), 442 deletions(-)
 rewrite _posts/2014-07-27-SICPSection1_1.md (72%)
 rename _posts/{#2014-07-27-SICPSection1_1.md# => 2014-07-28-SICPSolution1_1.md} (72%)
 delete mode 100644 _posts/2014-07-29-SICPSection1_2.md
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git push origin master
Username for 'https://github.com': mohanrajendran
Password for 'https://mohanrajendran@github.com': Raffles123

To https://github.com/mohanrajendran/mohanrajendran.github.io.git
   e6f1c0c..1ea1105  master -> master
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ jekyll serve --watch &
[1] 5590
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ Configuration file: /home/mohan/Desktop/mohanrajendran.github.io/_config.yml
       Deprecation: The 'pygments' configuration option has been renamed to 'highlighter'. Please update your config file accordingly. The allowed values are 'rouge', 'pygments' or null.
            Source: /home/mohan/Desktop/mohanrajendran.github.io
       Destination: /home/mohan/Desktop/mohanrajendran.github.io/_site
      Generating... 
                    done.
 Auto-regeneration: enabled for '/home/mohan/Desktop/mohanrajendran.github.io'
Configuration file: /home/mohan/Desktop/mohanrajendran.github.io/_config.yml
       Deprecation: The 'pygments' configuration option has been renamed to 'highlighter'. Please update your config file accordingly. The allowed values are 'rouge', 'pygments' or null.
    Server address: http://0.0.0.0:4000/
  Server running... press ctrl-c to stop.
ls
404.html  atom.xml  CNAME  _config.yml  images  _includes  #index.html#  index.html  _layouts  _posts  public  SICP.md  _site
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$       Regenerating: 2 files at 2014-07-30 20:06:21 ...done.
      Regenerating: 2 files at 2014-07-30 20:12:12 ...done.
      Regenerating: 2 files at 2014-07-30 20:12:45 ...done.
      Regenerating: 2 files at 2014-07-30 20:13:37 ...done.
      Regenerating: 1 files at 2014-07-30 20:14:55 ...done.
      Regenerating: 3 files at 2014-07-30 20:15:55 ...done.
      Regenerating: 2 files at 2014-07-30 20:18:29 ...done.
      Regenerating: 1 files at 2014-07-30 20:26:57 ...done.
      Regenerating: 1 files at 2014-07-30 20:28:01 ...done.
      Regenerating: 1 files at 2014-07-30 20:28:35 ...done.
      Regenerating: 1 files at 2014-07-30 20:28:51 ...done.
      Regenerating: 1 files at 2014-07-30 20:30:14 ...done.
      Regenerating: 3 files at 2014-07-30 20:30:24 ...done.
      Regenerating: 2 files at 2014-07-30 20:30:39 ...done.
      Regenerating: 2 files at 2014-07-30 20:31:07 ...done.
      Regenerating: 1 files at 2014-07-30 20:31:43 ...done.
      Regenerating: 3 files at 2014-07-30 20:31:49 ...done.
      Regenerating: 2 files at 2014-07-30 20:32:14 ...done.
      Regenerating: 1 files at 2014-07-30 20:33:00 ...done.
      Regenerating: 3 files at 2014-07-30 20:33:30 ...done.
      Regenerating: 2 files at 2014-07-30 20:34:04 ...done.
      Regenerating: 2 files at 2014-07-30 20:34:16 ...done.
      Regenerating: 2 files at 2014-07-30 20:34:30 ...done.
      Regenerating: 2 files at 2014-07-30 20:34:46 ...done.
      Regenerating: 1 files at 2014-07-30 20:35:03 ...done.
      Regenerating: 3 files at 2014-07-30 20:35:07 ...done.
      Regenerating: 2 files at 2014-07-30 20:35:14 ...done.
      Regenerating: 1 files at 2014-07-30 20:37:38 ...done.
      Regenerating: 2 files at 2014-07-30 20:38:27 ...done.
      Regenerating: 2 files at 2014-07-30 20:38:39 ...done.
      Regenerating: 2 files at 2014-07-30 20:38:49 ...done.
git log
WARNING: terminal is not fully functional
-  (press RETURN)
commit 1ea11057726b4c96cc5b71a5f371874f9f1c6491
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Wed Jul 30 20:04:00 2014 -0500

    Split Section 1.1 summary and solutions

commit 4fc953da7a8e40f91042917d747d0041d7470853
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 22:01:49 2014 -0500

    Fixed links in SICP.md

commit e6f1c0c04e9cac76a416897099394fcecb5e97ac
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 21:36:12 2014 -0500

    Finished Section 1.1

commit b0692655325ed0e0c7593707051d5d7c1dcce914
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 20:59:31 2014 -0500

    Until exercise 1.5
:git log -p -2

commit 63312fa09ebbe5ccfcbd678ef79fbf8366544fa5
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 19:35:56 2014 -0500

    Until Exercise 1.4

commit 5205de39b22aa0d185ac4eb9e906c5cff09dabdd
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 18:52:20 2014 -0500

    Added CNAME file

commit 34edbe147d0b02d6465c6f174b14f184c8a88ccf
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sun Jul 27 22:30:34 2014 -0500

    Added first post

commit b58a9360116b1fde94d2a422897efeead8c21eb2
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sun Jul 27 21:09:04 2014 -0500

:      Regenerating: 1 files at 2014-07-30 20:41:24  git log SICP.md 
    Added first post. Edited CSS to give beautiful MathJax output

commit 34b2e0b1376910ad6ff373c442cd291c6e0444d4
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sun Jul 27 17:41:36 2014 -0500

    Added SICP page

commit 6cbf5aaf44fc57b589cc9e9c344c5e49ff057b7e
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sat Jul 26 19:06:03 2014 -0500

    Removed CNAME

commit d70ff4434fcaa0c129bbdcda05f127e44f665c7f
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sat Jul 26 19:04:01 2014 -0500

    Empty Lanyon theme page
(END)

...skipping...
commit 1ea11057726b4c96cc5b71a5f371874f9f1c6491
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Wed Jul 30 20:04:00 2014 -0500

    Split Section 1.1 summary and solutions

commit 4fc953da7a8e40f91042917d747d0041d7470853
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 22:01:49 2014 -0500

    Fixed links in SICP.md

commit e6f1c0c04e9cac76a416897099394fcecb5e97ac
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 21:36:12 2014 -0500

    Finished Section 1.1

commit b0692655325ed0e0c7593707051d5d7c1dcce914
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 20:59:31 2014 -0500

    Until exercise 1.5

commit 63312fa09ebbe5ccfcbd678ef79fbf8366544fa5
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 19:35:56 2014 -0500

    Until Exercise 1.4

commit 5205de39b22aa0d185ac4eb9e906c5cff09dabdd
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 18:52:20 2014 -0500

    Added CNAME file

commit 34edbe147d0b02d6465c6f174b14f184c8a88ccf
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sun Jul 27 22:30:34 2014 -0500

    Added first post

commit b58a9360116b1fde94d2a422897efeead8c21eb2
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sun Jul 27 21:09:04 2014 -0500

...skipping...
commit 1ea11057726b4c96cc5b71a5f371874f9f1c6491
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Wed Jul 30 20:04:00 2014 -0500

    Split Section 1.1 summary and solutions

commit 4fc953da7a8e40f91042917d747d0041d7470853
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 22:01:49 2014 -0500

    Fixed links in SICP.md

commit e6f1c0c04e9cac76a416897099394fcecb5e97ac
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 21:36:12 2014 -0500

    Finished Section 1.1

commit b0692655325ed0e0c7593707051d5d7c1dcce914
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 20:59:31 2014 -0500

    Until exercise 1.5

commit 63312fa09ebbe5ccfcbd678ef79fbf8366544fa5
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 19:35:56 2014 -0500

    Until Exercise 1.4

commit 5205de39b22aa0d185ac4eb9e906c5cff09dabdd
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 18:52:20 2014 -0500

    Added CNAME file

commit 34edbe147d0b02d6465c6f174b14f184c8a88ccf
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sun Jul 27 22:30:34 2014 -0500

    Added first post

commit b58a9360116b1fde94d2a422897efeead8c21eb2
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sun Jul 27 21:09:04 2014 -0500

    Added first post. Edited CSS to give beautiful MathJax output
:...done.
 git log SICP.md 

commit 34b2e0b1376910ad6ff373c442cd291c6e0444d4
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sun Jul 27 17:41:36 2014 -0500

    Added SICP page

commit 6cbf5aaf44fc57b589cc9e9c344c5e49ff057b7e
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sat Jul 26 19:06:03 2014 -0500

    Removed CNAME

commit d70ff4434fcaa0c129bbdcda05f127e44f665c7f
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sat Jul 26 19:04:01 2014 -0500

    Empty Lanyon theme page
(END)

...skipping...
commit 1ea11057726b4c96cc5b71a5f371874f9f1c6491
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Wed Jul 30 20:04:00 2014 -0500

    Split Section 1.1 summary and solutions

commit 4fc953da7a8e40f91042917d747d0041d7470853
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 22:01:49 2014 -0500

    Fixed links in SICP.md

commit e6f1c0c04e9cac76a416897099394fcecb5e97ac
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 21:36:12 2014 -0500

    Finished Section 1.1

commit b0692655325ed0e0c7593707051d5d7c1dcce914
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 20:59:31 2014 -0500

    Until exercise 1.5

commit 63312fa09ebbe5ccfcbd678ef79fbf8366544fa5
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 19:35:56 2014 -0500

    Until Exercise 1.4

commit 5205de39b22aa0d185ac4eb9e906c5cff09dabdd
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 18:52:20 2014 -0500

    Added CNAME file

commit 34edbe147d0b02d6465c6f174b14f184c8a88ccf
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sun Jul 27 22:30:34 2014 -0500

    Added first post

commit b58a9360116b1fde94d2a422897efeead8c21eb2
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sun Jul 27 21:09:04 2014 -0500

...skipping...
commit 1ea11057726b4c96cc5b71a5f371874f9f1c6491
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Wed Jul 30 20:04:00 2014 -0500

    Split Section 1.1 summary and solutions

commit 4fc953da7a8e40f91042917d747d0041d7470853
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 22:01:49 2014 -0500

    Fixed links in SICP.md

commit e6f1c0c04e9cac76a416897099394fcecb5e97ac
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 21:36:12 2014 -0500

    Finished Section 1.1

commit b0692655325ed0e0c7593707051d5d7c1dcce914
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 20:59:31 2014 -0500

    Until exercise 1.5

commit 63312fa09ebbe5ccfcbd678ef79fbf8366544fa5
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 19:35:56 2014 -0500

    Until Exercise 1.4

commit 5205de39b22aa0d185ac4eb9e906c5cff09dabdd
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 18:52:20 2014 -0500

    Added CNAME file

commit 34edbe147d0b02d6465c6f174b14f184c8a88ccf
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sun Jul 27 22:30:34 2014 -0500

    Added first post

commit b58a9360116b1fde94d2a422897efeead8c21eb2
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Sun Jul 27 21:09:04 2014 -0500

    Added first post. Edited CSS to give beautiful MathJax output
:git show HEAD~2:SICP.md


...skipping...
commit 1ea11057726b4c96cc5b71a5f371874f9f1c6491
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Wed Jul 30 20:04:00 2014 -0500

    Split Section 1.1 summary and solutions

commit 4fc953da7a8e40f91042917d747d0041d7470853
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 22:01:49 2014 -0500

    Fixed links in SICP.md

commit e6f1c0c04e9cac76a416897099394fcecb5e97ac
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 21:36:12 2014 -0500

    Finished Section 1.1

commit b0692655325ed0e0c7593707051d5d7c1dcce914
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 20:59:31 2014 -0500

    Until exercise 1.5
:      Regenerating: 1 files at 2014-07-30 20:42:26 ...done.
ls
Log file "how HEAD\~2:SICP.md"  (press RETURN)
:

:
commit 63312fa09ebbe5ccfcbd678ef79fbf8366544fa5
:
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
:ls
Log file "how HEAD\~2:SICP.md"  (press RETURN)
:git exit
...skipping...
commit 1ea11057726b4c96cc5b71a5f371874f9f1c6491
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Wed Jul 30 20:04:00 2014 -0500

    Split Section 1.1 summary and solutions

commit 4fc953da7a8e40f91042917d747d0041d7470853
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 22:01:49 2014 -0500

    Fixed links in SICP.md

commit e6f1c0c04e9cac76a416897099394fcecb5e97ac
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 21:36:12 2014 -0500

    Finished Section 1.1

commit b0692655325ed0e0c7593707051d5d7c1dcce914
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 20:59:31 2014 -0500

    Until exercise 1.5

:
commit 63312fa09ebbe5ccfcbd678ef79fbf8366544fa5
:git hist
...skipping...
commit 1ea11057726b4c96cc5b71a5f371874f9f1c6491
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Wed Jul 30 20:04:00 2014 -0500

    Split Section 1.1 summary and solutions

commit 4fc953da7a8e40f91042917d747d0041d7470853
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 22:01:49 2014 -0500

    Fixed links in SICP.md

commit e6f1c0c04e9cac76a416897099394fcecb5e97ac
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 21:36:12 2014 -0500

    Finished Section 1.1

commit b0692655325ed0e0c7593707051d5d7c1dcce914
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 20:59:31 2014 -0500

    Until exercise 1.5
...skipping...

                   SUMMARY OF LESS COMMANDS

      Commands marked with * may be preceded by a number, N.
      Notes in parentheses indicate the behavior if N is given.

  h  H                 Display this help.
  q  :q  Q  :Q  ZZ     Exit.
 ---------------------------------------------------------------------------

                           MOVING

  e  ^E  j  ^N  CR  *  Forward  one line   (or N lines).
  y  ^Y  k  ^K  ^P  *  Backward one line   (or N lines).
  f  ^F  ^V  SPACE  *  Forward  one window (or N lines).
  b  ^B  ESC-v      *  Backward one window (or N lines).
  z                 *  Forward  one window (and set window to N).
  w                 *  Backward one window (and set window to N).
  ESC-SPACE         *  Forward  one window, but don't stop at end-of-file.
  d  ^D             *  Forward  one half-window (and set half-window to N).
  u  ^U             *  Backward one half-window (and set half-window to N).
  ESC-)  RightArrow *  Left  one half screen width (or N positions).
  ESC-(  LeftArrow  *  Right one half screen width (or N positions).
HELP -- Press RETURN for more, or q when done      Regenerating: 2 files at 2014-07-30 20:43:19 ...done.
HELP -- Press RETURN for more, or q when doneq
...skipping...
commit 1ea11057726b4c96cc5b71a5f371874f9f1c6491
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Wed Jul 30 20:04:00 2014 -0500

    Split Section 1.1 summary and solutions

commit 4fc953da7a8e40f91042917d747d0041d7470853
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 22:01:49 2014 -0500

    Fixed links in SICP.md

commit e6f1c0c04e9cac76a416897099394fcecb5e97ac
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 21:36:12 2014 -0500

    Finished Section 1.1

commit b0692655325ed0e0c7593707051d5d7c1dcce914
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 20:59:31 2014 -0500

    Until exercise 1.5

:      Regenerating: 1 files at 2014-07-30 20:43:35 q

mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ ls
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ 404.html  CNAME        how HEAD~2:SICP.md  _includes     index.html  _posts  #SICP.md#  _site
atom.xml  _config.yml  images              #index.html#  _layouts    public  SICP.md    t
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ ...done.
git hist
git: 'hist' is not a git command. See 'git --help'.

Did you mean this?
	bisect
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$       Regenerating: 1 files at 2014-07-30 20:44:29 ...donls
404.html  CNAME        how HEAD~2:SICP.md  _includes     index.html  _posts  #SICP.md#  _site
atom.xml  _config.yml  images              #index.html#  _layouts    public  SICP.md    t
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git diff SICP.md 
WARNING: terminal is not fully functional
-  (press RETURN)
diff --git a/SICP.md b/SICP.md
index 3adb877..bff8ce5 100644
--- a/SICP.md
+++ b/SICP.md
@@ -13,5 +13,9 @@ To supplement the book, I will also be following the original online lectures by
 
 #### Contents
 
+
+
+<!--
 - [Section 1.1 The Elements of Programming]({% post_url 2014-07-27-SICPSection1_1 %})
-- [Solutions to section 1.1 exercise]({% post_url 2014-07-28-SICPSolution1_1 %})
+- [Solutions to exercises in section 1.1]({% post_url 2014-07-28-SICPSolution1_1 %})
+-->
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$      Regenerating: 1 files at 2014-07-30 20:46:14 ...done.
      Regenerating: 1 files at 2014-07-30 20:47:46 ...done.
      Regenerating: 3 files at 2014-07-30 20:49:05 ...done.
      Regenerating: 2 files at 2014-07-30 20:49:58 ...done.
      Regenerating: 2 files at 2014-07-30 20:52:46 ...done.
      Regenerating: 1 files at 2014-07-30 20:53:02 ...done.
      Regenerating: 3 files at 2014-07-30 20:53:29 ...done.
      Regenerating: 2 files at 2014-07-30 20:54:09 ...done.
      Regenerating: 1 files at 2014-07-30 20:55:25 ...done.
      Regenerating: 3 files at 2014-07-30 20:56:04 ...done.
      Regenerating: 2 files at 2014-07-30 20:57:11 ...done.
      Regenerating: 2 files at 2014-07-30 20:59:34 ...done.
      Regenerating: 2 files at 2014-07-30 20:59:59 ...done.
      Regenerating: 2 files at 2014-07-30 21:00:15 ...done.
      Regenerating: 2 files at 2014-07-30 21:00:34 ...done.
      Regenerating: 2 files at 2014-07-30 21:01:02 ...done.
      Regenerating: 2 files at 2014-07-30 21:02:37 ...done.
      Regenerating: 2 files at 2014-07-30 21:04:31 ...done.
      Regenerating: 2 files at 2014-07-30 21:05:43 ...done.
      Regenerating: 2 files at 2014-07-30 21:06:09 ...done.
      Regenerating: 2 files at 2014-07-30 21:08:04 ...done.
      Regenerating: 2 files at 2014-07-30 21:08:15 ...done.
      Regenerating: 2 files at 2014-07-30 21:08:38 ...done.
      Regenerating: 2 files at 2014-07-30 21:10:48   Liquid Exception: Unknown operator category in SICP.md
...error:
             Error: Unknown operator category
             Error: Run jekyll build --trace for more information.
      Regenerating: 2 files at 2014-07-30 21:12:18   Liquid Exception: Unknown operator category in SICP.md
...error:
             Error: Unknown operator category
             Error: Run jekyll build --trace for more information.
      Regenerating: 2 files at 2014-07-30 21:12:40   Liquid Exception: Unknown operator category in SICP.md
...error:
             Error: Unknown operator category
             Error: Run jekyll build --trace for more information.
      Regenerating: 2 files at 2014-07-30 21:13:14   Liquid Exception: Unknown operator category in SICP.md
...error:
             Error: Unknown operator category
             Error: Run jekyll build --trace for more information.
      Regenerating: 2 files at 2014-07-30 21:15:44   Liquid Exception: Unknown operator category in SICP.md
...error:
             Error: Unknown operator category
             Error: Run jekyll build --trace for more information.
      Regenerating: 2 files at 2014-07-30 21:16:25   Liquid Exception: Unknown operator category in SICP.md
...error:
             Error: Unknown operator category
             Error: Run jekyll build --trace for more information.
      Regenerating: 2 files at 2014-07-30 21:16:51 ...done.
      Regenerating: 2 files at 2014-07-30 21:18:33 ...done.
      Regenerating: 2 files at 2014-07-30 21:20:17 ...done.
      Regenerating: 2 files at 2014-07-30 21:20:58 ...done.
      Regenerating: 2 files at 2014-07-30 21:21:13 ...done.
      Regenerating: 2 files at 2014-07-30 21:21:32 ...done.
      Regenerating: 2 files at 2014-07-30 21:23:22 ...done.
      Regenerating: 2 files at 2014-07-30 21:24:01 ...done.
      Regenerating: 2 files at 2014-07-30 21:24:29 ...done.
      Regenerating: 2 files at 2014-07-30 21:26:20 ...done.
      Regenerating: 2 files at 2014-07-30 21:27:28 ...done.
      Regenerating: 2 files at 2014-07-30 21:27:39 ...done.
      Regenerating: 2 files at 2014-07-30 21:28:09 ...done.
      Regenerating: 2 files at 2014-07-30 21:28:32 ...done.
      Regenerating: 1 files at 2014-07-30 21:29:24 ...done.
      Regenerating: 3 files at 2014-07-30 21:34:58   Liquid Exception: Unknown tag 'endfor' in SICP.md
...error:
             Error: Unknown tag 'endfor'
             Error: Run jekyll build --trace for more information.
      Regenerating: 2 files at 2014-07-30 21:35:14 ...done.
      Regenerating: 2 files at 2014-07-30 21:35:39 ...done.
      Regenerating: 2 files at 2014-07-30 21:36:56 ...done.
      Regenerating: 1 files at 2014-07-30 21:37:24 ...done.
      Regenerating: 3 files at 2014-07-30 21:37:47 ...done.
      Regenerating: 2 files at 2014-07-30 21:38:05 ...done.
      Regenerating: 1 files at 2014-07-30 21:40:06 ...done.
      Regenerating: 1 files at 2014-07-30 21:40:43 ...done.
      Regenerating: 3 files at 2014-07-30 21:40:58 ...done.
      Regenerating: 2 files at 2014-07-30 21:41:15 ...done.
      Regenerating: 2 files at 2014-07-30 21:43:21 ...done.
      Regenerating: 2 files at 2014-07-30 21:43:41 ...done.
      Regenerating: 2 files at 2014-07-30 21:46:25 ...done.
      Regenerating: 2 files at 2014-07-30 21:48:09 ...done.
      Regenerating: 2 files at 2014-07-30 21:48:39 ...done.
      Regenerating: 2 files at 2014-07-30 21:49:12 ...done.
      Regenerating: 2 files at 2014-07-30 21:49:54 ...done.
      Regenerating: 2 files at 2014-07-30 21:50:12 ...done.
      Regenerating: 1 files at 2014-07-30 21:51:07 ...done.
      Regenerating: 1 files at 2014-07-30 21:52:50 ...done.
      Regenerating: 3 files at 2014-07-30 21:53:54 ...done.
      Regenerating: 2 files at 2014-07-30 21:54:06 ...done.
      Regenerating: 1 files at 2014-07-30 21:54:39 ...done.
      Regenerating: 3 files at 2014-07-30 21:55:01 ...done.
      Regenerating: 2 files at 2014-07-30 21:56:06 ...done.
      Regenerating: 2 files at 2014-07-30 21:56:44 ...done.
git status
# On branch master
# Changes not staged for commit:
#   (use "git add <file>..." to update what will be committed)
#   (use "git checkout -- <file>..." to discard changes in working directory)
#
#	modified:   SICP.md
#	modified:   _posts/2014-07-27-SICPSection1_1.md
#	modified:   _posts/2014-07-28-SICPSolution1_1.md
#
# Untracked files:
#   (use "git add <file>..." to include in what will be committed)
#
#	#index.html#
#	.editorconfig
#	.gitignore
#	_posts/2014-07-30-SICPSolution1_2.md
#	how HEAD~2:SICP.md
#	t
no changes added to commit (use "git add" and/or "git commit -a")
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git add SICP.md 
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ gitt add _posts/2014-07-*
No command 'gitt' found, did you mean:
 Command 'gift' from package 'gnuift' (universe)
 Command 'gitit' from package 'gitit' (universe)
 Command 'gitg' from package 'gitg' (universe)
 Command 'gitk' from package 'gitk' (main)
 Command 'git' from package 'git' (main)
 Command 'gist' from package 'yorick' (universe)
gitt: command not found
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git add _posts/2014-07-*
The following paths are ignored by one of your .gitignore files:
_posts/2014-07-28-SICPSolution1_1.md~
_posts/2014-07-30-SICPSolution1_2.md~
Use -f if you really want to add them.
fatal: no files added
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git status
# On branch master
# Changes to be committed:
#   (use "git reset HEAD <file>..." to unstage)
#
#	modified:   SICP.md
#
# Changes not staged for commit:
#   (use "git add <file>..." to update what will be committed)
#   (use "git checkout -- <file>..." to discard changes in working directory)
#
#	modified:   _posts/2014-07-27-SICPSection1_1.md
#	modified:   _posts/2014-07-28-SICPSolution1_1.md
#
# Untracked files:
#   (use "git add <file>..." to include in what will be committed)
#
#	#index.html#
#	.editorconfig
#	.gitignore
#	_posts/2014-07-30-SICPSolution1_2.md
#	how HEAD~2:SICP.md
#	t
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git add _posts/2014-07-27-SICPSection1_1.md 
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git add _posts/2014-07-28-SICPSolution1_1.md
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git staus
git: 'staus' is not a git command. See 'git --help'.

Did you mean this?
	status
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git status
# On branch master
# Changes to be committed:
#   (use "git reset HEAD <file>..." to unstage)
#
#	modified:   SICP.md
#	modified:   _posts/2014-07-27-SICPSection1_1.md
#	modified:   _posts/2014-07-28-SICPSolution1_1.md
#
# Untracked files:
#   (use "git add <file>..." to include in what will be committed)
#
#	#index.html#
#	.editorconfig
#	.gitignore
#	_posts/2014-07-30-SICPSolution1_2.md
#	how HEAD~2:SICP.md
#	t
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git commit -m "Added liquid code to generate contents automatically"
[master f2702b0] Added liquid code to generate contents automatically
 Committer: mohan <mohan@mohan-Inspiron-1545.(none)>
Your name and email address were configured automatically based
on your username and hostname. Please check that they are accurate.
You can suppress this message by setting them explicitly:

    git config --global user.name "Your Name"
    git config --global user.email you@example.com

After doing this, you may fix the identity used for this commit with:

    git commit --amend --reset-author

 3 files changed, 39 insertions(+), 14 deletions(-)
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git push origin master
Username for 'https://github.com': mohanrajendran
Password for 'https://mohanrajendran@github.com': Raffles123

To https://github.com/mohanrajendran/mohanrajendran.github.io.git
   1ea1105..f2702b0  master -> master
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$       Regenerating: 1 files at 2014-07-30 22:15:00 ...done.
      Regenerating: 3 files at 2014-07-30 22:15:08 ...done.
      Regenerating: 1 files at 2014-07-30 22:15:48 ...done.
      Regenerating: 1 files at 2014-07-30 22:19:08 ...done.
      Regenerating: 3 files at 2014-07-30 22:20:17 ...done.
      Regenerating: 1 files at 2014-07-30 22:21:12 ...done.
      Regenerating: 1 files at 2014-07-30 22:22:51 ...done.
      Regenerating: 1 files at 2014-07-30 22:24:10 ...done.
      Regenerating: 1 files at 2014-07-30 22:25:25 ...done.
      Regenerating: 3 files at 2014-07-30 22:25:58   Liquid Exception: Unknown tag 'endhightlight' in _posts/2014-07-30-SICPSolution1_2.md/#excerpt
...error:
             Error: Unknown tag 'endhightlight'
             Error: Run jekyll build --trace for more information.
      Regenerating: 1 files at 2014-07-30 22:28:32   Liquid Exception: Unknown tag 'endhightlight' in _posts/2014-07-30-SICPSolution1_2.md/#excerpt
...error:
             Error: Unknown tag 'endhightlight'
             Error: Run jekyll build --trace for more information.
      Regenerating: 1 files at 2014-07-30 22:30:21   Liquid Exception: Unknown tag 'endhightlight' in _posts/2014-07-30-SICPSolution1_2.md/#excerpt
...error:
             Error: Unknown tag 'endhightlight'
             Error: Run jekyll build --trace for more information.
      Regenerating: 1 files at 2014-07-30 22:31:31   Liquid Exception: Unknown tag 'endhightlight' in _posts/2014-07-30-SICPSolution1_2.md/#excerpt
...error:
             Error: Unknown tag 'endhightlight'
             Error: Run jekyll build --trace for more information.
      Regenerating: 1 files at 2014-07-30 22:33:12   Liquid Exception: Unknown tag 'endhightlight' in _posts/2014-07-30-SICPSolution1_2.md/#excerpt
...error:
             Error: Unknown tag 'endhightlight'
             Error: Run jekyll build --trace for more information.
      Regenerating: 3 files at 2014-07-30 22:33:30   Liquid Exception: Unknown tag 'endhightlight' in _posts/2014-07-30-SICPSolution1_2.md/#excerpt
...error:
             Error: Unknown tag 'endhightlight'
             Error: Run jekyll build --trace for more information.
      Regenerating: 2 files at 2014-07-30 22:34:14 ...done.
      Regenerating: 2 files at 2014-07-30 22:34:41 ...done.
      Regenerating: 2 files at 2014-07-30 22:34:59 ...done.
      Regenerating: 1 files at 2014-07-30 22:35:20 ...done.
      Regenerating: 3 files at 2014-07-30 22:35:25 ...done.
      Regenerating: 2 files at 2014-07-30 22:36:03 ...done.
      Regenerating: 2 files at 2014-07-30 22:36:33 ...done.
      Regenerating: 2 files at 2014-07-30 22:36:50 ...done.
      Regenerating: 2 files at 2014-07-30 22:37:23 ...done.
      Regenerating: 2 files at 2014-07-30 22:39:51 ...done.
      Regenerating: 1 files at 2014-07-30 22:40:36 ...done.
      Regenerating: 3 files at 2014-07-30 22:42:24   Liquid Exception: Unknown tag 'endhightlight' in _posts/2014-07-30-SICPSolution1_2.md/#excerpt
...error:
             Error: Unknown tag 'endhightlight'
             Error: Run jekyll build --trace for more information.
      Regenerating: 2 files at 2014-07-30 22:42:35 ...done.
      Regenerating: 2 files at 2014-07-30 22:43:43 ...done.
      Regenerating: 2 files at 2014-07-30 22:44:02 ...done.
      Regenerating: 2 files at 2014-07-30 22:44:51 ...done.
      Regenerating: 2 files at 2014-07-30 22:46:00 ...done.
git status
# On branch master
# Untracked files:
#   (use "git add <file>..." to include in what will be committed)
#
#	#index.html#
#	.editorconfig
#	.gitignore
#	_posts/2014-07-30-SICPSolution1_2.md
#	how HEAD~2:SICP.md
#	t
nothing added to commit but untracked files present (use "git add" to track)
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git add _posts/2014-07-30-SICPSolution1_2.md
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git commit -m "Until Exercise 1.10"
[master 67badbd] Until Exercise 1.10
 Committer: mohan <mohan@mohan-Inspiron-1545.(none)>
Your name and email address were configured automatically based
on your username and hostname. Please check that they are accurate.
You can suppress this message by setting them explicitly:

    git config --global user.name "Your Name"
    git config --global user.email you@example.com

After doing this, you may fix the identity used for this commit with:

    git commit --amend --reset-author

 1 file changed, 176 insertions(+)
 create mode 100644 _posts/2014-07-30-SICPSolution1_2.md
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git diff --staged
WARNING: terminal is not fully functional
-  (press RETURN)
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git reset _posts/2014-07-30-SICPSolution1_2.md
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git status
# On branch master
# Your branch is ahead of 'origin/master' by 1 commit.
#
# Untracked files:
#   (use "git add <file>..." to include in what will be committed)
#
#	#index.html#
#	.editorconfig
#	.gitignore
#	how HEAD~2:SICP.md
#	t
nothing added to commit but untracked files present (use "git add" to track)
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git status
# On branch master
# Your branch is ahead of 'origin/master' by 1 commit.
#
# Untracked files:
#   (use "git add <file>..." to include in what will be committed)
#
#	#index.html#
#	.editorconfig
#	.gitignore
#	how HEAD~2:SICP.md
#	t
nothing added to commit but untracked files present (use "git add" to track)
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git branch sicp_sec_1_2
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git checkout sicp_sec_1_2
Switched to branch 'sicp_sec_1_2'
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git add _posts/2014-07-30-SICPSolution1_2.md
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git status
# On branch sicp_sec_1_2
# Untracked files:
#   (use "git add <file>..." to include in what will be committed)
#
#	#index.html#
#	.editorconfig
#	.gitignore
#	how HEAD~2:SICP.md
#	t
nothing added to commit but untracked files present (use "git add" to track)
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git add _posts/2014-07-30-SICPSolution1_2.md
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git commit -m "Until exercise 1.10"
# On branch sicp_sec_1_2
# Untracked files:
#   (use "git add <file>..." to include in what will be committed)
#
#	#index.html#
#	.editorconfig
#	.gitignore
#	how HEAD~2:SICP.md
#	t
nothing added to commit but untracked files present (use "git add" to track)
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git checkout maser
error: pathspec 'maser' did not match any file(s) known to git.
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git checkout master
Switched to branch 'master'
Your branch is ahead of 'origin/master' by 1 commit.
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git log
WARNING: terminal is not fully functional
-  (press RETURN)
commit 67badbd3c4130c536ee0e17474e8ea821abfd092
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Wed Jul 30 22:46:46 2014 -0500

    Until Exercise 1.10

commit f2702b0691eb6405a3034c8f41a2d54dd71c24d9
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Wed Jul 30 21:58:47 2014 -0500

    Added liquid code to generate contents automatically

commit 1ea11057726b4c96cc5b71a5f371874f9f1c6491
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Wed Jul 30 20:04:00 2014 -0500

    Split Section 1.1 summary and solutions

commit 4fc953da7a8e40f91042917d747d0041d7470853
Author: mohan <mohan@mohan-Inspiron-1545.(none)>
Date:   Mon Jul 28 22:01:49 2014 -0500

    Fixed links in SICP.md
:q
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ git rm _posts/2014-07-30-SICPSolution1_2.md
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ rm '_posts/2014-07-30-SICPSolution1_2.md'
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$      Regenerating: 1 files at 2014-07-30 22:57:54 ...done.
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$      Regenerating: 1 files at 2014-07-30 22:57:54 ...done.
bash: mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$: No such file or directory
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$ ls
404.html  CNAME        how HEAD~2:SICP.md  _includes     index.html  _posts  SICP.md  t
atom.xml  _config.yml  images              #index.html#  _layouts    public  _site
mohan@mohan-Inspiron-1545:~/Desktop/mohanrajendran.github.io$      Regenerating: 1 files at 2014-07-30 22:58:35 ...done.
