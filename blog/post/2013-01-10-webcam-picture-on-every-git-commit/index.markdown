---
title: Webcam picture on every git commit
date: 2013-01-10
---

About a month back, I came across a [Hacker News item](http://news.ycombinator.com/item?id=4151327) about taking a picture at every git commit. And as I liked the idea, I looked at what people where doing to get it to work. As most solutions seemed to complicated to me, I decided to implement my own. Below I'll give insight into the pieces of my git webcam snapshotting setup.

Taking a webcam snapshot
------------------------
First, getting the webcam snapshots to work on Linux. There are programs out there, but the easiest thing I could think of was putting [gstreamer](http://gstreamer.freedesktop.org/) to work. I created a file called `bin/webcamsnapshot` (the `bin` directory there is on my `PATH`).

    #!/bin/bash
    timestamp="`date +%Y%m%dT%H:%M:%S%:z`"

    activity=$1
    if [ -z "$activity" ]; then
        activity="none"
    fi
    if [ $# -gt 1 ]; then
        echo "To many arguments given"
        exit 1
    fi
    echo "Snapshot activity: ${activity}"
    outputPath="${HOME}/Pictures/webcam/${activity}"
    mkdir -p "${outputPath}"
    cd "${outputPath}" 
    /usr/bin/gst-launch -e v4l2src num-buffers=1 ! jpegenc \
        ! filesink location="webcam ${timestamp}.jpg" \
        > /dev/null 2>&1 < /dev/null &

The script is simple enough and requires you to give an activity as the first argument, which will be the name of the directory. For example, calling `webcamsnapshot "writing blog"` will output `Snapshot activity: writing blog` and write a jpeg file in `~/Pictures/webcam/writing blog/webcam 20130109T22:11:02+01:00.jpg`.
As you can see, the whole script is pretty over-engineered with an _activity_ and a _timezone_ in the time-stamp. Anyway, on to git.

Intercepting your git commit
----------------------------
I didn't want to create a git commit hook, because I work with multiple repositories. The easiest solution, is to just take a place on your `PATH` before the real git, and add another bash script called `git` to intercept the git command:


    #!/bin/bash
    /usr/bin/git "$@"
    ESTATUS="$?"
    if [ "$ESTATUS" = "0" ]; then
        if [ "$1" = "commit" ]; then
            webcamsnapshot "git commit"
        fi
    fi
    exit "${ESTATUS}"


Because we want this script to be an interceptor for the real git, there are three main things it has to do:

1. Make sure all arguments are passed on to the real `git`: `"$@"`.
2. Make sure the exit status of the real git is returned (available trough `$?`)
3. Make sure stdin/stdout/stderr is passed on, but bash already does this for us.

Only if the `git` command is successful the `webcamsnapshot` command will be executed. This means that every time a git commit goes through, a picture is taken.

Result
------
Best part of course are the pictures.

<div class="row">
    <div class="six columns">
        <a href="snapshot1.jpg" class="th">
            <img width="320" height="240" src="snapshot1.jpg" alt="Snapshot 1" />
        </a>
    </div>
    <div class="six columns">
        <a href="snapshot2.jpg" class="th">
            <img width="320" height="240" src="snapshot2.jpg" alt="Snapshot 2" />
        </a>
    </div>
</div>
<div class="row">
    <div class="six columns">
        <a href="snapshot3.jpg" class="th">
            <img width="320" height="240" src="snapshot3.jpg" alt="Snapshot 3" />
        </a>
    </div>
    <div class="six columns">
        <a href="snapshot4.jpg" class="th">
            <img width="320" height="240" src="snapshot4.jpg" alt="Snapshot 4" />
        </a>
    </div>
</div>





