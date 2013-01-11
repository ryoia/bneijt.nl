---
title: The multiple return arguments argument
date: 2013-01-11
---
When new languages appear, like Scala, people start raving about the language features that give you the freedom to do things with less characters.
One of these things is multiple return arguments.
Generally speaking it is the ability to assign values to members of anonymous containers (often _only_ a tuple).

I've convinced myself that if I feel like I need multiple return arguments to work in a language, that I'm not creating a proper API.
Only problem is, have I just convinced myself or is my argument convincing? Here goes.

People coming from more dynamic languages like
[Python](http://python.org/about/), 
[PHP](http://php.net/), 
[Ruby](http://www.ruby-lang.org/en/about/),
[Javascript](https://developer.mozilla.org/en/JavaScript) and now [Scala](http://www.scala-lang.org/)
have come accustomed to be able to use multiple return arguments by initializing a tuple with the return value of a function. Say

    (book, location) = bookcase.firstShelf.grabFromShelf(12.th.book());

Looking at only this example, two sides of an argument can be quickly listed. The dynamic/messy people will say:

> It feels nice and terse to get two local variables initialized with a single function call/assignment.
> The grabFromShelf member has to do the lookup anyway, so why let the location go to waste?
> Having to create a type to return a collection like that is just extra work.
> Naming a type for each of these kinds of calls is even harder.
> Did you know it comes in handy when you want to add a succeeded/failed status boolean to the return code?

The other side, the sluggish/clean people will say:

> One function, one responsibility so it should never have to return more then one object
> It's better to have a clean design then trying to saver every cycle. Ever heard of premature optimization?
> If the data you return cannot be placed in a single variable/class, it should not be returned together.
> Naming should not be a problem if you have a good OO design.
> Adding a succeeded/failure status boolean is stupid, failure should be an exception.

And it is this last point which I would like to take a closer look at. Specifically the `FileNotFoundException`. The two APIs are:


    (file, success) = open(filename)


and


    if(File(filename).exists())
    {
        file = open(filename);
    }


The first example just tries to open the file and if that fails, success will be false and file invalid or a closed `File` object.
The second example will test for the existence of the file and consider failure beyond that point an exception.

First thing that you may not like about the second piece of code is that it clearly states a race condition which is caught by an exception.
If the race condition occurs, the programmer will be a bit stupefied by it because the code seems to say:
only try to open the file if it is actually there. If you feel this is the case, you might prefer:

    try {
        file = open(filename);
    } catch(FileNotFoundException e) {
        //Handle the no success case
    }

I think this last step, using try catch block, will split the scope of your success and failure handling code and will probably make your life more difficult:
how much code will go into the try block, how much code will go into the catch block.

I think checking for file existence, opening the file and passing the Exception up in the tree is the best way to go.
The first version may return a useless parameter (`file`) if it failed to open the file and the function would be better off returning an object
with the right members, making the API more self-documenting.

I think the second piece of code also has a lesson: if you feel like exception handling is cutting up your scope to much, use state checking to promote the Exception to a _real_ exceptional case.
After that, I don't see why you would have to have multiple return argument support anymore.
It may be handy, but it certainly is not a feature I would consider exciting when choosing a new language.

