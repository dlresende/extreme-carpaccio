# DNX+CSharp Extreme-Carpaccio

## What's in the box?

You want to go through the nice path of C# but within the shell, your prefered editor and an *nix box? _This template is for you!_

Withing the root folder you'll find a standard DNX structure:

* src/
  * xcarpaccio/
    * app.cs -> the start point of your code!
    * project.json -> the config & dependencies
* test/
  * xcarpaccio.tests/
    * tests.cs -> a first test sample
    * project.json -> the config & dependencies

In order to build and run the template, you need to have installed Mono and the Dnx tooling and runtimes. If you're not familiar with that, please go to [Asp.Net Home](https://github.com/aspnet/home) and check the install sections (like [Installing ASP.NET 5 on Mac OS X](https://docs.asp.net/en/latest/getting-started/installing-on-mac.html))

## Getting started

If you don't know how to start, run the `build.sh` command to prepare the template. It's not a real build system, but just enough commands to get the dependencies and be able to start (read it to get the basic commands ;-))

Then, the best way to be efficient during the carpaccio is to have a live reload web server and continuous testing.

For the web server -aka the code you'll write for the carpaccio- you'll have the live reload by launching the `watch-web.sh`. For the continuous testing, in the same way, just launch `watch-tests.sh`.

My usual window configuration for this exercice is an Atom editor on the left half of the screen and a terminal splitted in 3 the right half (one for web watch, one for the continuous testing and one for commits)


## Additional Information

If you want to have an acceptable tooling for C# dev on *nix, you can install [Microsoft VSCODE](https://code.visualstudio.com) (easiest way) or you can use an editor like [Atom](http://atom.io) with the [Omnisharp](http://www.omnisharp.net/) plugin (the most up-to-date). Btw, know that VsCode is an MS tool based on Atom shell + Omnisharp...
