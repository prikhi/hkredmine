HKRedmine
==========

A Redmine CLI client written in Haskell.

You can do things like track time towards an Issue, create new Issues, view
Versions & Projects, filter & sort Issues, etc.

You can also reuse the API library for your own projects.


Quickstart
-----------

1. Clone this repository: `git clone http://bugs.sleepanarchy.com/hkredmine.git`
1. Install hkredmine: `cd hkredmine; cabal install -j`
1. Add it to your path: `export PATH=$PATH:~/.cabal/bin`
1. Add your Redmine URL to the config file: `echo 'url=http://redmine.yourdomain.com/' > ~/.hkredminerc`
1. And your API key: `echo 'apikey="longalphanumericstring"' >> ~/.hkredminerc`
1. List available projects: `hkredmine projects`
1. List the issues of a project: `hkredmine issues -p <project_identifier>`
1. List the next version of a project: `hkredmine nextversion <project_identifier>`
1. Start tracking time on an issue: `hkredmine startwork <issue_id>`
1. Stop tracking time and create a new time entry: `hkredmine stopwork`
1. Learn how to use everything else: `hkredmine --help`


Installation
-------------

No packages currently exist, if you have experience packaging Haskell programs
for Debian, Arch Linux, etc. please drop us a patch or some hints.

You can still build and install directly from the source. You will need
`cabal`, `git` and `zlib`:

    # Arch-Linux
    sudo pacman -S cabal-install git zlib

    # Debian/Ubuntu
    sudo apt-get install cabal-install git zlib1g-dev

Next update your cabal package list:

    cabal update

Then grab the source:

    git clone http://bugs.sleepanarchy.com/hkredmine.git

Build and install the program:

    cd hkredmine
    cabal sandbox init
    cabal install -j

Then copy the executable somewhere in your path:

    sudo cp dist/build/hkredmine/hkredmine /usr/bin
    cp dist/build/hkredmine/hkredmine ~/.bin

You may then remove the `hkredmine` directory:

    cd ..
    rm -rf hkredmine


Configuration
--------------

`hkredmine` expects a configuration file at `~/.hkredminerc`. It should follow
the basic INI `key = value` style. Only two settings are required, `url` and
`apikey`:

    # ~/.hkredminerc
    url = http://redmine.mydomain.com/
    apikey = "longalphanumericapikey"

Be sure to enclose the API key in quotes and to include the trailing slash in
the URL.

Multiple accounts may be used by putting keys under `[AccountName]` headers:

    # ~/.hkredminerc
    [account1]
    url = ...
    apikey = "..."

    [account2]
    url = ...
    apikey = "..."

You can then switch accounts using the `use` command, like so:

    hkredmine use account1
    hkredmine projects
    # Projects from account1 tracker are shown
    hkredmine use account2
    hkredmine projects
    # Projects from account2 tracker are shown


General Usage
--------------

Run `hkredmine --help` for a listing of available commands and options.
Run `hkredmine <command> --help` for additional help and available parameters
for a specific command.

You can generate bash completion files by passing the `bash` argument to the
`help` flag:

    hkredmine --help=bash > hkredmine.comp
    source hkredmine.comp

These will also work with zsh if you enable `bashcompinit` before sourcing the
completion file:

    autoload bashcompinit
    bashcompinit

    source hkredmine.comp

The following commands are currently available:

    Commands:
        use          Switch to a different redmine account.
        [status]     Print the current Account, Issue and Tracked Time.
    Projects:
        project      Print the details of a Project.
        projects     Print all Projects.
    Issues:
        issue        Print the details of an Issue.
        issues       Filter and Print Issues.
        watched      Filter and Print your Watched Issues.
        newissue     Create a New Issue.
        update       Update an New Issue.
        close        Close an Issue.
        watch        Watch an Issue.
        unwatch      Unwatch an Issue.
    Options:
        fields       Print available field values(Statuses, Priorities, etc.).
        categories   Print a Project's Categories.
        newcategory  Create a Category.
    Time Tracking:
        startwork    Start tracking time for an Issue.
        stopwork     Stop time tracking and submit a time entry.
        pause        Pause time tracking.
        resume       Resume time tracking.
        abort        Abort time tracking.
    Versions:
        version      Print the details of a Version.
        versions     Print all of a Project's Versions.
        nextversion  Print the next Version due for a Project.



Creating Issues
----------------

You can use the `newissue` command to create a new Issue. You need to pass it a
project identifier and a subject:

    hkredmine newissue -p my_proj -s "Refactor Foobar Class"

If you want to write out a longer description, pass the `-e` flag to open up
your `$EDITOR`:

    export EDITOR=vim
    hkredmine newissue -p my_proj -s "Refactor that thing" -e

Once you happy with the description, save and quit. You might encounter issues
with asynchronous editors like `gvim`. You could make an alias:

    alias hkredmine="EDITOR=vim hkredmine"

Vim user's may be interested in [this syntax plugin][vim-redminewiki]. We
follow the `*.redmine` extension convention.


Updating Issues
----------------

You can modify existing Issues using the `update` command:

    hkredmine update 77 --priority High -t Bug --category=UI -n "Fix Category"

You can pass the `-e` flag to edit the description in your `$EDITOR`:

    hkredmine update 31 --priority Immediate -s "A NU Start" -e


Creating Field Options
-----------------------

Right now, you can only create new Categories for Projects, using the
`newcateogry` command:

    hkredmine newcategory a-project Models

The `-m` flag will assign any Issues in the Category to you:

    hkredmine newcategory my-dotfiles "Vim Plugins" -m


Viewing Things
---------------

You can use the `projects` command to list all Projects and the `project`
command to list one Project's details:

    hkredmine projects
    hkredmine project my-projects-identifier

You can use the `issues`, `watched` and `version` commands to show Issues:

    hkredmine issues --project my-project --status "In Progress"
    hkredmine watched --sort=priority
    hkredmine version 12

And the `issue` command to show details of a single Issue:

    hkredmine issue 42

To view a Project's available Versions, use `versions`:

    hkredmine versions my-projects-identifier

You can see the available Issue Statuses, Priorities, Categories or Time Entry
Activites using the `fields` and `categories` commands:

    hkredmine fields
    hkredmine categories a-project-with-categories


Time Tracking
--------------

You can initiate time tracking for an Issue by using the `startwork` command:

    hkredmine startwork 154

You can also `pause`, `resume` and `abort` time tracking:

    hkredmine pause
    hkredmine resume
    hkredmine abort

When you're ready to submit a new time entry, run the `stopwork` command:

    hkredmine stopwork

You can skip the prompts by passing parameters on the command line:

    hkredmine stopwork --activity=Development --comment="Writing @stopwork@ documentation"


Closing Issues
---------------

When Issues are considered finished, you can run the `close` command:

    hkredmine close 154

This will change the Issue's status to `Closed`, the done ratio to `100%` and
the due date to today(but only if it is not already set).

You can specify a comment to leave when updating the Issue using the `comment`
flag:

    hkredmine close 69 --comment "Hotfix pushed to prod"


Workflow
---------

Let's begin our day by picking an account to use. This is only necessary is you
have accounts on multiple Redmine instances:

    hkredmine use sleepanarchy

As a quick refresher, we'll see what Issues we are watching:

    hkredmine watched

Nothing we feel like tackling at the moment... Let's checkout all available
projects:

    hkredmine projects

We find a project we want to work on, let's check out the open issues in the
project's next version:

    hkredmine nextversion acorn-accounting

We see an Issue that catches our eye, let's see it's details:

    hkredmine issue 154

Looks good, let's mark it as "In Progress", set the start date to today and
start tracking time towards the Issue:

    hkredmine startwork 154

An emergency has come up in a project that is on a different Redmine instance.
First, we'll stop tracking time for the current Issue and create a new time
entry:

    hkredmine stopwork --type=Development --comment="Started writing tests."

We'll switch to our account on the other Redmine instance, then create a new Issue:

    hkredmine use acorn
    hkredmine newissue --project=it-computers -t Support -i Immediate -e -s "Emergency Request"

Our editor should open up to let us enter a detailed description for the Issue.
After saving and exiting from our editor, the issue will be created. Let's
start work on it:

    hkredmine startwork 280

While working on the Issue, we notice something out of scope and quickly make a
new issue:

    hkredmine newissue -p it-computers -t Bug -s "Something is wrong with X!"

Let's watch it so we remember to come back to it:

    hkredmine watch 281

Finally, we're done with the emergency Issue.Let's stop work on it:

    hkredmine stopwork --activity=Administration --comment="Help Y with X."

Let's mark the Issue as Closed, set the Due Date to today and the Done Ratio to
100%:

    hkredmine close 280

We can jump back to our first Issue now:

    hkredmine use sleepanarchy
    hkredmine startwork 154

After working a bit more, we go for a walk:

    hkredmine pause

Once we get back, we start tracking time again:

    hkredmine resume

But... we forgot what we were doing:

    hkredmine status
    hkredmine issue 154

OK, we've done as much as we can, we need some feedback to continue:

    hkredmine update 154 --status="Feedback"

Finally we'll submit a time entry and watch the Issue:

    hkredmine stopwork --activity=Development comment="Fixed thing, waiting on confirmation."
    hkredmine watch 154


Contribute
-----------

Request features, report bugs and submit patches:
http://bugs.sleepanarchy.com/projects/hkredmine

A github mirror is available if you prefer pull requests:
https://github.com/prikhi/hkredmine

Use `cabal` to build the documentation:

    cabal haddock


[vim-redminewiki]: https://github.com/s3rvac/vim-syntax-redminewiki/
