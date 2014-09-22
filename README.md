HKRedmine
==========

A Redmine CLI client written in Haskell.

This is not a fully featured API client, but one built for a specific workflow.

Build the documentation:

    cabal haddock

Build and install:

    cabal install -j

Configuration
--------------

`hkredmine` expects a configuration file at `~/.hkredminerc`. It should follow
the basic `key=value` INI style. Only two values are required, `url` and
`apikey`:

    # ~/.hkredminerc
    url = http://redmine.mydomain.com/
    apikey = "longalphanumericapikey"

Be sure to enclose the apikey in quotes and to include the trailing slash in
the URL.

Multiple accounts may be used by putting keys under `[AccountName]` headers:

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


Usage
------

Run `hkredmine --help` for a listing of available commands and options.
Run `hkredmine <command> --help` for additional help and avaialble parameters
for a specific command.

You can generate bash completion files by passing the ``bash`` argument to the
``help`` flag:

    hkredmine --help=bash > hkredmine.comp
    source hkredmine.comp

The following commands are currently available:

    hkredmine [COMMAND] ... [OPTIONS]

    Commands:
        use          Switch to a different redmine account.
        [status]     Print the current Account, Issue and Tracked Time.
        projects     Print all Projects.
        project      Print the details of a Project.
    Issues:
        issues       Print all Issues of a Project.
        watch        Watch an Issue.
        unwatch      Unwatch an Issue.
    Time Tracking:
        startwork    Start tracking time for an Issue.
        stopwork     Stop time tracking and submit a time entry.
        pause        Pause time tracking.
        resume       Resume time tracking.
        abort        Abort time tracking.
    Versions:
        versions     Print all of a Project's Versions.
        version      Print the details of a Version.
        nextversion  Print the next Version due for a Project.


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

Workflow
---------

The client is not yet complete, but the proposed workflow is similar to the
following:

    # Wake up, pick a tracker:
    hkredmine use sleepanarchy

    # What am I watching?
    hkredmine watched

    # Hmm, nothing interesting. What projects could I work on?
    hkredmine projects

    # accounting-app seems cool, lets see the next milestone:
    hkredmine nextversion accounting-app --sort=priority --status=New

    # OK I found an interesting issue, let's see the details
    hkredmine details 154

    # Looks good, let's mark as "In Progress" and start tracking our
    # time.
    hkredmine startwork 154

    # Something came up, we need to stop work and add a time entry
    hkredmine stopwork --type=Development --comment="Tested/Implemented X."
    # And switch trackers and make a new issue
    hkredmine use acorn
    hkredmine newissue --project=it-computers

    # Our editor should open up to let us edit the subject, description,
    # priority, etc. The issue should be created and the details displayed.

    # We'll start on the issue
    hkredmine startwork 280

    # We notice something out of scope and make a new issue
    hkredmine newissue --project=it-computers
    # We'll watch the issue so we remember to come back to it
    hkredmine watch 281

    # We've finished, let's log our time and close the issue
    hkredmine stopwork --type=Support --comment="Help Y with X."
    hkredmine update 280 --status=Closed --due="$(date -I)"

    # Going back to the old issue
    hkredmine use sleepanarchy
    hkredmine startwork 154

    # Let's take a little break
    hkredmine pause

    # We come back
    hkredmine resume
    # Forgot what we were working on
    hkredmine status
    # Fix the issue and mark as resolved.
    hkredmine stopwork --type=Development comment="Fixed thing, waiting on confirmation."
    hkredmine watch 154
