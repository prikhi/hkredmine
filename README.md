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

Run `hkredmine` for a listing of available commands and options.

The client is still in the initial development stages, but the proposed
workflow is similar to the following:

    # Wake up, pick a tracker:
    hkr use sleepanarchy

    # What am I watching?
    hkr watched

    # Hmm, nothing interesting. What projects could I work on?
    hkr projects

    # accounting-app seems cool, lets see the next milestone:
    hkr nextversion accounting-app --sort=priority --status=New

    # OK I found an interesting issue, let's see the details
    hkr details 154

    # Looks good, let's mark as "In Progress" and start tracking our
    # time.
    hkr startwork 154

    # Something came up, we need to stop work and add a time entry
    hkr stopwork --type=Development --comment="Tested/Implemented X."
    # And switch trackers and make a new issue
    hkr use acorn
    hkr newissue --project=it-computers

    # Our editor should open up to let us edit the subject, description,
    # priority, etc. The issue should be created and the details displayed.

    # We'll start on the issue
    hkr startwork 280

    # We notice something out of scope and make a new issue
    hkr newissue --project=it-computers
    # We'll watch the issue so we remember to come back to it
    hkr watch 281

    # We've finished, let's log our time and close the issue
    hkr stopwork --type=Support --comment="Help Y with X."
    hkr update 280 --status=Closed --due="$(date -I)"

    # Going back to the old issue
    hkr use sleepanarchy
    hkr startwork 154

    # Let's take a little break
    hkr pause

    # We come back
    hkr resume
    # Forgot what we were working on
    hkr status
    # Fix the issue and mark as resolved.
    hkr stopwork --type=Development comment="Fixed thing, waiting on confirmation."
    hkr watch 154
