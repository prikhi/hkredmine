HKRedmine
==========

A Redmine CLI client written in Haskell.

This is not a fully featured API client, but one built for a specific workflow.


Quickstart
-----------

1. Clone this repository: `git clone http://bugs.sleepanarchy.com/hkredmine.git`
2. Install hkredmine: `cd hkredmine; cabal install -j`
3. Add your Redmine URL to the config file: `echo 'url=http://redmine.yourdomain.com/' > ~/.hkredminerc`
4. And your API key: `echo 'apikey="longalphanumericstring"' >> ~/.hkredminerc`
5. List available projects: `hkredmine projects`
6. List the issues of a project: `hkredmine issues -p <project_identifier>`
7. List the next version of a project: `hkredmine nextversion <project_identifier>`
8. Start tracking time on an issue: `hkredmine startwork <issue_id>`
9. Stop tracking time and create a new time entry: `hkredmine stopwork`
10. Learn how to use everything else: `hkredmine --help`


Installation
-------------

Build and install:

    cabal install -j

Contributers should build the documentation:

    cabal haddock


Configuration
--------------

`hkredmine` expects a configuration file at `~/.hkredminerc`. It should follow
the basic INI `key=value` style. Only two values are required, `url` and
`apikey`:

    # ~/.hkredminerc
    url = http://redmine.mydomain.com/
    apikey = "longalphanumericapikey"

Be sure to enclose the API key in quotes and to include the trailing slash in
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
        fields       Print the available field values(Statuses, Priorities, etc.).
    Projects:
        project      Print the details of a Project.
        projects     Print all Projects.
    Issues:
        issue        Print the details of an Issue.
        issues       Filter and Print Issues.
        newissue     Create a New Issue.
        watch        Watch an Issue.
        unwatch      Unwatch an Issue.
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

Vim user's may be interested in [this syntax plugin](vim-redminewiki). We
follow the `*.redmine` extension convention.


Updating Issues
----------------

You can modify existing Issues using the `update` command:

    hkredmine update 77 --priority High -t Bug --category=UI -n "Fix Category"

You can pass the `-e` flag to edit the description in your `$EDITOR`:

    hkredmine update 31 --priority Immediate -s "A NU Start" -e


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
the due date to today(but only if it was not set).


Workflow
---------

At v1.0 you should be able to do the following:

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
    hkredmine stopwork --activity=Support --comment="Help Y with X."
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
    hkredmine stopwork --activity=Development comment="Fixed thing, waiting on confirmation."
    hkredmine watch 154

Still need to implement:
* delete

Maybe eventually:
* newproject
* newcategory, newstatus, etc.
* newaccount
* wiki
* editwiki

Open tickets with any requests.


Contribute
-----------

Submit bugs and patches: http://bugs.sleepanarchy.com/projects/hkredmine

Or on github: https://github.com/prikhi/hkredmine


[vim-redminewiki]: https://github.com/s3rvac/vim-syntax-redminewiki/
