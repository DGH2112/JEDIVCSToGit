JEDI VCS to GIT
===============

Author:   David Hoyle

Version:  1.0

Date:     17 Feb 2018

Web Page: TBA

## Overview

The purpose of this application is to migrate a JEDI VCS repository to Git repository. This is written in Embarcadero RAD Studio 10.2 Tokyo however you should be able to compile the application with any version of RAD Studio that supports FireDAC as this is used to communicate with the JEDI VCS database.

## Use

The first thing you need to do is updated the `FireDACConfig.ini` file with the appropriate FireDAC connection settings for your JEDI VCS database. This file then needs to be passed to the application as the first parameter.

When the application is up there are a number of settings to be maked as follows:

 * Provide a new EMPTY git repository path (you must have run GIT INIT);
 * Provide the path of the existing JEDI VCS repository - this is used only to determine relative paths - not files are copied to or from the repository (they are all retrieved from the database);
 * Provide a SQL pattern to match the single or multiple projects to extract;
 * Press the Revisions button to start.

During the process a number of prompts will require handling as follows:

 * You are asked to confirm the name of the directories to be created (Git is more sensitive to path and filename cases that it should be);
 * If you have references to modules outside you repository, you will be based where you want them to be placed in the Git repository (this was my biggest issue due to bad planning).

As the application is processing the revisions and committing them the Git commands and their messages are output to a listview at the bottom which is saved to the file `Git.log` in the new Git repository root.

If any error occur (which hopefully they don't), a dialgoue will appear with the Git error and the option to **Abort** or **Ignore**.
