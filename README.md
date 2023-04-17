## Overview
A small utility that adds some useful functionality for working with Subversion repositories.

    usage: sv <command> [<args>]
           sv [-v | --version]
           sv [-h | --help | help]
    
    The available commands are:
    
      log       Display formatted log entries
      branch    Display current branch or list branches
      show      Show the details of a given revision
      filerevs  Display commit revisions of files across tags and branches
      stash     Stash away changes to a dirty working copy
      bisect    Use binary search to find the commit that introduced a bug
      ignore    Write ignore properties to stdout in .gitignore format
    
    For help about a particular command type 'sv help <command>'
    Commands and options may be abbreviated to their shortest unique prefix

    
## License


    Copyright (c) 2023 Curt Sellmer
    
    Permission is hereby granted, free of charge, to any person obtaining
    a copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:
    
    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.
    
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
