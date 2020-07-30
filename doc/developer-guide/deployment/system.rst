

System integration
==================

Zotonic is coming with a tool helping for its integration into your system.

This is a escript called `zotonic_system` under `apps/zotonic_system/bin/`.

This script is embedding some packages including Zotonic Django templates and uses Zotonic configurations.

Zotonic configurations are YAML files output by `zotonic` script in `apps/zotonic_launcher/bin`.

.. important::
	
  Trust does not exclude control. Always verify what `zotonic_system` propose you. 
  `zotonic_system` is an helper tool but you are responsible of your acts. 

Usage
-----

.. code-block:: bash

  Usage: zotonic_system [-h] [-v <verbose>] [-V] [-L] [-C] [-A] [-T]
                        [-c <category>] [-a <app>] [-t <target>]
                        [-p <path>] [-n]
                                              
    -h, --help        This help
    -v, --verbose     Verbosity - [0..9] 0 mini, 9 maxi
    -V, --version     Version
    -L, --list        List all (category/app/target)
    -C, --categories  List all categories
    -A, --apps        List all applications for a category (require -c)
    -T, --targets     List all targets for category and application (require 
                      -c, -a).
    -c, --category    Category
    -a, --app         Application
    -t, --target      Target (require -c, -a)
    -p, --path        Path to a category/app/target (see -L output)
    -n, --noop        Do not read stdin nor evaluate template


`zotonic_system` packages are sorted by categories :

.. code-block:: bash

  $> zotonic_system -C
  proxy
  shell

Each `Category` includes one or more `Application`. Each `Application` include one or more `Target`.
Package names are made of a category, an application and a target, joined with a slash like pathes, for instance `shell/bash/completion`.

A path to a package can be given to the tool, either directly with `-p` option, or splitted into `-c` `-a` `-t` options.

Option `-n` (noop) allow to see the package infos and template without evaluation, and do not expect any configuration in standard input (stdin).

.. code-block:: bash

  $> zotonic_system -n -p shell/bash/completion

is same than

.. code-block:: bash

  $> zotonic_system -n -c shell -a bash -t completion

**It is recommanded to use this option -n first, before using a package**, as tag `Requires` will tell you what configurations are required, among Zotonic ones :

- zotonic
- erlang
- site

and among the ones automatically added by `zotonic_system` itself :

- vars : Environment variables
- os   : OS informations


In order to use a package in real, the required YAML configurations need to be read by `zotonic_system` in standard input. 
Some packages, however, have no requirements and therefore there is no need to evaluate a template.

In such case, `-n` option can be used :

.. code-block:: bash

  $> zotonic_system -n -p shell/bash/completion

or a simple echo in standard input will do the trick :

.. code-block:: bash

  $> echo | zotonic_system -p shell/bash/completion

For packages requiring some configurations for template, do :

.. code-block:: bash

  $> zotonic config all | zotonic_system -p proxy/nginx/conf

.. note::
  
  Using `zotonic config all` is the simpler way to be sure to have all the required configuration. But using a concatenation of only the required configuration is possible too. In all case `zotonic_system` will check that requirements are met.


Output
^^^^^^

Package output is splitted between standard error (for infos) and standard output (for payload), so that payload can be redirected into a file.

.. code-block:: text

  $> zotonic config all | zotonic_system -p category/app/target
  Package: category/app/target                <-- Name of package
  Version: x.y.z                              <-- Version in semver format
  Provides: /path/to/usual/target             <-- Path to usual target file (may differ depending OSes)
  Maintainer: John Doe <john@doe.com>         <-- Maintainer of package (github account URL allowed)
  Homepage: http://application/doc/           <-- URL of application (to know exactly to what application it is refered)
  Description: Short description              
                                              
    Some more detailed description            <-- Potential other target file locations may be recorded here
    or installation tips.                     
                                              
  Requires: zotonic, vars                     <-- Configurations requirement
  ---8<--- FileBasename ---------             <-- Payload delimiter
      PAYLOAD CONTENT                         <== Payload on stdout (can be redirected into target file)
  ---8<--------------------------             <-- Payload delimiter

If payload is OK for you, you can redirect it in the target file (or another temporary file if some modification are necessary).
Only infos are shown on stderr in such case.

.. code-block:: bash

  $> zotonic config all | zotonic_system -p category/app/target > /path/to/usual/target
  Package: category/app/target                
  Version: x.y.z                              
  Provides: /path/to/usual/target             
  Maintainer: John Doe <john@doe.com>         
  Homepage: http://application/doc/           
  Description: Short description              
                                              
    Some more detailed description            
    or installation tips.                     
                                              
  Requires: zotonic, vars                     
  ---8<--- FileBasename --------              
                                              
  ---8<--------------------------             











