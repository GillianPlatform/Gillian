Developing Gillian
******************

Executing a command in the test environment
===========================================

``dune`` lets you execute a command in an execution environment where all built binaries and installed files are correctly added to your path. In particular, Gillian-JS, Gillian-C and wisl require to be executed in this environment to work correctly and find their runtime files.

To run any command under this environment:

.. code-block:: bash

    dune exec -- <command>

To access the different manuals, you can use:

.. code-block:: bash

    dune exec -- gillian-js --help
    dune exec -- gillian-c --help
    dune exec -- wisl --help

You can get even more precise help about specific commands; for example:

.. code-block:: bash

    dune exec -- gillian-js verify --help

Rebuilding after modifications
==============================
Since ``dune`` is our build system, running the ``dune build`` command without any arguments will rebuild the project after modification.

You can automatically rebuild on changes by running:

.. code-block:: bash

    dune build --watch

Code style
==========

You can automatically format the code by running:

.. code-block:: bash

    dune fmt

It's recommended that you install the provided git hooks (by running ``githooks/install.ml``) to enforce code style.

We advise using `the _intf trick <https://www.craigfe.io/posts/the-intf-trick>`_ where appropriate to avoid code duplication.

When you have a function that returns an ``option``, and an extension-raising equivalent, you should name them ``foo`` and ``foo_exn`` respectively, rather than ``foo_opt`` and ``foo``.

A rule of thumb for choosing whether to make a function argument labeled: is the argument's purpose obvious from its type and the name of the function? If not, it should be labeled.

.. code-block:: ocaml

   (* This doesn't need labels *)
   val use_query: database -> query -> query_result

   (* Wait, what's this weird first argument? Better make it labeled *)
   val use_query: (string -> string) -> database -> query -> query_result

   (* These arguments have the same type - how do I know which one is which? *)
   val copy_content: channel -> channel -> unit

   (* This is better *)
   val copy_content : in:channel -> out:channel -> unit

Documentation
=============

Gillian's documentation is built with :code:`sphinx`, with an API reference generated using :code:`odoc`.

Building Documentation
----------------------

#. Install the prerequisites

   .. code-block:: bash

    opam install odoc
    pip install sphinx furo

#. Build the documentation

   .. code-block:: bash

    make docs

After building, you'll find the sphinx documentation at ``_docs/sphinx``, and the API reference at ``_docs/odoc``.

Rebuild on change
^^^^^^^^^^^^^^^^^

If you want to automatically rebuild on changes, take these additional steps.

#. Install (more) prerequisites:

   .. code-block:: bash

    apt install inotify-tools
    pip install sphinx-autobuild

#. Watch for changes

   .. code-block:: bash

    make docs-watch
