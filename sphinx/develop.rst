Developing Gillian
******************

Executing a command in the test environment
===========================================

``esy`` lets you execute a command in a `test environment <https://esy.sh/docs/en/environment.html#test-environment>`_ where all built binaries and installed files are correctly added to your path. In particular, Gillian-JS, Gillian-C and wisl export specific environment variables that allow them to properly find their respective runtime files.

To run any command under this environment:

.. code-block:: bash

    esy x <command>

To access the different manuals, you can use:

.. code-block:: bash

    esy x gillian-js --help
    esy x gillian-c --help
    esy x wisl --help

You can get even more precise help about specific commands; for example:

.. code-block:: bash

    esy x gillian-js verify --help

Rebuilding after modifications
==============================
Since ``esy`` is our build system, running the ``esy`` command without any arguments will rebuild the project after modification.

You can automatically rebuild on changes by running:

.. code-block:: bash

    esy watch

Code style
==========

You can automatically format the code by running:

.. code-block:: bash

    esy format

It's recommended that you install the provided git hooks (by running ``githooks/install.ml``) to enforce code style.

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

    esy docs

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

    esy docs:watch
