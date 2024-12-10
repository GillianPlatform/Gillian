Installation
============

Linux / macOS
-------------

.. admonition:: Developing with opam

    Gillian uses opam for dependency management. We advise using a local opam switch, so your global environment remains untouched.
    It should be perfectly safe (and recommended) to install the development environment directly on your machine.

#. Install prerequisites
   
   On Debian or Ubuntu, run:

   .. code-block:: bash

     sudo apt install git curl npm build-essential pkg-config \
         m4 python3-distutils python3-apt z3

   On macOS, make sure you have the XCode command line tools installed:

   .. code-block:: bash

     xcode-select --install

   ...then make sure your development tools are up to date.

#. Install opam

   .. code-block:: bash

    sudo apt install opam

#. Clone the source repository

   .. note::

    If you are evaluating Gillian as an artifact, please use the source code provided to you as part of the artifact submission. 

   .. code-block:: bash

    git clone https://github.com/GillianPlatform/Gillian.git

#. Prepare dependencies and build Gillian

   .. code-block:: bash

    cd Gillian
    make init-dev
    eval $(opam env)
    dune build

Windows
-------

Gillian is not supported on (native) Windows, but works fine through WSL (more specifically, default Ubuntu on WSL2).

Docker
------

You can build a docker image from the source code:

.. code-block:: bash

    git clone https://github.com/GillianPlatform/Gillian.git
    cd Gillian
    docker build -t gillian .

Once built, run Gillian with

.. code-block:: text

    docker build --target test -t gillian

This will start the container and give you access through a ``zsh`` shell.

Inside the container, you'll find:

* The Gillian repository in ``/app/Gillian``
* Our fork of ``Test262`` in ``/app/test262``
* Our fork of ``Collections-C`` in ``/app/collections-c``

Testing your setup
------------------

After installing and building (or entering the docker container), try running the following tests.

Gillian-JS
^^^^^^^^^^

.. code-block:: text

    dune exec -- gillian-js verify Gillian-JS/Examples/JaVerT/BST.js

..

    Expected output

    .. code-block:: text

        Parsing and compiling...
        Preprocessing...
        Obtaining specs to verify...
        Obtaining lemmas to verify...
        Obtained 5 symbolic tests in total
        Running symbolic tests: 0.381137
        Verifying one spec of procedure insert... s s s s Success
        Verifying one spec of procedure remove... s s s s s s s s s Success
        Verifying one spec of procedure findMin... s s Success
        Verifying one spec of procedure find... s s s s Success
        Verifying one spec of procedure makeNode... s Success
        All specs succeeded: 2.935246

Gillian-C
^^^^^^^^^

.. code-block:: text

    dune exec -- gillian-c bulk-exec Gillian-C/examples/concrete

..

    Expected output

    .. code-block:: text


        Registering tests...
        Testing Running 1 test suites.
        This run has ID `DACA1B06-6CB2-474C-AC1B-3C24CC108C2C`.
        [OK]                _          0   Gillian-C_examples_concrete_bst_c.
        [OK]                _          1   Gillian-C_examples_concrete_kvmap_c.
        [OK]                _          2   Gillian-C_examples_concrete_priQ_c.
        [OK]                _          3   Gillian-C_examples_concrete_sort_c.
        [OK]                _          4   Gillian-C_examples_concrete_sll_c.
        [OK]                _          5   Gillian-C_examples_concrete_dll_c.
        The full test results are available in `/Users/...`.
        Test Successful in 1.000s. 6 tests run.
