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

.. code-block:: bash

    docker run -it gillian

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

.. code-block:: bash

    dune exec -- gillian-js verify Gillian-JS/Examples/JaVerT/BST.js -l disabled

..

    Expected output

    .. code-block:: bash

        Obtaining specs to verify.
        Obtained 5 symbolic tests
        Running symbolic tests: 0.080211
        Verifying one spec of procedure makeNode... Success
        Verifying one spec of procedure find... Success
        Verifying one spec of procedure findMin... Success
        Verifying one spec of procedure remove... Success
        Verifying one spec of procedure insert... Success
        All specs succeeded: [Time]

Gillian-C
^^^^^^^^^

.. code-block:: bash

    dune exec -- gillian-c bulk-exec Gillian-C/examples/concrete

..

    Expected output

    .. code-block:: bash

        Registering tests...
        Running 1 test suite
        PASS  .

        Test Suites: 0 failed, 1 passed, 1 total
        Tests:       0 failed, 6 passed, 6 total
        Time:        [Time]
