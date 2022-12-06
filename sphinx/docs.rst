Documentation
=============

Gillian's documentation is built with :code:`sphinx`, with an API reference generated using :code:`odoc`.

Building Documentation
----------------------

First, install the necessary prerequisites: ::

  opam install odoc
  pip install sphinx furo

Then, to build: ::

  esy docs

Rebuild on change
^^^^^^^^^^^^^^^^^

If you want to automatically rebuild on changes, you'll also need these: ::

  apt install inotify-tools
  pip install sphinx-autobuild

Then, to watch for changes: ::

  esy docs:watch