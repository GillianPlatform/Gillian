SSFT 2025
=========

Installation
------------

Before you start, please install the following:

* `Docker <https://docs.docker.com/desktop/>`_
* `VSCode <https://code.visualstudio.com/>`_
* VSCode's `Dev Containers extension <https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers>`_

Once that's done, follow these steps:

#. Clone the lab repository.

   .. code-block:: bash

        git clone https://github.com/GillianPlatform/gillian-lab.git  --depth 1 --branch SSFT25
  
   .. warning::

      This tag may not yet be present; if so, it will become available nearer to the lab date.

#. If necessary, select *Yes, I trust the authors*.

   .. image:: /_static/img/ssft/trust_authors.png

#. Launch the dev container by clicking *"Reopen in Container"* on the popup.

   .. image:: /_static/img/ssft/reopen_in_container.png

   * If you cannot find the popup, open the Command Palette (F1 or Ctrl+Shift+P by default) and select *"Dev Containers: Open Folder in Container..."* and select the :code:`gillian-lab` folder.

     .. image:: /_static/img/ssft/open_folder_in_container.png

#. Gillian's Docker image should then be automatically downloaded, and VSCode will use it for your development environment.