.. _contribute:

==========
Contribute
==========

Start using the git repo
========================

1. Remove ``workgroups2`` package you installed from Melpa
#. Clone `the repo from Github
   <https://github.com/pashinin/workgroups2>`_ (or make a submodule in
   your .emacs repo)

   .. code-block:: bash

       cd ~/some/path
       git clone https://github.com/pashinin/workgroups2.git

   .. code-block:: bash

       cd ~/.emacs.d
       git submodule add git://github.com/pashinin/workgroups2.git workgroups2

#. Add repo's ``src/`` directory to ``load-path`` and then use a simple
   ``(require ...)``

   .. code-block:: cl

       (add-to-list 'load-path "~/.emacs.d/workgroups2/src")
       (require 'workgroups2)

       ;; your existing settings...
       (workgroups-mode 1)


Then to make changes I think you need to understand :doc:`How this
extension work <how-it-works>`.

Modify something
================
