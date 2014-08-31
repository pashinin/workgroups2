==============
 Installation
==============

Very simple with recent Emacs. Make sure you have these lines:

.. code-block:: cl

   (require 'package)
   (add-to-list 'package-archives
                '("melpa" . "http://melpa.milkbox.net/packages/") t)

before

.. code-block:: cl

   (package-initialize)

Then type :kbd:`M-x list-packages`

.. image:: /_static/list-packages.png
   :align: center

mark ``workgroups2`` with :kbd:`i` and install with :kbd:`x`.

Then :doc:`Configure and activate workgroups-mode <usage>`.
