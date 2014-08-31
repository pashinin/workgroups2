==========
 Problems
==========

.. contents:: You do have problems, right?
   :local:


Buffer was not restored
=======================

I doubt it was a simple file buffer (or `report a bug
<https://github.com/pashinin/workgroups2/issues/new>`_).

.. warning::

   You know ``major-mode`` you use **better than me**. So please if you
   ask to add support for any particular ``major-mode`` - write how you
   install, configure and run yours.

Such complex buffers are called "special buffers". A simple way to
restore them is to use ``wg-support`` macro:

.. code-block:: cl

     (wg-support 'inferior-emacs-lisp-mode 'ielm
       `((deserialize . ,(lambda (buffer vars)
                           (ielm) (get-buffer "*ielm*")))))

To understand how this works - see :doc:`special-buffers`

Restored, but not the way I want
--------------------------------

`Discuss it <https://github.com/pashinin/workgroups2/issues/new>`_
