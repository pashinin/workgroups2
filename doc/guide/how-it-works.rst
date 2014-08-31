===================
 How does it work?
===================

.. note::

   The most important part to understand is :doc:`Data structures
   <data-structures>`. After that it's easy to write code in other
   parts.


.. _serialize:

Serialization / Deserialization of objects
==========================================

In Emacs we have many types of objects like:

- ``#<buffer tests.el>``
- ``#<marker at 3427 in tests.el>``
- simple ``"string"``
- integers 123
- ... and other

And we have to represent them as text to save. This is done using
``wg-pickel`` and functions defined in this var:

.. code-block:: cl

   (defvar wg-pickel-object-serializers
     '((integer    . identity)
       (float      . identity)
       (string     . identity)
       (symbol     . wg-pickel-symbol-serializer)
       (cons       . wg-pickel-cons-serializer)
       (vector     . wg-pickel-vector-serializer)
       (hash-table . wg-pickel-hash-table-serializer)
       (buffer     . wg-pickel-buffer-serializer)
       (marker     . wg-pickel-marker-serializer))
     "Alist mapping types to object serialization functions.")

So when you meet an object that cannot be represented as text - you:

1. Add it's type in this variable
#. Write mentioned "serializer" function itself

For example for "buffer" objects:

.. code-block:: cl

   (defun wg-pickel-buffer-serializer (buffer)
     "Return BUFFER's UID in workgroups buffer list."
     (list 'b (wg-add-buffer-to-buf-list buffer)))

'b - is just a marker that will tell to run
``wg-pickel-deserialize-buffer`` when restoring a buffer.

Last element is buffer UID and it is enough to restore the buffer with
``(wg-restore-buffer (wg-find-buf-by-uid uid))``

Loading a session file
======================

It is done in ``wg-open-session``. First you read a
:ref:`Session object <wg-session>` from file in this line:

.. code-block:: cl

   (let ((session (read (f-read-text filename))))
     ...

Then you just switch to 1 of the saved workgroups in this object
according to settings.


Saving session
==============

Writing objects to file is done in... (function stack):

    wg-write-sexp-to-file
        wg-pickel-all-session-parameters
            wg-pickel-workgroup-parameters
                wg-pickel <-- main function

So the main function to transform Lisp objects to strings is ``wg-pickel``.

Switching workgroups
====================
