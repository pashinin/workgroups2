.. _data_structures:

=================
 Data structures
=================

Let's look at ~/.workgroups file:

.. code-block:: cl

    [cl-struct-wg-session "0G3A08BU1E35GEA0-18GPMY" ...
      ([cl-struct-wg-workgroup "0G3A08D8APKR11T4-1C1G10" "Tasks" ...
         [cl-struct-wg-wconfig "0GGI0JY4B3HD0WEO-86RSR3" ...
            [cl-struct-wg-wtree ...
               ([cl-struct-wg-win ...
                [cl-struct-wg-win ...


General info
============

All these structs (better to say functions to work with these objects)
are created with :keyword:`wg-defstruct` macro. For example for:

.. code-block:: cl

    (wg-defstruct wg session
      (uid (wg-generate-uid))
      (field-2)
      ...

wg-defstruct creates functions like ``wg-make-session``,
``wg-copy-session`` and ``wg-session-...``, (to manipulate
structures). Then you will have ``(wg-session-field-2 obj)`` and other
defined fields to read properties of this object.

To set values ``(setf ...)`` function is used.

Example for current session object:

.. code-block:: cl

     ;; Read
     (wg-session-file-name (wg-current-session))         ; Get a filename of current session
     (wg-workgroup-parameters (wg-current-workgroup))    ; Get workgroup parameters

     ;; Write (used just before saving session to file)
     (setf (wg-session-file-name (wg-current-session)) filename)    ; Set session filename
     (setf (wg-session-version (wg-current-session)) wg-version)    ; Write workgroups version

.. warning::

   Changing these defstructs themselves may break everyone's session
   files. That's why many of them have :ref:`parameters` field. This one
   is exactly for extending saved information.


How to work with these structures?
----------------------------------

Ok, we define a session structure, and you can get the
value of it with (wg-current-session)

wg-defstruct creates functions like wg-session-..., wg-make-session (to
manipulate structures). So if you have (wg-defstruct wg session ...) -
then you have wg-session-file-name and other defined fields.

.. _wg-session:

Session
=======

The ``session`` object is the top level "class" that has workgroups in it.

.. code-block:: cl

    (wg-defstruct wg session
      (uid (wg-generate-uid))
      (name)
      (modified)
      (parameters)
      (file-name)
      (version wg-version)
      (workgroup-list)
      (buf-list))

.. note::

   List of buffers is a common pool for all workgroups. When you open a
   file (doesn't matter in which workgroup) - the corresponding
   :ref:`wg-buffer` object will be added in
   ``wg-session-buf-list``

.. _wg-workgroup:

Workgroup
=========

``workgroups`` contain frame states (that includes window configuration)

.. code-block:: cl

    (wg-defstruct wg workgroup
      (uid (wg-generate-uid))
      (name)
      (modified)
      (parameters)
      (base-wconfig)
      (selected-frame-wconfig)
      (saved-wconfigs)
      (strong-buf-uids)
      (weak-buf-uids))

.. _wg-wconfig:

Wconfig
=======

.. code-block:: cl

    (wg-defstruct wg wconfig
      (uid (wg-generate-uid))
      (name)
      (parameters)
      (left)
      (top)
      (width)
      (height)
      (vertical-scroll-bars)
      (scroll-bar-width)
      (wtree))

What's the difference between wconfig and wtree? Well a workgroup can
have several wconfigs (buffer layouts). But to keep it simple let's say
each workgroup has only 1 wconfig.

wconfig = wtree + additional parameters

.. _wg-wtree:

Wtree
=====

.. code-block:: cl

     (wg-defstruct wg wtree
       (uid)
       (dir)
       (edges)
       (wlist))


.. _wg-win:

Win
===

.. code-block:: cl

     (wg-defstruct wg win
       (uid)
       (parameters)
       (edges)
       (point)
       (start)
       (hscroll)
       (dedicated)
       (selected)
       (minibuffer-scroll)
       (buf-uid))


.. _wg-buffer:

Buffer
======


.. _parameters:

Parameters
==========

Changing main structures may lead to huge problems in
compatibility. That's why there are parameters for :ref:`wg-session`,
:ref:`wg-workgroup`, :ref:`wg-wconfig` and :ref:`wg-win`
objects. They allow you to save your custom data.

For example to set (key, value) pair for current workgroup:

.. code-block:: cl

     ;; Write (key, value)
     (wg-set-workgroup-parameter
      'ecb                                            ; name
      (and (boundp 'ecb-minor-mode) ecb-minor-mode))  ; value

Usually these functions are called like:

.. code-block:: cl

     wg-<object>-parameter          ; to read
     wg-set-<object>-parameter      ; to set
     wg-remove-<object>-parameter   ; to remove parameter

For session: wg-session-parameter, wg-set-session-parameter, wg-remove-session-parameter
For workgroup: wg-workgroup-parameter, wg-set-workgroup-parameter, wg-remove-workgroup-parameter
