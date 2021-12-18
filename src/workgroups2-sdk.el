;;; workgroups2-sdk.el --- sdk of workgroups2  -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)

(defvar wg-special-buffer-serdes-functions
  '(wg-serialize-comint-buffer)
  "Functions providing serialization/deserialization for complex buffers.

Use `wg-support' macro and this variable will be filled
automatically.

An entry should be either a function symbol or a lambda, and should
accept a single Emacs buffer object as an argument.

When a buffer is to be serialized, it is passed to each of these
functions in turn until one returns non-nil, or the list ends.  A
return value of nil indicates that the function can't handle
buffers of that type.  A non-nil return value indicates that it
can.  The first non-nil return value becomes the buffer's special
serialization data.  The return value should be a cons, with a
deserialization function (a function symbol or a lambda) as the car,
and any other serialization data as the cdr.

When it comes time to deserialize the buffer, the deserialization
function (the car of the cons mentioned above) is passed the
wg-buf object, from which it should restore the buffer.  The
special serialization data itself can be accessed
with (cdr (wg-buf-special-data <wg-buf>)).  The deserialization
function must return the restored Emacs buffer object.

See the definitions of the functions in this list for examples of
how to write your own.")

(provide 'workgroups2-sdk)
;;; workgroups2-sdk.el ends here
