# editcmacro - Edit C macro in a separate buffer

*Author:* Anders Lindgren<br>
*Version:* 0.0.1<br>
*URL:* [https://github.com/Lindydancer/editcmacro](https://github.com/Lindydancer/editcmacro)<br>

This package lets you edit a C macro in a separate buffer, without
end of line backslashes.

In C, macros are defined using the `#define` construct.
Technically, a C macro expand to a single line of tokens.  However,
for readability, a macro definition can be split into multiple
lines by using end of line backslashes.  For example:

    #define FOREVER      \
      for (;;)           \
      {                  \
      }

Unfortunately, the backslashes makes the code hard to edit.  This
package lets you edit a macro, without the end of line backslashes,
in a separate buffer.  When done, the macro is written back to the
original buffer, with backslashes.

If you are familiar with source block editing in Org mode, you will
feel right at home with this package.

## Keys

Press <kbd>C-c </kbd>' to edit a C macro in a separate buffer, without the
backslashes.  When done editing, you can press <kbd>C-c </kbd>' again to
write the macro back into the original buffer, with backslashes.

You can discard your changes and kill the temporary buffer by
pressing <kbd>C-c C-k</kbd>.

## Installation

Enable the minor mode `editcmacro-mode` in C-like buffers.
Typically, you can add the following to an appropriate init file:

        (add-hook 'c-mode-hook   #'editcmacro-mode)
        (add-hook 'c++-mode-hook #'editcmacro-mode)


---
Converted from `editcmacro.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
