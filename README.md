# lngrd

> One programmer's quest for their eternal programming language.

## Language

    # number
    0
    100
    -100
    +100

    # string
    ""
    "plain"
    "\\ \" \n \r \t"
    "\x00 \xff"

    # lookup
    @global

    # assign
    "value" |@variable

    # unassign
    %@variable

    # invoke
    (@function "arg 1" "arg ..." "arg n")

    # branch
    if "test" "pass"

    # loop
    while "test" "body"

    # catch
    catch "failable"

    # throw
    throw "error"

    # group
    \
      # expressions
    /

    # built-ins
    @add @subtract @multiply @divide @modulo @increment @decrement @and @or @not
    @precedes @succeeds @equals @length @slice @merge @write @query @type
