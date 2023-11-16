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

    # function
    <>

    # lookup
    $local
    @global

    # assign
    "value" |$local
    "value" |@global

    # unassign
    %$local
    %@global

    # invoke
    (@function "argument 1" "argument ..." "argument n")

    # branch
    if "test" "pass"
    if "test" "pass" else "fail"

    # loop
    while "test" "body"

    # catch
    catch "failable"

    # throw
    throw "error"

    # argument
    argument "index"

    # group
    \
      # expressions
    /

    # built-ins
    @add @subtract @multiply @divide @modulo @increment @decrement @and @or @not
    @precedes @succeeds @equals @measure @slice @merge @read @write @delete
    @query @exit @serialize @deserialize @classify @evaluate
