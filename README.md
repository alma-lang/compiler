# alma

The alma compiler.

alma is an ML language that borrows from Elm, F# and OCaml.

It is a work in progress, right now it compiles to JS, and is very much
incomplete.

## Roadmap

### TODO

- Exhaustiveness checks for pattern matching and compilation with decision trees
- Cross file module resolution
- `|>` pipes
- Emit warnings for type signature only definitions
- Flesh out the standard library
- String interpolation
- Backpassing or await or do notation. Something to ease callback hell
- Type inference: Split global type ids into file local and global
- Error message improvement pass
- Multi-threaded compilation

### Done

- Modules and in-file modules
- Let bindings
- Lambdas
- Operators (binary, unary, math and logic)
- Functions
- Union types
- Extensible records
- Type signature only definitions
- FFI, external type and functions
- JS backend
- Pattern matching
  - Parenthesized pattern
  - Constructor pattern
  - String pattern
  - Float pattern
  - Named aliases
  - Or patterns
  - Record pattern
  - `when` expression with branches
  - if conditions in branches
  - multiple matches comma separated

### Future research

- Linear types for mutable variables
- Multiple backends (e.g. Lua, Php, C)

### Things I'll probably never have time to do

- Language server
- Formatter
- Test runner
- Package manager

## Example

```elm
module Demo exposing (main)

import Demo.Vec2 as Vec2 exposing (Vec2)
import Demo.User as User exposing (User)

external log : String -> {}
external now : {} -> Float

main _ =
    let _ = log "Hi!"
    let v = Vec2.zero
    let v = Vec2.moveRight 2.0 v
    let v = Vec2.moveLeft 1.0 v
    let _ = log (printX v)

    let u = User.make "Bob" 42
    let _ = log (User.loginMessage u)
    let u = User.ban u
    let _ = log (User.loginMessage u)
    {}

printX : { a | x : Float } -> String
printX { x } = "x = " + Float.toString x

module Demo.Vec2 exposing (Vec2, zero, moveRight, moveLeft)
    alias Vec2 = { x : Float, y : Float }

    zero : Vec2
    zero = { x = 0.0, y = 0.0 }

    moveRight : Float -> Vec2 -> Vec2
    moveRight v { x, y } = { x = x + v, y }

    moveLeft : Float -> Vec2 -> Vec2
    moveLeft v { x, y } = { x = x - v, y }

module Demo.User exposing (User)
    alias User = { name : String, age : Int, status : Status }
    type Status = Active { lastSeen: Float } | Inactive Reason
    type Reason = Banned | Deleted

    make : String -> Int -> User
    make name age =
        {
            name,
            age,
            status =
                if age < 18 then
                    Inactive Deleted
                else
                    Active { lastSeen = now {} }
        }

    ban : User -> User
    ban user = { user | status = Inactive Banned }

    loginMessage : User -> String
    loginMessage user =
        when user.status is
            Active { lastSeen } ->
                "Welcome back, " + user.name + "! Last seen: " + Float.toString lastSeen

            Inactive Banned -> "Your account has been banned."
            Inactive Deleted -> "This account has been deleted."

```
