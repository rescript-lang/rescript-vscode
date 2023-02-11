/***Module level documentation goes here. */

/** This type represents stuff. */
type t = {
  /** The name of the stuff.*/
  name: string,
  /** Whether stuff is online.*/
  online: bool,
}

/** Create stuff.

```rescript example
let stuff = make("My name")
```
*/
let make = name => {
  name,
  online: true,
}

/** Stuff goes offline.*/
let asOffline = (t: t) => {...t, online: false}

module SomeInnerModule = {
  /*** Another module level docstring here.*/
  type status =
    | /** If this is started or not */ Started(t) | /** Stopped? */ Stopped | /** Now idle.*/ Idle

  /** These are all the valid inputs.*/
  type validInputs = [#something | #"needs-escaping" | #withPayload(int)]

  type callback = (t, ~status: status) => unit
}

module AnotherModule = {
  /*** Mighty fine module here too!*/

  /**
  Testing what this looks like.*/
  type callback = SomeInnerModule.status => unit
}

// ^dex
