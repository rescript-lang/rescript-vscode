let myVar = true

// let myFunc = m
//               ^co2

type rec someVariant = One | Two | Three(bool, option<someVariant>)

// let myFunc: someVariant = O
//                            ^co2

// let myFunc: someVariant = Three(t)
//                                  ^co2

// let myFunc: someVariant = Three(true, S)
//                                        ^co2

// let myFunc: someVariant = Three(true, Some(O))
//                                             ^co2

type nestedRecord = {
  on: bool,
  off?: bool,
  maybeVariant?: someVariant,
}

type someRecord = {nested: option<nestedRecord>, variant: someVariant}

// let myFunc: someRecord = {}
//                           ^co2

// let myFunc: someRecord = {n}
//                            ^co2

// let myFunc: someRecord = {variant: O}
//                                     ^co2

// let myFunc: someRecord = {nested: {maybeVariant: Three(false, t)}}
//                                                                ^co2

// let myFunc: someRecord = {nested: {maybeVariant: One}, variant: }
//                                                                ^co2

// let myFunc: someRecord = {nested: {maybeVariant: One, }}
//                                                      ^co2

// let myFunc: someRecord = {nested: {maybeVariant: One}, }
//                                                       ^co2
