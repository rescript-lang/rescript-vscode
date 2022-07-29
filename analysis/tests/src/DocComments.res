@ns.doc("  Doc comment with a triple-backquote example
  
  ```res example
    let a = 10
    /*
     * stuff
     */
  ```
")
let docComment1 = 12
//       ^hov

/**
  Doc comment with a triple-backquote example
  
  ```res example
    let a = 10
    /*
     * stuff
     */
  ```
*/
let docComment2 = 12
//    ^hov
