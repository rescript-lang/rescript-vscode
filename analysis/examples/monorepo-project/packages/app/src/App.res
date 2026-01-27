/* monorepo subpackage test */
// App module - main application

let main = () => {
  let greeting = Lib.greet("World")
  Console.log(greeting)

  let sum = Lib.add(1, 2)
  Console.log("Sum: " ++ Int.toString(sum))
}

// This function is never used (dead code)
let unusedAppFunction = () => "Unused in app"

// Run main
let _ = main()
