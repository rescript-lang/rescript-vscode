var fs = require('fs')
var content = fs.readFileSync("tests/src/expected/Auto.res.txt", { encoding: 'utf-8' })
console.log("content is:===")
console.log(content)
console.log("--- end of content")
let len = content.length
console.log("char count:", len)
var arr = []
for (let i = 0; i < len; i++) {
	arr.push(content.charCodeAt(i))
}
console.log("char codes:", arr.toString())
