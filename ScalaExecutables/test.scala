object test extends App {
var a : Int = 5
var b : Int = 7
println(a+b)
var intOne : Int = 10
var intTwo : Int = 20
var intThree : Int = 70
println(intOne + intThree)
println("I have " + intTwo + " apples!")
println(intOne/intTwo*intThree/intTwo*2)
var testOne : Int = 10
var testTwo : Int = 450
var testThree : Int = 500
var lol : Int = 0
if(testOne < testTwo) {lol = testOne}
else if(testTwo < testThree) {lol = testTwo}
else if(testThree < testOne) {lol = testThree}
else {println("Error in if statement.")}

var i : Int = 0
while(i<10){
i = i+1
println("i: " + i)
}
println(printTime(10,7))
println(lol)
def printTime(a:Int,b:Int) : Int = {
return a+b
}
}
