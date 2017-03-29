object math extends App {

def add(a:Int,b:Int) : Int = {

return a+b

}

def subtract(a:Int,b:Int) : Int = {

return a-b

}

def divide(a:Int,b:Int) : Int = {

return a/b

}

def multiply(a:Int,b:Int) : Int = {

return a*b

}

def mod(a:Int,b:Int) : Int = {

return a%b

}

def max(a:Int,b:Int) : Int = {

var maximum : Int = 0

if(a<b) {
maximum = b}
else if(b<a) {
maximum = a}
else {
maximum = 0}


return maximum

}

def min(a:Int,b:Int) : Int = {

var minimum : Int = 0

if(a<b) {
minimum = a}
else if(b<a) {
minimum = b}
else {
minimum = 0}


return minimum

}

println("7+4 = " + add(7,4))
println()
println("8-3 = " + subtract(8,3))
println()
println("10/5 = " + divide(10,5))
println()
println("10*5 = " + multiply(10,5))
println()
println("8%3 = " + mod(8,3))
println()
println("Max 11, 5 = " + max(11,5))
println()
println("Min 40, 75 = " + min(40, 75))

}
