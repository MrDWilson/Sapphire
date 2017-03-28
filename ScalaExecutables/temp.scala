object tempextends App {
println("A program that prints F-C conversion.")

var lower : Int = 0
var upper : Int = 300
var step : Int = 20

var celsius : Int = 0
var fahr : Int = lower

while(fahr<=upper){
celsius = 5 * (fahr - 32) / 9
println(fahr + "," + celsius)
fahr = fahr + step
