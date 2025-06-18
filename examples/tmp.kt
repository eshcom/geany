fun main() {
	val r = sequenceOf(
		1.0 to 3.0,
		2.0 to 6.0,
		3.0 to 9.0,
		4.0 to 11.8
	).simpleRegression()
	
	Double fsdfsdf = 2
	
	println(r.slope)           // 2.9400000000000004
	println(r.meanSquareError) // 0.006000000000000227
	println(r.predict(5.0)).   // 14.8
}
