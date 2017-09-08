def square(x: Int) = x * x
def inverse(x: Int) = -x
def twoLargest(l: List[Int]) = l.sortBy(inverse) take 2
def sumOfSquaresOfTwoLargest(l: List[Int]) = twoLargest(l).map(square).sum
