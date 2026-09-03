my_var1 <- 30 #la variable es numérica. 
my_var2 <- "Sally" #la variable es un texto.
#numeric
x1 <- 10.5
class (x1)
#integer
x2 <- 1000L
class (x2)
#complex
x3 <- 9i+3
class (x3)
#String/character
x4 <- "R is exciting"
class (x4)
#Boolean/logical
x5 <- TRUE
class (x5)
#Numbers: variables of number types are created when you assign a value to them. 
x6 <- 10.5 #numeric
y1 <- 10L #integer (enteros)
z <- 1i #complex
#Numeric: 
x6 <- 10.5
y2 <- 55
#Print values of x and y.
x6
y2
#Print the class name of x and y.
class (x6)
class (y2)
#Integer:
x7 <- 1000L
y3 <- 55L
#Print values of x and y. 
x7
y3
#Print the class name of x and y. 
class (x7)
class (y3)
#Complex:
x8 <- 3+5i
y4 <- 5i
#Print values of x and y. 
x8
y4
#Print the class name of x and y. 
class (x8)
class (y4)
#Type Conversion. 
#Convert from integer to numeric.
a <- as.numeric (x7)
#Convert form numeric to integer.
b <- as.integer (x6)
#Print values of x7 and x6.
x7
x6
#Print the class name of a and b.
class (a)
class (b)
#Strings
"Hello"
'Hello'
str <- "Hello"
str #Print the value of str.
#Multiline Strings.
str1 <- "Lorem ipsum dolor sit amet,
consectetur adipiscing elit,
sed do eiusmod tempor incididunt
ut labore et dolore magna aliqua."
str1 #Print the value of str1. 
cat (str1)
#string Length.
str2 <- "Hello World"
nchar (str2)
#Check a String
grepl ("H", str2)
grepl ("Hello", str2)
grepl ("X", str2)
#Combine two strings
str3 <- "World"
paste (str,str3)
#Booleans/Logical values
10 > 9 #True 
10 == 9 #False
10 < 9 #False
#Compare two variables
c <- 10
d <- 9
c > d
#If statement
e <- 200
f <- 33
if (f > e){print ("f is greater than e")} else {print("f is not greater than e")}
#Nested if statements
g <- 41
if (g>10) {print("above 10")
if (g>20) {print("and also above 20")
 } else {
    print ("but not above 20")}
}else {print ("below 10")}
#Operators
10 + 5
#Assignment Operators
my_var3 <- 3
my_var <<- 3
3 -> my_var3
my_var #print my var
#Comparison operators
c == d #equal
c != d #not equal
c > d #greater than
c < d #Less than
c >= d #greater than or equal to
c <= d #less than or equal to
#While loops
i <- 1
while (i < 6) {
  print(i)
  i<- i+1
}
#Break
while (i < 6) {
  print (i)
  i<- i+1
  if (i==4) {
    break
  }
}
#Next Statement
while (i<6){
  i<- i+1
  if (i==3) {
    next
  }
  print (i)
}
#if ... else combined with a while loop
dice <- 1
while (dice <= 6){
  if (dice<6){
    print("No yahtzee")
  } else {
    print("Yahtzee")
  }
  dice <- dice +1
}
# for loops
for (x in 1:10) {
  print (x)
}
#print items in a list
fruits<- list("apple", "banana", "cherry")
for (x in fruits) {
  print(x)
}
#Print the number of dices
for (x in dice) {
  print (x)
}
#Break
fruits <- list("apple", "banana", "cherry")

for (x in fruits) {
  if (x == "cherry") {
    break
  }
  print(x)
}
#Next
for (x in fruits){
  if (x == "banana") {
    next
  }
  print (x)
}
#If ... else combined with a for loop
dice <- 1:6
for (x in dice){
  if (x == 6){
    print(paste ("The number is", x, "Yahtzee"))
  } else {
    print(paste("The dice number is",x, "Not Yahtzee"))
  }
}
#Vectors
#Vector of stings
fruits <- c("banana", "apple", "orange")
#Print fruits
fruits
#Vector of numerical values
numbers <- c(1,2,3)
#Print numbers
numbers
#Vector with numerical values in a sequence
numbers2 <- 1:10
numbers2
#Vector with numerical decimals in a sequence
numbers3 <- 1.5:6.5
numbers3
#Vector with numerical decimals in a sequence where the last element is not used
numbers4 <- 1.5:6.3
numbers4
# Vector of logical values
log_values <- c(TRUE, FALSE, TRUE, FALSE)
log_values
#Vector Length
fruits <- c("banana", "apple", "orange")
length(fruits)
#sort a vector
fruits <- c("banana", "apple", "orange", "mango", "lemon")
numbers <- c(13, 3, 5, 7, 20, 2)
sort(fruits)  
sort(numbers) 
#Access vectors
fruits[1]#access the first item
#Access multiple elements
fruits [c(1,3)]
#Access all items except...
fruits [c(-1)]
#Change an item
fruits [1] <- "pear"
fruits
#Repeat vectors (each value)
repeat_each <- rep(c(1,2,3), each = 3)
repeat_each
#Repeat vectors (the sequence)
repeat_times <- rep(c(1,2,3), times = 3)
repeat_times
#Repeat vectors (each value independently)
repeat_indepent <- rep(c(1,2,3), times = c(5,2,1))
repeat_indepent
#Generating Sequenced Vectors
numbers <- seq(from = 0, to = 100, by = 20)
numbers
#Lists
# List of strings
thislist <- list("apple", "banana", "cherry")
# Print the list
thislist
#Access list
thislist <- list("apple", "banana", "cherry")
thislist[1]
#Change item value
thislist[1] <- "blackcurrant"
# Print the updated list
thislist
#List length
length(thislist)
#Check if item exists
"apple" %in% thislist
#Add list items
append(thislist, "orange")
#Add item after x
append(thislist, "orange", after = 2)
#Remove list items
newlist <- thislist[-1]
# Print the new list
newlist
#Range of indexes
thislist2 <- list("apple", "banana", "cherry", "orange", "kiwi", "melon", "mango")
(thislist2)[2:5]
#loop through a list
thislist <- list("apple", "banana", "cherry")
for (x in thislist) {
  print(x)
  }
#Join two lists
list1 <- list("a", "b", "c")
list2 <- list(1,2,3)
list3 <- c(list1,list2)
list3
#Matrices
#Create a Matrix
matrix1 <- matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2)
matrix1
#Matrix with strings
matrix2 <- matrix(c("apple", "banana", "cherry", "orange"), nrow = 2, ncol =2 )
matrix2
#Access items
matrix2 [1,2]
#Access rows
matrix2 [2,]
#Access columns
matrix2 [,2]
#Access more than one row
matrix3 <- matrix(c("apple", "banana", "cherry", "orange","grape", "pineapple", "pear", "melon", "fig"), nrow = 3, ncol = 3)
matrix3 [c(1,2),]
#Access more than one column
matrix3 [,c(1,2)]
#Add rows and columns
matrix4 <- cbind(matrix3, c("strawberry","blueberry", "raspberry"))
matrix4
matrix5 <- rbind(matrix4, c("strawberry", "blueberry", "raspberry", "apple"))
matrix5
#Remove rows and columns
matrix6 <- matrix5 [-c(1),-c(1)]
matrix6
#Check if an item is in the matrix
"apple" %in% matrix6
#Number of rows and columns
dim(matrix6)
#Matrix length
length (matrix6)
#Loop through a matrix
for (rows in 1: nrow (matrix2)) {
  for (columns in 1: ncol (matrix2)) {
    print (matrix2 [ rows, columns])
  }
}
#Combine two matrices
matrix7 <- matrix(c("apple", "banana", "cherry", "grape"), nrow = 2, ncol = 2)
matrix8 <- matrix(c("orange", "mango", "pineapple", "watermelon"), nrow = 2, ncol = 2)
# Adding it as a rows
matrix_combined1 <- rbind(matrix7, matrix8)
matrix_combined1
# Adding it as a columns
matrix_combined2 <- cbind(matrix7, matrix8)
matrix_combined2
#Arrays
#Arrays with one dimension
array1 <- c(1:24)
array1
#Arrays with more than one dimension
array2 <- array( array1, dim = c(4,3,2))
array2
#Access Array items
array2 [2,3,2]
#Access arrays by using c
array2 <- array(array1, dim = c(4,3,2))
array2 [,c(1),1]
#Check if an item exists
2 %in% array2
#Amount of rows and columns
dim(array2)
#Array length
length (array2)
#Loop through the array
for (x in array2) {
  print (x)
}
#Data frames
#Create a data frame
df <- data.frame(
  training = c("strenght", "stamina", "other"),
  pulse = c(100, 150, 120),
  Duration = c( 60, 30, 45)
)
df
#Summarize the data
summary(df)
#Access items
df [1]
df [["training"]]
df$training
#Add rows
df1 <- rbind(df, c("strength", 110, 110))
df1
#Add columns
df2 <- cbind(df, steps = c(1000, 6000, 2000))
df2
#Remove rows and columns
df3 <- df2[-c(1), -c(1)]
df3
#Amount of rows and columns
dim(df3)
#Data frame length
length (df3)
#Combining data frames
df4 <- data.frame (
  Training = c("Strength", "Stamina", "Other"),
  Pulse = c(100, 150, 120),
  Duration = c(60, 30, 45))

df5 <- data.frame (
  Training = c("Stamina", "Stamina", "Strength"),
  Pulse = c(140, 150, 160),
  Duration = c(30, 30, 20)
)
df6 <- rbind( df4, df5)
#Combine data frames horizontally
df7 <- cbind(df5, df4)
df7
#Factors
music_genre <- factor (c("jazz", "rock", "classic","classic", "pop", "jazz", "rock", "jazz"))
music_genre
levels(music_genre)
music_genre <- factor(c("Jazz", "Rock", "Classic", "Classic", "Pop", "Jazz", "Rock", "Jazz"), levels = c("Classic", "Jazz", "Pop", "Rock", "Other"))
levels(music_genre)
#Factor length
length (music_genre)
#Access factors
music_genre [3]
#Change value item
music_genre[3] <- "Pop"
music_genre[3]