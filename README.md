# MET-CS-767

Term Project for MetCS 767 Machine Learning Fall 2017

## Requirements 
- scala 2.12.3 (or higher) 
- sbt 0.13.8 (or higher)

## Building and Testing the project 
Navigate to root directory and type: 
> sbt clean compile \
> sbt test 

## Running the Experiment
Experimentation is broken down into 3 parts
- Generating training data 
- Training the Neural Net 
- Testing the Neural Net

### Generating Training Data
Go to DataGenerator.scala and run the main method, takes no command line arguments

Modifiable parameters: 
- Starting state (line 45). Can use any of the other test cases 1 through 4. Default is testCase4. 
- Number of iterations (line 46). Default is 500
- Number of turns before ending a game (line 52). Default is 10

```
45    val pieces = testCase4
46    for(i <- 1 to 500) {
47      var gameOver = false
48     var turn: Color = White
49      println(i)
50      while (!gameOver && turn == White) {
51        val game = new ChessGame(pieces, White)
52        game.runGame(10)
53        turn = game.turn
54        gameOver = game.isGameOver
55      }
56    }
```

### Training/Testing the Neural Net
Go to MainExperiment.scala and run the main method, takes no command line arguments.

Modifiable parameters: 

- Starting state (line 11). Can use any of the other test cases 1 through 4. Defaults to testCase4

```
7   def main(args: Array[String]) {
8     val model = new InferenceModel
9     val s = "case_specific_training_sets/final"
10    model.train("training_data.csv")
11    val pieces = DataGenerator.testCase4
12    val game = new ChessGame(pieces, White)
13    game.runGame(10, Some(model))
14    println(game.numMoves)
15    val move = model.computeMoveVector(StateVector(pieces, White))
16    println(move.toReadableMove)
17  }
```





