# Haskell Net

See a demo of the library [here](https://kennedymj97.github.io/haskell-net/frontend/index.html).

For more information about the project please read [the report](https://raw.githubusercontent.com/kennedymj97/haskell-net/master/haskell-net.pdf).

This README will briefly explain the repository structure and detail which code was written from scratch, adapted and what is third party code. Any files not mentioned here should not be important.

## /frontend

This folder contains all of the code used to produce the front-end. All the code here was written from scratch, parts of the canvas.js code borrowed from stack overflow. Most of the files can be ignored. The import to note are:

* src/Main.elm
    - This file contains the application logic. It is the only file that uses Elm.
    - Main.elm compiles to main.js in the route directory so do not try to read that.
* canvas.js
    - This file contains the logic for drawing a number in the box. It will send data through a port to Main.elm.
* static/css/styles.css
    - The styling for the front-end.
* index.html
    - Entry point for the front-end. Collates all the above files into an html document.

## /backend/data

Contains datasets used to train and test the network code. Is not of import for viewing. Also contains some pretrained networks.

## /backend/src/Data

Some code to generate and read from datasets to train and test the network. Not too important to look at.

## /backend/src/HaskellNet

This contains the code for the neural network library. Much of it is written from scratch. The Networks.hs file used [Justin Le's blog](https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html) as a foundation, the serialization code is exactly as appears in the post, I believe everything else has been adapted. Connections.hs was also adapted from the blog. The rest of the files were written from scratch.

* Activation.hs
    - Contains the Activations type class and a couple of pre-defined activation functions.
* Connections.hs
    - Includes the connections data type and relevant functions.
* Init.hs
    - Has the Init type class and a couple of pre-defined init functions.
* Network.hs
    - Contains the main body of code in the library.
    - Includes the Network data type and all the functions for using the networks.
* Optimiser.hs
    - Has the optimser type class and a couple of pre-defined optimiser functions.
* Params.hs
    - Includes the data type used to provide the learning parameters needed for training a network.
* Train.hs
    - Contains a couple of example functions for training networks.

## /backend/Numeric/Static/

Includes a file that contains functions that provide some extra functions to those in HMatrix's static library.

## /backend/Server

This contains the file Api.hs which includes the server code. Provides an end point which takes in JSON and converts it to a vector to run through a pretrained network. Returns the predictions to the client. Written from scratch.
