# Enigma M3 simulator

This is an implementation of the Enigma cypher machine model M3. 
There are 5 different rotor types (I, II, III, IV, V), 2 reflectors (B or C) and a plugboard simulated.
The Enigma is a symmetrical cypher; entering cyphertext with the same key used to encrypt it will give the plaintext.
The input first passes the plugboard, then through the rotors from right to left. The reflector 'reflects' the input and it passes through the rotors from left to right, and lastly out through the plugboard again to give the encrypted/decrypted output.
Here is an online [Enigma](https://people.physik.hu-berlin.de/~palloks/js/enigma/enigma-u_v261_en.html) machine for comparison (make sure the M3 model is selected).

## How to use
The machine has to be setup first before encryption. 
First, select the rotor type (out of five possible choices) - For example entering 543 means rotor V in the leftmost position, rotor IV in the middle, and rotor III in the rightmost position.
Then, select the starting position for each rotor. This is commonly called the message key.
Using our selection above, entering ABC would mean rotor V will start at A, IV at B and III at C.
The ring selection for each rotor is next. There are 26 positions for each rotor, 1 from 26.
Entering 1 6 25 means for rotor V (the leftmost rotor) the ring setting is 1, for IV is 6, and for III is 25.
There are 2 choices of reflectors, B or C.
Lastly are the plugboard pair(s) selection. Each pair, for example A-B, means when A is entered it becomes a B, and vice versa. Enter each unique plugboard pairs as AB, SE, etc. Type QQ to finish.
Type the letter you want to encrypt or decrypt. Rotors - shows the current rotor setting as the machine steps. 

## How to test?

Used QuickCheck to test properties of critical functions like passRotor, which performs the actual transformation of the letter to its corresponding encrypted letter.
Used the online Enigma machine to encrypt inputs and check if this implementation can decrypt them.

# EMURGO Academy Haskell Course: Solo Project

This repository contains a preconfigured Haskell development environment, allowing you to start coding in Haskell with zero installation using [Gitpod](https://www.gitpod.io/) and a browser-based version of VS Code.

The environment contains a skeleton for a simple Haskell project, designed as a starter for the solo project of EMURGO Academy's Haskell course.

## Create Your Environment

1. Fork this repository
2. Copy the link to your new repository and prefix it with "https://gitpod.io/#" in your browser
3. Click `Continue with GitHub` and `Authorize gitpod-io`
4. Wait for the environment to build. This can take a while the first time.
5. Select "VS Code Browser" as your editor.

## Build Your Project

This template contains a basic structure for a simple Haskell project. Add your project code to the empty `*.hs` files:

* `src/Lib.hs`: this module is intended to contain the core business logic of your program - it should consist of pure functions (not IO actions). You can rename this or create additional modules as needed (be sure to adjust the `other-modules` section of `emurgo-project.cabal` to reflect any changes/additions)
* `src/Types.hs`: use this module to define any custom types and synonyms that you'll use in other modules
* `src/Actions.hs`: this module should contain helper IO actions that will be used inside the `main` action of `app/Main.hs`. Most of your effectful code should live in this module.
* `app/Main.hs`: compose a minimal `main` action using helper actions defined in `src/Actions.hs` to run your application (this is what will be used if you )

Add any additional packages you need for your project below `base` in the `build-depends` section of `emurgo-project.cabal`. Follow instructions if you encounter any errors due to a "hidden package": these refer to packages that are part of the standard library but aren't imported into a Haskell project by default. The editor tooling will identify the name of the package you need to add to `build-depends` in such cases.

Use `cabal repl` in the terminal and the `:l` command followed by a specific module name (`Lib`, `Types`, `Actions`, etc.) to test your code.

Use `cabal run` to run your completed program.

As you complete the assignments, stage, commit and push your changes to Github using the `Source Control` tab in the left panel.

