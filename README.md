# Ensembl2Symbol

Convert Ensembl IDs to gene symbols

Author: Min Zhang (min_z at outlook.com)

Liscence: BSD3

## installation

Ensembl2Symbol is written in Haskell using stack for package management. It can be easily installed in different systems.

### pull most updated version from git

```bash
git clone https://github.com/Min-/Ensembl2Symbol.git
cd Ensembl2Symbol
```
### install Ensembl2Symbol through stack

```bash
stack setup && stack build
```

### test Ensembl2Symbol installation, output file will be located in the same location as input with .symbol.txt suffix
```bash
stack exec Ensembl2Symbol mm9 inputfile
```
### Notes

May 24, 2017
current exec is running on Linux Ubuntu, not on my Macbook Pro
need an approch to make both work

Mar 27, 2020
It works on Mac as well now.
