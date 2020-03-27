"""
min zhang
May 21, 2019

move along ID column to first, that will allow Ensembl2Symbol to work
"""

import sys

def main():
    print("usage: python moveColumns.py inputfile idColumnNumber(zero-based)")
    args = sys.argv
    inputfile = args[1]
    outputfile = inputfile + '.out'
    idCol = int(args[2])
    switchColumn(inputfile, outputfile, idCol)

def switchColumn(inputfile, outputfile, idCol):
    f = open(inputfile, 'r')

    out = []
    for line in f:
        cols = line.rstrip().split('\t')
        idname = cols[idCol]
        newcols = [idname] + cols
        out.append('\t'.join(newcols))
    f.close()

    outf = open(outputfile, 'w')
    outf.write('\n'.join(out))
    outf.close()
    print('done')


if __name__ == '__main__':
    main()