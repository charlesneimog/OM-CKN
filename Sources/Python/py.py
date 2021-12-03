#! /usr/bin/python

# -*- Mode: Python -*-
# -*- coding: UTF-8 -*-
# Copyright (C) 2009 by Artur Ventura
#
# File: pnil.py
# Time-stamp: Tue Sep 15 15:02:12 2009
#
# Author: Artur Ventura
#
import compiler
import sys
from pprint import pprint
from copy import copy

nodeDispatcher = {}
def register(node,handler):
    nodeDispatcher[node] = handler
    
def dispatch(node):
    if node.__class__.__name__ not in nodeDispatcher:
        raise Exception("node '%s' has not a disptacher associated"% node.__class__.__name__ )
    else:
        return nodeDispatcher[node.__class__.__name__](node)

def respondsTo(string):
    return lambda x: register(string,x); return x

@respondsTo("Return")
def returnDispatcher (node):
    return "(return %s)" % dispatch(node.value)

@respondsTo("Mul")
def mulDispatcher(node):
    return "(* %s %s)" % (dispatch(node.left), dispatch(node.right))

@respondsTo("Name")
def nameDisptacher(node):
    return node.name

@respondsTo("Const")
def nameDisptacher(node):
    return node.value

@respondsTo("Function")
def funcDisp(node):
    result = ""
    for stmt in node.code:
        result = dispatch(stmt)
    return "(defun %s (%s) %s)" % (node.name, reduce(lambda x,y: "%s %s" % (x,y), node.argnames), result)

def main():
    s = sys.argv[1]
    ast = compiler.parseFile(s)
    if ast.doc:
        o = copy(ast.doc)
        print ("# " + o.replace("\n","\n;; "))
    for i in ast.node:
        print (dispatch(i))


if __name__ == "__main__":
    main()


