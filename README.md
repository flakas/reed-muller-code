# Implementation of Reed-Muller code RM(m, r)

This is an implementation of Reed-Muller RM(m, r) code for Coding Theory lecture.

Lecturer: Gintaras Skersys, [http://uosis.mif.vu.lt/~skersys/](http://uosis.mif.vu.lt/~skersys/)


## Purpose

The purpose of this application is to implement RM code and demonstrate it's effectiveness by means of single vector, text and image transmission.

The current implementation could transmit any type of file, but only JPEG images are allowed in order to mitigate the risk of destroying file headers, yet still preserving a posibility for demonstration, as image pixel color values can still be distorted.

## Build

Use `sbt` to build:

`sbt assembly` will generate `/target/scala-2.11/RMCode.jar` distributable standalone JAR file.
Follow help for more usage information: `java -jar /target/scala-2.11/RMCode.jar --help`
