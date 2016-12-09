#!/bin/bash
echo "Please enter file name: "
read fileName
sapphire_dir="SapphireExecutables"
scala_dir="ScalaExecutables"
if [ ! -f $sapphire_dir/$fileName ]; then
  echo "The specified file was not found"
  exit 0
fi
echo "Processing..."
echo ""
./Sapphire $fileName
fileNameScala=${fileName%.*}
scalaFileType=".scala"
scalaReady="$fileNameScala$scalaFileType"
scalac -d ScalaExecutables $scala_dir/$scalaReady
echo ""
echo "Running Scala executable..."
echo ""
scala -cp ScalaExecutables $fileNameScala
