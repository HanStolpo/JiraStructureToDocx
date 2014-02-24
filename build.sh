#!/bin/bash
echo Building
cabal install
echo Copying build output
cp -uv ../sandbox/bin/JiraStructureToDocx.exe ../bin
