#!/bin/bash
echo Building
cabal build
echo Copying build output
cp -uv ./dist/build/JiraStructureToDocx/JiraStructureToDocx.exe ./bin
