# F# tokens

## Prerequisites

Paket should be installed as global cli tool:

> dotnet tool install -g Paket

## Installation

> yarn

Installs both .NET and NodeJs dependencies.
Generates `.paket/load/netstandard2.0/main.group.fsx` afterwards.

## Build

> yarn webpack

## Run

> yarn webpack-dev-server

## Docker

(in PowerShell)

> docker run -it --rm -v "${PWD}:/app" -w "/app" -p "8080:8080"  nojaf/fable:2.2 sh