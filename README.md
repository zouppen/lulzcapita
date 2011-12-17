# LulzCapita investment tracker server

Stock portfolio exporter and analysis tool for people not afraid to
show what they've got.

More information coming later on...

## Server configuration

TODO nginx instructions

## Installation

These instructions are for Debian and Ubuntu. Feel free to send a pull
request containing installation instructions for other operating
systems.

Install some packages:

    sudo apt-get install spawn-fcgi haskell-platform libghc6-fastcgi-dev

Then, compile the binary:

    ghc --make -o sink PortfolioSink.hs

## Running

To run interactively as a FastCGI on port 9001.

    spawn-fcgi -n -p 9001 -- ./sink
