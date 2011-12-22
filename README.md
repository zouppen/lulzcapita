# LulzCapita investment tracker server

Stock portfolio exporter and analysis tool for people not afraid to
show what they've got.

More information coming later on...

## Database schema

The database contain documents with the following fields:

* <tt>\_id</tt>: The ID of the transaction, prefixed with bank id (like <tt>non_</tt>)
* <tt>portfolio</tt>: The portfolio ID, prefixed with bank id, like above
* <tt>date</tt>: Date of transaction
* <tt>type</tt>: Type of transaction. One of the following:
  * <tt>account</tt>: Deposit or withdraw, depending of the sign of sum
  * <tt>tax</tt>: Taxation.
  * <tt>sale</tt>: Purchase or sale, depending of the sign of count
  * <tt>income</tt>: Return of capital, dividents, interests
* <tt>count</tt>: In case of sales, this contains the number of stock or 
  security sold. In case of purcase, this is positive and in case of sale
  this is negative.
* <tt>isin</tt>: The ISIN of the stock in question.

In database, taxation on account interest, dividents and sales have
the type of "tax". In case of account taxation, "isin" field contains
empty string, otherwise it contains the ISIN of that stock or security that
has been taxated.

## Server configuration

TODO nginx instructions

## Installation

These instructions are for Debian and Ubuntu. Feel free to send a pull
request containing installation instructions for other operating
systems.

Install some packages:

    sudo apt-get install spawn-fcgi haskell-platform libghc6-fastcgi-dev

Then, compile the binary:

    ghc --make -o sink Sink.hs

## Running

To run interactively as a FastCGI on port 9001.

    spawn-fcgi -n -p 9001 -- ./sink
