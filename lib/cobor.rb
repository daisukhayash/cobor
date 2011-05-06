libdir = File.dirname(__FILE__)
$LOAD_PATH.unshift(libdir) unless $LOAD_PATH.include?(libdir)

require 'cobor/type'
require 'cobor/record'
require 'cobor/recordset'
# require 'cobor/invoker'
