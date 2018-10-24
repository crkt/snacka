-module(parser).
-import(xmerl, [export/3]).

-export([parse/1]).

parse([]) ->
    true;
parse(_Packet) ->
    ok;
parse(_) ->
    ok.

% initial_stream() ->
%    xmerl.
%     <?xml version='1.0'?>
%      <stream:stream
%          from='juliet@im.example.com'
%          to='im.example.com'
%          version='1.0'
%          xml:lang='en'
%          xmlns='jabber:client'
%          xmlns:stream='http://etherx.jabber.org/streams'>.