snacka
=====

An OTP application

Build
-----

    $ rebar3 compile
    $ rebar3 shell
    > application:ensure_all_started(snacka).    


Cassandra
-----

    $ docker run --name snacka-cassandra -p 9042:9042 -p 9060:9060 -d cassandra